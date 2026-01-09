# Libraries
library(tidyverse)
library(sf)
library(bcmaps)
library(readxl)
library(bcdata)

lan_folder = "//SFP.IDIR.BCGOV/S140/S40203/WFC AEB/General/"
proj_wd = getwd()
onedrive_wd = "//SFP.IDIR.BCGOV/S140/S40203/WFC AEB/General/2 SCIENCE - Invasives/AIS_R_Projects/LargeDataFiles/"

# Data
# Copy excel results from LAN folder IF it has all the right columns. This is
# intended to save us from overwriting the local data with some garbage 
# LAN file that has replaced our goldenboy data file.
new_potential_dat_file = read_excel(paste0(lan_folder,"2 SCIENCE - Invasives/SPECIES/Whirling Disease/Monitoring/WD_sampling_results_fish_eDNA_used_for_making_maps_CMADSEN.xlsx"), sheet = "Fish and eDNA")
local_data_file = read_excel('data/WD_sampling_results_fish_eDNA_used_for_making_maps_CMADSEN.xlsx')

if(identical(names(new_potential_dat_file),names(local_data_file)) & nrow(new_potential_dat_file) == nrow(local_data_file)){
  print("New data file has identical column names and number of rows. Copying locally...")
  file.copy(from = paste0(lan_folder,"2 SCIENCE - Invasives/SPECIES/Whirling Disease/Monitoring/WD_sampling_results_fish_eDNA_used_for_making_maps_CMADSEN.xlsx"),
            to = paste0('data/WD_sampling_results_fish_eDNA_used_for_making_maps_CMADSEN.xlsx'),
            overwrite = T)
}

dat = read_excel('data/WD_sampling_results_fish_eDNA_used_for_making_maps_CMADSEN.xlsx', sheet = "Fish and eDNA")

dat = purrr::set_names(dat, snakecase::to_snake_case)

dat = dat |> filter(!is.na(lat) & !is.na(long))

# Split rows that have both eDNA and fish sampling into two rows each.
dat = dat |> 
  tidyr::separate_longer_delim(cols = sampling_method, delim = " + ")

dat = sf::st_as_sf(dat, coords = c("long","lat"), crs = 4326)

# Typo corrections etc.
dat = dat |> 
  dplyr::mutate(fish_sampling_results_q_pcr_mc_detected = ifelse(str_detect(fish_sampling_results_q_pcr_mc_detected,"Positive"),"Positive",fish_sampling_results_q_pcr_mc_detected)) |> 
  dplyr::mutate(comments = ifelse(comments == '', NA, comments))

if(file.exists('app/www/sampling_results.gpkg')) file.remove('app/www/sampling_results.gpkg')
sf::write_sf(dat, 'app/www/sampling_results.gpkg')

# --------------------------

col = sf::read_sf("//SFP.IDIR.BCGOV/S140/S40203/WFC AEB/General/2 SCIENCE - Invasives/AIS_R_Projects/CMadsen_Wdrive/shared_data_sets/Columbia_River_Big_Watershed.shp") |> 
  dplyr::summarise() |> 
  sf::st_transform(4326)

sf::write_sf(col, "app/www/columbia_watershed.gpkg")

# ---------------------------
subw = sf::read_sf("//SFP.IDIR.BCGOV/S140/S40203/WFC AEB/General/2 SCIENCE - Invasives/AIS_R_Projects/CMadsen_Wdrive/shared_data_sets/Columbia_River_Big_Watershed.shp") |> 
  sf::st_transform(4326) |> 
  dplyr::select(watershed_name = MAJOR_WA_1)

if(file.exists('app/www/subwatershed_groups.gpkg')) file.remove('app/www/subwatershed_groups.gpkg')
sf::write_sf(subw,'app/www/subwatershed_groups.gpkg')

# -------------------------------

## get the 2025 data 

dat_2025 = read_excel("data/Whirling_Disease_2025_Sample_Tracking.xlsx")

dat_2025 = purrr::set_names(dat_2025, snakecase::to_snake_case)

dat_2025 = dat_2025 |> filter(!is.na(latitude) & !is.na(longitude))

dat_2025 = dat_2025 |> 
  tidyr::separate_longer_delim(cols = sampling_method_e_dna_or_fish_or_both, delim = " and ") |> 
  rename(sampling_method = sampling_method_e_dna_or_fish_or_both)

dat_2025 <- dat_2025 |>
  mutate(
    sampling_method = case_when(
      sampling_method == "fish" ~ "Fish",
      TRUE ~ sampling_method
    )
  )
  
dat_2025 = sf::st_as_sf(dat_2025, coords = c("longitude","latitude"), crs = 4326)

if(file.exists('app/www/sampling_results_2025.gpkg')) file.remove('app/www/sampling_results_2025.gpkg')
sf::write_sf(dat_2025, 'app/www/sampling_results_2025.gpkg')
