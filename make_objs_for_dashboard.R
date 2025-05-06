# Libraries
library(tidyverse)
library(sf)
library(bcmaps)
library(readxl)
library(bcdata)

lan_folder = "//SFP.IDIR.BCGOV/S140/S40203/RSD_ FISH & AQUATIC HABITAT BRANCH/General/"
proj_wd = getwd()
onedrive_wd = paste0(str_extract(getwd(),"C:/Users/[A-Z]+/"),"OneDrive - Government of BC/data/")

# Data
# Copy excel results from LAN folder IF it has all the right columns. This is
# intended to save us from overwriting the local data with some garbage 
# LAN file that has replaced our goldenboy data file.
new_potential_dat_file = read_excel(paste0(lan_folder,"2 SCIENCE - Invasives/SPECIES/Whirling Disease/Monitoring/WD_sampling_results_fish_eDNA_used_for_making_maps_CMADSEN.xlsx"), sheet = "eDNA")
local_data_file = read_excel('data/WD sampling results_fish_eDNA_MASTER LIST.xlsx', sheet = "eDNA")

if(identical(names(new_potential_dat_file),names(local_data_file)) & nrow(new_potential_dat_file) == nrow(local_data_file)){
  print("New data file has identical column names and number of rows. Copying locally...")
  file.copy(from = paste0(lan_folder,"2 SCIENCE - Invasives/SPECIES/Whirling Disease/Monitoring/WD_sampling_results_fish_eDNA_used_for_making_maps_CMADSEN.xlsx"),
            to = paste0('data/WD sampling results_fish_eDNA_MASTER LIST.xlsx'),
            overwrite = T)
}

dat = read_excel('data/WD sampling results_fish_eDNA_MASTER LIST.xlsx', sheet = "eDNA")

dat = purrr::set_names(dat, snakecase::to_snake_case)

dat = dat |> filter(!is.na(lat) & !is.na(long))

# Split rows that have both eDNA and fish sampling into two rows each.
dat = dat |> 
  tidyr::separate_longer_delim(cols = sampling_method, delim = " + ")

dat = sf::st_as_sf(dat, coords = c("long","lat"), crs = 4326)

sf::write_sf(dat, 'app/www/sampling_results.gpkg')

# --------------------------

col = sf::read_sf("W:/CMadsen/shared_data_sets/Columbia_River_Big_Watershed.shp") |> 
  dplyr::summarise() |> 
  sf::st_transform(4326)

sf::write_sf(col, "app/www/columbia_watershed.gpkg")

# ---------------------------
subw = sf::read_sf("W:/CMadsen/shared_data_sets/Columbia_River_Big_Watershed.shp") |> 
  sf::st_transform(4326) |> 
  dplyr::select(watershed_name = MAJOR_WA_1)

if(file.exists('app/www/subwatershed_groups.gpkg')) file.remove('app/www/subwatershed_groups.gpkg')
sf::write_sf(subw,'app/www/subwatershed_groups.gpkg')

