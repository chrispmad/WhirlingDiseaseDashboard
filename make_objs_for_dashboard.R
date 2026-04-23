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

# dat <- dat |>
#   dplyr::mutate(date_collected = as.numeric(date_collected)) |> 
#   dplyr::mutate(
#     date_collected = dplyr::case_when(
#       is.numeric(date_collected) ~ as.Date(date_collected, origin = "1899-12-30"),
#       TRUE ~ as.Date(date_collected)
#     )
#   )

dat = dat |> filter(!is.na(lat) & !is.na(long))

# Split rows that have both eDNA and fish sampling into two rows each.
dat = dat |> 
  tidyr::separate_longer_delim(cols = sampling_method, delim = " + ")

dat = sf::st_as_sf(dat, coords = c("long","lat"), crs = 4326)

# Typo corrections etc.
dat = dat |> 
  dplyr::mutate(fish_sampling_results_q_pcr_mc_detected = ifelse(str_detect(fish_sampling_results_q_pcr_mc_detected,"Positive"),"Positive",fish_sampling_results_q_pcr_mc_detected)) |> 
  dplyr::mutate(comments = ifelse(comments == '', NA, comments))

dat = dat |> 
  filter(sampled_in_2024_y_n == "Y" | sampled_in_2024_y_n == "Y (Alternate site to Columbia River)")

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

# dat_2025_edna = read_excel("data/Whirling_Disease_2025_Sample_Tracking Final.xlsx", sheet = "eDNA with Dates")
# 
# dat_2025_fish = read_excel("data/Whirling_Disease_2025_Sample_Tracking Final.xlsx", sheet = "Fish with Dates",guess_max = 0, col_types = "text")

dat_2025_edna = read_excel("data/Results_2025_all_Phelan_J.xlsx", sheet = "eDNA")
dat_2025_fish = read_excel("data/Results_2025_all_Phelan_J.xlsx", sheet = "Fish with Dates")

dat_2025_edna = dat_2025_edna |> 
  rename(sample_name_site_name_replicate_number_batch_number = `Sample Name (Site name, replicate, batch )`)


# fix the names first
dat_2025_edna <- dat_2025_edna |>
  mutate(sample_name_site_name_replicate_number_batch_number = str_remove( sample_name_site_name_replicate_number_batch_number,
                                                                           "\\s*(#\\d+.*$|Rep\\d+$|Batch\\s*\\d+$|\\s\\d+$)"
  )) |> 
  ## remove the blanks
  filter(!str_detect(sample_name_site_name_replicate_number_batch_number, "(?i)blank")) |> 
  filter(!str_detect(sample_name_site_name_replicate_number_batch_number, "FDBL")) |> 
  mutate(sample_name_site_name_replicate_number_batch_number = str_remove(sample_name_site_name_replicate_number_batch_number, "^2025\\s+")) |> 
  mutate(sample_name_site_name_replicate_number_batch_number = str_remove(sample_name_site_name_replicate_number_batch_number, "-$")) |> 
  mutate(sample_name_site_name_replicate_number_batch_number = str_remove(sample_name_site_name_replicate_number_batch_number, "^2025_")) |> 
  mutate(sample_name_site_name_replicate_number_batch_number = str_remove(sample_name_site_name_replicate_number_batch_number, "_.*$")) |> 
  ## Looks like there are B for Blanks at the end of the String. Lets get rid of those
  mutate(sample_name_site_name_replicate_number_batch_number = str_remove(sample_name_site_name_replicate_number_batch_number, " B$")) |> 
  # Filter Size has some words in there we don't want
  mutate(`Filter Size` = str_remove(`Filter Size`, " um glass fiber$")) |> 
  mutate(`Filter Size` = str_remove(`Filter Size`, " um self preserving$")) |> 
  mutate(`Filter Size` = str_remove(`Filter Size`, " um self preserving$")) |> 
  mutate(`Filter Size` = str_remove(`Filter Size`, " um selfl-preserving$")) |> 
   ## trim the white space then
  mutate(across(where(is.character), str_trim))  




dat_2025_fish$`Date Collected` <-
  ifelse(
    grepl("^\\d+$", dat_2025_fish$`Date Collected`),  # only pure numbers
    format(
      as.Date(as.numeric(dat_2025_fish$`Date Collected`),
              origin = "1899-12-30"),
      "%d/%m/%Y"
    ),
    dat_2025_fish$`Date Collected`
  )


dat_2025_edna = dat_2025_edna |> 
  rename(Date = `DateYYYY-MM-DD`)

dat_2025_edna$Date <-
  ifelse(
    grepl("^\\d+$", dat_2025_edna$Date),  # only pure numbers
    format(
      as.Date(as.numeric(dat_2025_edna$Date),
              origin = "1899-12-30"),
      "%d/%m/%Y"
    ),
    dat_2025_edna$Date
  )

dat_2025_edna = purrr::set_names(dat_2025_edna, snakecase::to_snake_case) 

dat_2025_edna = dat_2025_edna |> filter(!is.na(lat) & !is.na(long))

dat_2025_fish = purrr::set_names(dat_2025_fish, snakecase::to_snake_case) 

dat_2025_fish = dat_2025_fish |> filter(!is.na(latitude) & !is.na(longitude))

# dat_2025_fish = dat_2025_fish |> 
#   rename(fish_sampling_results_q_pcr_mc_detected = result)

dat_2025_edna = dat_2025_edna |> 
  mutate(across(everything(), as.character))

dat_2025_fish = dat_2025_fish |> 
  mutate(across(everything(), as.character))

dat_2025_edna <- dat_2025_edna %>%
  dplyr::mutate(
    sampling_method = "eDNA"
  )

dat_2025_fish <- dat_2025_fish %>%
  dplyr::mutate(
    sampling_method = "Fish"
  )

source("./scripts/spatial_manipulations.R")

dat_2025_edna = dat_2025_edna |> 
  rename(latitude = lat, longitude = long)

dat_2025_edna = convert_coords(dat_2025_edna)


dat_2025_edna = dat_2025_edna |> 
  rename(filtration_start_time = filtration_start_time_hh_mm,
         filtration_end_time = filtration_end_time_hh_mm)



dat_2025_edna = dat_2025_edna |>
  mutate(
    filtration_start_time =
      case_when(
        # Excel fractional day
        str_detect(filtration_start_time, "^0\\.") ~
          format(
            as.POSIXct(as.numeric(filtration_start_time) * 86400,
                       origin = "1899-12-30", tz = "UTC"),
            "%H:%M"
          ),
        
        # Times with am/pm
        str_detect(filtration_start_time, "(?i)am|pm") ~
          format(parse_date_time(filtration_start_time, "I:M p"), "%H:%M"),
        
        # Plain HH:MM
        str_detect(filtration_start_time, "^\\d{1,2}:\\d{2}$") ~
          format(parse_date_time(filtration_start_time, "H:M"), "%H:%M"),
        
        TRUE ~ NA_character_
      )
  ) |> 
  mutate(
    filtration_end_time =
      case_when(
        # Excel fractional day
        str_detect(filtration_end_time, "^0\\.") ~
          format(
            as.POSIXct(as.numeric(filtration_end_time) * 86400,
                       origin = "1899-12-30", tz = "UTC"),
            "%H:%M"
          ),
        
        # Times with am/pm
        str_detect(filtration_end_time, "(?i)am|pm") ~
          format(parse_date_time(filtration_end_time, "I:M p"), "%H:%M"),
        
        # Plain HH:MM
        str_detect(filtration_end_time, "^\\d{1,2}:\\d{2}$") ~
          format(parse_date_time(filtration_end_time, "H:M"), "%H:%M"),
        
        TRUE ~ NA_character_
      )
  )




dat_2025_edna_collapsed <- dat_2025_edna |>
  group_by(
    waterbody,
    sample_name_site_name_replicate_number_batch_number,
    date,
    batch_no,
    delivery
  ) |>
  summarise(
    latitude = first(latitude),
    longitude = first(longitude),
    name_s_of_people_collecting_sample = first(name_s_of_people_collecting_sample),
    comments = paste(comments, collapse = ", "),
    e_dna_result = first(e_dna_result),
    sampling_method = first(sampling_method),
    
    filtration_start_time = paste(filtration_start_time, collapse = ", "),
    filtration_end_time = paste(filtration_end_time, collapse = ", "),
    total_volume_filtered_l = paste(total_volume_filtered_l, collapse = ", "),
    filter_size = paste(filter_size, collapse = ", "),
    
    .groups = "drop"
  )

dat_2025_edna_collapsed = dat_2025_edna_collapsed |> 
  mutate(across(everything(), as.character))

dat_2025_fish = dat_2025_fish |> 
  mutate(across(everything(), as.character))

## before we join, we should name the columns appropriately
dat_2025_edna_collapsed = dat_2025_edna_collapsed |> 
  rename(waterbody_name = waterbody, final_sample_site_name = sample_name_site_name_replicate_number_batch_number,
         date_collected = date, sampling_organization = delivery, result_mc = e_dna_result)





dat_2025_fish = dat_2025_fish |> 
  rename(fish_sampling_results_q_pcr_mc_detected = result)


dat_2025 = bind_rows(dat_2025_edna_collapsed, dat_2025_fish)




# dat_2025 = dat_2025 |> 
#   tidyr::separate_longer_delim(cols = sampling_method_e_dna_or_fish_or_both, delim = " and ") |> 
#   rename(sampling_method = sampling_method_e_dna_or_fish_or_both)

dat_2025 <- dat_2025 |>
  mutate(
    sampling_method = case_when(
      sampling_method == "fish" ~ "Fish",
      TRUE ~ sampling_method
    )
  )

### There are 2 NA value for Vaseux Lake - we will drop them as the comments say that the lake was not sampled?
dat_2025 <- dat_2025 |>
  dplyr::mutate(
    latitude  = as.numeric(trimws(latitude)),
    longitude = as.numeric(trimws(longitude))
  ) |>
  dplyr::filter(
    (!is.na(latitude) & !is.na(longitude))
  )
  
dat_2025 = sf::st_as_sf(dat_2025, coords = c("longitude","latitude"), crs = 4326)

dat_2025 = dat_2025 |> 
  distinct()



if(file.exists('app/www/sampling_results_2025.gpkg')) file.remove('app/www/sampling_results_2025.gpkg')
sf::write_sf(dat_2025, 'app/www/sampling_results_2025.gpkg')
