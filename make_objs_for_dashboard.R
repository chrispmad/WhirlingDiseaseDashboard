# Libraries
library(tidyverse)
library(sf)
library(bcmaps)
library(readxl)
library(bcdata)

# Data
# fish = read_excel('data/2024 WD sampling results_fish_dat_MASTER LIST.xlsx', sheet = "Fish sampling")
dat = read_excel('data/2024 WD sampling results_fish_eDNA_MASTER LIST.xlsx', sheet = "eDNA")

# fish = purrr::set_names(fish, snakecase::to_snake_case)
dat = purrr::set_names(dat, snakecase::to_snake_case)

# fish = fish |> filter(!is.na(lat) & !is.na(long))
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

