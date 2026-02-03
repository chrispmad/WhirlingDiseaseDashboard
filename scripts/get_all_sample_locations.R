library(tidyverse)
library(openxlsx)

dat = sf::read_sf("app/www/sampling_results.gpkg")


locations = dat |> 
  select(sample_site_name, geom, sampling_method)

locations = locations |>
  dplyr::mutate(
    longitude = sf::st_coordinates(locations)[, "X"],
    latitude  = sf::st_coordinates(locations)[, "Y"]
  ) |> 
  st_drop_geometry()

new_order <- c("sample_site_name", "latitude", "longitude", "sampling_method")

locations = locations[, new_order]
