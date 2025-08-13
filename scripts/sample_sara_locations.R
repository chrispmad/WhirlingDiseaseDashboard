library(tidyverse)
library(sf)
onedrive_wd = "//SFP.IDIR.BCGOV/S140/S40203/WFC AEB/General/2 SCIENCE - Invasives/AIS_R_Projects/LargeDataFiles/"
dat = sf::read_sf("app/www/sampling_results.gpkg")
# Read in SARA-listed species plus Sockeye Salmon (for all of BC)
dfo_sar = sf::read_sf(paste0(onedrive_wd,"CNF/dfo_sara_and_crit_hab_bulltrout_and_sockeye_data.gpkg")) |> sf::st_transform(4326)

fras = sf::read_sf(paste0(onedrive_wd,"CNF/fraser_watershed_priority_area.gpkg"))
col = sf::read_sf(paste0(onedrive_wd,"CNF/columbia_watershed_priority_area.gpkg"))
frascol = dplyr::bind_rows(fras,col) |> dplyr::summarise() |> sf::st_transform(4326)


dat = dat |> dplyr::filter(stringr::str_detect(sampled_in_2024_y_n, "^Y"))
dat = dat |> dplyr::filter(delivery_agency == "Shuswap Band")

ggplot()+
  geom_sf(data = dfo_sar, aes(color = Common_Name_EN), size = 0.5) +
  geom_sf(data = dat, aes(color = delivery_agency), size = 1.5)
  #geom_sf(data = frascol, fill = "lightblue", color = "black", alpha = 0.5) +
  
ggsave("images/sampling_results_sara.png", width = 10, height = 8, dpi = 300)
overlap <- st_intersection(dfo_sar, dat)

overlap_species <- overlap |> 
  count(Common_Name_EN) |> 
  arrange(desc(n))
