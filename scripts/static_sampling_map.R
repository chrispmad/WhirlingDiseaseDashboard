library(tidyverse)
library(sf)
library(dplyr)
library(ggplot2)
library(basemaps)
library(ggspatial)
library(ggmap)
library(tibble)

dat = sf::read_sf("app/www/sampling_results.gpkg")
col = sf::read_sf("app/www/columbia_watershed.gpkg")
subw = sf::read_sf("app/www/subwatershed_groups.gpkg")
dat_2025 = sf::read_sf("app/www/sampling_results_2025.gpkg")


# Remove sites that weren't sampled.
dat = dat |> dplyr::filter(stringr::str_detect(sampled_in_2024_y_n, "^Y"))

# Convert levels for eDNA to 'Positive' and 'Negative'
# unique(dat$e_dna_results_mc)
# unique(dat$e_dna_results_tubifex)
# unique(dat$fish_sampling_results_q_pcr_mc_detected)
dat = dat |> 
  dplyr::mutate(e_dna_results_mc = dplyr::case_when(
    e_dna_results_mc == "Not Detected" ~ "Negative",
    !is.na(e_dna_results_mc) ~ "Positive",
    T ~ NA
  )) |> 
  dplyr::mutate(e_dna_results_tubifex = dplyr::case_when(
    e_dna_results_tubifex == "Not Detected" ~ "Negative",
    !is.na(e_dna_results_tubifex) ~ "Positive",
    T ~ NA
  ))

# unique(dat$e_dna_results_mc)
# unique(dat$e_dna_results_tubifex)
# unique(dat$fish_sampling_results_q_pcr_mc_detected)

# # Drop rows where sample result is NA for eDNA and fish
# dat = dat |> dplyr::filter(!(is.na(e_dna_results_mc) & is.na(e_dna_results_tubifex) & is.na(fish_sampling_results_q_pcr_mc_detected)))
# Replace true NA with 'NA' for rows with NO results but that claim to have been sampled. 
dat_no_results = dat |> 
  dplyr::filter((is.na(e_dna_results_mc) & is.na(e_dna_results_tubifex) & is.na(fish_sampling_results_q_pcr_mc_detected))) |> 
  dplyr::mutate(e_dna_results_mc = "NA",
                e_dna_results_tubifex = "NA",
                fish_sampling_results_q_pcr_mc_detected = "NA")

dat = dat |> 
  dplyr::filter(!(is.na(e_dna_results_mc) & is.na(e_dna_results_tubifex) & is.na(fish_sampling_results_q_pcr_mc_detected))) #|> 
# dplyr::bind_rows(dat_no_results)

rm(dat_no_results)

# Temporary correction to highlight NAs in dataset:
dat = dat |> dplyr::mutate(sampling_method = tidyr::replace_na(sampling_method, "NA"))

# Relabel the geometry column.
dat = sf::st_set_geometry(dat, "geom")

# Assign a colour based on sampling type, and also a colour based on test result.
dat = dat |> 
  dplyr::mutate(sample_type_colour = dplyr::case_when(
    sampling_method == "eDNA" ~ 'gold',
    sampling_method == "Fish" ~ 'salmon',
    # sampling_method == "Fish + eDNA" ~ 'gold',
    T ~ 'black'
  )) |> 
  dplyr::mutate(fish_results_colour = dplyr::case_when(
    fish_sampling_results_q_pcr_mc_detected == "Negative" ~ 'lightgreen',
    fish_sampling_results_q_pcr_mc_detected == "Positive" ~ 'purple',
    fish_sampling_results_q_pcr_mc_detected == "Pending" ~ 'pink',
    T ~ 'black'
  )) |> 
  dplyr::mutate(e_dna_myx_colour = ifelse(e_dna_results_mc == 'Positive', '#F97912', '#612073')) |> 
  dplyr::mutate(e_dna_tubifex_colour = ifelse(e_dna_results_tubifex == "Positive", "#F97912", "#612073"))

dat = dat |> 
  dplyr::mutate(e_dna_results_tubifex = ifelse(e_dna_results_tubifex == "Positive", "Present", "Absent"))

dat = dat |> 
  dplyr::mutate(
    delivery_agency = dplyr::case_when(
      delivery_agency == "ONA (Eleanor)" ~ "ONA",
      delivery_agency == "ONA (Sam)" ~ "ONA",
      delivery_agency == "WLRS - AEB fisheries" ~ "WLRS",
      delivery_agency == "WLRS (COS AIS inspectors)" ~ "WLRS",
      delivery_agency == "WLRS - region 3" ~ "WLRS",
      delivery_agency == "WLRS (headquarters)" ~ "WLRS",
      delivery_agency == "WLRS - Kootenay region" ~ "WLRS",
      delivery_agency == "WLRS Cultus" ~ "WLRS",
      delivery_agency == "WLRS Region 2" ~ "WLRS",
      TRUE ~ delivery_agency
    )
  )


#-----------------------------------------------------------------------------------------------------------


# 
# dat_2025 = dat_2025 |> dplyr::filter(stringr::str_detect(confirmed_to_have_been_sampled_in_2025_y_n, "^Y"))
# 


dat_2025 = dat_2025 |> 
  dplyr::rename(reach = sub_watershed_reach_name, sample_site_name = final_sample_site_name,
                delivery_agency = sampling_organization, sampled_in_2025_y_n = confirmed_to_have_been_sampled_in_2025_y_n,
                e_dna_results_mc = result_mc)

dat_2025$e_dna_results_tubifex = NA

# dat_2025 = dat_2025 |> 
#   dplyr::mutate(e_dna_results_mc = dplyr::case_when(
#     e_dna_results_mc == "Not Detected" ~ "Negative",
#     !is.na(e_dna_results_mc) ~ "Positive",
#     T ~ NA
#   )) |> 
#   dplyr::mutate(e_dna_results_tubifex = dplyr::case_when(
#     e_dna_results_tubifex == "Not Detected" ~ "Negative",
#     !is.na(e_dna_results_tubifex) ~ "Positive",
#     T ~ NA
#   ))

dat_no_results_2025 = dat_2025 |> 
  dplyr::filter((is.na(e_dna_results_mc) & is.na(e_dna_results_tubifex) & is.na(fish_sampling_results_q_pcr_mc_detected))) |> 
  dplyr::mutate(e_dna_results_mc = "NA",
                e_dna_results_tubifex = "NA",
                fish_sampling_results_q_pcr_mc_detected = "NA")

# dat_2025 = dat_2025 |>
#   dplyr::filter(!(is.na(e_dna_results_mc) & is.na(e_dna_results_tubifex) & is.na(fish_sampling_results_q_pcr_mc_detected))) #|>
# dplyr::bind_rows(dat_no_results)

## Placeholder as this is not in the results
dat_2025 = dat_2025 |> 
  dplyr::rename(fish_species_sampled = fish_species)


dat_2025 = dat_2025 |> 
  dplyr::mutate(sample_type_colour = dplyr::case_when(
    sampling_method == "eDNA" ~ 'gold',
    sampling_method == "Fish" ~ 'salmon',
    # sampling_method == "Fish + eDNA" ~ 'gold',
    T ~ 'black'
  )) |> 
  dplyr::mutate(fish_results_colour = dplyr::case_when(
    fish_sampling_results_q_pcr_mc_detected == "Negative" ~ 'lightgreen',
    fish_sampling_results_q_pcr_mc_detected == "Positive" ~ 'purple',
    fish_sampling_results_q_pcr_mc_detected == "Pending" ~ 'pink',
    T ~ 'black'
  )) |> 
  dplyr::mutate(e_dna_myx_colour = ifelse(e_dna_results_mc == 'Positive', '#F97912', '#612073')) |> 
  dplyr::mutate(e_dna_tubifex_colour = ifelse(e_dna_results_tubifex == "Positive", "#F97912", "#612073"))

dat_2025 = dat_2025 |> 
  dplyr::mutate(e_dna_results_tubifex = ifelse(e_dna_results_tubifex == "Positive", "Present", "Absent"))

dat_2025 = dat_2025 |> 
  dplyr::mutate(
    delivery_agency = dplyr::case_when(
      delivery_agency == "ONA (Eleanor)" ~ "ONA",
      delivery_agency == "ONA (Sam)" ~ "ONA",
      delivery_agency == "WLRS - AEB fisheries" ~ "WLRS",
      delivery_agency == "WLRS (COS AIS inspectors)" ~ "WLRS",
      delivery_agency == "WLRS - region 3" ~ "WLRS",
      delivery_agency == "WLRS (headquarters)" ~ "WLRS",
      delivery_agency == "WLRS - Kootenay region" ~ "WLRS",
      delivery_agency == "WLRS Cultus" ~ "WLRS",
      delivery_agency == "WLRS Region 2" ~ "WLRS",
      TRUE ~ delivery_agency
    )
  )

#------------
# New request - no more tubifex

dat = dat |> 
  dplyr::select(-e_dna_results_tubifex) |> 
  dplyr::mutate(Year = 2024)

dat_2025 = dat_2025 |> 
  dplyr::select(-e_dna_results_tubifex) |> 
  dplyr::mutate(Year = 2025)


dat = dat |> 
  dplyr::mutate(date_collected = as.character(date_collected))
#---- combine the data ------#
dat_all = dplyr::bind_rows(dat,dat_2025) 

dat_all = dat_all |> 
  dplyr::select(-c(original_lat,original_long))

dat_all = dat_all |> 
  dplyr::mutate(
    delivery_agency = dplyr::case_when(
      delivery_agency == "ONA (Eleanor)" ~ "ONA",
      delivery_agency == "ONA (Sam)" ~ "ONA",
      delivery_agency == "WLRS - AEB fisheries" ~ "WLRS",
      delivery_agency == "WLRS (COS AIS inspectors)" ~ "WLRS",
      delivery_agency == "WLRS - region 3" ~ "WLRS",
      delivery_agency == "WLRS (headquarters)" ~ "WLRS",
      delivery_agency == "WLRS - Kootenay region" ~ "WLRS",
      delivery_agency == "WLRS Cultus" ~ "WLRS",
      delivery_agency == "WLRS Region 2" ~ "WLRS",
      TRUE ~ delivery_agency
    )
  )

fish_data <- dat_all |> dplyr::filter(sampling_method == "Fish")
edna_data <- dat_all |> dplyr::filter(sampling_method == "eDNA")


# get limits of dat_all
dat_all_bbox <- st_bbox(dat_all)

# Custom BC bounding box
bc_station_view <- st_bbox(
  tibble(lon = c(dat_all_bbox[1]-1, dat_all_bbox[3]+1),
         lat = c(dat_all_bbox[2]-1, dat_all_bbox[4]+1)) |> 
    st_as_sf(coords = c("lon","lat"), crs = 4326)
)

# Minimal basemap (white/light)
bc_stations_basemap_white <- basemap_terra(
  ext = bc_station_view, 
  map_service = 'carto', 
  map_type = 'light'
)

dat_all <- dat_all |> 
  mutate(
    result = if_else(
      coalesce(e_dna_results_mc, "Negative") == "Positive" | 
        coalesce(fish_sampling_results_q_pcr_mc_detected, "Negative") == "Positive",
      "Positive",
      "Negative"
    )
  )

dat_all <- dat_all |> 
  mutate(result = factor(result, levels = c("Negative", "Positive")))

p1 = ggplot() +
  tidyterra::geom_spatraster_rgb(data = bc_stations_basemap_white) +
  geom_sf(data = col, color = "darkgreen", fill = NA)+
  geom_sf(
    data = dat_all |> arrange(result),  # draws Negative first, Positive last
    aes(
      shape = as.factor(Year),
      color = result
    ),
    size = 5
  ) +
  scale_shape_manual(
    name = "Sampling Year",          # better legend title
    values = c(16, 17)
  ) +
  scale_color_manual(
    name = "Detection Result",       # better legend title
    values = c("Positive" = "orange", "Negative" = "blue")
  ) +
  guides(
    shape = guide_legend(order = 1), # control order in combined legend
    color = guide_legend(order = 2)
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 10)
  )

ggsave("./images/sampling_locations.png",p1, width = 12, height = 10, dpi = 500)  


p2 = ggplot() +
  tidyterra::geom_spatraster_rgb(data = bc_stations_basemap_white) +
  geom_sf(data = col, color = "darkgreen", fill = NA)+
  geom_sf(
    data = dat_all |> filter(result == "Positive"),  # draws Negative first, Positive last
    aes(
      shape = as.factor(Year),
      color = result
    ),
    size = 5
  ) +
  scale_shape_manual(
    name = "Sampling Year",          # better legend title
    values = c(16, 17)
  ) +
  scale_color_manual(
    name = "Detection Result",       # better legend title
    values = c("Positive" = "orange", "Negative" = "blue")
  ) +
  guides(
    shape = guide_legend(order = 1), # control order in combined legend
    color = guide_legend(order = 2)
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 10)
  )

ggsave("./images/sampling_locations_positive.png",p2, width = 12, height = 10, dpi = 500)  


p1_facet <- ggplot() +
  tidyterra::geom_spatraster_rgb(data = bc_stations_basemap_white) +
  geom_sf(data = col, color = "darkgreen", fill = NA)+
  geom_sf(
    data = dat_all |> arrange(result),  # draws Negative first, Positive last
    aes(
      shape = as.factor(Year),
      color = result
    ),
    size = 5
  ) +
  scale_shape_manual(
    name = "Sampling Year",          
    values = c(16, 17)
  ) +
  scale_color_manual(
    name = "Detection Result",       
    values = c("Positive" = "orange", "Negative" = "blue")
  ) +
  guides(
    shape = guide_legend(order = 1),
    color = guide_legend(order = 2)
  ) +
  facet_wrap(~ Year) +   # <-- split plots by Year
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 10),
    strip.text = element_text(face = "bold", size = 16)   # facet title size
  )

# Save the faceted plot
ggsave("./images/sampling_locations_facet.png", plot = p1_facet, width = 12, height = 10, dpi = 500)


