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



# sites <- tibble(
#   site_name = c(
#     "Outlet of Columbia Lake 1",
#     "Outlet of Columbia Lake 1",
#     "Outlet of Columbia Lake 2",
#     "Outlet of Columbia Lake 3",
#     "Windermere Lake outlet 1",
#     "Windermere Lake outlet 2",
#     "Windermere Lake outlet 3"
#   ),
#   sample_date = as.Date(c(
#     "2024-09-10",
#     "2024-09-10",
#     "2024-09-10",
#     "2024-09-10",
#     "2024-09-10",
#     "2024-09-10",
#     "2024-09-10"
#   )),
#   easting = c(
#     581438, 581438, 581438, 581438,
#     569468, 569468, 569468
#   ),
#   northing = c(
#     5573882, 5573882, 5573882, 5573882,
#     5596558, 5596558, 5596558
#   )
# )
# 
# 
# sites_sf <- sites %>%
#   st_as_sf(
#     coords = c("easting", "northing"),
#     crs = 32611        # UTM Zone 11N (WGS84)
#   ) %>%
#   st_transform(4326) # Leaflet requires lon/lat


# Helper: DMS to decimal degrees
dms_to_dd <- function(deg, min, sec) deg + min/60 + sec/3600

# Helper: UTM Zone 11N (EPSG:26911) → WGS84
utm_to_dd <- function(easting, northing) {
  pts <- st_as_sf(data.frame(x = easting, y = northing),
                  coords = c("x", "y"), crs = 26911)
  coords <- st_coordinates(st_transform(pts, 4326))
  data.frame(lat_dd = coords[, 2], lon_dd = coords[, 1])
}

# ── 1. DMS sites ──────────────────────────────────────────────────────────────
# NOTE: Perry Creek longitude listed as 115° 94' 38'' W — minutes can't be 94.
# Assumed typo: 115° 04' 38'' W. Verify with field notes.

dms_sites <- tribble(
  ~sample_name,                   ~date,         ~lat_dd,                ~lon_dd,
  "Whiteswan 1",                  "2024-10-03",  dms_to_dd(50,10, 3),   -dms_to_dd(115,27,52),
  "Whiteswan 2",                  "2024-10-03",  dms_to_dd(50,10, 3),   -dms_to_dd(115,27,52),
  "Whiteswan 3",                  "2024-10-03",  dms_to_dd(50,10, 3),   -dms_to_dd(115,27,52),
  "Premier Lake 1",               "2024-10-02",  dms_to_dd(49,55, 5),   -dms_to_dd(115,38,50),
  "Premier Lake 2",               "2024-10-02",  dms_to_dd(49,55, 5),   -dms_to_dd(115,38,50),
  "Premier Lake 3",               "2024-10-02",  dms_to_dd(49,55, 5),   -dms_to_dd(115,38,50),
  "Mainstem near Elko 1",         "2024-08-27",  dms_to_dd(49,18,24),   -dms_to_dd(115, 4,55),
  "Mainstem near Elko 2",         "2024-08-27",  dms_to_dd(49,18,24),   -dms_to_dd(115, 4,55),
  "Mainstem near Elko 3",         "2024-08-27",  dms_to_dd(49,18,24),   -dms_to_dd(115, 4,55),
  "Mainstem near Fernie 1",       "2024-08-27",  dms_to_dd(49,29,53),   -dms_to_dd(115, 4, 7),
  "Mainstem near Fernie 2",       "2024-08-27",  dms_to_dd(49,29,53),   -dms_to_dd(115, 4, 7),
  "Mainstem near Fernie 3",       "2024-08-27",  dms_to_dd(49,29,53),   -dms_to_dd(115, 4, 7),
  "Mainstem near Olson Pit 1",    "2024-08-27",  dms_to_dd(49,38,49),   -dms_to_dd(114,55, 5),
  "Mainstem near Olson Pit 2",    "2024-08-27",  dms_to_dd(49,38,49),   -dms_to_dd(114,55, 5),
  "Mainstem near Olson Pit 3",    "2024-08-27",  dms_to_dd(49,38,49),   -dms_to_dd(114,55, 5),
  "Sand Creek 1",                 "2024-08-28",  dms_to_dd(49,21, 2),   -dms_to_dd(115,17,46),
  "Sand Creek 2",                 "2024-08-28",  dms_to_dd(49,21, 2),   -dms_to_dd(115,17,46),
  "Sand Creek 3",                 "2024-08-28",  dms_to_dd(49,21, 2),   -dms_to_dd(115,17,46),
  "Perry Creek 1",                "2024-09-13",  dms_to_dd(49,57,24),   -dms_to_dd(115, 4,38),  # ⚠ 94' corrected to 04'
  "Perry Creek 2",                "2024-09-13",  dms_to_dd(49,57,24),   -dms_to_dd(115, 4,38),
  "Perry Creek 3",                "2024-09-13",  dms_to_dd(49,57,24),   -dms_to_dd(115, 4,38),
  "Kikomun Creek 1",              "2024-09-18",  dms_to_dd(49,16, 4),   -dms_to_dd(115,14,30),
  "Kikomun Creek 2",              "2024-09-18",  dms_to_dd(49,16, 4),   -dms_to_dd(115,14,30),
  "Kikomun Creek 3",              "2024-09-18",  dms_to_dd(49,16, 4),   -dms_to_dd(115,14,30),
  "Goat River 1",                 "2024-09-17",  dms_to_dd(49, 4,51),   -dms_to_dd(116,31,17),
  "Goat River 2",                 "2024-09-17",  dms_to_dd(49, 4,51),   -dms_to_dd(116,31,17),
  "Goat River 3",                 "2024-09-17",  dms_to_dd(49, 4,51),   -dms_to_dd(116,31,17),
  "Boulder Creek 1",              "2024-09-09",  dms_to_dd(49,16,57),   -dms_to_dd(116,39,13),
  "Boulder Creek 2",              "2024-09-09",  dms_to_dd(49,16,57),   -dms_to_dd(116,39,13),
  "Boulder Creek 3",              "2024-09-09",  dms_to_dd(49,16,57),   -dms_to_dd(116,39,13),
  "Duck Creek 1",                 "2024-09-17",  dms_to_dd(49,11, 1),   -dms_to_dd(116,32,55),
  "Duck Creek 2",                 "2024-09-17",  dms_to_dd(49,11, 1),   -dms_to_dd(116,32,55),
  "Duck Creek 3",                 "2024-09-17",  dms_to_dd(49,11, 1),   -dms_to_dd(116,32,55),
  "Flathead Mainstem 1",          "2024-09-11",  dms_to_dd(49, 7,17),   -dms_to_dd(114,29,45),
  "Flathead Mainstem 2",          "2024-09-11",  dms_to_dd(49, 7,17),   -dms_to_dd(114,29,45),
  "Flathead Mainstem 3",          "2024-09-11",  dms_to_dd(49, 7,17),   -dms_to_dd(114,29,45),
  "Lower Sage Creek 1",           "2024-09-10",  dms_to_dd(49, 4,36),   -dms_to_dd(114,27,47),
  "Lower Sage Creek 2",           "2024-09-10",  dms_to_dd(49, 4,36),   -dms_to_dd(114,27,47),
  "Lower Sage Creek 3",           "2024-09-10",  dms_to_dd(49, 4,36),   -dms_to_dd(114,27,47),
  "Commerce Creek 1",             "2024-09-12",  dms_to_dd(49, 8, 4),   -dms_to_dd(114,28,40),
  "Commerce Creek 2",             "2024-09-12",  dms_to_dd(49, 8, 4),   -dms_to_dd(114,28,40),
  "Commerce Creek 3",             "2024-09-12",  dms_to_dd(49, 8, 4),   -dms_to_dd(114,28,40),
)

# ── 2. UTM Zone 11N sites ─────────────────────────────────────────────────────
# NOTE: Nicholson bridge northing is 567888 — likely missing a leading 5, should
# be 5678880. Kept as-is; verify with field notes.

utm_raw <- tribble(
  ~sample_name,                             ~date,          ~easting,  ~northing,
  "Ben Abel 1",                             "2024-09-04",   568900,    5593406,
  "Ben Abel 2",                             "2024-09-04",   568900,    5593406,
  "Ben Abel 3",                             "2024-09-04",   568900,    5593406,
  "Ben Abel 2.1",                           "2024-09-04",   568900,    5593406,
  "Ben Abel 3.1",                           "2024-09-04",   568900,    5593406,
  "Stoddart Creek 1",                       "2024-09-04",   568793,    5601669,
  "Stoddart Creek 2",                       "2024-09-04",   568793,    5601669,
  "Stoddart Creek 3",                       "2024-09-04",   568793,    5601669,
  "Stoddart Creek FIELD BLANK",             "2024-09-04",   568793,    5601669,
  "Hospital Creek 1",                       "2024-09-05",   501807,    5684786,
  "Hospital Creek 2",                       "2024-09-05",   501807,    5684786,
  "Hospital Creek 3",                       "2024-09-05",   501807,    5684786,
  "Holt Creek 1",                           "2024-09-05",   499224,    5686340,
  "Holt Creek 2",                           "2024-09-05",   499224,    5686340,
  "Holt Creek 3",                           "2024-09-05",   499224,    5686340,
  "Holt Creek FIELD BLANK",                 "2024-09-05",   499224,    5686340,
  "Marl Creek 1",                           "2024-09-09",   486177,    5706207,
  "Marl Creek 2",                           "2024-09-09",   486177,    5706207,
  "Marl Creek 3",                           "2024-09-09",   486177,    5706207,
  "Marl Creek FIELD BLANK",                 "2024-09-09",   486177,    5706207,
  "Outlet of Columbia Lake 1",              "2024-09-10",   581438,    5573882,
  "Outlet of Columbia Lake 1b",             "2024-09-10",   581438,    5573882,  # duplicate row in source
  "Outlet of Columbia Lake 2",              "2024-09-10",   581438,    5573882,
  "Outlet of Columbia Lake 3",             "2024-09-10",   581438,    5573882,
  "Windermere Lake outlet 1",               "2024-09-10",   569468,    5596558,
  "Windermere Lake outlet 2",               "2024-09-10",   569468,    5596558,
  "Windermere Lake outlet 3",               "2024-09-10",   569468,    5596558,
  "Windermere Lake outlet FIELD BLANK",     "2024-09-10",   569468,    5596558,
  "Templeton River 1",                      "2024-09-12",   547724,    5631412,
  "Templeton River 2",                      "2024-09-12",   547724,    5631412,
  "Templeton River 3",                      "2024-09-12",   547724,    5631412,
  "Templeton River FIELD BLANK",            "2024-09-12",   547724,    5631412,
  "Jordan River 1",                         "2024-09-19",   412673,    5653245,
  "Jordan River 2",                         "2024-09-19",   412673,    5653245,
  "Jordan River 3",                         "2024-09-19",   412673,    5653245,
  "Jordan River FIELD BLANK",               "2024-09-19",   412673,    5653245,
  "Brisco Bridge 1",                        "2024-10-17",   550242,    5631034,
  "Brisco Bridge 2",                        "2024-10-17",   550242,    5631034,
  "Brisco Bridge 3",                        "2024-10-17",   550242,    5631034,
  "Brisco Bridge 4",                        "2024-10-17",   550242,    5631034,
  "Brisco Bridge 5",                        "2024-10-17",   550242,    5631034,
  "Brisco Bridge FIELD BLANK",              "2024-10-17",   550242,    5631034,
  "Kicking Horse drive bridge 1",           "2024-10-18",   500408,    5684592,
  "Kicking Horse drive bridge 2",           "2024-10-18",   500408,    5684592,
  "Kicking Horse drive bridge 3",           "2024-10-18",   500408,    5684592,
  "Kicking Horse drive bridge 4",           "2024-10-18",   500408,    5684592,
  "Kicking Horse drive bridge FIELD BLANK", "2024-10-18",   500408,    5684592,
  "Nicholson bridge 1",                     "2024-10-18",   506088,    5678882,   # ⚠ northing suspect
  "Nicholson bridge 2",                     "2024-10-18",   506088,    5678882,
  "Nicholson bridge 3",                     "2024-10-18",   506088,    5678882,
  "Nicholson bridge 4",                     "2024-10-18",   506088,    5678882,
  "Nicholson bridge FIELD BLANK",           "2024-10-18",   506088,    5678882,
  "Beaverfoot Rd. Bridge 1",                "2024-10-20",   524200,    5675824,
  "Beaverfoot Rd. Bridge 2",                "2024-10-20",   524200,    5675824,
  "Beaverfoot Rd. Bridge 3",                "2024-10-20",   524200,    5675824,
  "Beaverfoot Rd. Bridge 4",                "2024-10-20",   524200,    5675824,
  "Beaverfoot Rd. Bridge FIELD BLANK",      "2024-10-20",   524200,    5675824,
)

utm_coords <- utm_to_dd(utm_raw$easting, utm_raw$northing)
utm_sites <- bind_cols(utm_raw, utm_coords) |> select(sample_name, date, lat_dd, lon_dd)

# ── 3. Decimal degree sites ───────────────────────────────────────────────────
dd_sites <- tribble(
  ~sample_name,                        ~date,         ~lat_dd,   ~lon_dd,
  "Forum Lake 1",                      "2024-09-29",  49.0113,   -114.07257,
  "Forum Lake 2a",                     "2024-09-29",  49.01102,  -114.07644,
  "Forum Lake 2b",                     "2024-09-29",  49.01102,  -114.07644,
  "Forum Lake 3",                      "2024-09-29",  49.00940,  -114.07365,
  "Forum Lake A-K Prov Park Control",  "2024-09-29",  49.01102,  -114.07649,
)

# ── Combine ───────────────────────────────────────────────────────────────────
dat <- bind_rows(dms_sites, utm_sites, dd_sites) |>
  mutate(date = as.Date(date))

sites_sf<- dat |> 
  st_as_sf(
        coords = c("lon_dd", "lat_dd"),
        crs = 4326        # UTM Zone 11N (WGS84)
      )


library(leaflet)

dat_all = dat_all |> 
  st_transform(4326)

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  
  addCircleMarkers(
    data = st_transform(dat_all |> filter(Year == 2024)),
    color = "darkgreen",
    weight = 2,
    radius = 6,
    popup = lapply(seq_len(nrow(dat_all)), function(i) {
      row <- dat_all[i, ]
      parts <- c(
        paste0("<b>Site:</b> ", row$sample_site_name),
        if (!is.na(row$Year)) paste0("<b>Year:</b> ", row$Year),
        if (!is.na(row$fish_sampling_results_q_pcr_mc_detected))
          paste0("<b>Fish Sample:</b> ", row$fish_sampling_results_q_pcr_mc_detected),
        if (!is.na(row$e_dna_results_mc))
          paste0("<b>eDNA Sample:</b> ", row$e_dna_results_mc)
      )
      paste(parts, collapse = "<br>")
    })
  ) %>%
  
  addCircleMarkers(
    data = sites_sf,
    radius = 6,
    color = "purple",
    fillOpacity = 0.8,
    popup = ~paste0("<b>Site:</b> ", sample_name)
  )


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

# ggsave("./images/sampling_locations.png",p1, width = 12, height = 10, dpi = 500)  

# 
# p2 = ggplot() +
#   tidyterra::geom_spatraster_rgb(data = bc_stations_basemap_white) +
#   geom_sf(data = col, color = "darkgreen", fill = NA)+
#   geom_sf(
#     data = dat_all |> filter(result == "Positive"),  # draws Negative first, Positive last
#     aes(
#       shape = as.factor(Year),
#       color = result
#     ),
#     size = 5
#   ) +
#   scale_shape_manual(
#     name = "Sampling Year",          # better legend title
#     values = c(16, 17)
#   ) +
#   scale_color_manual(
#     name = "Detection Result",       # better legend title
#     values = c("Positive" = "orange", "Negative" = "blue")
#   ) +
#   guides(
#     shape = guide_legend(order = 1), # control order in combined legend
#     color = guide_legend(order = 2)
#   ) +
#   theme_minimal() +
#   theme(
#     legend.position = "right",
#     legend.title = element_text(face = "bold"),
#     legend.text = element_text(size = 10)
#   )
# 
# ggsave("./images/sampling_locations_positive.png",p2, width = 12, height = 10, dpi = 500)  


# p1_facet <- ggplot() +
#   tidyterra::geom_spatraster_rgb(data = bc_stations_basemap_white) +
#   geom_sf(data = col, color = "darkgreen", fill = NA)+
#   geom_sf(
#     data = dat_all |> arrange(result),  # draws Negative first, Positive last
#     aes(
#       shape = as.factor(Year),
#       color = result
#     ),
#     size = 5
#   ) +
#   scale_shape_manual(
#     name = "Sampling Year",          
#     values = c(16, 17)
#   ) +
#   scale_color_manual(
#     name = "Detection Result",       
#     values = c("Positive" = "orange", "Negative" = "blue")
#   ) +
#   guides(
#     shape = guide_legend(order = 1),
#     color = guide_legend(order = 2)
#   ) +
#   facet_wrap(~ Year) +   # <-- split plots by Year
#   theme_minimal() +
#   theme(
#     legend.position = "bottom",
#     legend.title = element_text(face = "bold"),
#     legend.text = element_text(size = 10),
#     strip.text = element_text(face = "bold", size = 16)   # facet title size
#   )

# # Save the faceted plot
# ggsave("./images/sampling_locations_facet.png", plot = p1_facet, width = 12, height = 10, dpi = 500)
# 
# 
# bc <- bcmaps::bc_bound()
# 
# bc_bbox <- st_bbox(bc)
# 
# target_crs <- st_crs(3005)
# 
# bc_t  <- st_transform(bc, target_crs)
# col_t <- st_transform(col, target_crs)
# 
# # Re-fetch basemap using the transformed bbox
# bc_bbox <- st_bbox(bc_t)
# bc_stations_basemap_white <- basemap_terra(
#   ext = bc_bbox,
#   map_service = 'carto',
#   map_type = 'light_no_labels'
# )


# p_final <- ggplot() +
#   tidyterra::geom_spatraster_rgb(data = bc_stations_basemap_white) +
#   geom_sf(data = bc_t,  fill = NA, color = "grey30", linewidth = 0.4) +
#   geom_sf(data = col_t, fill = "#a8c8a0", color = "#2d6a4f",       # muted greens
#           linewidth = 0.5, alpha = 0.7) +
#   theme_map()


# library(ggspatial)
# 
# final_plot =p_final +
#   annotation_scale(
#     location = "bl",           # bottom-right
#     width_hint = 0.2,
#     pad_x = unit(1.6, "cm"),
#     pad_y = unit(1.4, "cm")
#   ) +
#   annotation_north_arrow(
#     location = "bl",           # bottom-right
#     which_north = "true",
#     height = unit(2, "cm"),
#     width = unit(2, "cm"),
#     pad_x = unit(1.8, "cm"),
#     pad_y = unit(2, "cm"),   # higher value pushes it further up
#     style = north_arrow_fancy_orienteering(
#       fill = c("grey40", "white")
#     )
#   )
# 
# ggsave("./images/columbia_fraser.png", plot = final_plot, width = 12, height = 10, dpi = 500)
