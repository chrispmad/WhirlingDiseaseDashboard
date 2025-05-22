# dat = sf::read_sf("app/www/sampling_results.gpkg")
# col = sf::read_sf("app/www/columbia_watershed.gpkg")
# subw = sf::read_sf("app/www/subwatershed_groups.gpkg")
dat = sf::read_sf("www/sampling_results.gpkg")
col = sf::read_sf("www/columbia_watershed.gpkg")
subw = sf::read_sf("www/subwatershed_groups.gpkg")

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
dat = dat |> mutate(sampling_method = tidyr::replace_na(sampling_method, "NA"))

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

