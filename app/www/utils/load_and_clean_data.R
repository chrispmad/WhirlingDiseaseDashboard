dat = sf::read_sf("sampling_results.gpkg")
col = sf::read_sf("columbia_watershed.gpkg")
subw = sf::read_sf("subwatershed_groups.gpkg")

# Remove sites that weren't sampled.
dat = dat |> dplyr::filter(sampled_in_2024_y_n == "Y")

# Combine some fish species levels that I assume are the same:
dat = dat |> 
  dplyr::mutate(fish_species_sampled = dplyr::case_when(
    fish_species_sampled == "KO" ~ "KOK",
    fish_species_sampled == "RB" ~ "RBT",
    T ~ fish_species_sampled
  ))

# Drop rows where sample result is NA for both eDNA and fish
dat = dat |> dplyr::filter(!(is.na(e_dna_results_mc_detection) & is.na(fish_sampling_results_q_pcr_mc_detected)))

# Temporary correction to highlight NAs in dataset:
dat = dat |> mutate(sampling_method = tidyr::replace_na(sampling_method, "NA"))

# Split sampling results at commas into separate rows.
dat = dat |>
  tidyr::separate_longer_delim(cols = sampling_method, delim = ' + ')

# Also, replace NAs in eDNA results with 'pending', if the sampled in 2024 column
# says yes.
dat = dat |> 
  dplyr::mutate(e_dna_results_mc_detection = dplyr::case_when(
    sampled_in_2024_y_n == "Y" & sampling_method == 'eDNA' & is.na(e_dna_results_mc_detection) ~ "Pending",
    T ~ e_dna_results_mc_detection
  ))

# Relabel the geometry column.
dat = sf::st_set_geometry(dat, "geom")
# Remove extra text in sampling results.
dat = dat |> 
  dplyr::mutate(fish_sampling_results_q_pcr_mc_detected = stringr::str_remove(fish_sampling_results_q_pcr_mc_detected," \\(.*")) |> 
  dplyr::mutate(fish_sampling_results_q_pcr_mc_detected = stringr::str_to_title(fish_sampling_results_q_pcr_mc_detected)) |> 
  dplyr::mutate(fish_sampling_results_q_pcr_mc_detected = tidyr::replace_na(fish_sampling_results_q_pcr_mc_detected, "NA")) |> 
  dplyr::mutate(e_dna_results_mc_detection = stringr::str_to_title(e_dna_results_mc_detection),
                e_dna_results_mc_detection = tidyr::replace_na(e_dna_results_mc_detection,"NA"))

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
  dplyr::mutate(edna_results_colour = dplyr::case_when(
    e_dna_results_mc_detection == "Not Detected" ~ 'lightblue',
    e_dna_results_mc_detection == "Weak Detection" ~ 'orange',
    e_dna_results_mc_detection == "Pending" ~ 'pink',
    T ~ 'black'
  ))