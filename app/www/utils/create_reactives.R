dat_f = reactive({
  req(!is.null(input$fish_sp_f))

  dat |> 
    filter(sampling_method == "Fish") |> 
    filter(sampling_method %in% input$data_type_f) |> 
    # Filter for fish species
    filter(stringr::str_detect(fish_species_sampled,paste0("(",paste0(input$fish_sp_f, collapse = '|'),")")))
})

dat_e = reactive({
  dat |> filter(sampling_method == 'eDNA') |> 
    filter(sampling_method %in% input$data_type_f)
})

subw_as_matrices = reactive({
  req(!is.null(dat_f()))
  fish_res_by_subw = sf::st_join(
    dat_f(),subw
  ) |> 
    sf::st_drop_geometry() |> 
    dplyr::count(watershed_name,name = 'fish_results')
  
  plot_dat = subw |>
    dplyr::left_join(fish_res_by_subw, by = join_by(watershed_name)) |> 
    dplyr::mutate(fish_results = tidyr::replace_na(fish_results, 0))
  
  # Simplify the subwatershed geometries greatly.
  plot_dat = rmapshaper::ms_simplify(plot_dat, keep = 0.1)
  # Extract coordinates and data from the SF object
  plot_dat_coords <- tidyr::as_tibble(sf::st_coordinates(plot_dat))  # Get the coordinates of each geometry
  
  # Bind coordinates with the original attribute data (e.g., 'name')
  plot_dat_coord_tbl <- plot_dat_coords |>
    mutate(name = plot_dat[L2,]$watershed_name,
           fish_results = plot_dat[L2,]$fish_results)  # Add the 'name' column from the original sf object
  plot_dat_coord_tbl
})

fish_leafpops = reactive({
  out = dat_f() |> 
    sf::st_drop_geometry() |> 
    dplyr::select(sample_site_name,
                  sampled_in_2024_y_n,
                  sampling_method,
                  fish_species_sampled,
                  fish_sampling_results_q_pcr_mc_detected,
                  e_dna_results_mc_detection)
  names(out) = c("Sample Site", "Sampled in 2024", "Sampling Method",
                 "Fish Species Sampled", "Fish Sampling Results",
                 "eDNA Sampling Results")
  out |> leafpop::popupTable()
})

edna_leafpops = reactive({
  out = dat_e() |> 
    sf::st_drop_geometry() |> 
    dplyr::select(sample_site_name,
                  sampled_in_2024_y_n,
                  sampling_method,
                  fish_species_sampled,
                  fish_sampling_results_q_pcr_mc_detected,
                  e_dna_results_mc_detection)
  names(out) = c("Sample Site", "Sampled in 2024", "Sampling Method",
                 "Fish Species Sampled", "Fish Sampling Results",
                 "eDNA Sampling Results")
  out |> leafpop::popupTable()
})