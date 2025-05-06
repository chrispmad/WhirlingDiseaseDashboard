# Perform counts and things by subwatersheds!
output$edna_res_by_subw = renderPlot({
  req(!is.null(dat_e()))
  
  if(nrow(dat_e()) == 0) return(ggplot() + ggthemes::theme_pander())
  
  edna_res_by_subw = sf::st_join(
    dat_e(),subw
  ) |> 
    sf::st_drop_geometry() |> 
    dplyr::count(watershed_name,e_dna_results_mc_detection,name = 'edna_results') |> 
    tidyr::pivot_wider(names_from = e_dna_results_mc_detection, values_from = edna_results, values_fill = 0)
  
  plot_dat = subw |>
    dplyr::left_join(edna_res_by_subw, by = join_by(watershed_name))
  
  plot_dat = plot_dat |> 
    sf::st_drop_geometry() |> 
    tidyr::pivot_longer(cols = c(-watershed_name)) |> 
    dplyr::filter(!is.na(value))
  
  p = plot_dat |>
    ggplot() + 
    geom_col(aes(x = watershed_name, y = value, fill = name),
             position = position_dodge2(preserve = 'single', padding = 0)
    ) +
    facet_wrap( ~ watershed_name, nrow = 1, scales = 'free_x') +
    paletteer::scale_fill_paletteer_d("ButterflyColors::anteos_menippe") +
    labs(x = "Subwatershed Name", y = "Number of Results",
         fill = "Result") +
    ggthemes::theme_pander() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
          strip.text.x = element_blank())
  
  p
})

output$fish_res_by_subw = renderPlot({
  req(!is.null(dat_f()))
  
  if(nrow(dat_f()) == 0) return(ggplot() + ggthemes::theme_pander())
  
  fish_res_by_subw = sf::st_join(
    dat_f(),subw
  ) |> 
    sf::st_drop_geometry() |> 
    dplyr::count(watershed_name,fish_sampling_results_q_pcr_mc_detected,name = 'fish_results') |> 
    tidyr::pivot_wider(names_from = fish_sampling_results_q_pcr_mc_detected, values_from = fish_results, values_fill = 0)
  
  plot_dat = subw |>
    dplyr::left_join(fish_res_by_subw, by = join_by(watershed_name))
  
  plot_dat = plot_dat |> 
    sf::st_drop_geometry() |> 
    tidyr::pivot_longer(cols = -watershed_name) |> 
    dplyr::filter(!is.na(value))
  
  p = plot_dat |> 
    ggplot() + 
    geom_col(aes(x = watershed_name, y = value, fill = name),
             position = position_dodge2(preserve = 'single', padding = 0)
    ) + 
    facet_wrap( ~ watershed_name, nrow = 1, scales = 'free_x') +
    paletteer::scale_fill_paletteer_d("ButterflyColors::anteos_menippe") + 
    labs(x = "Subwatershed Name", y = "Number of Results", 
         fill = "Result") +
    ggthemes::theme_pander() + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
          strip.text.x = element_blank())
  
  p
})

output$subwatershed_fig = renderUI({
  
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
  # browser()
  plot_dat = rmapshaper::ms_simplify(plot_dat, keep = 0.1)
  # Extract coordinates and data from the SF object
  plot_dat_coords <- tidyr::as_tibble(sf::st_coordinates(plot_dat))  # Get the coordinates of each geometry
  
  # Bind coordinates with the original attribute data (e.g., 'name')
  plot_dat_coord_tbl <- plot_dat_coords |>
    mutate(name = plot_dat[L2,]$watershed_name,
           fish_results = plot_dat[L2,]$fish_results)  # Add the 'name' column from the original sf object
  
  p = ggplot()+
    geom_polygon_z(aes(x=X, y=Y, fill = fish_results, z = fish_results), color = "white", extrude = TRUE,
                   data = plot_dat_coord_tbl)+
    scale_fill_viridis_c(option = "C") +
    theme_ggrgl() + 
    # labs(
    #   title = "ggrgl::geom_polygon_z()",
    #   subtitle = "with {devoutrgl}"
    # ) + 
    coord_fixed(1.2)
  devoutrgl::rgldev(fov = 30, view_angle = -30)
  p
  # try(rgl::rgl.close())
  # rgl::rglwidget(p)
})