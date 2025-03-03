
credentials <- data.frame(
  user = c("bcwd"), # mandatory
  password = c("mop1")
)

server <- function(input, output, session) {
  
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  observe({
  if(length(as.numeric(reactiveValuesToList(res_auth))) > 0){
      if(!stringr::str_detect(getwd(),"www\\/$")) setwd(paste0(getwd(),"/www"))
      
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
      
      dat_f = reactive({
        req(!is.null(input$fish_sp_f))
        dat |> 
          filter(sampling_method == "Fish") |> 
          filter(sampling_method %in% input$data_type_f) |> 
          # Filter for fish species filter input
          filter(stringr::str_detect(fish_species_sampled,paste0("(",paste0(input$fish_sp_f, collapse = '|'),")")))
      })
      
      dat_e = reactive({
        dat |> filter(sampling_method == 'eDNA') |> 
          filter(sampling_method %in% input$data_type_f)
      })
      
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
        # browser()
        plot_dat = rmapshaper::ms_simplify(plot_dat, keep = 0.1)
        # Extract coordinates and data from the SF object
        plot_dat_coords <- tidyr::as_tibble(sf::st_coordinates(plot_dat))  # Get the coordinates of each geometry
        
        # Bind coordinates with the original attribute data (e.g., 'name')
        plot_dat_coord_tbl <- plot_dat_coords |>
          mutate(name = plot_dat[L2,]$watershed_name,
                 fish_results = plot_dat[L2,]$fish_results)  # Add the 'name' column from the original sf object
        plot_dat_coord_tbl
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
      
      # output$watersheds_rgl = rgl::renderRglwidget({
      #   try(close3d(), silent = TRUE)
      #   browser()
      #   subw_as_matrices()
      #   # x <- seq(-5, 5, length.out = 50)
      #   # y <- seq(-5, 5, length.out = 50)
      #   # z <- outer(x, y, function(x, y) sin(sqrt(x^2 + y^2)))
      #   # 
      #   # # Create the 3D surface plot
      #   # rgl::surface3d(x, y, z, col = "blue", alpha = 1)
      #   rgl::rglwidget()
      # })
      # var_for_fill = reactive({
      #   input$var_for_fill
      # })
      
      # var_for_fill_label = reactive({
      #   dplyr::case_when(
      #     var_for_fill() == "sample_type_colour" ~ "sampling_method",
      #     var_for_fill() == "fish_results_colour" ~ "fish_sampling_results_q_pcr_mc_detected",
      #     var_for_fill() == "edna_results_colour" ~ "e_dna_results_mc_detection",
      #   )
      # })
      
      output$my_leaf = renderLeaflet({
        
        leaflet() |> 
          addTiles() |> 
          addPolygons(
            data = subw,
            color = 'grey',
            weight = 1.5, 
            fill = 'transparent',
            label = ~watershed_name
          ) |> 
          addPolygons(
            data = col,
            fillColor = 'transparent',
            color = 'purple',
            weight = 1.5,
            options = pathOptions(clickable = FALSE)
          ) |> 
          leaflet.extras::addResetMapButton() |> 
          leaflet::addScaleBar('bottomright')
      })
      
      observe({
        
        l = leafletProxy("my_leaf") |> 
          leaflet::clearMarkers() |> 
          leaflet::clearMarkerClusters() |> 
          leaflet::clearControls()
        
        if(nrow(dat_f()) > 0){
          
          l = l |> 
            # Add fish sampling
            addCircleMarkers(
              data = dat_f(),
              fillColor = ~fish_results_colour,
              fillOpacity = 0.6,
              color = 'black',
              weight = 1,
              label = lapply(fish_leafpops(), htmltools::HTML)
            ) |> 
            addLegend(title = 'Fish Results',
                      colors = unique(dat_f()$fish_results_colour),
                      labels = unique(dat_f()$fish_sampling_results_q_pcr_mc_detected))
        }
        if(nrow(dat_e()) > 0){
          l = l |> 
            addCircleMarkers(
              data = dat_e(),
              fillColor = ~edna_results_colour,
              fillOpacity = 0.6,
              color = 'black',
              weight = 1,
              label = lapply(edna_leafpops(), htmltools::HTML)
            ) |> 
            addLegend(title = "eDNA Results",
                      colors = unique(dat_e()$edna_results_colour),
                      labels = unique(dat_e()$e_dna_results_mc_detection))
        }
      })
    }
  })
}