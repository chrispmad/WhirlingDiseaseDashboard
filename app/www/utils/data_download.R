create_wb_for_year <- function(year_val, con, dat_dl) {
  
  sf::sf_use_s2(FALSE)
  
  year <- year_val
  
  dat_year = dat_dl
  
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Results")
  openxlsx::addWorksheet(wb, "Species Look-up")
  
  if (nrow(dat_year) > 0) {
    
    coords <- sf::st_coordinates(sf::st_point_on_surface(dat_year$geom))
    
    dat_year <- dat_year %>%
      dplyr::mutate(
        Longitude = coords[, 1],
        Latitude  = coords[, 2]
      )
    
    
    # Column selection differs by year
    if (year == "2025") {
      results <- dat_year %>%
        dplyr::group_by(sample_site_name) %>%
        dplyr::summarise(
          `Sampling Method` = paste(sort(unique(sampling_method)), collapse = " + "),
          waterbody_name = dplyr::first(waterbody_name),
          fish_species_sampled = tidyr::replace_na(
            dplyr::first(fish_species_sampled), "NA"
          ),
          `Delivery Agency` = dplyr::first(delivery_agency),
          fish_sampling_results_q_pcr_mc_detected =
            tidyr::replace_na(
              dplyr::first(fish_sampling_results_q_pcr_mc_detected), "NA"
            ),
          e_dna_results_mc =
            tidyr::replace_na(
              dplyr::first(e_dna_results_mc), "NA"
            ),
          Latitude  = round(dplyr::first(Latitude), 4),
          Longitude = round(dplyr::first(Longitude), 4),
          `Date Collected` =
            paste(sort(unique(date_collected)), collapse = ", "),
          `Volume Sampled (l)` =
            paste(sort(unique(volume_sampled)), collapse = ", "),
          `Filter Size (µm)` =
            paste(sort(unique(filter_size)), collapse = ", "),
          .groups = "drop"
        ) |>
        sf::st_drop_geometry()
      
      
      results <- results %>%
        tidyr::separate(
          `Volume Sampled (l)`,
          into = paste0("Volume Sampled (l) ", 1:3),
          sep = ", ",
          fill = "right",
          extra = "drop"
        ) %>%
        tidyr::separate(
          `Filter Size (µm)`,
          into = paste0("Filter Size (µm) ", 1:3),
          sep = ", ",
          fill = "right",
          extra = "drop"
        )
      
      
      results <- results %>%
        dplyr::select(
          `Waterbody Name` = waterbody_name,
          `Sample Site` = sample_site_name,
          Latitude,
          Longitude,
          `Sampling Method`,
          `Delivery Agency`,
          `Fish Species Sampled` = fish_species_sampled,
          `Fish Sampling Results` =
            fish_sampling_results_q_pcr_mc_detected,
          `eDNA Sampling Results (M. cerebralis - parasite)` =
            e_dna_results_mc,
          `Date Collected`,
          dplyr::starts_with("Volume Sampled (l)"),
          dplyr::starts_with("Filter Size (µm)")
        )
      
    } else {
      
      results <- dat_year %>%
        dplyr::group_by(sample_site_name) %>%
        dplyr::summarise(
          `Sampling Method` = paste(sort(unique(sampling_method)), collapse = " + "),
          fish_species_sampled = tidyr::replace_na(dplyr::first(fish_species_sampled), "NA"),
          `Delivery Agency` = dplyr::first(delivery_agency),
          fish_sampling_results_q_pcr_mc_detected =
            tidyr::replace_na(dplyr::first(fish_sampling_results_q_pcr_mc_detected), "NA"),
          e_dna_results_mc = tidyr::replace_na(dplyr::first(e_dna_results_mc), "NA"),
          Latitude  = round(dplyr::first(Latitude), 4),
          Longitude = round(dplyr::first(Longitude), 4),
          .groups = "drop"
        ) |> 
        sf::st_drop_geometry()
      
      
      
      results <- results %>%
        dplyr::select(
          
          `Sample Site` = sample_site_name,
          Latitude,
          Longitude,
          `Sampling Method`,
          `Delivery Agency`,
          `Fish Species Sampled` = fish_species_sampled,
          `Fish Sampling Results` =
            fish_sampling_results_q_pcr_mc_detected,
          
          `eDNA Sampling Results (M. cerebralis - parasite)` =
            e_dna_results_mc
        )
    }
    
    results <- results %>%
      dplyr::arrange(`Sample Site`)
    
    openxlsx::writeData(wb, "Results", results)
    openxlsx::setColWidths(
      wb,
      sheet = "Results",
      cols = seq_len(ncol(results)),
      widths = "auto"
    )
    
  } else {
    
    openxlsx::writeData(
      wb,
      "Results",
      "No data available for this year"
    )
  }
  
  # Species lookup (same for all years)
  openxlsx::writeData(
    wb,
    "Species Look-up",
    tibble::tibble(
      acronym = c("BT", "EBT", "KOK", "MW", "RBT", "SK", "WCT"),
      species = c(
        "Bull Trout",
        "Eastern Brook Trout",
        "Kokanee",
        "Mountain Whitefish",
        "Rainbow Trout",
        "Sockeye Salmon",
        "Westslope Cutthroat Trout"
      )
    )
  )
  
  openxlsx::saveWorkbook(wb, con, overwrite = TRUE)
}

output$data_dl_2024 <- downloadHandler(
  filename = function() { "BC_WhirlingDisease_2024.xlsx" },
  content = function(con) { create_wb_for_year("2024", con, dat) }
)

output$data_dl_2025 <- downloadHandler(
  filename = function() { "BC_WhirlingDisease_2025.xlsx" },
  content = function(con) { create_wb_for_year("2025", con, dat_2025) }
)

