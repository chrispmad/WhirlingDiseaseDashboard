create_wb_for_year <- function(year, con, dat_dl) {
  
  
  
  dat_year <- dat_dl %>% dplyr::filter(year == year)
  
  
  
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Results")
  openxlsx::addWorksheet(wb, "Species Look-up")
  
  if (nrow(dat_year) > 0) {
    
    dat_year <- dat_year %>%
      dplyr::mutate(
        Longitude = sf::st_coordinates(geom)[, 1],
        Latitude  = sf::st_coordinates(geom)[, 2]
      )
    
    
    
    
    # Column selection differs by year
    if (year == "2025") {
      
      results <- dat_year %>%
        dplyr::group_by(sample_site_name) %>%
        dplyr::summarise(
          `Sampling Method` = paste(sort(unique(sampling_method)), collapse = " + "),
          waterbody_name = dplyr::first(waterbody_name),
          fish_species_sampled = tidyr::replace_na(dplyr::first(fish_species_sampled), "NA"),
          fish_sampling_results_q_pcr_mc_detected =
            tidyr::replace_na(dplyr::first(fish_sampling_results_q_pcr_mc_detected), "NA"),
          e_dna_results_mc = tidyr::replace_na(dplyr::first(e_dna_results_mc), "NA"),
          Latitude  = round(dplyr::first(Latitude), 4),
          Longitude = round(dplyr::first(Longitude), 4),
          .groups = "drop"
        )|> 
        sf::st_drop_geometry()
      
      results <- results %>%
        dplyr::select(
          
          `Waterbody Name` = waterbody_name,
          
          `Sample Site` = sample_site_name,
          Latitude,
          Longitude,
          `Sampling Method`,
          
          `Fish Species Sampled` = fish_species_sampled,
          
          `Fish Sampling Results` =
            fish_sampling_results_q_pcr_mc_detected,
          
          `eDNA Sampling Results (M. cerebralis - parasite)` =
            e_dna_results_mc
        )
      
    } else {
      
      results <- dat_year %>%
        dplyr::group_by(sample_site_name) %>%
        dplyr::summarise(
          `Sampling Method` = paste(sort(unique(sampling_method)), collapse = " + "),
          fish_species_sampled = tidyr::replace_na(dplyr::first(fish_species_sampled), "NA"),
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

