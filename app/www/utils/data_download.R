# Helper function to create Excel workbook
create_wb_for_year <- function(year, con, dat_dl) {
  dat_year <- dat_dl %>% dplyr::filter(year == year)
  
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Results")
  openxlsx::addWorksheet(wb, "Species Look-up")
  
  if(nrow(dat_year) > 0) {
    results <- dat_year %>%
      dplyr::group_by(sample_site_name) %>%
      dplyr::mutate(sampling_method = paste0(sampling_method, collapse = " + ")) %>%
      dplyr::ungroup() %>%
      dplyr::distinct() %>%
      dplyr::mutate(
        Latitude  = if("geom" %in% names(.) & !all(is.na(geom))) round(sf::st_coordinates(geom)[, 2], 4) else NA,
        Longitude = if("geom" %in% names(.) & !all(is.na(geom))) round(sf::st_coordinates(geom)[, 1], 4) else NA
      ) %>%
      dplyr::mutate(
        dplyr::across(
          c(
            fish_sampling_results_q_pcr_mc_detected,
            fish_species_sampled,
            e_dna_results_mc,
            e_dna_results_tubifex
          ),
          ~ tidyr::replace_na(.x, "NA")
        )
      ) %>%
      sf::st_drop_geometry()
    
    
    if(year == "2025") {
      
      results <- results %>%
        dplyr::select(
          `Waterbody Name` = waterbody_name,
          `Sample Site` = sample_site_name,
          Latitude,
          Longitude,
          `Sampling Method` = sampling_method,
          `Fish Species Sampled` = fish_species_sampled,
          `Fish Sampling Results` = fish_sampling_results_q_pcr_mc_detected,
          `eDNA Sampling Results (M. cerebralis - parasite)` = e_dna_results_mc,
          `eDNA Sampling Results (Tubifex worm)` = e_dna_results_tubifex
        )
    } else {
      results <- results %>%
        dplyr::select(
          `Sample Site` = sample_site_name,
          Latitude,
          Longitude,
          `Sampling Method` = sampling_method,
          `Fish Species Sampled` = fish_species_sampled,
          `Fish Sampling Results` = fish_sampling_results_q_pcr_mc_detected,
          `eDNA Sampling Results (M. cerebralis - parasite)` = e_dna_results_mc,
          `eDNA Sampling Results (Tubifex worm)` = e_dna_results_tubifex
        )
    }
    
    results <- results %>% dplyr::arrange(`Sample Site`)
    
    openxlsx::writeData(wb, "Results", results)
    openxlsx::setColWidths(wb, sheet = 1, cols = 1:ncol(results), widths = "auto")
    
  } else {
    openxlsx::writeData(wb, "Results", "No data available for this year")
  }
  
  # Species lookup (same for all years)
  openxlsx::writeData(
    wb,
    "Species Look-up",
    tibble::tibble(
      acronym = c("BT","EBT","KOK","MW","RBT","SK","WCT"),
      species = c(
        "Bull Trout","Eastern Brook Trout","Kokanee",
        "Mountain Whitefish","Rainbow Trout",
        "Sockeye Salmon","Westslope Cutthroat Trout"
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

