output$data_dl <- downloadHandler(
  filename = function() {
    paste0('BC_WhirlingDisease_', 
           stringr::str_extract(lubridate::with_tz(Sys.Date(), tzone = "America/Vancouver"), "[0-9]{4}-[0-9]{2}-[0-9]{2}"),
           '.xlsx')
  },
  content = function(con) {
    my_wb = openxlsx::createWorkbook()
    openxlsx::addWorksheet(my_wb, "Results")
    openxlsx::addWorksheet(my_wb, "Species Look-up")

    results_for_download = dat |>
      dplyr::group_by(sample_site_name) |>
      dplyr::mutate(sampling_method = paste0(sampling_method, collapse = ' + ')) |>
      dplyr::ungroup() |>
      dplyr::distinct() |>
      dplyr::mutate(Latitude = round(sf::st_coordinates(geom)[,2],4),
                    Longitude = round(sf::st_coordinates(geom)[,1],4)) |>
      dplyr::mutate(fish_sampling_results_q_pcr_mc_detected = tidyr::replace_na(fish_sampling_results_q_pcr_mc_detected, "NA")) |>
      dplyr::mutate(fish_species_sampled = tidyr::replace_na(fish_species_sampled, "NA")) |>
      dplyr::mutate(e_dna_results_mc = tidyr::replace_na(e_dna_results_mc, "NA")) |>
      dplyr::mutate(e_dna_results_tubifex = tidyr::replace_na(e_dna_results_tubifex, "NA")) |>
      sf::st_drop_geometry() |>
      dplyr::select(`Sample Site` = sample_site_name,
                    Latitude,
                    Longitude,
                    `Sampling Method` = sampling_method,
                    `Fish Species Sampled` = fish_species_sampled,
                    `Fish Sampling Results` = fish_sampling_results_q_pcr_mc_detected,
                    `eDNA Sampling Results (M. cerebralis - parasite)` = e_dna_results_mc,
                    `eDNA Sampling Results (Tubifex worm)` = e_dna_results_tubifex) |>
      dplyr::distinct() |>
      dplyr::arrange(`Sample Site`)


    openxlsx::writeData(my_wb, "Results", results_for_download)
    openxlsx::setColWidths(my_wb, sheet = 1, cols = 1:ncol(results_for_download), widths = "auto")

    species_lookup_tbl = data.frame(
      acronym = c("BT","EBT","KOK","MW","RBT","SK","WCT"),
      species = c("Bull Trout","Eastern Brook Trout", "Kokanee",
                  "Mountain Whitefish","Rainbow Trout","Sockeye Salmon",
                  "Westslope Cutthroat Trout")
    )

    openxlsx::writeData(my_wb,"Species Look-up", species_lookup_tbl)


    openxlsx::saveWorkbook(my_wb, con, overwrite = T)
  }
)