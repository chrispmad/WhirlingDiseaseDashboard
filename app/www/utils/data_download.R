output$data_dl <- downloadHandler(
  filename = function() {
    paste0('BC_WhirlingDisease_', Sys.Date(), '.csv')
  },
  content = function(con) {
    write.csv(dat |> 
                sf::st_drop_geometry() |> 
                dplyr::select(`Sample Site` = sample_site_name,
                              `Sampled in 2024` = sampled_in_2024_y_n,
                              `Sampling Method` = sampling_method,
                              `Fish Species Sampled` = fish_species_sampled,
                              `Fish Sampling Results` = fish_sampling_results_q_pcr_mc_detected,
                              `eDNA Sampling Results (Mc)` = e_dna_results_mc,
                              `eDNA Sampling Results (Tubifex)` = e_dna_results_tubifex), 
              con, row.names = F)
  }
)