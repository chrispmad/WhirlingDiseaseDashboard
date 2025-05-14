output$file_update_date = renderText({
  paste0("Data Updated on ",format(as.Date(file.info('sampling_results.gpkg')$mtime),format='%Y-%b-%d'))
})

leaflet_tables = dat |> 
  sf::st_drop_geometry() |> 
  dplyr::select(sample_site_name,
                sampled_in_2024_y_n,
                sampling_method,
                fish_species_sampled,
                fish_sampling_results_q_pcr_mc_detected,
                e_dna_results_mc,
                e_dna_results_tubifex) |> 
  purrr::set_names(c("Sample Site", "Sampled in 2024", "Sampling Method",
                          "Fish Species Sampled", "Fish Sampling Results",
                          "eDNA Sampling Results (Mc)",
                          "eDNA Sampling Results (Tubifex)")) |> 
  leafpop::popupTable()

# edna_leafpops = reactive({
#   out = dat_e() |> 
#     sf::st_drop_geometry() |> 
#     dplyr::select(sample_site_name,
#                   sampled_in_2024_y_n,
#                   sampling_method,
#                   fish_species_sampled,
#                   fish_sampling_results_q_pcr_mc_detected,
#                   e_dna_results_mc_detection)
#   names(out) = c("Sample Site", "Sampled in 2024", "Sampling Method",
#                  "Fish Species Sampled", "Fish Sampling Results",
#                  "eDNA Sampling Results")
#   out |> leafpop::popupTable()
# })