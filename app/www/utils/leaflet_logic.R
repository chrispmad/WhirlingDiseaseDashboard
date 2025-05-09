make_leaf_tbl = function(dat){
  dat |> 
    sf::st_drop_geometry() |> 
    dplyr::select(`Sample Site` = sample_site_name,
                  `Sampled in 2024` = sampled_in_2024_y_n,
                  `Sampling Method` = sampling_method,
                  `Fish Species Sampled` = fish_species_sampled,
                  `Fish Sampling Results` = fish_sampling_results_q_pcr_mc_detected,
                  `eDNA Sampling Results (Mc)` = e_dna_results_mc,
                  `eDNA Sampling Results (Tubifex)` = e_dna_results_tubifex) |> 
    leafpop::popupTable()
}

fish_pos = leaflet::makeIcon(iconUrl = "https://cdn-icons-png.flaticon.com/512/15735/15735559.png",
                             iconWidth = 25, iconHeight = 25,
                             iconAnchorX = 12, iconAnchorY = 12,
                             className = 'red-square')

fish_neg = leaflet::makeIcon(iconUrl = "https://cdn-icons-png.flaticon.com/512/15735/15735559.png",
                             iconWidth = 25, iconHeight = 25,
                             iconAnchorX = 12, iconAnchorY = 12,
                             className = 'blue-square')

legend_html <- HTML(
  paste0(
    "<div style='background: #ffffff00; padding: 0px; border-radius: 5px;'>
     <strong>Fish Results</strong>
     <li><img src='https://cdn-icons-png.flaticon.com/512/15735/15735559.png' height='24' style='vertical-align:middle;' class = 'red-square'> Positive
     <li><img src='https://cdn-icons-png.flaticon.com/512/15735/15735559.png' height='24' style='vertical-align:middle;' class = 'blue-square'> Negative
   </div>"
  )
)

output$my_leaf = renderLeaflet({
  
  leaflet() |> 
    addTiles() |> 
    addLayersControl(overlayGroups = c("Fish Results","eDNA Results"),
                     position = 'bottomleft',
                     options = leaflet::layersControlOptions(collapsed = F)) |> 
    addMapPane(name = 'fish_results', zIndex = 400) |> 
    addMapPane(name = 'eDNA_results', zIndex = 500) |> 
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
  
  if(nrow(dat) > 0){
    
    pos_fish_dat = dat |> 
      dplyr::filter(sampling_method == 'Fish') |> 
      dplyr::filter(fish_sampling_results_q_pcr_mc_detected == "Positive") |> 
      dplyr::filter(!is.na(fish_sampling_results_q_pcr_mc_detected))
    
    neg_fish_dat = dat |> 
      dplyr::filter(sampling_method == 'Fish') |> 
      dplyr::filter(fish_sampling_results_q_pcr_mc_detected == "Negative") |> 
      dplyr::filter(!is.na(fish_sampling_results_q_pcr_mc_detected))
    
    pos_fish_tbl = make_leaf_tbl(pos_fish_dat)
    neg_fish_tbl = make_leaf_tbl(neg_fish_dat)
    
    l = l |> 
      # Add fish sampling
      addMarkers(
        data = pos_fish_dat,
        icon = ~fish_pos,
        group = "Fish Results",
        label = lapply(pos_fish_tbl, htmltools::HTML),
        options = pathOptions(pane = 'fish_results')
      ) |> 
      addMarkers(
        data = neg_fish_dat,
        icon = ~fish_neg,
        group = "Fish Results",
        label = lapply(neg_fish_tbl, htmltools::HTML),
        options = pathOptions(pane = 'fish_results')
      ) |> 
      addControl(
        html = legend_html,
        position = "topright"
      )

    edna_dat = dat |> 
      dplyr::filter(sampling_method == 'Fish') |> 
      dplyr::filter(!is.na(e_dna_results_mc)) |> 
      dplyr::filter(e_dna_results_mc != "NA")
      
    edna_dat_tbl = make_leaf_tbl(edna_dat)
    
    l = l |> 
      # Add eDNA sampling
      addCircleMarkers(
        data = edna_dat,
        fillColor = ~edna_results_colour,
        fillOpacity = 0.6,
        color = 'black',
        weight = 1,
        group = 'eDNA Results',
        label = lapply(edna_dat_tbl, htmltools::HTML),
        options = pathOptions(pane = 'eDNA_results')
      ) |> 
      addLegend(title = 'eDNA (Mc) Results',
                colors = unique(edna_dat$edna_results_colour),
                labels = unique(na.omit(edna_dat$e_dna_results_mc))
      )
    
    l
  }
})