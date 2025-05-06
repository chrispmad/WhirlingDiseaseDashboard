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