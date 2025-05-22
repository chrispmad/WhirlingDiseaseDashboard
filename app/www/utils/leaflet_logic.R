make_leaf_tbl = function(dat){
  dat |> 
    dplyr::mutate(Latitude = sf::st_coordinates(geom)[,2],
                  Longitude = sf::st_coordinates(geom)[,1]) |> 
    sf::st_drop_geometry() |> 
    dplyr::select(`Sample Site` = sample_site_name,
                  Latitude,
                  Longitude,
                  `Sampling Method` = sampling_method,
                  `Fish Species Sampled` = fish_species_sampled,
                  `Fish Sampling Results` = fish_sampling_results_q_pcr_mc_detected,
                  `eDNA Sampling Results (M. cerebralis - parasite)` = e_dna_results_mc,
                  `eDNA Sampling Results (Tubifex worm)` = e_dna_results_tubifex) |> 
    leafpop::popupTable()
}

fish_pos = leaflet::icons(iconUrl = "https://www.freeiconspng.com/uploads/orange-square-image-2.png",
                             iconWidth = 25, iconHeight = 25,
                             iconAnchorX = 12, iconAnchorY = 12,
                             className = 'orange-marker')

fish_neg = leaflet::makeIcon(iconUrl = "https://www.freeiconspng.com/uploads/orange-square-image-2.png",
                             iconWidth = 25, iconHeight = 25,
                             iconAnchorX = 12, iconAnchorY = 12,
                             className = 'purple-marker')

edna_tub_pos = leaflet::makeIcon(iconUrl = "https://www.freeiconspng.com/uploads/orange-triangle-image-vector-0.png",
                             iconWidth = 25, iconHeight = 25,
                             iconAnchorX = 12, iconAnchorY = 12,
                             className = 'orange-marker')

edna_tub_neg = leaflet::makeIcon(iconUrl = "https://www.freeiconspng.com/uploads/orange-triangle-image-vector-0.png",
                             iconWidth = 25, iconHeight = 25,
                             iconAnchorX = 12, iconAnchorY = 12,
                             className = 'purple-marker')

full_legend = HTML(
    paste0(
      "<div style = 'background: #ffffff00; text-align: center; padding: 0px; border-radius: 5px;'>
       <strong style = 'font-size: large;'>Results</strong>
        <div class = 'legend-custom-row'> <u>eDNA (M. cerebralis - parasite)</u> </div>
        <div class = 'legend-custom-row'>
            <i class='fa-solid fa-circle orange-marker-legend' style='margin-left:5px;'></i>
            <i class='fa-regular fa-circle' style='margin-left:5px;filter:brightness(0)'></i>
            Positive
            <i class='fa-solid fa-circle purple-marker-legend' style='margin-left:5px;'></i>
            <i class='fa-regular fa-circle' style='margin-left:5px;filter:brightness(0)'></i>
            Negative
        </div>
        <div class = 'legend-custom-row'> <u>eDNA (Tubifex worm)</u> </div>
        <div class = 'legend-custom-row'>
            <img src='https://www.freeiconspng.com/uploads/orange-triangle-image-vector-0.png' height='24' style='vertical-align:middle;' class = 'orange-marker'> 
            Positive
            <img src='https://www.freeiconspng.com/uploads/orange-triangle-image-vector-0.png' height='24' style='vertical-align:middle;' class = 'purple-marker'> 
            Negative
        </div>
        <div class = 'legend-custom-row'> <u>Fish</u> </div>
        <div class = 'legend-custom-row'>
          <img src='https://www.freeiconspng.com/uploads/orange-square-image-2.png' height='24' style='vertical-align:middle;' class = 'orange-marker'> 
          Positive
          <img src='https://www.freeiconspng.com/uploads/orange-square-image-2.png' height='24' style='vertical-align:middle;' class = 'purple-marker'> 
          Negative
        </div>
        <p>Note: metadata can be seen by <br>hovering over the points.</p>
     </div>"
    )
  )

results_legend = HTML(
  paste0(
    "<div style='background: #ffffff00; padding: 0px; border-radius: 5px;'>
       <strong>Result</strong>
        <div class = 'legend-custom-row'>
            <i class='fa-solid fa-circle orange-marker-legend' style='margin-left:5px;'></i>
            <i class='fa-regular fa-circle' style='margin-left:5px;filter:brightness(0)'></i>
            Positive
        </div>
        <div class = 'legend-custom-row'>
            <i class='fa-solid fa-circle purple-marker-legend' style='margin-left:5px;'></i> 
            <i class='fa-regular fa-circle' style='margin-left:5px;filter:brightness(0)'></i> 
            Negative
        </div>
     </div>"
  )
)
output$my_leaf = renderLeaflet({
  
  leaflet() |> 
    addTiles() |> 
    addLayersControl(overlayGroups = c("eDNA Results (M. cerebralis - parasite)", "eDNA Results (Tubifex worm)","Fish Results"),
                     position = 'bottomleft',
                     options = leaflet::layersControlOptions(collapsed = F)) |> 
    addMapPane(name = 'Fish Results', zIndex = 400) |> 
    addMapPane(name = 'eDNA Results (Tubifex)', zIndex = 500) |> 
    addMapPane(name = 'eDNA Results (parasite)', zIndex = 600) |> 
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
        options = pathOptions(pane = 'Fish Results')
      ) |> 
      addMarkers(
        data = neg_fish_dat,
        icon = ~fish_neg,
        group = "Fish Results",
        label = lapply(neg_fish_tbl, htmltools::HTML),
        options = pathOptions(pane = 'Fish Results')
      ) |> 
      addControl(
        html = full_legend,
        position = 'topright'
      ) #|> 
      # addControl(
      #   html = results_legend,
      #   position = 'topright'
      # )

    edna_dat_myx = dat |> 
      dplyr::filter(!is.na(e_dna_results_mc)) |> 
      dplyr::filter(e_dna_results_mc != "NA")
      
    edna_dat_myx_tbl = make_leaf_tbl(edna_dat_myx)
    
    edna_dat_tub = dat |> 
      # dplyr::filter(sampling_method == 'Fish') |>
      dplyr::filter(!is.na(e_dna_results_mc)) |> 
      dplyr::filter(e_dna_results_tubifex != "NA")
    
    edna_dat_tub_neg = edna_dat_tub |> 
      dplyr::filter(e_dna_results_tubifex == 'Negative')
    
    edna_dat_tub_pos = edna_dat_tub |> 
      dplyr::filter(e_dna_results_tubifex == 'Positive')
    
    edna_dat_tub_pos_tbl = make_leaf_tbl(edna_dat_tub_pos)
    edna_dat_tub_neg_tbl = make_leaf_tbl(edna_dat_tub_neg)
    
    l = l |> 
      # Add Myx eDNA sampling
      addCircleMarkers(
        data = edna_dat_myx,
        fillColor = ~e_dna_myx_colour,
        fillOpacity = 0.8,
        color = 'black',
        weight = 2.5,
        opacity = 0.8,
        radius = 12,
        group = 'eDNA Results (M. cerebralis - parasite)',
        label = lapply(edna_dat_myx_tbl, htmltools::HTML),
        options = pathOptions(pane = 'eDNA Results (parasite)')
      ) |> 
      # Add Tubifex eDNA sampling
      addMarkers(
        data = edna_dat_tub_neg,
        icon = ~edna_tub_neg,
        group = "eDNA Results (Tubifex worm)",
        label = lapply(edna_dat_tub_neg_tbl, htmltools::HTML),
        options = pathOptions(pane = 'eDNA Results (Tubifex)')
      ) |> 
      addMarkers(
        data = edna_dat_tub_pos,
        icon = ~edna_tub_pos,
        group = "eDNA Results (Tubifex worm)",
        label = lapply(edna_dat_tub_pos_tbl, htmltools::HTML),
        options = pathOptions(pane = 'eDNA Results (Tubifex)')
      )
    
    l
  }
})