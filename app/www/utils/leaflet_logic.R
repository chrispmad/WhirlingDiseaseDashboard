# ----------------------------
# Leaflet zoom reactives
# ----------------------------
leaf_zoom_2024 <- reactive({
  req(!is.null(input$my_leaf_2024_zoom))
  input$my_leaf_2024_zoom
})

leaf_zoom_2025 <- reactive({
  req(!is.null(input$my_leaf_2025_zoom))
  input$my_leaf_2025_zoom
})

# ----------------------------
# Shunt amounts
# ----------------------------
shunt_amount_2024 <- reactive({
  case_when(
    leaf_zoom_2024() <= 8  ~ 0.1,
    leaf_zoom_2024() == 9  ~ 0.05,
    leaf_zoom_2024() == 10 ~ 0.025,
    leaf_zoom_2024() == 11 ~ 0.01,
    leaf_zoom_2024() == 12 ~ 0.005,
    leaf_zoom_2024() == 13 ~ 0.0025,
    TRUE ~ 0.001
  )
})

shunt_amount_2025 <- reactive({
  case_when(
    leaf_zoom_2025() <= 8  ~ 0.1,
    leaf_zoom_2025() == 9  ~ 0.05,
    leaf_zoom_2025() == 10 ~ 0.025,
    leaf_zoom_2025() == 11 ~ 0.01,
    leaf_zoom_2025() == 12 ~ 0.005,
    leaf_zoom_2025() == 13 ~ 0.0025,
    TRUE ~ 0.001
  )
})

# ----------------------------
# Shunt functions (identical to working single-year code)
# ----------------------------
shunt_dat_2024 <- function(dat, type){
  if("geom" %in% names(dat)) dat <- dplyr::rename(dat, geometry = geom)
  
  if(type == "fish"){
    dat <- dat %>%
      mutate(long_to_jitter = sf::st_coordinates(geometry)[,1],
             lat_to_jitter = sf::st_coordinates(geometry)[,2]) %>%
      mutate(long_to_jitter = long_to_jitter + shunt_amount_2024()) %>%
      sf::st_drop_geometry() %>%
      sf::st_as_sf(coords = c("long_to_jitter","lat_to_jitter"), crs = 4326)
  }
  
  if(type == "parasite"){
    dat <- dat %>%
      mutate(long_to_jitter = sf::st_coordinates(geometry)[,1],
             lat_to_jitter = sf::st_coordinates(geometry)[,2]) %>%
      mutate(long_to_jitter = long_to_jitter - shunt_amount_2024()) %>%
      sf::st_drop_geometry() %>%
      sf::st_as_sf(coords = c("long_to_jitter","lat_to_jitter"), crs = 4326)
  }
  
  dat
}

shunt_dat_2025 <- function(dat, type){
  if("geom" %in% names(dat)) dat <- dplyr::rename(dat, geometry = geom)
  
  if(type == "fish"){
    dat <- dat %>%
      mutate(long_to_jitter = sf::st_coordinates(geometry)[,1],
             lat_to_jitter = sf::st_coordinates(geometry)[,2]) %>%
      mutate(long_to_jitter = long_to_jitter + shunt_amount_2025()) %>%
      sf::st_drop_geometry() %>%
      sf::st_as_sf(coords = c("long_to_jitter","lat_to_jitter"), crs = 4326)
  }
  
  if(type == "parasite"){
    dat <- dat %>%
      mutate(long_to_jitter = sf::st_coordinates(geometry)[,1],
             lat_to_jitter = sf::st_coordinates(geometry)[,2]) %>%
      mutate(long_to_jitter = long_to_jitter - shunt_amount_2025()) %>%
      sf::st_drop_geometry() %>%
      sf::st_as_sf(coords = c("long_to_jitter","lat_to_jitter"), crs = 4326)
  }
  
  dat
}

# ----------------------------
# Leaflet popup tables (exactly as one-year code)
# ----------------------------
make_leaf_tbl_2024 <- function(dat){
  if(!inherits(dat,"sf") || nrow(dat)==0) return(NULL)
  
  dat %>%
    mutate(Latitude = sf::st_coordinates(geom)[,2],
           Longitude = sf::st_coordinates(geom)[,1]) %>%
    sf::st_drop_geometry() %>%
    select(`Sample Site` = sample_site_name,
           Latitude,
           Longitude,
           `Sampling Method` = sampling_method,
           `Fish Species Sampled` = fish_species_sampled,
           `Fish Sampling Results` = fish_sampling_results_q_pcr_mc_detected,
           `eDNA Sampling Results (M. cerebralis - parasite)` = e_dna_results_mc) %>%
    leafpop::popupTable()
}

make_leaf_tbl_2025 <- function(dat){
  if(!inherits(dat,"sf") || nrow(dat)==0) return(NULL)
  
  if("geom" %in% names(dat)) dat <- dplyr::rename(dat, geometry = geom)
  coords <- sf::st_coordinates(sf::st_geometry(dat))
  dat <- dat %>%
    mutate(Latitude = coords[,2], Longitude = coords[,1]) %>%
    sf::st_drop_geometry()
  
  select_cols <- c(
    "waterbody_name", "sample_site_name", "Latitude", "Longitude",
    "sampling_method", "fish_species_sampled",
    "fish_sampling_results_q_pcr_mc_detected",
    "e_dna_results_mc"
  )
  existing_cols <- select_cols[select_cols %in% names(dat)]
  dat <- dat %>% dplyr::select(all_of(existing_cols))
  
  rename_map <- c(
    "Waterbody Name" = "waterbody_name",
    "Site Name" = "sample_site_name",
    "Sampling Method" = "sampling_method",
    "Fish Species Sampled" = "fish_species_sampled",
    "Fish Sampling Results" = "fish_sampling_results_q_pcr_mc_detected",
    "eDNA Results (M. cerebralis - parasite)" = "e_dna_results_mc"
  )
  rename_map <- rename_map[names(rename_map) %in% names(dat)]
  dat <- dplyr::rename(dat, !!!rename_map)
  if(nrow(dat)==0) return(NULL)
  
  leafpop::popupTable(dat)
}

# ----------------------------
# Leaflet icons
# ----------------------------
fish_pos <- makeIcon("https://www.freeiconspng.com/uploads/orange-square-image-2.png",
                     iconWidth = 25, iconHeight = 25,
                     iconAnchorX = 12, iconAnchorY = 12,
                     className = 'orange-marker')
fish_neg <- makeIcon("https://www.freeiconspng.com/uploads/orange-square-image-2.png",
                     iconWidth = 25, iconHeight = 25,
                     iconAnchorX = 12, iconAnchorY = 12,
                     className = 'purple-marker')

edna_tub_pos <- makeIcon("https://www.freeiconspng.com/uploads/orange-triangle-image-vector-0.png",
                         iconWidth = 25, iconHeight = 25,
                         iconAnchorX = 12, iconAnchorY = 12,
                         className = 'orange-marker')
edna_tub_neg <- makeIcon("https://www.freeiconspng.com/uploads/orange-triangle-image-vector-0.png",
                         iconWidth = 25, iconHeight = 25,
                         iconAnchorX = 12, iconAnchorY = 12,
                         className = 'purple-marker')

# ----------------------------
# Full legend (identical for both years)
# ----------------------------
full_legend = HTML(
  paste0(
    "<div style = 'background: #ffffff00; text-align: center; padding: 0px; border-radius: 5px;'>
       <strong style = 'font-size: large;'>Results</strong>
        <div class = 'legend-custom-row'> <u>eDNA (M. cerebralis - parasite)</u> </div>
        <div class = 'legend-custom-row'>
            <i class='fa-solid fa-circle orange-marker-legend' style='margin-left:5px;'></i>
            <i class='fa-regular fa-circle' style='margin-left:5px;filter:brightness(0)'></i>
            Suspect
            <i class='fa-solid fa-circle purple-marker-legend' style='margin-left:5px;'></i>
            <i class='fa-regular fa-circle' style='margin-left:5px;filter:brightness(0)'></i>
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
        <p>Note: Overlapping results are displayed <br> with a slight jitter to facilitate viewing</p>
     </div>"
  )
)

# ----------------------------
# Leaflet renderers for 2024 and 2025
# ----------------------------
output$my_leaf_2024 <- renderLeaflet({
  leaflet() %>%
    addTiles() %>%
    addLayersControl(
      overlayGroups = c("eDNA Results (M. cerebralis - parasite)",
                        "Fish Results"),
      position = "bottomleft",
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    addMapPane("Fish Results", zIndex = 400) %>%
    addMapPane("eDNA Results (parasite)", zIndex = 600) %>%
    addPolygons(data = subw, color = "grey", weight = 1.5, fill = "transparent", label = ~watershed_name) %>%
    addPolygons(data = col, color = "purple", weight = 1.5, fill = "transparent", options = pathOptions(clickable = FALSE)) %>%
    addResetMapButton() %>%
    addScaleBar("bottomleft")
})

output$my_leaf_2025 <- renderLeaflet({
  leaflet() %>%
    addTiles() %>%
    addLayersControl(
      overlayGroups = c("eDNA Results (M. cerebralis - parasite)",
                        "Fish Results"),
      position = "bottomleft",
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    addMapPane("Fish Results", zIndex = 400) %>%
    addMapPane("eDNA Results (parasite)", zIndex = 600) %>%
    addPolygons(data = subw, color = "grey", weight = 1.5, fill = "transparent", label = ~watershed_name) %>%
    addPolygons(data = col, color = "purple", weight = 1.5, fill = "transparent", options = pathOptions(clickable = FALSE)) %>%
    addResetMapButton() %>%
    addScaleBar("bottomleft")
})

# ----------------------------
# Observers for 2024 and 2025
# ----------------------------
observe({
  req(dat)
  if(!inherits(dat,"sf") || nrow(dat)==0) return()
  
  l <- leafletProxy("my_leaf_2024") %>%
    clearMarkers() %>%
    clearMarkerClusters() %>%
    clearControls()
  
  # Fish
  pos_fish <- dat %>% filter(sampling_method=="Fish", fish_sampling_results_q_pcr_mc_detected=="Positive")
  neg_fish <- dat %>% filter(sampling_method=="Fish", fish_sampling_results_q_pcr_mc_detected=="Negative")
  
  if(nrow(pos_fish) > 0) l <- l %>% addMarkers(
    data = shunt_dat_2024(pos_fish,"fish"),
    icon = ~fish_pos,
    group = "Fish Results",
    label = lapply(make_leaf_tbl_2024(pos_fish), htmltools::HTML),
    options = pathOptions(pane="Fish Results")
  )
  
  if(nrow(neg_fish) > 0) l <- l %>% addMarkers(
    data = shunt_dat_2024(neg_fish,"fish"),
    icon = ~fish_neg,
    group = "Fish Results",
    label = lapply(make_leaf_tbl_2024(neg_fish), htmltools::HTML),
    options = pathOptions(pane="Fish Results")
  )
  
  # # eDNA Tubifex
  # edna_tub_present <- dat %>% filter(!is.na(e_dna_results_tubifex), e_dna_results_tubifex=="Present")
  # edna_tub_absent <- dat %>% filter(!is.na(e_dna_results_tubifex), e_dna_results_tubifex=="Absent")
  # 
  # 
  # if(nrow(edna_tub_present) > 0) l <- l %>% addMarkers(
  #   data = shunt_dat_2024(edna_tub_present,"parasite"),
  #   icon = ~edna_tub_pos,
  #   group = "eDNA Results (Tubifex worm)",
  #   label = lapply(make_leaf_tbl_2024(edna_tub_present), htmltools::HTML),
  #   options = pathOptions(pane="eDNA Results (Tubifex)")
  # )
  # 
  # if(nrow(edna_tub_absent) > 0) l <- l %>% addMarkers(
  #   data = shunt_dat_2024(edna_tub_absent,"parasite"),
  #   icon = ~edna_tub_neg,
  #   group = "eDNA Results (Tubifex worm)",
  #   label = lapply(make_leaf_tbl_2024(edna_tub_absent), htmltools::HTML),
  #   options = pathOptions(pane="eDNA Results (Tubifex)")
  # )
  
  # eDNA Myx
  edna_myx <- dat %>% filter(!is.na(e_dna_results_mc), e_dna_results_mc!="NA")
  if(nrow(edna_myx) > 0) l <- l %>% addCircleMarkers(
    data = shunt_dat_2024(edna_myx,"parasite"),
    fillColor = ~e_dna_myx_colour,
    fillOpacity = 0.8,
    color = "black",
    weight = 2.5,
    opacity = 0.8,
    radius = 12,
    group = "eDNA Results (M. cerebralis - parasite)",
    label = lapply(make_leaf_tbl_2024(edna_myx), htmltools::HTML),
    options = pathOptions(pane="eDNA Results (parasite)")
  )
  
  l <- l %>% addControl(html = full_legend, position = "topright")
})

observe({
  req(dat_2025)
  if(!inherits(dat_2025,"sf") || nrow(dat_2025)==0) return()
  
  l <- leafletProxy("my_leaf_2025") %>%
    clearMarkers() %>%
    clearMarkerClusters() %>%
    clearControls()
  
  # Fish
  pos_fish <- dat_2025 %>% filter(sampling_method=="Fish", fish_sampling_results_q_pcr_mc_detected=="Positive")
  neg_fish <- dat_2025 %>% filter(sampling_method=="Fish", fish_sampling_results_q_pcr_mc_detected=="Negative")
  
  if(nrow(pos_fish) > 0) l <- l %>% addMarkers(
    data = shunt_dat_2025(pos_fish,"fish"),
    icon = ~fish_pos,
    group = "Fish Results",
    label = lapply(make_leaf_tbl_2025(pos_fish), htmltools::HTML),
    options = pathOptions(pane="Fish Results")
  )
  
  if(nrow(neg_fish) > 0) l <- l %>% addMarkers(
    data = shunt_dat_2025(neg_fish,"fish"),
    icon = ~fish_neg,
    group = "Fish Results",
    label = lapply(make_leaf_tbl_2025(neg_fish), htmltools::HTML),
    options = pathOptions(pane="Fish Results")
  )
  
  # # eDNA Tubifex
  # edna_tub_present <- dat_2025 %>% filter(!is.na(e_dna_results_tubifex), e_dna_results_tubifex=="Present")
  # edna_tub_absent <- dat_2025 %>% filter(!is.na(e_dna_results_tubifex), e_dna_results_tubifex=="Absent")
  # 
  # if(nrow(edna_tub_present) > 0) l <- l %>% addMarkers(
  #   data = shunt_dat_2025(edna_tub_present,"parasite"),
  #   icon = ~edna_tub_pos,
  #   group = "eDNA Results (Tubifex worm)",
  #   label = lapply(make_leaf_tbl_2025(edna_tub_present), htmltools::HTML),
  #   options = pathOptions(pane="eDNA Results (Tubifex)")
  # )
  # 
  # if(nrow(edna_tub_absent) > 0) l <- l %>% addMarkers(
  #   data = shunt_dat_2025(edna_tub_absent,"parasite"),
  #   icon = ~edna_tub_neg,
  #   group = "eDNA Results (Tubifex worm)",
  #   label = lapply(make_leaf_tbl_2025(edna_tub_absent), htmltools::HTML),
  #   options = pathOptions(pane="eDNA Results (Tubifex)")
  # )
  
  # eDNA Myx
  edna_myx <- dat_2025 %>% filter(!is.na(e_dna_results_mc), e_dna_results_mc!="NA")
  if(nrow(edna_myx) > 0) l <- l %>% addCircleMarkers(
    data = shunt_dat_2025(edna_myx,"parasite"),
    fillColor = ~e_dna_myx_colour,
    fillOpacity = 0.8,
    color = "black",
    weight = 2.5,
    opacity = 0.8,
    radius = 12,
    group = "eDNA Results (M. cerebralis - parasite)",
    label = lapply(make_leaf_tbl_2025(edna_myx), htmltools::HTML),
    options = pathOptions(pane="eDNA Results (parasite)")
  )
  
  # Add the legend
  l <- l %>% addControl(html = full_legend, position = "topright")
})


#---------------------------
# Lets make new leaflets. We need one for 2024 edna, one for 2024 fish, 2025 edna, one for 2025 fish
# We should split the data first, then make the leaflet objects. 

edna_2024 = dat |> 
  filter(sampling_method == "eDNA")
fish_2024 = dat |> 
  filter(sampling_method == "Fish")  
edna_2025 = dat_2025 |> 
  filter(sampling_method == "eDNA")
fish_2025 = dat_2025 |> 
  filter(sampling_method == "Fish")
edna_2025 <- edna_2025 |> 
  filter(!is.na(e_dna_results_mc))


fish_2025 <- fish_2025 |> 
  mutate(
    fish_sampling_results_q_pcr_mc_detected = case_when(
      is.na(fish_sampling_results_q_pcr_mc_detected) ~ "Waiting Results",
      TRUE ~ fish_sampling_results_q_pcr_mc_detected
    )
  )


dat_2025 <- dat_2025 %>%
  # Colour by sample type
  mutate(sample_type_colour = case_when(
    sampling_method == "eDNA" ~ 'gold',
    sampling_method == "Fish" ~ 'salmon',
    sampling_method == "Fish + eDNA" ~ 'orange',  # optional
    TRUE ~ 'black'
  )) %>%
  # Standardized result colours for both fish and eDNA
  mutate(result_colour = case_when(
    fish_sampling_results_q_pcr_mc_detected == "Negative" ~ "purple",
    fish_sampling_results_q_pcr_mc_detected == "Positive" ~ "orange",
    fish_sampling_results_q_pcr_mc_detected == "Waiting Results" ~ "grey",
    e_dna_results_mc == "Negative" ~ "purple",
    e_dna_results_mc == "Positive" ~ "orange",
    e_dna_results_mc == "Waiting Results" ~ "grey",
    TRUE ~ "black"
  ))


dat <- dat %>%
  # Colour by sample type
  mutate(sample_type_colour = case_when(
    sampling_method == "eDNA" ~ 'gold',
    sampling_method == "Fish" ~ 'salmon',
    sampling_method == "Fish + eDNA" ~ 'orange',  # optional
    TRUE ~ 'black'
  )) %>%
  # Standardized result colours for both fish and eDNA
  mutate(result_colour = case_when(
    fish_sampling_results_q_pcr_mc_detected == "Negative" ~ "purple",
    fish_sampling_results_q_pcr_mc_detected == "Positive" ~ "orange",
    fish_sampling_results_q_pcr_mc_detected == "Waiting Results" ~ "grey",
    e_dna_results_mc == "Negative" ~ "purple",
    e_dna_results_mc == "Positive" ~ "orange",
    e_dna_results_mc == "Waiting Results" ~ "grey",
    TRUE ~ "black"
  ))


fish_2024 <- fish_2024 %>%
  mutate(fish_result = case_when(
    fish_sampling_results_q_pcr_mc_detected == "Positive" ~ "Positive",
    fish_sampling_results_q_pcr_mc_detected == "Negative" ~ "Negative",
    fish_sampling_results_q_pcr_mc_detected %in% c("Pending","Waiting Results") ~ "Waiting Results",
    TRUE ~ "Waiting Results"
  ))

edna_2024 <- edna_2024 %>%
  mutate(edna_result = case_when(
    e_dna_results_mc == "Positive" ~ "Positive",
    e_dna_results_mc == "Negative" ~ "Negative",
    e_dna_results_mc %in% c("Pending","Waiting Results") ~ "Waiting Results",
    TRUE ~ "Waiting Results"
  ))

fish_2025 <- fish_2025 %>%
  mutate(fish_result = case_when(
    fish_sampling_results_q_pcr_mc_detected == "Positive" ~ "Positive",
    fish_sampling_results_q_pcr_mc_detected == "Negative" ~ "Negative",
    fish_sampling_results_q_pcr_mc_detected %in% c("Pending","Waiting Results") ~ "Waiting Results",
    TRUE ~ "Waiting Results"
  ))

edna_2025 <- edna_2025 %>%
  mutate(edna_result = case_when(
    e_dna_results_mc == "Positive" ~ "Positive",
    e_dna_results_mc == "Negative" ~ "Negative",
    e_dna_results_mc %in% c("Pending","Waiting Results") ~ "Waiting Results",
    TRUE ~ "Waiting Results"
  ))


result_cols <- c(
  "Positive" = "orange",
  "Negative" = "purple",
  "Waiting Results" = "grey"
)

# For fish 2025 (include Waiting Results)
pal_fish_2025 <- colorFactor(
  #palette = c("Positive" = "orange", "Negative" = "purple", "Waiting Results" = "grey"),
  #levels = c("Positive", "Negative", "Waiting Results")
  palette = c("Positive" = "orange", "Negative" = "purple"),
  levels = c("Positive", "Negative")
)

# For all other maps (no Waiting Results)
pal_no_waiting <- colorFactor(
  palette = c("Positive" = "orange", "Negative" = "purple"),
  levels = c("Positive", "Negative")
)

output$my_leaf_2024_fish <- renderLeaflet({
  leaflet(fish_2024) %>%
    addTiles() %>%
    addMapPane("watersheds", zIndex = 300) %>%
    addPolygons(
      data = subw, color = "grey", weight = 1.5,
      fill = "transparent", label = ~watershed_name,
      options = pathOptions(pane = "watersheds")
    ) %>%
    addMapPane("boundaries", zIndex = 400) %>%
    addPolygons(
      data = col, color = "purple", weight = 1.5,
      fill = "transparent", options = pathOptions(clickable = FALSE, pane = "boundaries")
    ) %>%
    addMapPane("points", zIndex = 500) %>%
    addCircleMarkers(
      lng = ~sf::st_coordinates(geom)[,1],
      lat = ~sf::st_coordinates(geom)[,2],
      fillColor = ~pal_no_waiting(fish_result),
      color = "black",
      fillOpacity = 0.8,
      radius = 10,
      label = lapply(make_leaf_tbl_2024(fish_2024), htmltools::HTML),
      options = pathOptions(pane = "points")
    ) %>%
    addLegend(
      "bottomleft",
      pal = pal_no_waiting,
      values = c("Positive", "Negative"),
      title = "Fish Sampling Result",
      opacity = 1
    )
})

output$my_leaf_2024_edna <- renderLeaflet({
  leaflet(edna_2024) %>%
    addTiles() %>%
    
    addMapPane("watersheds", zIndex = 300) %>%
    addPolygons(
      data = subw,
      color = "grey", weight = 1.5,
      fill = "transparent", label = ~watershed_name,
      options = pathOptions(pane = "watersheds")
    ) %>%
    
    addMapPane("boundaries", zIndex = 400) %>%
    addPolygons(
      data = col,
      color = "purple", weight = 1.5,
      fill = "transparent",
      options = pathOptions(clickable = FALSE, pane = "boundaries")
    ) %>%
    
    addMapPane("points", zIndex = 500) %>%
    addCircleMarkers(
      lng = ~sf::st_coordinates(geom)[,1],
      lat = ~sf::st_coordinates(geom)[,2],
      fillColor = ~pal_no_waiting(edna_result),
      color = "black",
      fillOpacity = 0.8,
      radius = 10,
      label = lapply(make_leaf_tbl_2024(edna_2024), htmltools::HTML),
      options = pathOptions(pane = "points")
    ) %>%
    
    addLegend(
      "bottomleft",
      pal = pal_no_waiting,
      values = c("Positive", "Negative"),
      title = "eDNA Result - M. cerebralis",
      opacity = 1
    )
})
output$my_leaf_2025_fish <- renderLeaflet({
  leaflet(fish_2025) %>%
    addTiles() %>%
    addMapPane("watersheds", zIndex = 300) %>%
    addPolygons(
      data = subw, color = "grey", weight = 1.5,
      fill = "transparent", options = pathOptions(pane = "watersheds")
    ) %>%
    addMapPane("boundaries", zIndex = 400) %>%
    addPolygons(
      data = col, color = "purple", weight = 1.5,
      fill = "transparent", options = pathOptions(clickable = FALSE, pane = "boundaries")
    ) %>%
    addMapPane("points", zIndex = 500) %>%
    addCircleMarkers(
      lng = ~sf::st_coordinates(geom)[,1],
      lat = ~sf::st_coordinates(geom)[,2],
      fillColor = ~pal_fish_2025(fish_result),
      color = "black",
      fillOpacity = 0.8,
      radius = 10,
      label = lapply(make_leaf_tbl_2025(fish_2025), htmltools::HTML),
      options = pathOptions(pane = "points")
    ) %>%
    addLegend(
      "bottomleft",
      pal = pal_fish_2025,
      values = c("Positive", "Negative", "Waiting Results"),
      title = "Fish Sampling Result",
      opacity = 1
    )
})
output$my_leaf_2025_edna <- renderLeaflet({
  leaflet(edna_2025) %>%
    addTiles() %>%
    addMapPane("watersheds", zIndex = 300) %>%
    addPolygons(
      data = subw, color = "grey", weight = 1.5,
      fill = "transparent", label = ~watershed_name,
      options = pathOptions(pane = "watersheds")
    ) %>%
    addMapPane("boundaries", zIndex = 400) %>%
    addPolygons(
      data = col, color = "purple", weight = 1.5,
      fill = "transparent", options = pathOptions(clickable = FALSE, pane = "boundaries")
    ) %>%
    addMapPane("points", zIndex = 500) %>%
    addCircleMarkers(
      lng = ~sf::st_coordinates(geom)[,1],
      lat = ~sf::st_coordinates(geom)[,2],
      fillColor = ~pal_no_waiting(edna_result),
      color = "black",
      fillOpacity = 0.8,
      radius = 10,
      label = lapply(make_leaf_tbl_2025(edna_2025), htmltools::HTML),
      options = pathOptions(pane = "points")
    ) %>%
    addLegend(
      "bottomleft",
      pal = pal_no_waiting,
      values = c("Positive", "Negative"),
      title = "eDNA Result - M. cerebralis",
      opacity = 1
    )
})
