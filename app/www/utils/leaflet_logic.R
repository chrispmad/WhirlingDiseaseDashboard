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
           `eDNA Sampling Results (M. cerebralis - parasite)` = e_dna_results_mc,
           `eDNA Sampling Results (Tubifex worm)` = e_dna_results_tubifex) %>%
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
    "e_dna_results_mc", "e_dna_results_tubifex"
  )
  existing_cols <- select_cols[select_cols %in% names(dat)]
  dat <- dat %>% dplyr::select(all_of(existing_cols))
  
  rename_map <- c(
    "Waterbody Name" = "waterbody_name",
    "Site Name" = "sample_site_name",
    "Sampling Method" = "sampling_method",
    "Fish Species Sampled" = "fish_species_sampled",
    "Fish Sampling Results" = "fish_sampling_results_q_pcr_mc_detected",
    "eDNA Results (M. cerebralis - parasite)" = "e_dna_results_mc",
    "eDNA Results (Tubifex worm)" = "e_dna_results_tubifex"
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
        <div class = 'legend-custom-row'> <u>eDNA (Tubifex worm)</u> </div>
        <div class = 'legend-custom-row'>
            <img src='https://www.freeiconspng.com/uploads/orange-triangle-image-vector-0.png' height='24' style='vertical-align:middle;' class = 'orange-marker'> 
            Presence
            <img src='https://www.freeiconspng.com/uploads/orange-triangle-image-vector-0.png' height='24' style='vertical-align:middle;' class = 'purple-marker'> 
            Absence
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
                        "eDNA Results (Tubifex worm)",
                        "Fish Results"),
      position = "bottomleft",
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    addMapPane("Fish Results", zIndex = 400) %>%
    addMapPane("eDNA Results (Tubifex)", zIndex = 500) %>%
    addMapPane("eDNA Results (parasite)", zIndex = 600) %>%
    addPolygons(data = subw, color = "grey", weight = 1.5, fill = "transparent", label = ~watershed_name) %>%
    addPolygons(data = col, color = "purple", weight = 1.5, fill = "transparent", options = pathOptions(clickable = FALSE)) %>%
    addResetMapButton() %>%
    addScaleBar("bottomright")
})

output$my_leaf_2025 <- renderLeaflet({
  leaflet() %>%
    addTiles() %>%
    addLayersControl(
      overlayGroups = c("eDNA Results (M. cerebralis - parasite)",
                        "eDNA Results (Tubifex worm)",
                        "Fish Results"),
      position = "bottomleft",
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    addMapPane("Fish Results", zIndex = 400) %>%
    addMapPane("eDNA Results (Tubifex)", zIndex = 500) %>%
    addMapPane("eDNA Results (parasite)", zIndex = 600) %>%
    addPolygons(data = subw, color = "grey", weight = 1.5, fill = "transparent", label = ~watershed_name) %>%
    addPolygons(data = col, color = "purple", weight = 1.5, fill = "transparent", options = pathOptions(clickable = FALSE)) %>%
    addResetMapButton() %>%
    addScaleBar("bottomright")
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
  
  # eDNA Tubifex
  edna_tub_present <- dat %>% filter(!is.na(e_dna_results_tubifex), e_dna_results_tubifex=="Present")
  edna_tub_absent <- dat %>% filter(!is.na(e_dna_results_tubifex), e_dna_results_tubifex=="Absent")
  
  
  if(nrow(edna_tub_present) > 0) l <- l %>% addMarkers(
    data = shunt_dat_2024(edna_tub_present,"parasite"),
    icon = ~edna_tub_pos,
    group = "eDNA Results (Tubifex worm)",
    label = lapply(make_leaf_tbl_2024(edna_tub_present), htmltools::HTML),
    options = pathOptions(pane="eDNA Results (Tubifex)")
  )
  
  if(nrow(edna_tub_absent) > 0) l <- l %>% addMarkers(
    data = shunt_dat_2024(edna_tub_absent,"parasite"),
    icon = ~edna_tub_neg,
    group = "eDNA Results (Tubifex worm)",
    label = lapply(make_leaf_tbl_2024(edna_tub_absent), htmltools::HTML),
    options = pathOptions(pane="eDNA Results (Tubifex)")
  )
  
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
  
  # eDNA Tubifex
  edna_tub_present <- dat_2025 %>% filter(!is.na(e_dna_results_tubifex), e_dna_results_tubifex=="Present")
  edna_tub_absent <- dat_2025 %>% filter(!is.na(e_dna_results_tubifex), e_dna_results_tubifex=="Absent")
  
  if(nrow(edna_tub_present) > 0) l <- l %>% addMarkers(
    data = shunt_dat_2025(edna_tub_present,"parasite"),
    icon = ~edna_tub_pos,
    group = "eDNA Results (Tubifex worm)",
    label = lapply(make_leaf_tbl_2025(edna_tub_present), htmltools::HTML),
    options = pathOptions(pane="eDNA Results (Tubifex)")
  )
  
  if(nrow(edna_tub_absent) > 0) l <- l %>% addMarkers(
    data = shunt_dat_2025(edna_tub_absent,"parasite"),
    icon = ~edna_tub_neg,
    group = "eDNA Results (Tubifex worm)",
    label = lapply(make_leaf_tbl_2025(edna_tub_absent), htmltools::HTML),
    options = pathOptions(pane="eDNA Results (Tubifex)")
  )
  
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
    