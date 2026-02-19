# ----------------------------
# Leaflet icons
# ----------------------------
pos_marker_2025 <- makeIcon(
  "www/icons/orange_square.svg",
  iconWidth = 25, iconHeight = 25,
  iconAnchorX = 12, iconAnchorY = 12,
  shadowWidth = 200, 
  shadowHeight = 200,
  className = 'orange-marker'
)

neg_marker_2025 <- makeIcon(
  "www/icons/blue_square.svg",
  iconWidth = 25, iconHeight = 25,
  iconAnchorX = 12, iconAnchorY = 12,
  shadowWidth = 200, 
  shadowHeight = 200,
  className = 'purple-marker'
)

pos_marker_2024 <- makeIcon(
  "www/icons/orange_circle.svg",
  iconWidth = 25, iconHeight = 25,
  iconAnchorX = 12, iconAnchorY = 12,
  shadowWidth = 200, 
  shadowHeight = 200,
  className = 'orange-marker'
)

neg_marker_2024 <- makeIcon(
  "www/icons/blue_circle.svg",
  iconWidth = 25, iconHeight = 25,
  iconAnchorX = 12, iconAnchorY = 12,
  shadowWidth = 200, 
  shadowHeight = 200,
  className = 'purple-marker'
)


shunt_amount = reactive({
  dplyr::case_when(
    leaf_zoom() <= 6 ~ 0.5,
    leaf_zoom() <= 7 ~ 0.3,
    leaf_zoom() <= 8 ~ 0.1,
    leaf_zoom() == 9 ~ 0.05,
    leaf_zoom() == 10 ~ 0.025,
    leaf_zoom() == 11 ~ 0.01,
    leaf_zoom() == 12 ~ 0.005,
    leaf_zoom() == 13 ~ 0.0025,
    T ~ 0.001,
  )
})

rv <- reactiveValues(
  fish_zoom = NULL,
  edna_zoom = NULL
)

observeEvent(input$leaf_fish_zoom, {
  rv$fish_zoom <- input$leaf_fish_zoom
  message("Fish zoom updated:", rv$fish_zoom)
}, ignoreInit = TRUE)

observeEvent(input$leaf_edna_zoom, {
  rv$edna_zoom <- input$leaf_edna_zoom
  message("eDNA zoom updated:", rv$edna_zoom)
}, ignoreInit = TRUE)

leaf_zoom <- reactive({
  req(input$data_type)
  
  zoom <- switch(
    input$data_type,
    fish = rv$fish_zoom,
    edna = rv$edna_zoom
  )
  
  zoom
})

shunt_dat = function(dat, data_type){
  
  req(nrow(dat) > 0)
  req(!sf::st_is_empty(sf::st_geometry(dat)))
  
  if(data_type == '2024'){
    output = dat |> 
      dplyr::mutate(long_to_jitter = sf::st_coordinates(geom)[,1],
                    lat_to_jitter = sf::st_coordinates(geom)[,2]) |> 
      dplyr::mutate(long_to_jitter = long_to_jitter + shunt_amount()) |> 
      sf::st_drop_geometry() |> 
      sf::st_as_sf(coords = c("long_to_jitter","lat_to_jitter"), crs = 4326,sf_column_name = "geom")
  }
  if(data_type == '2025'){
    output = dat |> 
      dplyr::mutate(long_to_jitter = sf::st_coordinates(geom)[,1],
                    lat_to_jitter = sf::st_coordinates(geom)[,2]) |> 
      dplyr::mutate(long_to_jitter = long_to_jitter - shunt_amount()) |> 
      sf::st_drop_geometry() |> 
      sf::st_as_sf(coords = c("long_to_jitter","lat_to_jitter"), crs = 4326,sf_column_name = "geom")
  }
  output
}


# edna_tub_pos <- makeIcon(
#   "https://www.freeiconspng.com/uploads/orange-triangle-image-vector-0.png",
#   iconWidth = 25, iconHeight = 25,
#   iconAnchorX = 12, iconAnchorY = 12,
#   className = 'orange-marker'
# )
# 
# edna_tub_neg <- makeIcon(
#   "https://www.freeiconspng.com/uploads/orange-triangle-image-vector-0.png",
#   iconWidth = 25, iconHeight = 25,
#   iconAnchorX = 12, iconAnchorY = 12,
#   className = 'purple-marker'
# )

# ----------------------------
# Full legend (identical for both years)
# ----------------------------
year_legend <- HTML(
  paste0(
    "<div style='background: white; padding: 8px 10px; border-radius: 5px;'>
       <strong style='font-size: medium;'>Year</strong>
       <div style='margin-top:6px;'>
         <svg width='16' height='16' style='vertical-align:middle;'>
           <circle cx='8' cy='8' r='6' fill='grey' stroke='black' />
         </svg>
         <span style='margin-left:6px;'>2024</span>
       </div>
       <div style='margin-top:6px;'>
         <svg width='16' height='16' style='vertical-align:middle;'>
           <rect x='2' y='2' width='12' height='12' fill='grey' stroke='black' />
         </svg>
         <span style='margin-left:6px;'>2025</span>
       </div>
     </div>"
  )
)

# --------------------------------------
# Define color palettes
# --------------------------------------
result_cols <- c("Positive" = "orange", "Negative" = "blue")
pal_fish <- colorFactor(palette = result_cols, levels = names(result_cols))
pal_edna <- colorFactor(palette = result_cols, levels = names(result_cols))
pal_both = colorFactor(palette = result_cols, levels = names(result_cols))

# make_leaf_tbl = function(dat){
#   dat |> 
#     dplyr::mutate(Latitude = sf::st_coordinates(geom)[,2],
#                   Longitude = sf::st_coordinates(geom)[,1]) |> 
#     sf::st_drop_geometry() |> 
#     dplyr::select(`Sample Site` = sample_site_name,
#                   Latitude,
#                   Longitude,
#                   `Sampling Method` = sampling_method,
#                   `Fish Species Sampled` = fish_species_sampled,
#                   `Fish Sampling Results` = fish_sampling_results_q_pcr_mc_detected,
#                   `eDNA Sampling Results (M. cerebralis - parasite)` = e_dna_results_mc,
#                   `eDNA Sampling Results (Tubifex worm)` = e_dna_results_tubifex) |> 
#     leafpop::popupTable()
# }


marker_icons <- leaflet::iconList(
  pos_2025 = pos_marker_2025,
  neg_2025 = neg_marker_2025,
  pos_2024 = pos_marker_2024,
  neg_2024 = neg_marker_2024
  
)

make_leaf_tbl <- function(dat){
  
  dat |> 
    dplyr::mutate(
      Latitude  = sf::st_coordinates(geom)[,2],
      Longitude = sf::st_coordinates(geom)[,1]
    ) |> 
    sf::st_drop_geometry() |> 
    dplyr::select(
      `Sample Site` = sample_site_name,
      Latitude,
      Longitude,
      `Sampling Method` = sampling_method,
      `Fish Species Sampled` = fish_species_sampled,
      `Fish Sampling Results` = fish_sampling_results_q_pcr_mc_detected,
      `eDNA Sampling Results (M. cerebralis – parasite)` = e_dna_results_mc,
      `Volume(s) Sampled (l)` = volume_sampled,
      `Filter Size(s) (µm)` = filter_size,
      `Agency` = delivery_agency,
      `Date Collected` = date_collected,
      Year
    ) |> 
    leafpop::popupTable()
}

# --------------------------------------
# Function to generate Leaflet for any data
# --------------------------------------
make_leaflet <- function(dat, type = c("Fish","eDNA"), leaflet_id) {
  
  dat_2025 <- dat %>%
    dplyr::filter(Year == 2025) %>%
    dplyr::mutate(
      icon_type = if (type == "Fish") {
        ifelse(
          fish_sampling_results_q_pcr_mc_detected == "Positive",
          "pos_2025", "neg_2025"
        )
      } else {
        ifelse(
          e_dna_results_mc == "Positive",
          "pos_2025", "neg_2025"
        )
      }
    )
  dat_2024 <- dat %>%
    dplyr::filter(Year == 2024) %>%
    dplyr::mutate(
      icon_type = if (type == "Fish") {
        ifelse(
          fish_sampling_results_q_pcr_mc_detected == "Positive",
          "pos_2024", "neg_2024"
        )
      } else {
        ifelse(
          e_dna_results_mc == "Positive",
          "pos_2024", "neg_2024"
        )
      }
    )
  
  type <- match.arg(type)
  # making some tables for later
  tbl_2024 = make_leaf_tbl(dat %>% dplyr::filter(Year == 2024))
  tbl_2025 = make_leaf_tbl(dat %>% dplyr::filter(Year == 2025))
  
  
  
  leaflet(dat) %>%
    addTiles() %>%
    # Watersheds
    addMapPane("watersheds", zIndex = 300) %>%
    addPolygons(
      data = subw,
      color = "grey",
      weight = 1.5,
      fill = "transparent",
      label = ~watershed_name,
      options = pathOptions(pane = "watersheds")
    ) %>%
    
    # Boundaries
    addMapPane("boundaries", zIndex = 400) %>%
    addPolygons(
      data = col,
      color = "purple",
      weight = 1.5,
      fill = "transparent",
      options = pathOptions(clickable = FALSE, pane = "boundaries")
    ) %>%
    
    
    
    # Points for 2024
    addMapPane("points", zIndex = 500) %>%
    # addCircleMarkers(
    #   data = dat_2024,
    #   lng = ~sf::st_coordinates(geom)[,1],
    #   lat = ~sf::st_coordinates(geom)[,2],
    #   color = "black",
    #   fillColor = if(type=="Fish") ~pal_fish(fish_sampling_results_q_pcr_mc_detected)
    #   else ~pal_edna(e_dna_results_mc),
    #   fillOpacity = 0.8,
    #   radius = 10,
    #   label = lapply(tbl_2024, htmltools::HTML),
    #   group = paste0(type, " 2024"),
    #   options = pathOptions(pane = "points")
    # ) %>%
    addMarkers(
      data = dat_2024,
      lng = ~sf::st_coordinates(geom)[,1],
      lat = ~sf::st_coordinates(geom)[,2],
      icon = ~marker_icons[icon_type],
      label = lapply(tbl_2024, htmltools::HTML),
      
      group = paste0(type, " 2024"),
      options = pathOptions(pane = "points")
    ) |> 
    # Points for 2025
    addMarkers(
      data = dat_2025,
      lng = ~sf::st_coordinates(geom)[,1],
      lat = ~sf::st_coordinates(geom)[,2],
      icon = ~marker_icons[icon_type],
      label = lapply(tbl_2025, htmltools::HTML),
      group = paste0(type, " 2025"),
      options = pathOptions(pane = "points")
    ) |> 
    
    # Layers control for years
    addLayersControl(
      overlayGroups = c(paste0(type, " 2024"), paste0(type, " 2025")),
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    
    # Legend
    addLegend(
      "bottomleft",
      pal = if(type=="Fish") pal_fish else pal_edna,
      values = names(result_cols),
      title = paste(type, "Result"),
      opacity = 1
    ) |> 
    addControl(
      year_legend,
      position = "bottomleft"
    )
}

make_leaf_tbl_both = function(dat){
  
  dat |> 
    dplyr::mutate(Latitude = sf::st_coordinates(geom)[,2],
                  Longitude = sf::st_coordinates(geom)[,1]) |> 
    sf::st_drop_geometry() |> 
    dplyr::select(
                  `Year Sampled` = Year,
                  `Sampling Method` = sampling_method,
                  `Sample Site` = sample_site_name,
                  Latitude,
                  Longitude,,
                  `Delivery Agency` = delivery_agency,
                  `Fish Species Sampled` = fish_species_sampled,
                  `Fish Sampling Results` = fish_sampling_results_q_pcr_mc_detected,
                  `eDNA Sampling Results (M. cerebralis - parasite)` = e_dna_results_mc
                  ) |> 
    leafpop::popupTable()
}


make_all_years = function(dat, leaflet_id){
  
  
  dat_plot <- dat |>
    dplyr::mutate(
      icon_type = dplyr::case_when(
        Year == 2025 & sampling_method == "Fish" &
          fish_sampling_results_q_pcr_mc_detected == "Positive" ~ "pos_2025",
        
        Year == 2025 & sampling_method == "eDNA" &
          e_dna_results_mc == "Positive" ~ "pos_2025",
        
        Year == 2025 & sampling_method %in% c("Fish", "eDNA") ~ "neg_2025",
        
        Year == 2024 & sampling_method == "Fish" &
          fish_sampling_results_q_pcr_mc_detected == "Positive" ~ "pos_2024",
        
        Year == 2024 & sampling_method == "eDNA" &
          e_dna_results_mc == "Positive" ~ "pos_2024",
        
        Year == 2024 & sampling_method %in% c("Fish", "eDNA") ~ "neg_2024",
        
        TRUE ~ NA_character_
      )
    )
  
  
  type = "All"
  dat_tbl_all = make_leaf_tbl_both(dat_plot)
  
  
  
  leaflet(dat_plot) %>%
    addTiles() %>%
    # Watersheds
    addMapPane("watersheds", zIndex = 300) %>%
    addPolygons(
      data = subw,
      color = "grey",
      weight = 1.5,
      fill = "transparent",
      label = ~watershed_name,
      options = pathOptions(pane = "watersheds")
    ) %>%
    
    # Boundaries
    addMapPane("boundaries", zIndex = 400) %>%
    addPolygons(
      data = col,
      color = "purple",
      weight = 1.5,
      fill = "transparent",
      options = pathOptions(clickable = FALSE, pane = "boundaries")
    ) %>%
    # Points for 2024
    addMapPane("points", zIndex = 500) %>%
    # addCircleMarkers(
    #   data = dat_2024,
    #   lng = ~sf::st_coordinates(geom)[,1],
    #   lat = ~sf::st_coordinates(geom)[,2],
    #   color = "black",
    #   fillColor = if(type=="Fish") ~pal_fish(fish_sampling_results_q_pcr_mc_detected)
    #   else ~pal_edna(e_dna_results_mc),
    #   fillOpacity = 0.8,
    #   radius = 10,
    #   label = lapply(tbl_2024, htmltools::HTML),
    #   group = paste0(type, " 2024"),
    #   options = pathOptions(pane = "points")
    # ) %>%
    addMarkers(
      data = dplyr::filter(dat_plot, Year == 2024),
      lng = ~sf::st_coordinates(geom)[,1],
      lat = ~sf::st_coordinates(geom)[,2],
      icon = ~marker_icons[icon_type],
      label = lapply(dat_tbl_all[dat_plot$Year == 2024], htmltools::HTML),
      group = paste0(type, " 2024"),
      options = pathOptions(pane = "points")
    ) |> 
    # Points for 2025
    addMarkers(
      data = dplyr::filter(dat_plot, Year == 2025),
      lng = ~sf::st_coordinates(geom)[,1],
      lat = ~sf::st_coordinates(geom)[,2],
      icon = ~marker_icons[icon_type],
      label = lapply(dat_tbl_all[dat_plot$Year == 2025], htmltools::HTML),
      group = paste0(type, " 2025"),
      options = pathOptions(pane = "points")
    )|> 
    
    # Layers control for years
    addLayersControl(
      overlayGroups = c(paste0(type, " 2024"), paste0(type, " 2025")),
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    
    # Legend
    addLegend(
      "bottomleft",
      pal = pal_both,
      values = names(result_cols),
      title = paste(type, "Result"),
      opacity = 1
    ) |> 
    addControl(
      year_legend,
      position = "bottomleft"
    )
  
}

# --------------------------------------
# Render Leaflets
# --------------------------------------
output$leaf_fish <- renderLeaflet({
  make_leaflet(fish_data, type = "Fish", leaflet_id = "leaf_fish")
})

output$leaf_edna <- renderLeaflet({
  make_leaflet(edna_data, type = "eDNA", leaflet_id = "leaf_edna")
})


output$leaf_all_data <- renderLeaflet({
  make_all_years(dat_all, leaflet_id = "leaf_all_data")
})