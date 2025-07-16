library(shiny)
library(bslib)
library(leaflet)
library(tidyverse)

leaflet_card = card(
  leafletOutput('my_leaf')
)

ui <- page_navbar(
  theme = bs_theme(bootswatch = "flatly", version = 5),
  shiny::includeCSS("www/my_styles.css"),
  # shiny::includeScript("www/leaflet_point_spread.js"),
  title = h5("Whirling Disease 2024 Results"),
  bslib::nav_item(
    shiny::actionButton('attempt_WD_analysis_download', 
                        label = "Download 2025 WD Monitoring Analysis", 
                          class = 'download-data-btn'),
    style = 'right:20%;top:15%;position:absolute;'
  ),
  bslib::nav_item(
    div(
      shiny::downloadButton(outputId = 'data_dl', label = "Download Dashboard Data", class = 'download-data-btn'),
      p(textOutput('file_update_date')),
      class="data-update-text"
      )
  ),
  leaflet_card,
  card(class = 'hover-clone-pane', style = "position: fixed; pointer-events: none; z-index: 9999;")
)