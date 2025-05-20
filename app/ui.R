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
  title = h5("Whirling Disease 2024 Results"),
  bslib::nav_item(
    div(
      shiny::downloadButton(outputId = 'data_dl', label = "Download Data", class = 'download-data-btn'),
      p(textOutput('file_update_date')),
      class="data-update-text"
      )
  ),
  leaflet_card
)