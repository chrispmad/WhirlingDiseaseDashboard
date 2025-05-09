library(shiny)
library(bslib)
library(leaflet)
library(tidyverse)

# the_sidebar = sidebar(
#   width = "40%",
#   card(
#     bslib::layout_column_wrap(
#       width = 1/2,
#       checkboxGroupInput(
#         'data_type_f',
#         label = "Data to Show",
#         choices = c("eDNA","Fish"),
#         selected = c("eDNA","Fish"),
#         inline = T
#       ),
#       checkboxGroupInput(
#         'fish_sp_f',
#         label = 'Fish Species',
#         choices = c("RBT","EBT","WCT","MW","BT","KOK"),
#         selected = c("RBT","EBT","WCT","MW","BT","KOK"),
#         inline = TRUE
#       )
#     ),
#   style = "margin-top:-2rem;margin-bottom:-1rem;"
#   ),
#   # rgl::rglwidgetOutput('watersheds_rgl'),
#   tabsetPanel(
#     # tabPanel("eDNA Results", card(plotOutput('edna_res_by_subw'), max_height = '50%')),
#     tabPanel("eDNA Results", card(plotOutput('edna_res_by_subw'), max_height = '50%')),
#     tabPanel("Fish Results", card(plotOutput('fish_res_by_subw')))
#     
#   )
# )

leaflet_card = card(
  leafletOutput('my_leaf')
)

ui <- page_navbar(
  theme = bs_theme(bootswatch = "flatly", version = 5),
  shiny::includeCSS("www/my_styles.css"),
  title = h5("Whirling Disease Dashboard"),
  bslib::nav_item(
    div(
      shiny::downloadButton(outputId = 'data_dl', label = "Download Data", class = 'download-data-btn'),
      p("Data Updated on ",Sys.Date()),
      class="data-update-text"
      )
  ),
  # sidebar = the_sidebar,
  leaflet_card
)

# ui = secure_app(ui)