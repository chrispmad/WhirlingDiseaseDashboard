source("ui.R")

server <- function(input, output, session) {
  
  pending_download <- reactiveVal(NULL)
  
  source("www/utils/load_and_clean_data.R", local = environment())
  
  source('www/utils/create_reactives.R', local = environment())
  
  source('www/utils/leaflet_logic.R', local = environment())
  
  source('www/utils/data_download.R', local = environment())
  
  
  output$download_or_not_WD_analysis = renderUI(download_button_ui())
}