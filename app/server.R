source("ui.R")

server <- function(input, output, session) {
  
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  source("www/utils/load_and_clean_data.R", local = T)
  
  source('www/utils/create_reactives.R', local = T)
  
  source('www/utils/leaflet_logic.R', local = T)
  
  source('www/utils/data_download.R', local = T)
  
  
  output$download_or_not_WD_analysis = renderUI(download_button_ui())
}