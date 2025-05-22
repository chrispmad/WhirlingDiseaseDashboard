source("ui.R")

server <- function(input, output, session) {
  
  source("www/utils/load_and_clean_data.R", local = T)
  
  source('www/utils/create_reactives.R', local = T)
  
  source('www/utils/leaflet_logic.R', local = T)
  
  source('www/utils/data_download.R', local = T)
}