source("ui.R")

server <- function(input, output, session) {
  
  if(!stringr::str_detect(getwd(),"www\\/$")) setwd(paste0(getwd(),"/www"))
  
  source("utils/load_and_clean_data.R", local = T)
  
  source('utils/create_reactives.R', local = T)
  
  # source('utils/render_plots_and_widgets.R', local = T)
  
  source('utils/leaflet_logic.R', local = T)
  
  source('utils/data_download.R', local = T)
}