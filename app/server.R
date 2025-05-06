
credentials <- data.frame(
  user = c("bcwd"), # mandatory
  password = c("mop1")
)

server <- function(input, output, session) {
  
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  observe({
    if(length(as.numeric(reactiveValuesToList(res_auth))) > 0){
      if(!stringr::str_detect(getwd(),"www\\/$")) setwd(paste0(getwd(),"/www"))
      
      source("utils/load_and_clean_data.R", local = T)
      
      source('utils/create_reactives.R', local = T)
      
      source('utils/render_plots_and_widgets.R', local = T)

      source('utils/leaflet_logic.R', local = T)
      
    }
  })
}