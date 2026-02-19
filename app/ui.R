library(shiny)
library(shinymanager)
library(bslib)
library(leaflet)

creds <- data.frame(read.table("www/creds.txt", sep = ",", header = T))

credentials <- data.frame(
  user = creds$user,
  password = sapply(creds$password, scrypt::hashPassword),
  is_hashed_password = TRUE,
  stringsAsFactors = FALSE
)

ui <- secure_app(
  fluidPage(
    theme = bs_theme(bootswatch = "flatly", version = 5),
    
    titlePanel("Whirling Disease Results"),
    
    tags$style(HTML("
  .btn-forest {
    background-color: #1b5e20;  /* forest green */
    border-color: #1b5e20;
    color: #ffffff;
  }

  .btn-forest:hover,
  .btn-forest:focus {
    background-color: #144d18;  /* slightly darker */
    border-color: #144d18;
    color: #ffffff;
  }
")),
    
    fluidRow(
      column(
        width = 12,
        div(
          style = "display:flex; justify-content:flex-end; gap:10px; margin-bottom:10px;",
          
          downloadButton(
            "data_dl_2024",
            "2024 data",
            class = "btn btn-forest"
          ),
          
          downloadButton(
            "data_dl_2025",
            "2025 data",
            class = "btn btn-forest"
          )
        )
      )
    ),
    
    # ----------------------------
    # Data type tabs
    # ----------------------------
    tabsetPanel(
      id = "data_type",
      
      tabPanel(
        title = "Fish",
        value = "fish",
        # Single leaflet for all Fish layers (2024 + 2025)
        leafletOutput("leaf_fish", height = "80vh")
      ),
      
      tabPanel(
        title = "eDNA – M. cerebralis",
        value = "edna",
        # Single leaflet for all eDNA layers (2024 + 2025)
        leafletOutput("leaf_edna", height = "80vh")
      ),
      tabPanel(
        title = "Sampling Coverage - All Years",
        value = "all_years",
        # Single leaflet for all eDNA layers (2024 + 2025)
        leafletOutput("leaf_all_data", height = "80vh")
      )
      
    )
  )
)
