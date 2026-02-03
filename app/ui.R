library(shinymanager)
library(bslib)
library(leaflet)
library(leaflet.extras)
library(tidyverse)

download_ui <- function(year) {
  div(
    downloadButton(
      outputId = paste0("data_dl_", year),
      label = "Download Dashboard Data",
      class = "download-data-btn"
    ),
    p(textOutput(paste0("file_update_date_", year))),
    class = "data-update-text"
  )
}

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
    shiny::includeCSS("www/my_styles.css"),
    titlePanel("Whirling Disease Results"),
    
    # YEAR-level tabs (this stays tied to active_tab)
    tabsetPanel(
      id = "active_tab",
      
      # -------- 2024 --------
      tabPanel(
        title = "2024 Results",
        download_ui("2024"),
        tabsetPanel(
          id = "subtab_2024",
          
          tabPanel(
            title = "Fish",
            leafletOutput("my_leaf_2024_fish")
          ),
          
          tabPanel(
            title = "eDNA - M. cerebralis",
            leafletOutput("my_leaf_2024_edna")
          )
        )
      ),
      
      # -------- 2025 --------
      tabPanel(
        title = "2025 Results",
        download_ui("2025"),
        tabsetPanel(
          id = "subtab_2025",
          
          tabPanel(
            title = "Fish",
            
            leafletOutput("my_leaf_2025_fish")
          ),
          
          tabPanel(
            title = "eDNA - M. cerebralis",
            leafletOutput("my_leaf_2025_edna")
          )
        )
      )
    )
  )
)

