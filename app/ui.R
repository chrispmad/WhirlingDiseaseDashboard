library(shiny)
library(shinymanager)
library(bslib)
library(leaflet)
library(shinyjs)

creds <- data.frame(read.table("www/creds.txt", sep = ",", header = T))

APP_PASSWORD <- creds$password

ui <- navbarPage(
  id = "data_type",
  
  title = tags$a(
    href = "https://www2.gov.bc.ca",
    tags$img(src = "images/gov3_bc_logo.png", height = "50px")
  ),
  
  header = tagList(
    tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "css/bcgov.css?v=2"
      )
    ),
    useShinyjs(),
    fluidRow(
      column(
        width = 12,
        div(
          style = "display:flex; justify-content:flex-end; gap:10px; margin:10px 0;",
          actionButton("data_dl_2024", "2024 data", class = "btn btn-forest"),
          actionButton("data_dl_2025", "2025 data", class = "btn btn-forest")
        )
      )
    )
  ),
  
  tabPanel("Fish", value = "fish",
           leafletOutput("leaf_fish", height = "70vh")),
  
  tabPanel("eDNA – M. cerebralis", value = "edna",
           leafletOutput("leaf_edna", height = "70vh")),
  
  tabPanel("Sampling Coverage - All Years", value = "all_years",
           div(
             style = "font-size: 12px; color: #666; margin-bottom: 5px;",
             "Note: 2025 sampling locations are slightly offset to improve visibility where they overlap with 2024."
           ),
           leafletOutput("leaf_all_data", height = "80vh")),
  
  footer = column(
    width = 12,
    style = "background-color:#003366; border-top:2px solid #fcba19;",
    tags$footer(
      class = "footer",
      tags$div(
        class = "container",
        style = "display:flex; justify-content:left; flex-direction:column; text-align:center; height:46px;",
        tags$ul(
          style = "display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home", "Home", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/disclaimer", "Disclaimer", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/privacy", "Privacy", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/accessibility", "Accessibility", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
          tags$li(a(href="https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html", "Contact", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;"))
        )
      )
    )
  )
)

