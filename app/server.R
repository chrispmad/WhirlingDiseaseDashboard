source("ui.R")

server <- function(input, output, session) {
  
  source("www/utils/load_and_clean_data.R", local = T)
  
  source('www/utils/create_reactives.R', local = T)
  
  source('www/utils/leaflet_logic.R', local = T)
  
  source('www/utils/data_download.R', local = T)
  
  download_button_ui = reactiveVal(bslib::card())
  entered_username = reactiveVal()
  entered_password = reactiveVal()
  
  observeEvent(input$attempt_WD_analysis_download, {
    entered_username(NULL)
    entered_password(NULL)
    download_button_ui(bslib::card())
    
    shiny::showModal(
      shiny::modalDialog(
        textInput('username_WD_dl', label = "Username: "),
        passwordInput('password_WD_dl', label = "Password: "),
        actionButton('submit_user_pass',"Enter"),
        uiOutput("download_or_not_WD_analysis")
      )
    )
  })
  

  
  observeEvent(input$submit_user_pass, {
    
    entered_username(input$username_WD_dl)
    entered_password(input$password_WD_dl)
    
    if(entered_username() == "bcwd" & entered_password() == "ocean52"){
      download_button_ui(
        bslib::card(
          bslib::layout_column_wrap(
            1/2, 
            shiny::downloadButton(outputId = 'WD_analysis_markdown_dl', 
                                  label = "Download HTML File", 
                                  class = 'download-data-btn'),
            shiny::downloadButton(outputId = 'WD_analysis_excel_dl', 
                                  label = "Download Excel File", 
                                  class = 'download-data-btn')
          )
        )
      )
    } else {
      download_button_ui(bslib::card(h5("Incorrect username/password",
                                        style = 'color:red;text-align:center;')))
    }
  })
  
  output$download_or_not_WD_analysis = renderUI(download_button_ui())
}