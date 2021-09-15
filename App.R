
library(shiny)
library(Metrics)
library(tidyverse)




# ---------------------------------------------------------------------------- #
# User Interface:

ui <- shiny::tagList(
  
  shiny::h4(shiny::textOutput("CurrentDateTime")),
  shinythemes::themeSelector(),
  shiny::navbarPage(
    "MLModelAnalyst Shiny Application",
    # ------------------------------------------------------------------------ #
    shiny::tabPanel(
      title = icon("home")),
    # ------------------------------------------------------------------------ #
    shiny::navbarMenu(
      title = "Binary Classification Model",
      # -----------------------------------------------------------------------#
      shiny::tabPanel(
        title = "Model report - 1 dataset",
        shiny::sidebarLayout(
          position = "left",
          shiny::sidebarPanel(
            width = 4,
            shiny::fluidRow(
              shiny::column(
                width = 6,
                shiny::wellPanel(
                  shiny::h4(shiny::strong("Ground truth dataset:")),
                  shiny::fileInput(inputId = "Train_Ground_Truth_Dataset_1",
                                   label = shiny::h5("Train dataset:")),
                  shiny::fileInput(inputId = "Validation_Ground_Truth_Dataset_1", 
                                   label = shiny::h5("Validation dataset:")),
                  shiny::fileInput(inputId = "Test_Ground_Truth_Dataset_1", 
                                   label = shiny::h5("Test dataset:")))),
              shiny::column(
                width = 6,
                shiny::wellPanel(
                  shiny::h4(shiny::strong("Prediction dataset:")),
                  shiny::fileInput(inputId = "Train_Prediction_Dataset_1", 
                                   label = shiny::h5("Train dataset:")),
                  shiny::fileInput(inputId = "Validation_Prediction_Dataset_1",
                                   label = shiny::h5("Validation dataset:")),
                  shiny::fileInput(inputId = "Test_Prediction_Dataset_1", 
                                   label = shiny::h5("Test dataset:"))))),
            shiny::wellPanel(
              shiny::h4(shiny::strong("Settings:")),
              shiny::fluidRow(
                shiny::column(4, shiny::sliderInput(inputId = "CutOff_1",
                                                    label = shiny::h5("Cutoff:"),
                                                    min = 0.00,
                                                    max = 1.00,
                                                    value = 0.50,
                                                    step = 0.01))
              )),
            
            shiny::wellPanel(
              shiny::h4(shiny::strong("Save report:"))),
          ),
          shiny::mainPanel(
            width = 8,
            shiny::tabsetPanel(
              type = "pills",
              shiny::tabPanel(
                title = "Panel 1"),
              shiny::tabPanel(
                title = "Panel 2"),
              shiny::tabPanel(
                title = "Panel 3"))
          )
        )),
      # ---------------------------------------------------------------------- #
      shiny::tabPanel(
        "Model report - 2 datasets"),
      # ---------------------------------------------------------------------- #
      shiny::tabPanel(
        "Model report - 3 datasets")),
    shiny::navbarMenu(
      "Multiclass Classification Model",
      shiny::tabPanel(
        "Model report - 1 dataset"),
      shiny::tabPanel(
        "Model report - 2 datasets"),
      shiny::tabPanel(
        "Model report - 3 datasets")),
    shiny::navbarMenu(
      "Multilabel Classification Model",
      shiny::tabPanel(
        "Model report - 1 dataset"),
      shiny::tabPanel(
        "Model report - 2 datasets"),
      shiny::tabPanel(
        "Model report - 3 datasets")),
    shiny::navbarMenu(
      "Regression Model",
      shiny::tabPanel(
        "Model report - 1 dataset"),
      shiny::tabPanel(
        "Model report - 2 datasets"),
      shiny::tabPanel(
        "Model report - 3 datasets"))
  )
)











# ---------------------------------------------------------------------------- #
# Server:
server = function(input, output, session) {
  
  output$CurrentDateTime <- shiny::renderText({
    shiny::invalidateLater(millis = 1000,
                           session = session)
    paste(Sys.time())
  })
  
}





# ---------------------------------------------------------------------------- #
# Application:
shiny::shinyApp(ui = ui, server = server)


# ---------------------------------------------------------------------------- #
# 
# 
# shinyApp(
#   ui = tagList(
#     shinythemes::themeSelector(),
#     navbarPage(
#       # theme = "cerulean",  # <--- To use a theme, uncomment this
#       "shinythemes",
#       tabPanel("Navbar 1",
#                sidebarPanel(
#                  fileInput("file", "File input:"),
#                  textInput("txt", "Text input:", "general"),
#                  sliderInput("slider", "Slider input:", 1, 100, 30),
#                  tags$h5("Default actionButton:"),
#                  actionButton("action", "Search"),
#                  
#                  tags$h5("actionButton with CSS class:"),
#                  actionButton("action2", "Action button", class = "btn-primary")
#                ),
#                mainPanel(
#                  tabsetPanel(
#                    tabPanel("Tab 1",
#                             h4("Table"),
#                             tableOutput("table"),
#                             h4("Verbatim text output"),
#                             verbatimTextOutput("txtout"),
#                             h1("Header 1"),
#                             h2("Header 2"),
#                             h3("Header 3"),
#                             h4("Header 4"),
#                             h5("Header 5")
#                    ),
#                    tabPanel("Tab 2", "This panel is intentionally left blank"),
#                    tabPanel("Tab 3", "This panel is intentionally left blank")
#                  )
#                )
#       ),
#       tabPanel("Navbar 2", "This panel is intentionally left blank"),
#       tabPanel("Navbar 3", "This panel is intentionally left blank")
#     )
#   ),
#   server = function(input, output) {
#     output$txtout <- renderText({
#       paste(input$txt, input$slider, format(input$date), sep = ", ")
#     })
#     output$table <- renderTable({
#       head(cars, 4)
#     })
#   }
# )