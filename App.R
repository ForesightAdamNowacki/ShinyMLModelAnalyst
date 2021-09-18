library(shiny)
library(Metrics)
library(tidyverse)
library(stringr)
library(DT)
library(readr)

source("Functions.R")

# ------------------------------------------------------------------------------
# Additional options:
options(scipen = 20)
options(warn = -1)

sidebarPanel_width <- 3
mainPanel_width <- 9

# ---------------------------------------------------------------------------- #
# User Interface:

ui <- shiny::tagList(
  
  shiny::h4(shiny::textOutput("CurrentDateTime")),
  shinythemes::themeSelector(),
  shiny::navbarPage(
    "DataModelAnalyst - Shiny Application",
    # ------------------------------------------------------------------------ #
    shiny::tabPanel(
      title = icon("home")),
    # ------------------------------------------------------------------------ #
    shiny::navbarMenu(
      "Data Visualisation Tool",
      shiny::tabPanel(
        "Factor vs Factor",
        shiny::sidebarLayout(
          position = "left",
          shiny::sidebarPanel(
            width = sidebarPanel_width,
            
            shiny::wellPanel(
              shiny::h4(shiny::strong("Dataset import")),
              shiny::fluidRow(
                shiny::column(width = 12, shiny::fileInput(inputId = "File_Input_1_1", label = shiny::h5("Select import file"))))),
            
            shiny::wellPanel(
              shiny::h4(shiny::strong("Input data settings:")),
              shiny::fluidRow(
                shiny::column(width = 6, shiny::sliderInput(inputId = "sliderInput_1_1_2_1", label = shiny::h5("Data fraction:"), min = 0, max = 1, value = 0.5, step = 0.01)),
                shiny::column(width = 3, shiny::radioButtons(inputId = "radioButtons_1_1_2_2", label = shiny::h5("Set seed:"), choices = list("Yes" = TRUE, "No" = FALSE), selected = TRUE)),
                shiny::column(width = 3, shiny::numericInput(inputId = "numericInput_1_1_2_3", label = shiny::h5("Seed value:"), value = 42))),
              shiny::fluidRow(
                shiny::column(width = 6, shiny::textInput(inputId = "textInput_1_1_2_1", label = shiny::h5("1st factor variable name:"), value = "")),
                shiny::column(width = 6, shiny::textInput(inputId = "textInput_1_1_2_2", label = shiny::h5("2nd factor variable name:"), value = "")))),
            
            shiny::wellPanel(
              shiny::h4(shiny::strong("Global visualisation settings:")),
              
              shiny::wellPanel(
                shiny::h5(shiny::strong("Axis names:")),
                shiny::fluidRow(
                  shiny::column(width = 6, shiny::textInput(inputId = "textInput_1_1_3_1_1", label = shiny::h5("1st display axis name:"), value = "")),
                  shiny::column(width = 6, shiny::textInput(inputId = "textInput_1_1_3_1_2", label = shiny::h5("2nd display axis name:"), value = ""))),
                shiny::fluidRow(
                  shiny::column(width = 6, shiny::textInput(inputId = "textInput_1_1_3_1_3", label = shiny::h5("Count axis name:"), value = "")),
                  shiny::column(width = 6, shiny::textInput(inputId = "textInput_1_1_3_1_4", label = shiny::h5("Percentage axis name:"), value = ""))),
                shiny::fluidRow(
                  shiny::column(width = 6, shiny::textInput(inputId = "textInput_1_1_3_1_5", label = shiny::h5("Caption:"), value = "")))),
              
              shiny::wellPanel(
                shiny::h5(shiny::strong("Title, text & label sizes:")),
                shiny::fluidRow(
                  shiny::column(width = 3, shiny::numericInput(inputId = "numericInput_1_1_3_2_1", label = shiny::h5("Plot title size"), value = 12)),
                  shiny::column(width = 3, shiny::numericInput(inputId = "numericInput_1_1_3_2_2", label = shiny::h5("Axis text size:"), value = 10)),
                  shiny::column(width = 3, shiny::numericInput(inputId = "numericInput_1_1_3_2_3", label = shiny::h5("Label size"), value = 4)))),
              
              shiny::wellPanel(
                shiny::h5(shiny::strong("Additional settings:")),
                shiny::fluidRow()),
              
              shiny::wellPanel(
                shiny::h5(shiny::strong("Titles names:")),
                shiny::fluidRow(
                  shiny::column(width = 3, shiny::textInput(inputId = "textInput_1_1_3_4_1", label = shiny::h5("Plot 1:"), value = "")),
                  shiny::column(width = 3, shiny::textInput(inputId = "textInput_1_1_3_4_2", label = shiny::h5("Plot 2:"), value = "")),
                  shiny::column(width = 3, shiny::textInput(inputId = "textInput_1_1_3_4_3", label = shiny::h5("Plot 3:"), value = "")),
                  shiny::column(width = 3, shiny::textInput(inputId = "textInput_1_1_3_4_4", label = shiny::h5("Plot 4:"), value = ""))),
                shiny::fluidRow(
                  shiny::column(width = 3, shiny::textInput(inputId = "textInput_1_1_3_4_5", label = shiny::h5("Plot 5:"), value = "")),
                  shiny::column(width = 3, shiny::textInput(inputId = "textInput_1_1_3_4_6", label = shiny::h5("Plot 6:"), value = "")),
                  shiny::column(width = 3, shiny::textInput(inputId = "textInput_1_1_3_4_7", label = shiny::h5("Plot 7:"), value = "")),
                  shiny::column(width = 3, shiny::textInput(inputId = "textInput_1_1_3_4_8", label = shiny::h5("Plot 8:"), value = ""))),
                shiny::fluidRow(
                  shiny::column(width = 3, shiny::textInput(inputId = "textInput_1_1_3_4_9", label = shiny::h5("Plot 9:"), value = "")),
                  shiny::column(width = 3, shiny::textInput(inputId = "textInput_1_1_3_4_10", label = shiny::h5("Plot 10:"), value = "")),
                  shiny::column(width = 3, shiny::textInput(inputId = "textInput_1_1_3_4_11", label = shiny::h5("Plot 11:"), value = "")),
                  shiny::column(width = 3, shiny::textInput(inputId = "textInput_1_1_3_4_12", label = shiny::h5("Plot 12:"), value = ""))),
                shiny::fluidRow(
                  shiny::column(width = 3, shiny::textInput(inputId = "textInput_1_1_3_4_13", label = shiny::h5("Plot 13:"), value = "")),
                  shiny::column(width = 3, shiny::textInput(inputId = "textInput_1_1_3_4_14", label = shiny::h5("Plot 14:"), value = "")),
                  shiny::column(width = 3, shiny::textInput(inputId = "textInput_1_1_3_4_15", label = shiny::h5("Plot 15:"), value = "")),
                  shiny::column(width = 3, shiny::textInput(inputId = "textInput_1_1_3_4_16", label = shiny::h5("Plot 16:"), value = ""))),
                style = "padding: 5px;")),
            
            shiny::wellPanel(
              shiny::h4(shiny::strong("Save report:")),
              style = "padding: 5px;")),
          
          shiny::mainPanel(
            width = mainPanel_width,
            shiny::tabsetPanel(
              type = "pills",
              shiny::tabPanel(
                title = "Panel 1",  DT::dataTableOutput("dataset_1_1")),
              shiny::tabPanel(
                title = "Panel 2", 
                shiny::hr(),
                shiny::fluidRow(shiny::splitLayout(cellWidths = c("24.25%", "24.25%", "24.25%", "24.25%"), 
                                                   cellArgs = list(style = "padding: 5px"),
                                                   shiny::plotOutput("plot_1_1_1"),
                                                   shiny::plotOutput("plot_1_1_2"),
                                                   shiny::plotOutput("plot_1_1_3"),
                                                   shiny::plotOutput("plot_1_1_4"))),
                #shiny::br(),
                shiny::fluidRow(shiny::splitLayout(cellWidths = c("24.25%", "24.25%", "24.25%", "24.25%"),
                                                   cellArgs = list(style = "padding: 5px"),
                                                   shiny::plotOutput("plot_1_1_5"),
                                                   shiny::plotOutput("plot_1_1_6"),
                                                   shiny::plotOutput("plot_1_1_7"))),
                shiny::fluidRow(shiny::splitLayout(cellWidths = c("24.25%", "24.25%", "24.25%", "24.25%"),
                                                   cellArgs = list(style = "padding: 5px"),
                                                   shiny::plotOutput("plot_1_1_9"),
                                                   shiny::plotOutput("plot_1_1_10"),
                                                   shiny::plotOutput("plot_1_1_11"))),
                shiny::fluidRow(shiny::splitLayout(cellWidths = c("24.25%", "24.25%", "24.25%", "24.25%"), 
                                                   cellArgs = list(style = "padding: 5px"),
                                                   shiny::plotOutput("plot_1_1_13"),
                                                   shiny::plotOutput("plot_1_1_14"),
                                                   shiny::plotOutput("plot_1_1_15"),
                                                   shiny::plotOutput("plot_1_1_16")))),
              shiny::tabPanel(
                title = "Panel 3"))
          ))),
      # ---------------------------------------------------------------------- #
      shiny::tabPanel(
        "Factor vs Numeric"),
      shiny::tabPanel(
        "Numeric vs Numeric"),
      shiny::tabPanel(
        "Date vs Numeric"),
      shiny::tabPanel(
        "Datetime vs Numeric"),
      shiny::tabPanel(
        "Date vs Factor"),
      shiny::tabPanel(
        "Datetime vs Factor")),
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
  
  # 1.1
  dataset_1_1 <- shiny::reactive({
    file_import <- input$File_Input_1_1
    if (is.null(file_import))
      return(NULL)
    readr::read_csv2(file_import$datapath)})
  
  dataset_1_1_ <- shiny::reactive({ 
    data_sampler(data = dataset_1_1(),
                 data_fraction = input$sliderInput_1_1_2_1,
                 set_seed = input$radioButtons_1_1_2_2,
                 seed_value = input$numericInput_1_1_2_3)})
  
  output$plot_1_1_1 <- shiny::renderPlot({
    factor_count_histogram_plot(data = dataset_1_1_(),
                                factor_var = input$textInput_1_1_2_1,
                                title_size = input$numericInput_1_1_3_2_1,
                                text_size = input$numericInput_1_1_3_2_2,
                                label_size = input$numericInput_1_1_3_2_3,
                                factor_axis = input$textInput_1_1_3_1_1,
                                title = input$textInput_1_1_3_4_1,
                                count_axis = input$textInput_1_1_3_1_3,
                                caption = input$textInput_1_1_3_1_5)})
  
  output$plot_1_1_2 <- shiny::renderPlot({
    factor_percentage_histogram_plot(data = dataset_1_1_(),
                                     factor_var = input$textInput_1_1_2_1,
                                     title_size = input$numericInput_1_1_3_2_1,
                                     text_size = input$numericInput_1_1_3_2_2,
                                     label_size = input$numericInput_1_1_3_2_3,
                                     factor_axis = input$textInput_1_1_3_1_1,
                                     title = input$textInput_1_1_3_4_2,
                                     percentage_axis = input$textInput_1_1_3_1_4,
                                     caption = input$textInput_1_1_3_1_5)})
  
  output$plot_1_1_3 <- shiny::renderPlot({
    factor_waffle_plot(data = dataset_1_1_(),
                       factor_var = input$textInput_1_1_2_1,
                       factor_axis = input$textInput_1_1_3_1_1,
                       title_size = input$numericInput_1_1_3_2_1,
                       text_size = input$numericInput_1_1_3_2_2,
                       title = input$textInput_1_1_3_4_3,
                       caption = input$textInput_1_1_3_1_5)})
  
  output$plot_1_1_4 <- shiny::renderPlot({
    factor_circle_plot(data = dataset_1_1_(),
                       factor_var = input$textInput_1_1_2_1,
                       title_size = input$numericInput_1_1_3_2_1,
                       text_size = input$numericInput_1_1_3_2_2,
                       label_size = input$numericInput_1_1_3_2_3,
                       title = input$textInput_1_1_3_4_4,
                       caption = input$textInput_1_1_3_1_5)})
  
  output$plot_1_1_5 <- shiny::renderPlot({
    factor_vs_factor_count_plot(data = dataset_1_1_(),
                                factor_var_1 = input$textInput_1_1_2_1,
                                factor_var_2 = input$textInput_1_1_2_2,
                                factor_axis_1 = input$textInput_1_1_3_1_1,
                                factor_axis_2 = input$textInput_1_1_3_1_2,
                                title = input$textInput_1_1_3_4_5,
                                caption = input$textInput_1_1_3_1_5,
                                title_size = input$numericInput_1_1_3_2_1,
                                text_size = input$numericInput_1_1_3_2_2,
                                label_size = input$numericInput_1_1_3_2_3)})
  
  output$plot_1_1_6 <- shiny::renderPlot({
    factor_vs_factor_percentage_group_tile_plot(data = dataset_1_1_(),
                                                factor_var_1 = input$textInput_1_1_2_1,
                                                factor_var_2 = input$textInput_1_1_2_2,
                                                factor_axis_1 = input$textInput_1_1_3_1_1,
                                                factor_axis_2 = input$textInput_1_1_3_1_2,
                                                title = input$textInput_1_1_3_4_6,
                                                caption = input$textInput_1_1_3_1_5,
                                                title_size = input$numericInput_1_1_3_2_1,
                                                text_size = input$numericInput_1_1_3_2_2,
                                                label_size = input$numericInput_1_1_3_2_3)})
  
  output$plot_1_1_7 <- shiny::renderPlot({
    factor_vs_factor_percentage_group_tile_plot(data = dataset_1_1_(),
                                                factor_var_1 = input$textInput_1_1_2_2,
                                                factor_var_2 = input$textInput_1_1_2_1,
                                                factor_axis_1 = input$textInput_1_1_3_1_2,
                                                factor_axis_2 = input$textInput_1_1_3_1_1,
                                                title = input$textInput_1_1_3_4_7,
                                                caption = input$textInput_1_1_3_1_5,
                                                title_size = input$numericInput_1_1_3_2_1,
                                                text_size = input$numericInput_1_1_3_2_2,
                                                label_size = input$numericInput_1_1_3_2_3)})
  
  output$plot_1_1_9 <- shiny::renderPlot({
    factor_vs_factor_percentage_plot(data = dataset_1_1_(),
                                     factor_var_1 = input$textInput_1_1_2_1,
                                     factor_var_2 = input$textInput_1_1_2_2,
                                     factor_axis_1 = input$textInput_1_1_3_1_1,
                                     factor_axis_2 = input$textInput_1_1_3_1_2,
                                     title = input$textInput_1_1_3_4_9,
                                     caption = input$textInput_1_1_3_1_5,
                                     title_size = input$numericInput_1_1_3_2_1,
                                     text_size = input$numericInput_1_1_3_2_2,
                                     label_size = input$numericInput_1_1_3_2_3)})
  
  output$plot_1_1_10 <- shiny::renderPlot({
    factor_vs_factor_percentage_group_bar_plot(data = dataset_1_1_(),
                                               factor_var_1 = input$textInput_1_1_2_1,
                                               factor_var_2 = input$textInput_1_1_2_2,
                                               factor_axis_1 = input$textInput_1_1_3_1_1,
                                               factor_axis_2 = input$textInput_1_1_3_1_2,
                                               title = input$textInput_1_1_3_4_10,
                                               caption = input$textInput_1_1_3_1_5,
                                               title_size = input$numericInput_1_1_3_2_1,
                                               text_size = input$numericInput_1_1_3_2_2,
                                               label_size = input$numericInput_1_1_3_2_3)})
  
  output$plot_1_1_11 <- shiny::renderPlot({
    factor_vs_factor_percentage_group_bar_plot(data = dataset_1_1_(),
                                               factor_var_1 = input$textInput_1_1_2_2,
                                               factor_var_2 = input$textInput_1_1_2_1,
                                               factor_axis_1 = input$textInput_1_1_3_1_2,
                                               factor_axis_2 = input$textInput_1_1_3_1_1,
                                               title = input$textInput_1_1_3_4_11,
                                               caption = input$textInput_1_1_3_1_5,
                                               title_size = input$numericInput_1_1_3_2_1,
                                               text_size = input$numericInput_1_1_3_2_2,
                                               label_size = input$numericInput_1_1_3_2_3)})
  
  output$plot_1_1_13 <- shiny::renderPlot({
    factor_count_histogram_plot(data = dataset_1_1_(),
                                factor_var = input$textInput_1_1_2_2,
                                title_size = input$numericInput_1_1_3_2_1,
                                text_size = input$numericInput_1_1_3_2_2,
                                label_size = input$numericInput_1_1_3_2_3,
                                factor_axis = input$textInput_1_1_3_1_2,
                                title = input$textInput_1_1_3_4_13,
                                count_axis = input$textInput_1_1_3_1_3,
                                caption = input$textInput_1_1_3_1_5)})
  
  output$plot_1_1_14 <- shiny::renderPlot({
    factor_percentage_histogram_plot(data = dataset_1_1_(),
                                     factor_var = input$textInput_1_1_2_2,
                                     title_size = input$numericInput_1_1_3_2_1,
                                     text_size = input$numericInput_1_1_3_2_2,
                                     label_size = input$numericInput_1_1_3_2_3,
                                     factor_axis = input$textInput_1_1_3_1_2,
                                     title = input$textInput_1_1_3_4_14,
                                     percentage_axis = input$textInput_1_1_3_1_4,
                                     caption = input$textInput_1_1_3_1_5)})
  
  output$plot_1_1_15 <- shiny::renderPlot({
    factor_waffle_plot(data = dataset_1_1_(),
                       factor_var = input$textInput_1_1_2_2,
                       factor_axis = input$textInput_1_1_3_1_2,
                       title_size = input$numericInput_1_1_3_2_1,
                       text_size = input$numericInput_1_1_3_2_2,
                       title = input$textInput_1_1_3_4_15,
                       caption = input$textInput_1_1_3_1_5)})
  
  output$plot_1_1_16 <- shiny::renderPlot({
    factor_circle_plot(data = dataset_1_1_(),
                       factor_var = input$textInput_1_1_2_1,
                       title_size = input$numericInput_1_1_3_2_1,
                       text_size = input$numericInput_1_1_3_2_2,
                       label_size = input$numericInput_1_1_3_2_3,
                       title = input$textInput_1_1_3_4_16,
                       caption = input$textInput_1_1_3_1_5)})
  
  output$dataset_1_1 <- DT::renderDataTable(
    expr = dataset_1_1_(),
    options = list(pageLength = 25,
                   lengthMenu = list(c(5, 10, 25, 100, 1000, -1),
                                     c("5", "10", "25", "100", "1000", "All")),
                   searching = TRUE)
  )
  
  
}





# ---------------------------------------------------------------------------- #
# Application:
shiny::shinyApp(ui = ui, server = server)

# runApp(
#   list(ui = fluidPage(           
#     wellPanel(              
#       tags$div(class = "row",
#                tags$div(class = "span"),
#                tags$div(class = "span1", h1(numericInput(inputId="num", label="ID", value=NaN))),
#                tags$div(class = "span2", h1(sliderInput(inputId="age", "Age Range", min=32, max=99, value=c(32, 99), step=1))),
#                tags$div(class = "span1", h1(radioButtons(inputId="gender", "Gender", c("combined" = 0, "male" = 1, "female" = 2), inline=FALSE))),
#                tags$div(class = "span1", h1(textOutput("text")))
#       )
#       , style = "padding: 5px;")
#     , wellPanel(              
#       tags$div(class = "row",
#                tags$div(class = "span"),
#                tags$div(class = "span1", h1(numericInput(inputId="num1", label="ID", value=NaN))),
#                tags$div(class = "span2", h1(sliderInput(inputId="age1", "Age Range", min=32, max=99, value=c(32, 99), step=1))),
#                tags$div(class = "span1", h1(radioButtons(inputId="gender1", "Gender", c("combined" = 0, "male" = 1, "female" = 2), inline=FALSE))),
#                tags$div(class = "span1", h1(textOutput("text1")))
#       )
#       , style = "padding: 45px;")
#   )
#   , server = function(input, output, session){
#     
#   }
#   )
# )