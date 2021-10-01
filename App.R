
library(shiny)
library(Metrics)
library(tidyverse)
library(stringr)
library(DT)
library(readr)
library(shinydashboard)

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
      "1D - Data Visualisation Tool",
      shiny::tabPanel(
        "Factor",
        shiny::sidebarLayout(
          position = "left",
          shiny::sidebarPanel(
            width = sidebarPanel_width,
            
            shiny::wellPanel(
              shiny::h4(shiny::strong("Dataset import")),
              shiny::fluidRow(
                shiny::column(width = 8, shiny::fileInput(inputId = "File_Input_1_1", label = shiny::h5("Select import file:"))),
                shiny::br(),
                shiny::column(width = 4, shiny::actionButton(inputId = "actionButton_1_1", label = shiny::h5("Restart:"), icon = shiny::icon("fas fa-undo"))))),
            
            shiny::wellPanel(
              shiny::h4(shiny::strong("Input data settings:")),
              shiny::fluidRow(
                shiny::column(width = 6, shiny::sliderInput(inputId = "sliderInput_1_1_2_1", label = shiny::h5("Data fraction:"), min = 0, max = 1, value = 0.5, step = 0.01)),
                shiny::column(width = 3, shiny::radioButtons(inputId = "radioButtons_1_1_2_2", label = shiny::h5("Set seed:"), choices = list("Yes" = TRUE, "No" = FALSE), selected = TRUE)),
                shiny::column(width = 3, shiny::numericInput(inputId = "numericInput_1_1_2_3", label = shiny::h5("Seed value:"), value = 42))),
              shiny::fluidRow(
                shiny::column(width = 6, shiny::selectInput(inputId = "selectInput_1_1_2_4", label = shiny::h5("Factor variable name:"), choices = list())))),
            
            shiny::wellPanel(
              shiny::h4(shiny::strong("Global visualisation settings:")),
              
              shiny::wellPanel(
                shiny::h5(shiny::strong("Axis names:")),
                shiny::fluidRow(
                  shiny::column(width = 6, shiny::textInput(inputId = "textInput_1_1_3_1_1", label = shiny::h5("Display axis name:"), value = "")),
                  shiny::column(width = 6, shiny::textInput(inputId = "textInput_1_1_3_1_2", label = shiny::h5("Count axis name:"), value = ""))),
                shiny::fluidRow(
                  shiny::column(width = 6, shiny::textInput(inputId = "textInput_1_1_3_1_3", label = shiny::h5("Percentage axis name:"), value = "")),
                  shiny::column(width = 6, shiny::textInput(inputId = "textInput_1_1_3_1_4", label = shiny::h5("Caption:"), value = "")))),
              
              shiny::wellPanel(
                shiny::h5(shiny::strong("Numeric settings:")),
                shiny::fluidRow(
                  shiny::column(width = 3, shiny::numericInput(inputId = "numericInput_1_1_3_2_1", label = shiny::h5("Plot title size"), value = 12)),
                  shiny::column(width = 3, shiny::numericInput(inputId = "numericInput_1_1_3_2_2", label = shiny::h5("Axis text size:"), value = 10)),
                  shiny::column(width = 3, shiny::numericInput(inputId = "numericInput_1_1_3_2_3", label = shiny::h5("Label size"), value = 4)),
                  shiny::column(width = 3, shiny::selectInput(inputId = "selectInput_1_1_2_2_4", label = shiny::h5("Waffle grid:"), choices = select_box_input(vector = c(5, 10, 25, 50, 100)), selected = 10)))),
              
              shiny::wellPanel(
                shiny::h5(shiny::strong("Binary settings:")),
                shiny::fluidRow(
                  shiny::column(width = 3, shiny::radioButtons(inputId = "radioButtons_1_1_3_3_1", label = shiny::h5("Display legend:"), choices = list("Yes" = TRUE, "No" = FALSE), selected = TRUE)),
                  shiny::column(width = 3, shiny::radioButtons(inputId = "radioButtons_1_1_3_3_2", label = shiny::h5("Legend position:"), choices = select_box_input(vector = c("left", "top", "right", "bottom")), selected = "bottom")),
                  shiny::column(width = 3, shiny::radioButtons(inputId = "radioButtons_1_1_3_3_3", label = shiny::h5("Legend direction:"), choices = select_box_input(vector = c("horizontal", "vertical")), selected = "horizontal")),
                  shiny::column(width = 3, shiny::radioButtons(inputId = "radioButtons_1_1_3_3_4", label = shiny::h5("Repel labels:"), choices = list("Yes" = TRUE, "No" = FALSE), selected = FALSE)))),
              
              shiny::wellPanel(
                shiny::h5(shiny::strong("Titles names:")),
                shiny::fluidRow(
                  shiny::column(width = 3, shiny::textInput(inputId = "textInput_1_1_3_4_1", label = shiny::h5("Plot 1:"), value = "QUANTITY BAR PLOT")),
                  shiny::column(width = 3, shiny::textInput(inputId = "textInput_1_1_3_4_2", label = shiny::h5("Plot 2:"), value = "PERCENTAGE BAR PLOT")),
                  shiny::column(width = 3, shiny::textInput(inputId = "textInput_1_1_3_4_3", label = shiny::h5("Plot 3:"), value = "WAFFLE PLOT")),
                  shiny::column(width = 3, shiny::textInput(inputId = "textInput_1_1_3_4_4", label = shiny::h5("Plot 4:"), value = "CIRCLE PLOT"))),
                style = "padding: 5px;"))),
          
          shiny::mainPanel(
            width = mainPanel_width,
            shiny::tabsetPanel(
              type = "pills",
              shiny::tabPanel(
                title = "Panel 1",  DT::dataTableOutput("dataset_1_1")),
              shiny::tabPanel(
                title = "Panel 2", 
                shiny::hr(),
                shiny::fluidRow(shiny::splitLayout(cellWidths = c("24.5%", "24.5%", "24.5%", "24.5%"),
                                                   cellArgs = list(style = "padding: 2.5px"),
                                                   shiny::plotOutput("plot_1_1_1"),
                                                   shiny::plotOutput("plot_1_1_2"),
                                                   shiny::plotOutput("plot_1_1_3"),
                                                   shiny::plotOutput("plot_1_1_4"))),
                shiny::fluidRow(
                  shiny::column(width = 3, shiny::downloadButton(outputId = "downloadButton_1_1_2_1", label = NULL, icon = shiny::icon("fas fa-download"))),
                  shiny::column(width = 3, shiny::downloadButton(outputId = "downloadButton_1_1_2_2", label = NULL, icon = shiny::icon("fas fa-download"))),
                  shiny::column(width = 3, shiny::downloadButton(outputId = "downloadButton_1_1_2_3", label = NULL, icon = shiny::icon("fas fa-download"))),
                  shiny::column(width = 3, shiny::downloadButton(outputId = "downloadButton_1_1_2_4", label = NULL, icon = shiny::icon("fas fa-download")))),
                shiny::hr()),
              shiny::tabPanel(
                title = "Panel 3"))))),
      shiny::tabPanel(
        "Numeric"),
      shiny::tabPanel(
        "Date"),
      shiny::tabPanel(
        "Datetime")),
    
    # ------------------------------------------------------------------------ #
    shiny::navbarMenu(
      "2D - Data Visualisation Tool",
      shiny::tabPanel(
        "Factor vs Factor",
        shiny::sidebarLayout(
          position = "left",
          shiny::sidebarPanel(
            width = sidebarPanel_width,
            
            shiny::wellPanel(
              shiny::h4(shiny::strong("Dataset import")),
              shiny::fluidRow(
                shiny::column(width = 8, shiny::fileInput(inputId = "File_Input_2_1", label = shiny::h5("Select import file:"))),
                shiny::br(),
                shiny::column(width = 4, shiny::actionButton(inputId = "actionButton_2_1", label = shiny::h5("Restart:"), icon = shiny::icon("fas fa-undo"))))),
            
            shiny::wellPanel(
              shiny::h4(shiny::strong("Input data settings:")),
              shiny::fluidRow(
                shiny::column(width = 6, shiny::sliderInput(inputId = "sliderInput_2_1_2_1", label = shiny::h5("Data fraction:"), min = 0, max = 1, value = 0.5, step = 0.01)),
                shiny::column(width = 3, shiny::radioButtons(inputId = "radioButtons_2_1_2_2", label = shiny::h5("Set seed:"), choices = list("Yes" = TRUE, "No" = FALSE), selected = TRUE)),
                shiny::column(width = 3, shiny::numericInput(inputId = "numericInput_2_1_2_3", label = shiny::h5("Seed value:"), value = 42))),
              shiny::fluidRow(
                shiny::column(width = 6, shiny::selectInput(inputId = "selectInput_2_1_2_4", label = shiny::h5("1st factor variable name:"), choices = list())),
                shiny::column(width = 6, shiny::selectInput(inputId = "selectInput_2_1_2_5", label = shiny::h5("2nd factor variable name:"), choices = list())))),
            
            shiny::wellPanel(
              shiny::h4(shiny::strong("Global visualisation settings:")),
              
              shiny::wellPanel(
                shiny::h5(shiny::strong("Axis names:")),
                shiny::fluidRow(
                  shiny::column(width = 6, shiny::textInput(inputId = "textInput_2_1_3_1_1", label = shiny::h5("1st display axis name:"), value = "")),
                  shiny::column(width = 6, shiny::textInput(inputId = "textInput_2_1_3_1_2", label = shiny::h5("2nd display axis name:"), value = ""))),
                shiny::fluidRow(
                  shiny::column(width = 6, shiny::textInput(inputId = "textInput_2_1_3_1_3", label = shiny::h5("Count axis name:"), value = "")),
                  shiny::column(width = 6, shiny::textInput(inputId = "textInput_2_1_3_1_4", label = shiny::h5("Percentage axis name:"), value = ""))),
                shiny::fluidRow(
                  shiny::column(width = 6, shiny::textInput(inputId = "textInput_2_1_3_1_5", label = shiny::h5("Caption:"), value = "")))),
              
              shiny::wellPanel(
                shiny::h5(shiny::strong("Numeric settings:")),
                shiny::fluidRow(
                  shiny::column(width = 3, shiny::numericInput(inputId = "numericInput_2_1_3_2_1", label = shiny::h5("Plot title size"), value = 12)),
                  shiny::column(width = 3, shiny::numericInput(inputId = "numericInput_2_1_3_2_2", label = shiny::h5("Axis text size:"), value = 10)),
                  shiny::column(width = 3, shiny::numericInput(inputId = "numericInput_2_1_3_2_3", label = shiny::h5("Label size"), value = 4)),
                  shiny::column(width = 3, shiny::selectInput(inputId = "selectInput_2_1_2_2_4", label = shiny::h5("Waffle grid:"), choices = select_box_input(vector = c(5, 10, 25, 50, 100)), selected = 10)))),
              
              shiny::wellPanel(
                shiny::h5(shiny::strong("Binary settings:")),
                shiny::fluidRow(
                  shiny::column(width = 3, shiny::radioButtons(inputId = "radioButtons_2_1_3_3_1", label = shiny::h5("Display legend:"), choices = list("Yes" = TRUE, "No" = FALSE), selected = TRUE)),
                  shiny::column(width = 3, shiny::radioButtons(inputId = "radioButtons_2_1_3_3_2", label = shiny::h5("Legend position:"), choices = select_box_input(vector = c("left", "top", "right", "bottom")), selected = "bottom")),
                  shiny::column(width = 3, shiny::radioButtons(inputId = "radioButtons_2_1_3_3_3", label = shiny::h5("Legend direction:"), choices = select_box_input(vector = c("horizontal", "vertical")), selected = "horizontal")),
                  shiny::column(width = 3, shiny::radioButtons(inputId = "radioButtons_2_1_3_3_4", label = shiny::h5("Repel labels:"), choices = list("Yes" = TRUE, "No" = FALSE), selected = FALSE)))),
              
              shiny::wellPanel(
                shiny::h5(shiny::strong("Titles names:")),
                shiny::fluidRow(
                  shiny::column(width = 3, shiny::textInput(inputId = "textInput_2_1_3_4_1", label = shiny::h5("Plot 1:"), value = "QUANTITY BAR PLOT")),
                  shiny::column(width = 3, shiny::textInput(inputId = "textInput_2_1_3_4_2", label = shiny::h5("Plot 2:"), value = "PERCENTAGE BAR PLOT")),
                  shiny::column(width = 3, shiny::textInput(inputId = "textInput_2_1_3_4_3", label = shiny::h5("Plot 3:"), value = "WAFFLE PLOT")),
                  shiny::column(width = 3, shiny::textInput(inputId = "textInput_2_1_3_4_4", label = shiny::h5("Plot 4:"), value = "CIRCLE PLOT"))),
                shiny::fluidRow(
                  shiny::column(width = 3, shiny::textInput(inputId = "textInput_2_1_3_4_5", label = shiny::h5("Plot 5:"), value = "QUANTITY TILE PLOT")),
                  shiny::column(width = 3, shiny::textInput(inputId = "textInput_2_1_3_4_6", label = shiny::h5("Plot 6:"), value = "GROUP PERCENTAGE TILE PLOT")),
                  shiny::column(width = 3, shiny::textInput(inputId = "textInput_2_1_3_4_7", label = shiny::h5("Plot 7:"), value = "GROUP PERCENTAGE TILE PLOT")),
                  shiny::column(width = 3, shiny::textInput(inputId = "textInput_2_1_3_4_8", label = shiny::h5("Plot 8:"), value = "PARALLEL PLOT"))),
                shiny::fluidRow(
                  shiny::column(width = 3, shiny::textInput(inputId = "textInput_2_1_3_4_9", label = shiny::h5("Plot 9:"), value = "PERCENTAGE TILE PLOT")),
                  shiny::column(width = 3, shiny::textInput(inputId = "textInput_2_1_3_4_10", label = shiny::h5("Plot 10:"), value = "GROUP PERCENTAGE BAR PLOT")),
                  shiny::column(width = 3, shiny::textInput(inputId = "textInput_2_1_3_4_11", label = shiny::h5("Plot 11:"), value = "GROUP PERCENTAGE BAR PLOT")),
                  shiny::column(width = 3, shiny::textInput(inputId = "textInput_2_1_3_4_12", label = shiny::h5("Plot 12:"), value = ""))),
                shiny::fluidRow(
                  shiny::column(width = 3, shiny::textInput(inputId = "textInput_2_1_3_4_13", label = shiny::h5("Plot 13:"), value = "QUANTITY BAR PLOT")),
                  shiny::column(width = 3, shiny::textInput(inputId = "textInput_2_1_3_4_14", label = shiny::h5("Plot 14:"), value = "PERCENTAGE BAR PLOT")),
                  shiny::column(width = 3, shiny::textInput(inputId = "textInput_2_1_3_4_15", label = shiny::h5("Plot 15:"), value = "WAFFLE PLOT")),
                  shiny::column(width = 3, shiny::textInput(inputId = "textInput_2_1_3_4_16", label = shiny::h5("Plot 16:"), value = "CIRCLE PLOT"))),
                style = "padding: 5px;"))),
          
          shiny::mainPanel(
            width = mainPanel_width,
            shiny::tabsetPanel(
              type = "pills",
              shiny::tabPanel(
                title = "Panel 1",  DT::dataTableOutput("dataset_2_1")),
              shiny::tabPanel(
                title = "Panel 2", 
                shiny::hr(),
                shiny::fluidRow(shiny::splitLayout(cellWidths = c("24.5%", "24.5%", "24.5%", "24.5%"), 
                                                   cellArgs = list(style = "padding: 2.5px"),
                                                   shiny::plotOutput("plot_2_1_1"),
                                                   shiny::plotOutput("plot_2_1_2"),
                                                   shiny::plotOutput("plot_2_1_3"),
                                                   shiny::plotOutput("plot_2_1_4"))),
                shiny::fluidRow(
                  shiny::column(width = 3, shiny::downloadButton(outputId = "downloadButton_2_1_2_1", label = NULL, icon = shiny::icon("fas fa-download"))),
                  shiny::column(width = 3, shiny::downloadButton(outputId = "downloadButton_2_1_2_2", label = NULL, icon = shiny::icon("fas fa-download"))),
                  shiny::column(width = 3, shiny::downloadButton(outputId = "downloadButton_2_1_2_3", label = NULL, icon = shiny::icon("fas fa-download"))),
                  shiny::column(width = 3, shiny::downloadButton(outputId = "downloadButton_2_1_2_4", label = NULL, icon = shiny::icon("fas fa-download")))),
                shiny::hr(),
                shiny::fluidRow(shiny::splitLayout(cellWidths = c("24.5%", "24.5%", "24.5%", "24.5%"), 
                                                   cellArgs = list(style = "padding: 2.5px"),
                                                   shiny::plotOutput("plot_2_1_5"),
                                                   shiny::plotOutput("plot_2_1_6"),
                                                   shiny::plotOutput("plot_2_1_7"),
                                                   shiny::plotOutput("plot_2_1_8"))),
                shiny::fluidRow(
                  shiny::column(width = 3, shiny::downloadButton(outputId = "downloadButton_2_1_2_5", label = NULL, icon = shiny::icon("fas fa-download"))),
                  shiny::column(width = 3, shiny::downloadButton(outputId = "downloadButton_2_1_2_6", label = NULL, icon = shiny::icon("fas fa-download"))),
                  shiny::column(width = 3, shiny::downloadButton(outputId = "downloadButton_2_1_2_7", label = NULL, icon = shiny::icon("fas fa-download"))),
                  shiny::column(width = 3, shiny::downloadButton(outputId = "downloadButton_2_1_2_8", label = NULL, icon = shiny::icon("fas fa-download")))),
                shiny::hr(),
                shiny::fluidRow(shiny::splitLayout(cellWidths = c("24.5%", "24.5%", "24.5%", "24.5%"), 
                                                   cellArgs = list(style = "padding: 2.5px"),
                                                   shiny::plotOutput("plot_2_1_9"),
                                                   shiny::plotOutput("plot_2_1_10"),
                                                   shiny::plotOutput("plot_2_1_11"))),
                shiny::fluidRow(
                  shiny::column(width = 3, shiny::downloadButton(outputId = "downloadButton_2_1_2_9", label = NULL, icon = shiny::icon("fas fa-download"))),
                  shiny::column(width = 3, shiny::downloadButton(outputId = "downloadButton_2_1_2_10", label = NULL, icon = shiny::icon("fas fa-download"))),
                  shiny::column(width = 3, shiny::downloadButton(outputId = "downloadButton_2_1_2_11", label = NULL, icon = shiny::icon("fas fa-download"))),
                  shiny::column(width = 3, shiny::downloadButton(outputId = "downloadButton_2_1_2_12", label = NULL, icon = shiny::icon("fas fa-download")))),
                shiny::hr(),
                shiny::fluidRow(shiny::splitLayout(cellWidths = c("24.5%", "24.5%", "24.5%", "24.5%"), 
                                                   cellArgs = list(style = "padding: 2.5px"),
                                                   shiny::plotOutput("plot_2_1_13"),
                                                   shiny::plotOutput("plot_2_1_14"),
                                                   shiny::plotOutput("plot_2_1_15"),
                                                   shiny::plotOutput("plot_2_1_16"))),
                shiny::fluidRow(
                  shiny::column(width = 3, shiny::downloadButton(outputId = "downloadButton_2_1_2_13", label = NULL, icon = shiny::icon("fas fa-download"))),
                  shiny::column(width = 3, shiny::downloadButton(outputId = "downloadButton_2_1_2_14", label = NULL, icon = shiny::icon("fas fa-download"))),
                  shiny::column(width = 3, shiny::downloadButton(outputId = "downloadButton_2_1_2_15", label = NULL, icon = shiny::icon("fas fa-download"))),
                  shiny::column(width = 3, shiny::downloadButton(outputId = "downloadButton_2_1_2_16", label = NULL, icon = shiny::icon("fas fa-download")))),
                shiny::hr()),
              shiny::tabPanel(
                title = "Panel 3"))
          ))),
      # ---------------------------------------------------------------------- #
      shiny::tabPanel(
        "Factor vs Numeric",
        
        shiny::sidebarLayout(
          position = "left",
          shiny::sidebarPanel(
            width = sidebarPanel_width,
            
            shiny::wellPanel(
              shiny::h4(shiny::strong("Dataset import")),
              shiny::fluidRow(
                shiny::column(width = 8, shiny::fileInput(inputId = "File_Input_2_2", label = shiny::h5("Select import file:"))),
                shiny::br(),
                shiny::column(width = 4, shiny::actionButton(inputId = "actionButton_2_2", label = shiny::h5("Restart:"), icon = shiny::icon("fas fa-undo"))))),
            
            shiny::wellPanel(
              shiny::h4(shiny::strong("Input data settings:")),
              shiny::fluidRow(
                shiny::column(width = 6, shiny::sliderInput(inputId = "sliderInput_2_2_2_1", label = shiny::h5("Data fraction:"), min = 0, max = 1, value = 0.5, step = 0.01)),
                shiny::column(width = 3, shiny::radioButtons(inputId = "radioButtons_2_2_2_2", label = shiny::h5("Set seed:"), choices = list("Yes" = TRUE, "No" = FALSE), selected = TRUE)),
                shiny::column(width = 3, shiny::numericInput(inputId = "numericInput_2_2_2_3", label = shiny::h5("Seed value:"), value = 42))),
              shiny::fluidRow(
                shiny::column(width = 6, shiny::selectInput(inputId = "selectInput_2_2_2_4", label = shiny::h5("Factor variable name:"), choices = list())),
                shiny::column(width = 6, shiny::selectInput(inputId = "selectInput_2_2_2_5", label = shiny::h5("Numeric variable name:"), choices = list())))),
            
            shiny::wellPanel(
              shiny::h4(shiny::strong("Global visualisation settings:")),
              
              shiny::wellPanel(
                shiny::h5(shiny::strong("Axis names:")),
                shiny::fluidRow(
                  shiny::column(width = 6, shiny::textInput(inputId = "textInput_2_2_3_1_1", label = shiny::h5("Factor axis name:"), value = "")),
                  shiny::column(width = 6, shiny::textInput(inputId = "textInput_2_2_3_1_2", label = shiny::h5("Numeric axis name:"), value = ""))),
                shiny::fluidRow(
                  shiny::column(width = 6, shiny::textInput(inputId = "textInput_2_2_3_1_3", label = shiny::h5("Count axis name:"), value = "")),
                  shiny::column(width = 6, shiny::textInput(inputId = "textInput_2_2_3_1_4", label = shiny::h5("Percentage axis name:"), value = ""))),
                shiny::fluidRow(
                  shiny::column(width = 6, shiny::textInput(inputId = "textInput_2_2_3_1_5", label = shiny::h5("Caption:"), value = "")))),
              
              shiny::wellPanel(
                shiny::h5(shiny::strong("Numeric settings:")),
                shiny::fluidRow(
                  shiny::column(width = 3, shiny::numericInput(inputId = "numericInput_2_2_3_2_1", label = shiny::h5("Plot title size"), value = 12)),
                  shiny::column(width = 3, shiny::numericInput(inputId = "numericInput_2_2_3_2_2", label = shiny::h5("Axis text size:"), value = 10)),
                  shiny::column(width = 3, shiny::numericInput(inputId = "numericInput_2_2_3_2_3", label = shiny::h5("Label size"), value = 4)),
                  shiny::column(width = 3, shiny::selectInput(inputId = "selectInput_2_2_2_2_4", label = shiny::h5("Waffle grid:"), choices = select_box_input(vector = c(5, 10, 25, 50, 100)), selected = 10)))),
              
              shiny::wellPanel(
                shiny::h5(shiny::strong("Binary settings:")),
                shiny::fluidRow(
                  shiny::column(width = 3, shiny::radioButtons(inputId = "radioButtons_2_2_3_3_1", label = shiny::h5("Display legend:"), choices = list("Yes" = TRUE, "No" = FALSE), selected = TRUE)),
                  shiny::column(width = 3, shiny::radioButtons(inputId = "radioButtons_2_2_3_3_2", label = shiny::h5("Legend position:"), choices = select_box_input(vector = c("left", "top", "right", "bottom")), selected = "bottom")),
                  shiny::column(width = 3, shiny::radioButtons(inputId = "radioButtons_2_2_3_3_3", label = shiny::h5("Legend direction:"), choices = select_box_input(vector = c("horizontal", "vertical")), selected = "horizontal")),
                  shiny::column(width = 3, shiny::radioButtons(inputId = "radioButtons_2_2_3_3_4", label = shiny::h5("Repel labels:"), choices = list("Yes" = TRUE, "No" = FALSE), selected = FALSE)))),
              
              shiny::wellPanel(
                shiny::h5(shiny::strong("Titles names:")),
                shiny::fluidRow(
                  shiny::column(width = 3, shiny::textInput(inputId = "textInput_2_2_3_4_1", label = shiny::h5("Plot 1:"), value = "")),
                  shiny::column(width = 3, shiny::textInput(inputId = "textInput_2_2_3_4_2", label = shiny::h5("Plot 2:"), value = "")),
                  shiny::column(width = 3, shiny::textInput(inputId = "textInput_2_2_3_4_3", label = shiny::h5("Plot 3:"), value = "")),
                  shiny::column(width = 3, shiny::textInput(inputId = "textInput_2_2_3_4_4", label = shiny::h5("Plot 4:"), value = ""))),
                shiny::fluidRow(
                  shiny::column(width = 3, shiny::textInput(inputId = "textInput_2_2_3_4_5", label = shiny::h5("Plot 5:"), value = "")),
                  shiny::column(width = 3, shiny::textInput(inputId = "textInput_2_2_3_4_6", label = shiny::h5("Plot 6:"), value = "")),
                  shiny::column(width = 3, shiny::textInput(inputId = "textInput_2_2_3_4_7", label = shiny::h5("Plot 7:"), value = "")),
                  shiny::column(width = 3, shiny::textInput(inputId = "textInput_2_2_3_4_8", label = shiny::h5("Plot 8:"), value = ""))),
                shiny::fluidRow(
                  shiny::column(width = 3, shiny::textInput(inputId = "textInput_2_2_3_4_9", label = shiny::h5("Plot 9:"), value = "")),
                  shiny::column(width = 3, shiny::textInput(inputId = "textInput_2_2_3_4_10", label = shiny::h5("Plot 10:"), value = "")),
                  shiny::column(width = 3, shiny::textInput(inputId = "textInput_2_2_3_4_11", label = shiny::h5("Plot 11:"), value = "")),
                  shiny::column(width = 3, shiny::textInput(inputId = "textInput_2_2_3_4_12", label = shiny::h5("Plot 12:"), value = ""))),
                shiny::fluidRow(
                  shiny::column(width = 3, shiny::textInput(inputId = "textInput_2_2_3_4_13", label = shiny::h5("Plot 13:"), value = "")),
                  shiny::column(width = 3, shiny::textInput(inputId = "textInput_2_2_3_4_14", label = shiny::h5("Plot 14:"), value = "")),
                  shiny::column(width = 3, shiny::textInput(inputId = "textInput_2_2_3_4_15", label = shiny::h5("Plot 15:"), value = "")),
                  shiny::column(width = 3, shiny::textInput(inputId = "textInput_2_2_3_4_16", label = shiny::h5("Plot 16:"), value = ""))),
                style = "padding: 5px;"))),
          
          shiny::mainPanel(
            
            width = mainPanel_width,
            shiny::tabsetPanel(
              type = "pills",
              shiny::tabPanel(
                title = "Panel 1",  DT::dataTableOutput("dataset_2_2")),
              
              shiny::tabPanel(
                title = "Panel 2",
                shiny::hr(),
                shiny::fluidRow(shiny::splitLayout(cellWidths = c("24.5%", "24.5%", "24.5%", "24.5%"),
                                                   cellArgs = list(style = "padding: 2.5px"),
                                                   shiny::plotOutput("plot_2_2_1"),
                                                   shiny::plotOutput("plot_2_2_2"),
                                                   shiny::plotOutput("plot_2_2_3"),
                                                   shiny::plotOutput("plot_2_2_4"))),
                shiny::hr()
                
              )
              
            )
          )
        )
      ),
      shiny::tabPanel(
        "Numeric vs Numeric"),
      shiny::tabPanel(
        "Date vs Numeric"),
      shiny::tabPanel(
        "Datetime vs Numeric"),
      shiny::tabPanel(
        "Date vs Factor"),
      shiny::tabPanel(
        "Datetime vs Factor")
      # ------------------------------------------------------------------------ #
    )
  )
)



#     width = mainPanel_width,
#     shiny::tabsetPanel(
#       type = "pills",
#       shiny::tabPanel(
#         title = "Panel 1",  DT::dataTableOutput("dataset_2_2")),
#       shiny::tabPanel(
#         title = "Panel 2",
#         shiny::hr(),
#         shiny::fluidRow(shiny::splitLayout(cellWidths = c("24.5%", "24.5%", "24.5%", "24.5%"),
#                                            cellArgs = list(style = "padding: 2.5px"),
#                                            shiny::plotOutput("plot_2_2_1"),
#                                            shiny::plotOutput("plot_2_2_2"),
#                                            shiny::plotOutput("plot_2_2_3"),
#                                            shiny::plotOutput("plot_2_2_4"))),
#         shiny::hr(),
#         shiny::fluidRow(shiny::splitLayout(cellWidths = c("24.5%", "24.5%", "24.5%", "24.5%"),
#                                            cellArgs = list(style = "padding: 2.5px"),
#                                            shiny::plotOutput("plot_2_2_1"),
#                                            shiny::plotOutput("plot_2_2_2"),
#                                            shiny::plotOutput("plot_2_2_3"),
#                                            shiny::plotOutput("plot_2_2_4"))),
#         shiny::hr(),
#         shiny::fluidRow(shiny::splitLayout(cellWidths = c("24.5%", "24.5%", "24.5%", "24.5%"),
#                                            cellArgs = list(style = "padding: 2.5px"),
#                                            shiny::plotOutput("plot_2_2_1"),
#                                            shiny::plotOutput("plot_2_2_2"),
#                                            shiny::plotOutput("plot_2_2_3"),
#                                            shiny::plotOutput("plot_2_2_4"))),
#         shiny::hr(),
#         shiny::fluidRow(shiny::splitLayout(cellWidths = c("24.5%", "24.5%", "24.5%", "24.5%"),
#                                            cellArgs = list(style = "padding: 2.5px"),
#                                            shiny::plotOutput("plot_2_2_1"),
#                                            shiny::plotOutput("plot_2_2_2"),
#                                            shiny::plotOutput("plot_2_2_3"),
#                                            shiny::plotOutput("plot_2_2_4")))),
#       shiny::tabPanel(
#         title = "Panel 3")))






# ---------------------------------------------------------------------------- #
# Server:
server = function(input, output, session){
  
  output$CurrentDateTime <- shiny::renderText({
    shiny::invalidateLater(millis = 1000,
                           session = session)
    paste(Sys.time())})
  
  # -------------------------------------------------------------------------- #
  # 1.1
  source("1_1_Dataset.R", local = TRUE)
  source("1_1_Table.R", local = TRUE)
  source("1_1_Variables.R", local = TRUE)
  source("1_1_Plots.R", local = TRUE)
  source("1_1_Reset.R", local = TRUE)
  source("1_1_Download.R", local = TRUE)
  
  # -------------------------------------------------------------------------- #
  # 2.1
  source("2_1_Dataset.R", local = TRUE)
  source("2_1_Table.R", local = TRUE)
  source("2_1_Variables.R", local = TRUE)
  source("2_1_Plots.R", local = TRUE)
  source("2_1_Reset.R", local = TRUE)
  source("2_1_Download.R", local = TRUE)
  
  # -------------------------------------------------------------------------- #
  
  
  
  
  
  # 2.2
  dataset_2_2 <- shiny::reactive({
    file_import_2_2 <- input$File_Input_2_2
    if (is.null(file_import_2_2))
      return(NULL)
    readr::read_csv2(file_import_2_2$datapath)})
  
  dataset_2_2_ <- shiny::reactive({ 
    data_sampler(data = dataset_2_2(),
                 data_fraction = input$sliderInput_2_2_2_1,
                 set_seed = input$radioButtons_2_2_2_2,
                 seed_value = input$numericInput_2_2_2_3)})
  
  factor_variables_2_2 <- shiny::reactive({
    character_vars <- dataset_2_2_() %>%
      dplyr::select_if(is.character) %>%
      colnames()
    factor_vars <- dataset_2_2_() %>%
      dplyr::select_if(is.factor) %>%
      colnames()
    select_box_input(vector = c(character_vars, factor_vars))})
  
  numeric_variables_2_2 <- shiny::reactive({
    numeric_vars <- dataset_2_2_() %>%
      dplyr::select_if(is.numeric) %>%
      colnames()
    select_box_input(vector = numeric_vars)})
  
  shiny::observeEvent(eventExpr = input$File_Input_2_2, handlerExpr = {shiny::updateSelectInput(session, inputId = "selectInput_2_2_2_4", choices = factor_variables_2_2(), selected = factor_variables_2_2()[[1]])})
  shiny::observeEvent(eventExpr = input$File_Input_2_2, handlerExpr = {shiny::updateSelectInput(session, inputId = "selectInput_2_2_2_5", choices = numeric_variables_2_2(), selected = numeric_variables_2_2()[[1]])})
  
  output$plot_2_2_1 <- shiny::renderPlot({
    factor_count_histogram_plot(data = dataset_2_2_(),
                                factor_var = input$selectInput_2_2_2_4,
                                title_size = input$numericInput_2_2_3_2_1,
                                repel_label = input$radioButtons_2_2_3_3_4,
                                text_size = input$numericInput_2_2_3_2_2,
                                label_size = input$numericInput_2_2_3_2_3,
                                factor_axis = input$textInput_2_2_3_1_1,
                                title = input$textInput_2_2_3_4_1,
                                count_axis = input$textInput_2_2_3_1_3,
                                caption = input$textInput_2_2_3_1_5)})
  
  output$plot_2_2_2 <- shiny::renderPlot({
    factor_percentage_histogram_plot(data = dataset_2_2_(),
                                     factor_var = input$selectInput_2_2_2_4,
                                     title_size = input$numericInput_2_2_3_2_1,
                                     repel_label = input$radioButtons_2_2_3_3_4,
                                     text_size = input$numericInput_2_2_3_2_2,
                                     label_size = input$numericInput_2_2_3_2_3,
                                     factor_axis = input$textInput_2_2_3_1_1,
                                     title = input$textInput_2_2_3_4_2,
                                     percentage_axis = input$textInput_2_2_3_1_4,
                                     caption = input$textInput_2_2_3_1_5)})
  
  output$plot_2_2_3 <- shiny::renderPlot({
    factor_waffle_plot(data = dataset_2_2_(),
                       factor_var = input$selectInput_2_2_2_4,
                       factor_axis = input$textInput_2_2_3_1_1,
                       title_size = input$numericInput_2_2_3_2_1,
                       text_size = input$numericInput_2_2_3_2_2,
                       title = input$textInput_2_2_3_4_3,
                       grid_size = as.numeric(input$selectInput_2_2_2_2_4),
                       display_legend = input$radioButtons_2_2_3_3_1,
                       legend_position = input$radioButtons_2_2_3_3_2,
                       legend_direction = input$radioButtons_2_2_3_3_3,
                       caption = input$textInput_2_2_3_1_5)})
  
  output$plot_2_2_4 <- shiny::renderPlot({
    factor_circle_plot(data = dataset_2_2_(),
                       factor_var = input$selectInput_2_2_2_4,
                       title_size = input$numericInput_2_2_3_2_1,
                       text_size = input$numericInput_2_2_3_2_2,
                       repel_label = input$radioButtons_2_2_3_3_4,
                       label_size = input$numericInput_2_2_3_2_3,
                       title = input$textInput_2_2_3_4_4,
                       caption = input$textInput_2_2_3_1_5)})
  
  output$dataset_2_2 <- DT::renderDataTable(
    expr = dataset_2_2_(),
    options = list(pageLength = 25,
                   lengthMenu = list(c(5, 10, 25, 100, 1000, -1),
                                     c("5", "10", "25", "100", "1000", "All")),
                   searching = TRUE))
  
}





# ---------------------------------------------------------------------------- #
# Application:
shiny::shinyApp(ui = ui, server = server)

