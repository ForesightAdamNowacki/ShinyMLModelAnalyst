# 2_1_Download.R
output$downloadButton_2_1_2_1 <- shiny::downloadHandler(
  filename = function(){save_filename_paster(vector = c(input$selectInput_2_1_2_4, input$textInput_2_1_3_4_1))},
  content = function(file){ggplot2::ggsave(file, plot_2_1_1(), device = "png", limitsize = FALSE)})

output$downloadButton_2_1_2_2 <- shiny::downloadHandler(
  filename = function(){save_filename_paster(vector = c(input$selectInput_2_1_2_4, input$textInput_2_1_3_4_2))},
  content = function(file){ggplot2::ggsave(file, plot_2_1_2(), device = "png", limitsize = FALSE)})

output$downloadButton_2_1_2_3 <- shiny::downloadHandler(
  filename = function(){save_filename_paster(vector = c(input$selectInput_2_1_2_4, input$textInput_2_1_3_4_3))},
  content = function(file){ggplot2::ggsave(file, plot_2_1_3(), device = "png", limitsize = FALSE)})

output$downloadButton_2_1_2_4 <- shiny::downloadHandler(
  filename = function(){save_filename_paster(vector = c(input$selectInput_2_1_2_4, input$textInput_2_1_3_4_4))},
  content = function(file){ggplot2::ggsave(file, plot_2_1_4(), device = "png", limitsize = FALSE)})

output$downloadButton_2_1_2_5 <- shiny::downloadHandler(
  filename = function(){save_filename_paster(vector = c(input$selectInput_2_1_2_4, input$selectInput_2_1_2_5, input$textInput_2_1_3_4_5))},
  content = function(file){ggplot2::ggsave(file, plot_2_1_5(), device = "png", limitsize = FALSE)})

output$downloadButton_2_1_2_6 <- shiny::downloadHandler(
  filename = function(){save_filename_paster(vector = c(input$selectInput_2_1_2_4, input$selectInput_2_1_2_5, input$textInput_2_1_3_4_6))},
  content = function(file){ggplot2::ggsave(file, plot_2_1_6(), device = "png", limitsize = FALSE)})

output$downloadButton_2_1_2_7 <- shiny::downloadHandler(
  filename = function(){save_filename_paster(vector = c(input$selectInput_2_1_2_5, input$selectInput_2_1_2_4, input$textInput_2_1_3_4_7))},
  content = function(file){ggplot2::ggsave(file, plot_2_1_7(), device = "png", limitsize = FALSE)})

output$downloadButton_2_1_2_8 <- shiny::downloadHandler(
  filename = function(){save_filename_paster(vector = c(input$selectInput_2_1_2_4, input$selectInput_2_1_2_5, input$textInput_2_1_3_4_8))},
  content = function(file){ggplot2::ggsave(file, plot_2_1_8(), device = "png", limitsize = FALSE)})

output$downloadButton_2_1_2_9 <- shiny::downloadHandler(
  filename = function(){save_filename_paster(vector = c(input$selectInput_2_1_2_4, input$selectInput_2_1_2_5, input$textInput_2_1_3_4_9))},
  content = function(file){ggplot2::ggsave(file, plot_2_1_9(), device = "png", limitsize = FALSE)})

output$downloadButton_2_1_2_10 <- shiny::downloadHandler(
  filename = function(){save_filename_paster(vector = c(input$selectInput_2_1_2_4, input$selectInput_2_1_2_5, input$textInput_2_1_3_4_10))},
  content = function(file){ggplot2::ggsave(file, plot_2_1_10(), device = "png", limitsize = FALSE)})

output$downloadButton_2_1_2_11 <- shiny::downloadHandler(
  filename = function(){save_filename_paster(vector = c(input$selectInput_2_1_2_5, input$selectInput_2_1_2_4, input$textInput_2_1_3_4_11))},
  content = function(file){ggplot2::ggsave(file, plot_2_1_11(), device = "png", limitsize = FALSE)})

# output$downloadButton_2_1_2_12 <- shiny::downloadHandler(
#   filename = function(){save_filename_paster(vector = c(input$selectInput_2_1_2_4, input$selectInput_2_1_2_5, input$textInput_2_1_3_4_12))},
#   content = function(file){ggplot2::ggsave(file, plot_2_1_12(), device = "png", limitsize = FALSE)})

output$downloadButton_2_1_2_13 <- shiny::downloadHandler(
  filename = function(){save_filename_paster(vector = c(input$selectInput_2_1_2_5, input$textInput_2_1_3_4_13))},
  content = function(file){ggplot2::ggsave(file, plot_2_1_13(), device = "png", limitsize = FALSE)})

output$downloadButton_2_1_2_14 <- shiny::downloadHandler(
  filename = function(){save_filename_paster(vector = c(input$selectInput_2_1_2_5, input$textInput_2_1_3_4_14))},
  content = function(file){ggplot2::ggsave(file, plot_2_1_14(), device = "png", limitsize = FALSE)})

output$downloadButton_2_1_2_15 <- shiny::downloadHandler(
  filename = function(){save_filename_paster(vector = c(input$selectInput_2_1_2_5, input$textInput_2_1_3_4_15))},
  content = function(file){ggplot2::ggsave(file, plot_2_1_15(), device = "png", limitsize = FALSE)})

output$downloadButton_2_1_2_16 <- shiny::downloadHandler(
  filename = function(){save_filename_paster(vector = c(input$selectInput_2_1_2_5, input$textInput_2_1_3_4_16))},
  content = function(file){ggplot2::ggsave(file, plot_2_1_16(), device = "png", limitsize = FALSE)})