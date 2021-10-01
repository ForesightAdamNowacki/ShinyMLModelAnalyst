# 1_1_Download.R
output$downloadButton_1_1_2_1 <- shiny::downloadHandler(
  filename = function(){save_filename_paster(vector = c(input$selectInput_1_1_2_4, input$textInput_1_1_3_4_1))},
  content = function(file){ggplot2::ggsave(file, plot_1_1_1(), device = "png")})

output$downloadButton_1_1_2_2 <- shiny::downloadHandler(
  filename = function(){save_filename_paster(vector = c(input$selectInput_1_1_2_4, input$textInput_1_1_3_4_2))},
  content = function(file){ggplot2::ggsave(file, plot_1_1_2(), device = "png")})

output$downloadButton_1_1_2_3 <- shiny::downloadHandler(
  filename = function(){save_filename_paster(vector = c(input$selectInput_1_1_2_4, input$textInput_1_1_3_4_3))},
  content = function(file){ggplot2::ggsave(file, plot_1_1_3(), device = "png")})

output$downloadButton_1_1_2_4 <- shiny::downloadHandler(
  filename = function(){save_filename_paster(vector = c(input$selectInput_1_1_2_4, input$textInput_1_1_3_4_4))},
  content = function(file){ggplot2::ggsave(file, plot_1_1_4(), device = "png")})