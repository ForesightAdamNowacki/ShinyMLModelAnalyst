# 1_1_Plots.R
plot_1_1_1 <- shiny::reactive({
  factor_count_histogram_plot(data = dataset_1_1_(),
                              factor_var = input$selectInput_1_1_2_4,
                              title_size = input$numericInput_1_1_3_2_1,
                              repel_label = input$radioButtons_1_1_3_3_4,
                              text_size = input$numericInput_1_1_3_2_2,
                              label_size = input$numericInput_1_1_3_2_3,
                              factor_axis = input$textInput_1_1_3_1_1,
                              title = input$textInput_1_1_3_4_1,
                              count_axis = input$textInput_1_1_3_1_2,
                              caption = input$textInput_1_1_3_1_4)})

plot_1_1_2 <- shiny::reactive({
  factor_percentage_histogram_plot(data = dataset_1_1_(),
                                   factor_var = input$selectInput_1_1_2_4,
                                   title_size = input$numericInput_1_1_3_2_1,
                                   repel_label = input$radioButtons_1_1_3_3_4,
                                   text_size = input$numericInput_1_1_3_2_2,
                                   label_size = input$numericInput_1_1_3_2_3,
                                   factor_axis = input$textInput_1_1_3_1_1,
                                   title = input$textInput_1_1_3_4_2,
                                   percentage_axis = input$textInput_1_1_3_1_3,
                                   caption = input$textInput_1_1_3_1_4)})

plot_1_1_3 <- shiny::reactive({
  factor_waffle_plot(data = dataset_1_1_(),
                     factor_var = input$selectInput_1_1_2_4,
                     factor_axis = input$textInput_1_1_3_1_1,
                     title_size = input$numericInput_1_1_3_2_1,
                     text_size = input$numericInput_1_1_3_2_2,
                     title = input$textInput_1_1_3_4_3,
                     grid_size = as.numeric(input$selectInput_1_1_2_2_4),
                     display_legend = input$radioButtons_1_1_3_3_1,
                     legend_position = input$radioButtons_1_1_3_3_2,
                     legend_direction = input$radioButtons_1_1_3_3_3,
                     caption = input$textInput_1_1_3_1_4)})

plot_1_1_4 <- shiny::reactive({
  factor_circle_plot(data = dataset_1_1_(),
                     factor_var = input$selectInput_1_1_2_4,
                     title_size = input$numericInput_1_1_3_2_1,
                     text_size = input$numericInput_1_1_3_2_2,
                     repel_label = input$radioButtons_1_1_3_3_4,
                     label_size = input$numericInput_1_1_3_2_3,
                     title = input$textInput_1_1_3_4_4,
                     caption = input$textInput_1_1_3_1_4)})

output$plot_1_1_1 <- shiny::renderPlot({plot_1_1_1()})
output$plot_1_1_2 <- shiny::renderPlot({plot_1_1_2()})
output$plot_1_1_3 <- shiny::renderPlot({plot_1_1_3()})
output$plot_1_1_4 <- shiny::renderPlot({plot_1_1_4()})
