# 2_2_Plots.R
plot_2_2_1 <- shiny::reactive({
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

plot_2_2_2 <- shiny::reactive({
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

plot_2_2_3 <- shiny::reactive({
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

plot_2_2_4 <- shiny::reactive({
  factor_circle_plot(data = dataset_2_2_(),
                     factor_var = input$selectInput_2_2_2_4,
                     title_size = input$numericInput_2_2_3_2_1,
                     text_size = input$numericInput_2_2_3_2_2,
                     repel_label = input$radioButtons_2_2_3_3_4,
                     label_size = input$numericInput_2_2_3_2_3,
                     title = input$textInput_2_2_3_4_4,
                     caption = input$textInput_2_2_3_1_5)})

output$plot_2_2_1 <- shiny::renderPlot({plot_2_2_1()})
output$plot_2_2_2 <- shiny::renderPlot({plot_2_2_2()})
output$plot_2_2_3 <- shiny::renderPlot({plot_2_2_3()})
output$plot_2_2_4 <- shiny::renderPlot({plot_2_2_4()})
