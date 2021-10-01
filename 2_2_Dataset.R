# 2_1_Dataset.R
dataset_2_2 <- shiny::reactive({
  if (is.null(input$File_Input_2_2)){return(NULL)}
  readr::read_csv2(input$File_Input_2_2$datapath)})

dataset_2_2_ <- shiny::reactive({ 
  data_sampler(data = dataset_2_2(),
               data_fraction = input$sliderInput_2_2_2_1,
               set_seed = input$radioButtons_2_2_2_2,
               seed_value = input$numericInput_2_2_2_3)})