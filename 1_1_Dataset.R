# 1_1_Dataset.R
dataset_1_1 <- shiny::reactive({
  if (is.null(input$File_Input_1_1)){return(NULL)}
  readr::read_csv2(input$File_Input_1_1$datapath)})

dataset_1_1_ <- shiny::reactive({ 
  data_sampler(data = dataset_1_1(),
               data_fraction = input$sliderInput_1_1_2_1,
               set_seed = input$radioButtons_1_1_2_2,
               seed_value = input$numericInput_1_1_2_3)})