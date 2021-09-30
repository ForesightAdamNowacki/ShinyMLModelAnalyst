# 2_1_Dataset.R
dataset_2_1 <- shiny::reactive({
  if (is.null(input$File_Input_2_1)){return(NULL)}
  readr::read_csv2(input$File_Input_2_1$datapath)})

dataset_2_1_ <- shiny::reactive({ 
  data_sampler(data = dataset_2_1(),
               data_fraction = input$sliderInput_2_1_2_1,
               set_seed = input$radioButtons_2_1_2_2,
               seed_value = input$numericInput_2_1_2_3)})