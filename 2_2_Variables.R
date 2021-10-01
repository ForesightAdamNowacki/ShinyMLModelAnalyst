# 2_2_Variables.R
factor_variables_2_2 <- shiny::reactive({
  character_vars <- dataset_2_2_() %>% dplyr::select_if(is.character) %>% colnames()
  factor_vars <- dataset_2_2_() %>% dplyr::select_if(is.factor) %>% colnames()
  select_box_input(vector = c(character_vars, factor_vars))})

numeric_variables_2_2 <- shiny::reactive({
  numeric_vars <- dataset_2_2_() %>% dplyr::select_if(is.numeric) %>% colnames()
  select_box_input(vector = numeric_vars)})

shiny::observeEvent(eventExpr = input$File_Input_2_2, handlerExpr = {shiny::updateSelectInput(session, inputId = "selectInput_2_2_2_4", choices = factor_variables_2_2(), selected = factor_variables_2_2()[[1]])})
shiny::observeEvent(eventExpr = input$File_Input_2_2, handlerExpr = {shiny::updateSelectInput(session, inputId = "selectInput_2_2_2_5", choices = numeric_variables_2_2(), selected = numeric_variables_2_2()[[1]])})