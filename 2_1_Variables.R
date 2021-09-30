# 2_1_Variables.R
factor_variables_2_1 <- shiny::reactive({
  character_vars <- dataset_2_1_() %>% dplyr::select_if(is.character) %>% colnames()
  factor_vars <- dataset_2_1_() %>% dplyr::select_if(is.factor) %>% colnames()
  select_box_input(vector = c(character_vars, factor_vars))})
shiny::observeEvent(eventExpr = input$File_Input_2_1, handlerExpr = {shiny::updateSelectInput(session, inputId = "selectInput_2_1_2_4", choices = factor_variables_2_1(), selected = factor_variables_2_1()[[1]])})
shiny::observeEvent(eventExpr = input$File_Input_2_1, handlerExpr = {shiny::updateSelectInput(session, inputId = "selectInput_2_1_2_5", choices = factor_variables_2_1(), selected = factor_variables_2_1()[[2]])})
