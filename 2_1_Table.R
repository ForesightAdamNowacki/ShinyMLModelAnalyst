# 2_1_Table.R
output$dataset_2_1 <- DT::renderDataTable(
  expr = dataset_2_1_(),
  options = list(pageLength = 25,
                 lengthMenu = list(c(5, 10, 25, 100, 1000, -1), c("5", "10", "25", "100", "1000", "All")),
                 searching = TRUE))