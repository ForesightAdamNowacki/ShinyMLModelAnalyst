# 2_2_Table.R
output$dataset_2_2 <- DT::renderDataTable(
  expr = dataset_2_2_(),
  options = list(pageLength = 25,
                 lengthMenu = list(c(5, 10, 25, 100, 1000, -1), c("5", "10", "25", "100", "1000", "All")),
                 searching = TRUE))