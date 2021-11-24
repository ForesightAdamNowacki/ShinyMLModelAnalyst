library(Metrics)
library(tidyverse)
options(scipen = 10)

calculate_regression_table_metrics <- function(actual_tbl, 
                                               predicted_tbl,
                                               round = 4){
  
  join_column <- intersect(colnames(actual_tbl), colnames(predicted_tbl))
  actual_column <- setdiff(colnames(actual_tbl), join_column)
  predicted_column <- setdiff(colnames(predicted_tbl), join_column)
  
  tbl <- actual_tbl %>%
    dplyr::left_join(predicted_tbl, by = join_column)
  actual_vector <- tbl %>%
    dplyr::pull(actual_column)
  predicted_vector <- tbl %>%
    dplyr::pull(predicted_column)
  
  regression_table_metrics <- dplyr::bind_rows(tibble::tibble(Value = Metrics::rmse(actual = actual_vector, predicted = predicted_vector),
                                                              Abbreviation = "RMSE",
                                                              Fullname = "Root Mean Squared Error"),
                                               tibble::tibble(Value = Metrics::mse(actual = actual_vector, predicted = predicted_vector),
                                                              Abbreviation = "MSE",
                                                              Fullname = "Mean Squared Error"),
                                               tibble::tibble(Value = Metrics::bias(actual = actual_vector, predicted = predicted_vector),
                                                              Abbreviation = "BE",
                                                              Fullname = "Bias Error"),
                                               tibble::tibble(Value = Metrics::mae(actual = actual_vector, predicted = predicted_vector),
                                                              Abbreviation = "MAE",
                                                              Fullname = "Mean Absolute Error"),
                                               tibble::tibble(Value = Metrics::mape(actual = actual_vector, predicted = predicted_vector),
                                                              Abbreviation = "MAPE",
                                                              Fullname = "Mean Absolute Percentage Error"),
                                               tibble::tibble(Value = Metrics::mase(actual = actual_vector, predicted = predicted_vector),
                                                              Abbreviation = "MASE",
                                                              Fullname = "Mean Absolute Scaled Error"),
                                               tibble::tibble(Value = Metrics::mdae(actual = actual_vector, predicted = predicted_vector),
                                                              Abbreviation = "MDAE",
                                                              Fullname = "Median Absolute Error"),
                                               tibble::tibble(Value = Metrics::msle(actual = actual_vector, predicted = predicted_vector),
                                                              Abbreviation = "MSLE",
                                                              Fullname = "Mean Squared Log Error"),
                                               tibble::tibble(Value = Metrics::percent_bias(actual = actual_vector, predicted = predicted_vector),
                                                              Abbreviation = "PBE",
                                                              Fullname = "Percent Bias Error"),
                                               tibble::tibble(Value = Metrics::rae(actual = actual_vector, predicted = predicted_vector),
                                                              Abbreviation = "RAE",
                                                              Fullname = "Relative Absolute Error"),
                                               tibble::tibble(Value = Metrics::rmsle(actual = actual_vector, predicted = predicted_vector),
                                                              Abbreviation = "RMSLE",
                                                              Fullname = "Root Mean Squared Log Error"),
                                               tibble::tibble(Value = Metrics::rrse(actual = actual_vector, predicted = predicted_vector),
                                                              Abbreviation = "RRSE",
                                                              Fullname = "Root Relative Squared Error"),
                                               tibble::tibble(Value = Metrics::rse(actual = actual_vector, predicted = predicted_vector),
                                                              Abbreviation = "RSE",
                                                              Fullname = "Relative Squared Error"),
                                               tibble::tibble(Value = Metrics::smape(actual = actual_vector, predicted = predicted_vector),
                                                              Abbreviation = "SMAPE",
                                                              Fullname = "Symmetric Mean Absolute Percentage Error"),
                                               tibble::tibble(Value = Metrics::sse(actual = actual_vector, predicted = predicted_vector),
                                                              Abbreviation = "SSE",
                                                              Fullname = "Sum of Squared Errors"),
                                               tibble::tibble(Value = MLmetrics::MedianAPE(y_true = actual_vector, y_pred = predicted_vector),
                                                              Abbreviation = "MAPE",
                                                              Fullname = "Median Absolute Percentage Error"),
                                               tibble::tibble(Value = MLmetrics::R2_Score(y_true = actual_vector, y_pred = predicted_vector),
                                                              Abbreviation = "R2",
                                                              Fullname = "R-Squared Score"),
                                               tibble::tibble(Value = MLmetrics::RMSPE(y_true = actual_vector, y_pred = predicted_vector),
                                                              Abbreviation = "RMSPE",
                                                              Fullname = "Root Mean Squared Percentage Error")) %>%
    dplyr::arrange(Abbreviation) %>%
    dplyr::mutate(ID = dplyr::row_number(),
                  Value = round(Value, round)) %>%
    dplyr::select(ID, Abbreviation, Fullname, Value)
  
  return(regression_table_metrics)
}


actual <- round(runif(n = 1000, min = 0, max = 1))
predicted <- runif(n = 1000, min = 0, max = 1)
cutoff = 0.5
round = 4

actual_tbl <- tibble::tibble(Actual = actual) %>%
  dplyr::mutate(Id = dplyr::row_number())
predicted_tbl <- tibble::tibble(Predicted = predicted) %>%
  dplyr::mutate(Id = dplyr::row_number())
















calculate_regression_plot_metrics <- function(actual_tbl,
                                              predicted_tbl){
  
  join_column <- intersect(colnames(actual_tbl), colnames(predicted_tbl))
  actual_column <- setdiff(colnames(actual_tbl), join_column)
  predicted_column <- setdiff(colnames(predicted_tbl), join_column)
  
  tbl <- actual_tbl %>%
    dplyr::left_join(predicted_tbl, by = join_column)
  actual_vector <- tbl %>%
    dplyr::pull(actual_column)
  predicted_vector <- tbl %>%
    dplyr::pull(predicted_column)
  
  regression_plot_metrics <- tibble::tibble(ACTUAL = actual_vector,
                                            PREDICTED = predicted_vector,
                                            AE = Metrics::ae(actual = actual_vector, predicted = predicted_vector),
                                            APE = Metrics::ape(actual = actual_vector, predicted = predicted_vector),
                                            SE = Metrics::se(actual = actual_vector, predicted = predicted_vector),
                                            SLE = Metrics::sle(actual = actual_vector, predicted = predicted_vector),
                                            ERROR = actual_vector - predicted_vector,
                                            RATIO = predicted_vector/actual_vector)
  
  return(regression_plot_metrics)
}



data <- diamonds
actual <- data$price
multiplier <- 0.1
predicted <- actual + runif(n = nrow(data), min = -mean(actual) * multiplier, max = mean(actual) * multiplier)

actual_tbl <- tibble::tibble(Actual = actual) %>%
  dplyr::mutate(Id = dplyr::row_number())
predicted_tbl <- tibble::tibble(Predicted = predicted) %>%
  dplyr::mutate(Id = dplyr::row_number())

table_metrics <- calculate_regression_table_metrics(actual_tbl = actual_tbl,
                                   predicted_tbl = predicted_tbl)
plot_metrics <- calculate_regression_plot_metrics(actual_tbl = actual_tbl,
                                  predicted_tbl = predicted_tbl)

plot_metrics
metric <- "ACTUAL"
title = ""
caption = ""
vertical_axis = ""
horizontal_axis = ""
title_size = 10
text_size = 7
title_horizontal_position = 0.5
title_vertical_position = 0.5
n = 5
round = 2
display_labels = TRUE
repel_label = FALSE
label_padding = 0.25
label_size = 4


plot_regression_metric <- function(plot_metrics,
                                   metric,
                                   n = 1,
                                   display_labels = TRUE,
                                   repel_label = FALSE,
                                   label_padding = 0.25,
                                   label_size = 4,
                                   round = 2,
                                   title = "",
                                   caption = "",
                                   vertical_axis = "",
                                   horizontal_axis = "",
                                   title_size = 10,
                                   text_size = 7,
                                   title_horizontal_position = 0.5,
                                   title_vertical_position = 0.5){
  
  # Variables:
  metric <- rlang::sym(metric)
  metric <- dplyr::enquo(metric)
  metric_name <- dplyr::quo_name(metric)
  
  titles <- tibble::tibble(ACTUAL = "ACTUAL",
                           PREDICTED = "PREDICTED",
                           AE = "ABSOLUTE ERROR",
                           APE = "ABSOLUTE PERCENT ERROR",
                           SE = "SQUARED ERROR",
                           SLE = "SQUARED LOG ERROR",
                           ERROR = "ERROR",
                           RATIO = "RATIO")
  
  # Names:
  if(horizontal_axis == ""){horizontal_axis <- stringr::str_to_upper(metric_name)}
  if(title == ""){title <- dplyr::pull(titles, !!metric)}
  if(caption == ""){caption <- "SOURCE: UNKNOWN"}
  if(vertical_axis == ""){vertical_axis <- "OBSERVATION"}
  
  plot <- plot_metrics %>%
    dplyr::select(!!metric) %>%
    dplyr::filter(!is.na(!!metric)) %>%
    dplyr::arrange(!!metric) %>%
    dplyr::mutate(OBSERVATION = dplyr::row_number()) %>%
    ggplot2::ggplot(data = ., mapping = ggplot2::aes(x = OBSERVATION, y = !!metric)) +
    ggplot2::geom_line() +
    ggplot2::labs(x = vertical_axis,
                  y = horizontal_axis,
                  caption = caption,
                  title = title) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = title_size, color = "black", face = "bold", hjust = title_horizontal_position, vjust = title_vertical_position),
                   axis.text.x = ggplot2::element_text(size = text_size, color = "black", face = "plain"),
                   axis.title.x = ggplot2::element_text(size = text_size, color = "black", face = "bold"),
                   axis.text.y = ggplot2::element_text(size = text_size, color = "black", face = "plain"),
                   axis.title.y = ggplot2::element_text(size = text_size, color = "black", face = "bold"),
                   panel.grid.major.x = ggplot2::element_line(linetype = "blank"),
                   panel.grid.minor.x = ggplot2::element_line(linetype = "blank"),
                   panel.grid.major.y = ggplot2::element_line(color = "black", linetype = "dotted"),
                   panel.grid.minor.y = ggplot2::element_line(linetype = "blank"),
                   axis.ticks = ggplot2::element_line(size = 1, color = "black", linetype = "dotted"),
                   axis.ticks.length = ggplot2::unit(0.1, "cm"),
                   plot.background = ggplot2::element_rect(fill = "gray80", color = "black", linetype = "solid", size = 1.5),
                   panel.background = ggplot2::element_rect(fill = "gray90", color = "black", linetype = "solid"),
                   plot.caption = ggplot2::element_text(size = text_size, color = "black", face = "bold", hjust = 1))
  
  if(display_labels){
    
    labels <- plot_metrics %>%
      dplyr::select(!!metric) %>%
      dplyr::filter(!is.na(!!metric)) %>%
      dplyr::arrange(!!metric) %>%
      dplyr::mutate(OBSERVATION = dplyr::row_number()) %>%
      dplyr::mutate(CUTS = ggplot2::cut_number(!!metric, n = n)) %>%
      dplyr::group_by(CUTS) %>%
      dplyr::summarise(FROM = min(OBSERVATION),
                       COUNT = dplyr::n(),
                       MIN = round(min(!!metric), round),
                       MEAN = round(mean(!!metric), round),
                       MEDIAN = round(median(!!metric), round),
                       MAX = round(max(!!metric), round),
                       TO = max(OBSERVATION),
                       MAX_VALUE = max(!!metric)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(POSITION_X = (FROM + TO)/2,
                    POSITION_Y = (min(MIN) + max(MAX))/2,
                    LABEL = paste(paste("COUNT:", COUNT), 
                                  paste("MIN:", MIN), 
                                  paste("MEAN:", MEAN),
                                  paste("MEDIAN:", MEDIAN),
                                  paste("MAX:", MAX), 
                                  sep = "\n"))  
    
    plot <- plot +
      ggplot2::geom_vline(xintercept = labels$FROM, lty = 2) +
      ggplot2::geom_vline(xintercept = labels$TO, lty = 2)  
    
    if(repel_label){
      
      plot <- plot +
        ggrepel::geom_label_repel(data = labels,
                                  mapping = ggplot2::aes(x = POSITION_X, y = POSITION_Y, label = LABEL),
                                  color = "black", 
                                  fill = "white",
                                  fontface = 1,
                                  size = label_size, 
                                  label.padding = ggplot2::unit(label_padding, "lines"),
                                  label.r = ggplot2::unit(0, "lines"),
                                  direction = "y")
      
    } else {
      
      plot <- plot +
        ggplot2::geom_label(data = labels,
                            mapping = ggplot2::aes(x = POSITION_X, y = POSITION_Y, label = LABEL),
                            color = "black", 
                            fill = "white",
                            fontface = 1,
                            size = label_size, 
                            label.padding = ggplot2::unit(label_padding, "lines"),
                            label.r = ggplot2::unit(0, "lines"),
                            direction = "y")
      
    }
  }
  
  return(plot)
  
}


plot_metrics

plot_regression_metric(plot_metrics = plot_metrics,
                       metric = "SLE",
                       n = 5,
                       display_labels = TRUE,
                       repel_label = FALSE)
;




