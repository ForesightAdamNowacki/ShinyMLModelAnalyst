# ---------------------------------------------------------------------------- #
Total_Population_function <- function(True_Negative,
                                      False_Positive, 
                                      False_Negative, 
                                      True_Positive){
  
  return(True_Negative + False_Positive + False_Negative + True_Positive)
  
}

# ---------------------------------------------------------------------------- #
Actual_Positive_function <- function(True_Positive,
                                     False_Negative){
  
  return(True_Positive + False_Negative)
  
}

# ---------------------------------------------------------------------------- #
Actual_Negative_function <- function(False_Positive, 
                                     True_Negative){
  
  return(False_Positive + True_Negative)
  
}

# ---------------------------------------------------------------------------- #
Predicted_Positive_function <- function(True_Positive, 
                                        False_Positive){
  
  return(True_Positive + False_Positive)
  
}

# ---------------------------------------------------------------------------- #
Predicted_Negative_function <- function(False_Negative,
                                        True_Negative){
  
  return(False_Negative + True_Negative)
  
}

# ---------------------------------------------------------------------------- #
Prevelance_function <- function(Actual_Positive, 
                                Actual_Negative){
  
  return(Actual_Positive/(Actual_Positive + Actual_Negative))
  
}

# ---------------------------------------------------------------------------- #
Accuracy_function <- function(True_Negative, 
                              False_Positive, 
                              False_Negative,
                              True_Positive){
  
  return((True_Negative + True_Positive)/(True_Negative + False_Positive + False_Negative + True_Positive))
  
}

# ---------------------------------------------------------------------------- #
Positive_Predictive_Value_function <- function(True_Positive,
                                               Predicted_Positive){
  
  return(True_Positive/Predicted_Positive)
  
}

# ---------------------------------------------------------------------------- #
False_Discovery_Rate_function <- function(False_Positive,
                                          Predicted_Positive){
  
  return(False_Positive/Predicted_Positive)
  
}

# ---------------------------------------------------------------------------- #
False_Omission_Rate_function <- function(False_Negative, 
                                         Predicted_Negative){
  
  return(False_Negative/Predicted_Negative)
  
}

# ---------------------------------------------------------------------------- #
Negative_Predictive_Value_function <- function(True_Negative,
                                               Predicted_Negative){
  
  return(True_Negative/Predicted_Negative)
  
}

# ---------------------------------------------------------------------------- #
True_Positive_Rate_function <- function(True_Positive,
                                        Actual_Positive){
  
  return(True_Positive/Actual_Positive)
  
}

# ---------------------------------------------------------------------------- #
False_Negative_Rate_function <- function(False_Negative, 
                                         Actual_Positive){
  
  return(False_Negative/Actual_Positive)
  
}

# ---------------------------------------------------------------------------- #
False_Positive_Rate_function <- function(False_Positive, 
                                         Actual_Negative){
  
  return(False_Positive/Actual_Negative)
  
}

# ---------------------------------------------------------------------------- #
True_Negative_Rate_function <- function(True_Negative, 
                                        Actual_Negative){
  
  return(True_Negative/Actual_Negative)
  
}

# ---------------------------------------------------------------------------- #
Bookmaker_Informedness_function <- function(True_Positive_Rate,
                                            True_Negative_Rate){
  
  return(True_Positive_Rate - True_Negative_Rate - 1)
  
}

# ---------------------------------------------------------------------------- #
Prevelance_Threshold_function <- function(True_Positive_Rate,
                                          False_Positive_Rate){
  
  return((sqrt(True_Positive_Rate + False_Positive_Rate) - False_Positive_Rate)/(True_Positive_Rate - False_Positive_Rate))
  
}

# ---------------------------------------------------------------------------- #
Positive_Likelihood_Ratio_function <- function(True_Positive_Rate, 
                                               False_Positive_Rate){
  
  return(True_Positive_Rate/False_Positive_Rate)
  
}

# ---------------------------------------------------------------------------- #
Negative_Likelihood_Ratio_function <- function(False_Negative_Rate, 
                                               True_Negative_Rate){
  
  return(False_Negative_Rate/True_Negative_Rate)
  
}

# ---------------------------------------------------------------------------- #
Markedness_function <- function(Positive_Predictive_Value,
                                Negative_Predictive_Value){
  
  return(Positive_Predictive_Value - Negative_Predictive_Value - 1)
  
}

# ---------------------------------------------------------------------------- #
Diagnostic_Odds_Ratio_function <- function(Positive_Likelihood_Ratio, 
                                           Negative_Likelihood_Ratio){
  
  return(Positive_Likelihood_Ratio/Negative_Likelihood_Ratio)
  
}

# ---------------------------------------------------------------------------- #
Balanced_Accuracy_function <- function(True_Positive_Rate,
                                       True_Negative_Rate){
  
  return((True_Positive_Rate + True_Negative_Rate)/2)
  
}

# ---------------------------------------------------------------------------- #
F1_Score_function <- function(Positive_Predictive_Value, 
                              True_Positive_Rate){
  
  return((2 * Positive_Predictive_Value * True_Positive_Rate)/(Positive_Predictive_Value + True_Positive_Rate))
  
}

# ---------------------------------------------------------------------------- #
Fowlkes_Mallows_Index_function <- function(Positive_Predictive_Value,
                                           True_Positive_Rate){
  
  return(sqrt(Positive_Predictive_Value * True_Positive_Rate))
  
}

# ---------------------------------------------------------------------------- #
Matthews_Correlation_Coefficient_function <- function(True_Positive_Rate, 
                                                      True_Negative_Rate,
                                                      Positive_Predictive_Value,
                                                      Negative_Predictive_Value, 
                                                      False_Negative_Rate,
                                                      False_Positive_Rate,
                                                      False_Omission_Rate, 
                                                      False_Discovery_Rate){
  
  return(sqrt(True_Positive_Rate * True_Negative_Rate * Positive_Predictive_Value * Negative_Predictive_Value) - sqrt(False_Negative_Rate * False_Positive_Rate * False_Omission_Rate * False_Discovery_Rate))
  
}

# ---------------------------------------------------------------------------- #
Threat_Score_function <- function(True_Positive,
                                  False_Negative,
                                  False_Positive){
  
  return(True_Positive/(True_Positive + False_Negative + False_Positive))
  
}

# ---------------------------------------------------------------------------- #
Classification_Error_function <- function(True_Negative,
                                          False_Positive,
                                          False_Negative,
                                          True_Positive){
  
  return((False_Positive + False_Negative)/(True_Negative + False_Positive + False_Negative + True_Positive))
  
}

# ---------------------------------------------------------------------------- #
calculate_binary_classification_table_metrics <- function(actual_tbl, 
                                                          predicted_tbl,
                                                          cutoff = 0.5,
                                                          round = 4){
  
  join_column <- intersect(colnames(actual_tbl), colnames(predicted_tbl))
  actual_column <- setdiff(colnames(actual_tbl), join_column)
  predicted_column <- setdiff(colnames(predicted_tbl), join_column)
  predicted_class_column <- "Predicted_Class"
  
  tbl <- actual_tbl %>%
    dplyr::left_join(predicted_tbl, by = join_column) %>%
    dplyr::mutate(!!predicted_class_column := ifelse(Predicted >= cutoff, 1, 0))
  
  confussion_matrix_tbl <- tibble::as_tibble(MLmetrics::ConfusionMatrix(y_true = dplyr::pull(tbl, actual_column), y_pred = dplyr::pull(tbl, predicted_class_column))) %>%
    dplyr::mutate(Abbreviation = dplyr::case_when(y_true == 0 & y_pred == 0 ~ "TN",
                                                  y_true == 0 & y_pred == 1 ~ "FP",
                                                  y_true == 1 & y_pred == 0 ~ "FN",
                                                  y_true == 1 & y_pred == 1 ~ "TP"),
                  Fullname = dplyr::case_when(y_true == 0 & y_pred == 0 ~ "True Negative",
                                              y_true == 0 & y_pred == 1 ~ "False Positive/Type I Error",
                                              y_true == 1 & y_pred == 0 ~ "False Negative/Type II Error",
                                              y_true == 1 & y_pred == 1 ~ "True Positive")) %>%
    dplyr::rename(Value = n) %>%
    dplyr::select(Value, Abbreviation, Fullname) 
  
  True_Negative <- confussion_matrix_tbl[confussion_matrix_tbl$Abbreviation == "TN",]$Value
  False_Positive <- confussion_matrix_tbl[confussion_matrix_tbl$Abbreviation == "FP",]$Value
  False_Negative <- confussion_matrix_tbl[confussion_matrix_tbl$Abbreviation == "FN",]$Value
  True_Positive <- confussion_matrix_tbl[confussion_matrix_tbl$Abbreviation == "TP",]$Value
  
  Total_Population <- Total_Population_function(True_Negative = True_Negative,
                                                False_Positive = False_Positive,
                                                False_Negative = False_Negative,
                                                True_Positive = True_Positive)
  
  Actual_Positive <- Actual_Positive_function(True_Positive = True_Positive,
                                              False_Negative = False_Negative)
  
  Actual_Negative <- Actual_Negative_function(False_Positive = False_Positive,
                                              True_Negative = True_Negative)
  
  Predicted_Positive <- Predicted_Positive_function(True_Positive = True_Positive,
                                                    False_Positive = False_Positive)
  
  Predicted_Negative <- Predicted_Negative_function(False_Negative = False_Negative,
                                                    True_Negative = True_Negative)
  
  Prevelance <- Prevelance_function(Actual_Positive = Actual_Positive,
                                    Actual_Negative = Actual_Negative)
  
  Accuracy <- Accuracy_function(True_Negative = True_Negative,
                                False_Positive = False_Positive,
                                False_Negative = False_Negative,
                                True_Positive = True_Positive)
  
  Positive_Predictive_Value <- Positive_Predictive_Value_function(True_Positive = True_Positive,
                                                                  Predicted_Positive = Predicted_Positive)
  
  False_Discovery_Rate <- False_Discovery_Rate_function(False_Positive = False_Positive, 
                                                        Predicted_Positive = Predicted_Positive)
  
  False_Omission_Rate <- False_Omission_Rate_function(False_Negative = False_Negative,
                                                      Predicted_Negative = Predicted_Negative)
  
  Negative_Predictive_Value <- Negative_Predictive_Value_function(True_Negative = True_Negative,
                                                                  Predicted_Negative = Predicted_Negative)
  
  True_Positive_Rate <- True_Positive_Rate_function(True_Positive = True_Positive, 
                                                    Actual_Positive = Actual_Positive)
  
  False_Negative_Rate <- False_Negative_Rate_function(False_Negative = False_Negative,
                                                      Actual_Positive = Actual_Positive)
  
  False_Positive_Rate <- False_Positive_Rate_function(False_Positive = False_Positive,
                                                      Actual_Negative = Actual_Negative)
  
  True_Negative_Rate <- True_Negative_Rate_function(True_Negative = True_Negative,
                                                    Actual_Negative = Actual_Negative)
  
  Bookmaker_Informedness <- Bookmaker_Informedness_function(True_Positive_Rate = True_Positive_Rate,
                                                            True_Negative_Rate = True_Negative_Rate)
  
  Prevelance_Threshold <- Prevelance_Threshold_function(True_Positive_Rate = True_Positive_Rate, 
                                                        False_Positive_Rate = False_Positive_Rate)
  
  Positive_Likelihood_Ratio <- Positive_Likelihood_Ratio_function(True_Positive_Rate = True_Positive_Rate, 
                                                                  False_Positive_Rate = False_Positive_Rate)
  
  Negative_Likelihood_Ratio <- Negative_Likelihood_Ratio_function(False_Negative_Rate = False_Negative_Rate,
                                                                  True_Negative_Rate = True_Negative_Rate)
  
  Markedness <- Markedness_function(Positive_Predictive_Value = Positive_Predictive_Value,
                                    Negative_Predictive_Value = Negative_Predictive_Value)
  
  Diagnostic_Odds_Ratio <- Diagnostic_Odds_Ratio_function(Positive_Likelihood_Ratio = Positive_Likelihood_Ratio,
                                                          Negative_Likelihood_Ratio = Negative_Likelihood_Ratio)
  
  Balanced_Accuracy <- Balanced_Accuracy_function(True_Positive_Rate = True_Positive_Rate, 
                                                  True_Negative_Rate = True_Negative_Rate)
  
  F1_Score <- F1_Score_function(Positive_Predictive_Value = Positive_Predictive_Value, 
                                True_Positive_Rate = True_Positive_Rate)
  
  Fowlkes_Mallows_Index <- Fowlkes_Mallows_Index_function(Positive_Predictive_Value = Positive_Predictive_Value,
                                                          True_Positive_Rate = True_Positive_Rate)
  
  Matthews_Correlation_Coefficient <- Matthews_Correlation_Coefficient_function(True_Positive_Rate = True_Positive_Rate, 
                                                                                True_Negative_Rate = True_Negative_Rate, 
                                                                                Positive_Predictive_Value = Positive_Predictive_Value,
                                                                                Negative_Predictive_Value = Negative_Predictive_Value, 
                                                                                False_Negative_Rate = False_Negative_Rate, 
                                                                                False_Positive_Rate = False_Positive_Rate, 
                                                                                False_Omission_Rate = False_Omission_Rate, 
                                                                                False_Discovery_Rate = False_Discovery_Rate)
  
  Threat_Score <- Threat_Score_function(True_Positive = True_Positive,
                                        False_Negative = False_Negative,
                                        False_Positive = False_Positive)
  
  Classification_Error <- Classification_Error_function(True_Negative = True_Negative,
                                                        False_Positive = False_Positive, 
                                                        False_Negative = False_Negative,
                                                        True_Positive = True_Positive)
  
  binary_classification_table_metrics <- confussion_matrix_tbl %>%
    dplyr::bind_rows(tibble::tibble(Value = Total_Population,
                                    Abbreviation = "TP",
                                    Fullname = "Total Population"),
                     tibble::tibble(Value = Actual_Positive,
                                    Abbreviation = "AP",
                                    Fullname = "Actual Positive"),
                     tibble::tibble(Value = Actual_Negative,
                                    Abbreviation = "AN",
                                    Fullname = "Actual Negative"),
                     tibble::tibble(Value = Predicted_Positive,
                                    Abbreviation = "PP",
                                    Fullname = "Predicted Positive"),
                     tibble::tibble(Value = Predicted_Negative,
                                    Abbreviation = "PN",
                                    Fullname = "Predicted Negative"),
                     tibble::tibble(Value = Prevelance,
                                    Abbreviation = "P",
                                    Fullname = "Prevelance"),
                     tibble::tibble(Value = Accuracy,
                                    Abbreviation = "ACC",
                                    Fullname = "Accuracy"),
                     tibble::tibble(Value = Positive_Predictive_Value,
                                    Abbreviation = "PPV",
                                    Fullname = "Positive Predictive Value/Precision"),
                     tibble::tibble(Value = False_Discovery_Rate,
                                    Abbreviation = "FDR",
                                    Fullname = "False Discovery Rate"),
                     tibble::tibble(Value = False_Omission_Rate,
                                    Abbreviation = "FOR",
                                    Fullname = "False Omission Rate"),
                     tibble::tibble(Value = Negative_Predictive_Value,
                                    Abbreviation = "NPV",
                                    Fullname = "Negative Predictive Value"),
                     tibble::tibble(Value = True_Positive_Rate,
                                    Abbreviation = "TPR",
                                    Fullname = "True Positive Rate/Recall/Sensitivity (SEN)/Probability of Detection"),
                     tibble::tibble(Value = False_Negative_Rate,
                                    Abbreviation = "FNR",
                                    Fullname = "False Negative Rate/Miss Rate"),
                     tibble::tibble(Value = False_Positive_Rate,
                                    Abbreviation = "FPR",
                                    Fullname = "False Positive Rate/Fall-Out/Probability of False Alarm"),
                     tibble::tibble(Value = True_Negative_Rate,
                                    Abbreviation = "TNR",
                                    Fullname = "True Negative Rate/Specifity/Selectivity"),
                     tibble::tibble(Value = Bookmaker_Informedness,
                                    Abbreviation = "BM",
                                    Fullname = "Informedness/Bookmaker Informedness"),
                     tibble::tibble(Value = Prevelance_Threshold,
                                    Abbreviation = "PT",
                                    Fullname = "Prevelance Threshold"),
                     tibble::tibble(Value = Positive_Likelihood_Ratio,
                                    Abbreviation = "LR+",
                                    Fullname = "Positive Likelihood Ratio"),
                     tibble::tibble(Value = Negative_Likelihood_Ratio,
                                    Abbreviation = "LR-",
                                    Fullname = "Negative Likelihood Ratio"),
                     tibble::tibble(Value = Markedness,
                                    Abbreviation = "MK",
                                    Fullname = "Markedness/DeltaP"),
                     tibble::tibble(Value = Diagnostic_Odds_Ratio,
                                    Abbreviation = "DOR",
                                    Fullname = "Diagnostic Odds Ratio"),
                     tibble::tibble(Value = Balanced_Accuracy,
                                    Abbreviation = "BA",
                                    Fullname = "Balanced Accuracy"),
                     tibble::tibble(Value = F1_Score,
                                    Abbreviation = "F1",
                                    Fullname = "F1 Score"),
                     tibble::tibble(Value = Fowlkes_Mallows_Index,
                                    Abbreviation = "FM",
                                    Fullname = "Fowlkes-Mallows Index"),
                     tibble::tibble(Value = Matthews_Correlation_Coefficient,
                                    Abbreviation = "MCC",
                                    Fullname = "Matthews Correlation Coefficient"),
                     tibble::tibble(Value = Threat_Score,
                                    Abbreviation = "TS",
                                    Fullname = "Threat Score/Critical Succes Index (CSI)/Jaccard Index"),
                     tibble::tibble(Value = Classification_Error,
                                    Abbreviation = "CE",
                                    Fullname = "Classification Error/Normalized Zero-One Loss (NZOL)"),
                     tibble::tibble(Value = Metrics::auc(actual = dplyr::pull(tbl, actual_column), predicted = dplyr::pull(tbl, predicted_column)),
                                    Abbreviation = "AUC",
                                    Fullname = "Area Under the ROC Curve"),
                     tibble::tibble(Value = MLmetrics::Gini(y_true = dplyr::pull(tbl, actual_column), y_pred = dplyr::pull(tbl, predicted_column)),
                                    Abbreviation = "GC",
                                    Fullname = "Gini Coefficient"),
                     tibble::tibble(Value = MLmetrics::KS_Stat(y_true = dplyr::pull(tbl, actual_column), y_pred = dplyr::pull(tbl, predicted_column)),
                                    Abbreviation = "KS",
                                    Fullname = "Kolmogorov-Smirnov Statistic"),
                     tibble::tibble(Value = MLmetrics::LiftAUC(y_true = dplyr::pull(tbl, actual_column), y_pred = dplyr::pull(tbl, predicted_column)),
                                    Abbreviation = "LAUC",
                                    Fullname = "Area Under the Lift Chart"),
                     tibble::tibble(Value = MLmetrics::NormalizedGini(y_true = dplyr::pull(tbl, actual_column), y_pred = dplyr::pull(tbl, predicted_class_column)),
                                    Abbreviation = "NG",
                                    Fullname = "Normalized Gini Coefficient"),
                     tibble::tibble(Value = MLmetrics::Poisson_LogLoss(y_true = dplyr::pull(tbl, actual_column), y_pred = dplyr::pull(tbl, predicted_class_column)),
                                    Abbreviation = "PLL",
                                    Fullname = "Poisson Log Loss"),
                     tibble::tibble(Value = MLmetrics::PRAUC(y_true = dplyr::pull(tbl, actual_column), y_pred = dplyr::pull(tbl, predicted_column)),
                                    Abbreviation = "PRAUC",
                                    Fullname = "Area Under the Precision-Recall Curve"),
                     tibble::tibble(Value = MLmetrics::LogLoss(y_true = dplyr::pull(tbl, actual_column), y_pred = dplyr::pull(tbl, predicted_column)),
                                    Abbreviation = "LL/CEL",
                                    Fullname = "Log Loss/Cross-Entropy Loss")) %>%
    dplyr::arrange(Value) %>%
    dplyr::mutate(ID = dplyr::row_number()) %>%
    dplyr::select(ID, Abbreviation, Fullname, Value) %>%
    dplyr::mutate(Cutoff = cutoff)
  
  return(binary_classification_table_metrics)
  
}
