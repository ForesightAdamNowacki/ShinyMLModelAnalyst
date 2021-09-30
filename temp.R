shiny::fluidRow(
  shiny::column(
    width = 6,
    shiny::wellPanel(
      shiny::h4(shiny::strong("Ground truth dataset:")),
      shiny::fileInput(inputId = "Train_Ground_Truth_Dataset_1",
                       label = shiny::h5("Train dataset:")),
      shiny::fileInput(inputId = "Validation_Ground_Truth_Dataset_1", 
                       label = shiny::h5("Validation dataset:")),
      shiny::fileInput(inputId = "Test_Ground_Truth_Dataset_1", 
                       label = shiny::h5("Test dataset:")))),
  shiny::column(
    width = 6,
    shiny::wellPanel(
      shiny::h4(shiny::strong("Prediction dataset:")),
      shiny::fileInput(inputId = "Train_Prediction_Dataset_1", 
                       label = shiny::h5("Train dataset:")),
      shiny::fileInput(inputId = "Validation_Prediction_Dataset_1",
                       label = shiny::h5("Validation dataset:")),
      shiny::fileInput(inputId = "Test_Prediction_Dataset_1", 
                       label = shiny::h5("Test dataset:"))))),


readr::write_csv2(diamonds, "Diamonds.csv")


actual <- round(runif(n = 100, min = 0, max = 1))
predicted <- runif(n = 100, min = 0, max = 1)
cutoff = 0.5
FN_cost = 1
FP_cost = 1
TN_cost = 0
TP_cost = 0
type_info <- "train"


Binary_Classifier_Verification <- function(actual,
                                           predicted,
                                           type_info = "",
                                           cutoff = 0.5,
                                           FN_cost = 1,
                                           FP_cost = 1,
                                           TN_cost = 0,
                                           TP_cost = 0,
                                           save = FALSE,
                                           open = TRUE){
  
  sys_time <- Sys.time()
  
  # Confusion matrix explanation:
  result_1 <- tibble::tibble("Confusion Matrix" = c("Actual Negative (0)", "Actual Positive (1)"),
                             "Predicted Negative (0)" = c("True Negative (TN)", "False Negative (FN)"),
                             "Predicted Positive (1)" = c("False Positive (FP)", "True Positive (TP)"))
  
  probability <- predicted
  if(length(unique(predicted)) > 2){predicted <- ifelse(predicted < cutoff, 0, 1)}
  predicted <- factor(predicted, levels = c(0, 1), labels = c(0, 1))
  
  # Confusion matrix result:
  confusion_matrix <- table(actual, predicted)
  result_2 <- tibble::tibble("Confusion Matrix" = c("Actual Negative (0)", "Actual Positive (1)"),
                             "Predicted Negative (0)" = c(confusion_matrix[1, 1], confusion_matrix[2, 1]),
                             "Predicted Positive (1)" = c(confusion_matrix[1, 2], confusion_matrix[2, 2])) 
  
  # Assessment of classifier effectiveness:
  OBS <- sum(confusion_matrix); OBS_label <- "= TN + FP + FN + TP"
  TN <- confusion_matrix[1, 1]; TN_label <- "= TN"
  FP <- confusion_matrix[1, 2]; FP_label <- "= FP"
  FN <- confusion_matrix[2, 1]; FN_label <- "= FN"
  TP <- confusion_matrix[2, 2]; TP_label <- "= TP"
  P <- FN + TP; P_label <- "= FN + TP"
  N <- TN + FP; N_label <- "= TN + FP"
  
  # Accuracy (ACC):
  ACC <- (TN + TP)/(TN + FN + FP + TP)
  ACC_label <- "= (TN + TP)/(TN + FN + FP + TP) = (TN + TP)/(P + N)"
  
  # Balanced Accuracy (BACC):
  BACC <- (TN/(TN + FP) + TP/(FN + TP))/2
  BACC_label <- "= (TN/(TN + FP) + TP/(FN + TP))/2"
  
  # Area Under Curve (AUC):
  AUC <- Metrics::auc(actual = actual, predicted = probability)
  AUC_label <- "= Area Under ROC Curve"
  
  # Bias:
  BIAS <- mean(as.numeric(actual)) - mean(as.numeric(predicted))
  BIAS_label <- "= mean(actual) - mean(predicted)"
  
  # Classification Error (CE):
  CE <- (FN + FP)/(TN + FN + FP + TP)
  CE_label <- "= (FN + FP)/(TN + FN + FP + TP) = 1 - (TN + TP)/(TN + FN + FP + TP)"
  
  # Recall, Sensitivity, hit rate, True Positive Rate (TPR):
  TPR <- TP/(TP + FN)
  TPR_label <- "= TP/(TP + FN) = TP/P = 1 - FNR"
  
  # Specifity, selectivity, True Negative Rate (TNR):
  TNR <- TN/(TN + FP)
  TNR_label <- "= TN/(TN + FP) = TN/N = 1 - FPR"
  
  # Precision, Positive Prediction Value (PPV):
  PPV <- TP/(TP + FP)
  PPV_label <- "= TP/(TP + FP) = 1 - FDR"
  
  # Negative Predictive Value (NPV):
  NPV <- TN/(TN + FN)
  NPV_label <- "= TN/(TN + FN) = 1 - FOR"
  
  # False Negative Rate (FNR), miss rate:
  FNR <- FN/(FN + TP)
  FNR_label <- "= FN/(FN + TP) = FN/P = 1 - TPR"
  
  # False Positive Rate (FPR), fall-out:
  FPR <- FP/(FP + TN)
  FPR_label <- "= FP/(FP + TN) = FP/N = 1 - TNR"
  
  # False Discovery Rate (FDR):
  FDR <- FP/(FP + TP)
  FDR_label <- "= FP/(FP + TP) = 1 - PPV"
  
  # False Omission Rate (FOR):
  FOR <- FN/(FN + TN)
  FOR_label <- "= FN/(FN + TN) = 1 - NPV"
  
  # Threat Score (TS), Critical Success Index (CSI):
  TS <- TP/(TP + FN + FP)
  TS_label <- "= TP/(TP + FN + FP)"
  
  # F1:
  F1 <- (2 * PPV * TPR)/(PPV + TPR)
  F1_label <- "= (2 * PPV * TPR)/(PPV + TPR) = 2 * TP/(2 * TP + FP + FN)"
  
  # Informedness, Bookmaker Informedness (BM):
  BM <- TPR + TNR - 1
  BM_label <- "= TPR + TNR - 1"
  
  # Markedness (MK):
  MK <- PPV + NPV - 1
  MK_label <- "= PPV + NPV - 1"
  
  # Gini Index:
  GINI <- 2 * AUC - 1
  GINI_label <- "= 2 * AUC - 1"
  
  # Cost:
  COST <- FN * FN_cost + FP * FP_cost + TN * TN_cost + TP * TP_cost
  COST_label <- "= FN * FN_cost + FP * FP_cost + TN * TN_cost + TP * TP_cost"
  
  result_3 <- tibble::tibble(Metric = c("Number of Observations", "True Negative", "False Positive", "False Negative", "True Positive",
                                        "Condition Negative", "Condition Positive", "Accuracy", "Balanced Accuracy", "Area Under ROC Curve",
                                        "Bias", "Classification Error", "True Positive Rate", "True Negative Rate",
                                        "Positive Prediction Value", "Negative Predictive Value", "False Negative Rate", "False Positive Rate",
                                        "False Discovery Rate", "False Omission Rate", "Threat Score", "F1 Score",
                                        "Bookmaker Informedness", "Markedness", "Gini Index", "Cost"),
                             `Metric Abbreviation` = c("RECORDS", "TN", "FP", "FN", "TP",
                                                       "N", "P", "ACC", "BACC", "AUC",
                                                       "BIAS", "CE", "TPR", "TNR", 
                                                       "PPV", "NPV", "FNR", "FPR",
                                                       "FDR", "FOR", "TS", "F1",
                                                       "BM", "MK", "GINI", "COST"),
                             `Metric Name` = c("-", "-", "Type I Error", "Type II Error", "-",
                                               "-", "-", "-", "-", "-",
                                               "-", "-", "Sensitivity, Recall, Hit Rate", "Specifity, Selectivity",
                                               "Precision", "-", "Miss Rate", "Fall-Out",
                                               "-", "-", "Critical Success Index", "-",
                                               "-", "-", "-", "-"),
                             Score = round(c(OBS, TN, FP, FN, TP,
                                             N, P, ACC, BACC, AUC,
                                             BIAS, CE, TPR, TNR,
                                             PPV, NPV, FNR, FPR,
                                             FDR, FOR, TS, F1,
                                             BM, MK, GINI, COST), digits = 6),
                             Calculation = c(OBS_label, TN_label, FP_label, FN_label, TP_label,
                                             N_label, P_label, ACC_label, BACC_label, AUC_label,
                                             BIAS_label, CE_label, TPR_label, TNR_label,
                                             PPV_label, NPV_label, FNR_label, FPR_label,
                                             FDR_label, FOR_label, TS_label, F1_label,
                                             BM_label, MK_label, GINI_label, COST_label),
                             ID = c(1:7, 1:19)) %>%
    dplyr::select(ID, Metric, `Metric Abbreviation`, `Metric Name`, Score, Calculation)
  
  gt_table <- result_3 %>%
    dplyr::mutate(Group = ifelse(Metric %in% c("Number of Observations", "True Negative",
                                               "False Positive", "False Negative",
                                               "True Positive", "Condition Positive",
                                               "Condition Negative"), 
                                 "Confusion Matrix Result", "Assessment of Classifier Effectiveness")) %>%
    gt::gt(rowname_col = "ID", groupname_col = "Group") %>%
    gt::tab_header(title = gt::md(paste("Model's evaluation metrics", sys_time)),
                   subtitle = gt::md("Binary classification model")) %>%
    gt::tab_source_note(gt::md(paste0("**Options**: ",
                                      "**cutoff** = ", cutoff,
                                      ", **TN_cost** = ", TN_cost,
                                      ", **FP_cost** = ", FP_cost,
                                      ", **FN_cost** = ", FN_cost,
                                      ", **TP_cost** = ", TP_cost))) %>%
    gt::tab_source_note(gt::md("More information available at: **https://github.com/ForesightAdamNowacki**.")) %>%
    gt::tab_spanner(label = "Metrics section",
                    columns = dplyr::vars(Metric, `Metric Abbreviation`, `Metric Name`)) %>%
    gt::tab_spanner(label = "Performance section",
                    columns = dplyr::vars(Score, Calculation)) %>%
    gt::fmt_number(columns = dplyr::vars(Score),
                   decimals = 4,
                   use_seps = FALSE) %>%
    gt::cols_align(align = "left", columns = dplyr::vars(Metric, `Metric Abbreviation`, `Metric Name`, Calculation)) %>%
    gt::tab_options(heading.background.color = "black",
                    table.background.color = "grey",
                    column_labels.background.color = "black",
                    row_group.background.color = "black",
                    source_notes.background.color = "black",
                    table.border.top.color = "black",
                    table.border.top.width = gt::px(3),
                    table.border.bottom.color = "black",
                    table.border.bottom.width = gt::px(3),
                    heading.title.font.size = 16,
                    table.font.size = 12,
                    source_notes.font.size = 10,
                    table.width = gt::pct(100),
                    data_row.padding = gt::px(5),
                    row_group.padding = gt::px(10),
                    source_notes.padding = gt::px(5)) %>% 
    gt::opt_table_outline(width = gt::px(3), color = "black") %>%
    gt::opt_table_lines()
  
  gt::gtsave(data = gt_table,
             filename = paste(type_info, "binary_model_evaluation_metrics.png", sep = "_"))
  
  invisible(list("Confusion_Matrix_Explanation" = result_1,
                 "Confusion_Matrix_Result" = result_2,
                 "Assessment_of_Classifier_Effectiveness" = result_3))}



# https://gt.rstudio.com/reference/gt_output.html



# Axises and labels:
factor_axis_1 = NULL # name of 1st factor variable
factor_axis_2 = NULL # name of 2nd factor variable
count_axis = "COUNT" # name of count axis
percentage_axis = "PERCENTAGE" # name of percentage axis
density_axis = "DENSITY" # name of density axis
caption = NULL # caption
title_size = 9 # font size for title
text_size = 7 # font size for axises, labels, caption etc.
grid_size = 50 # graph grid density in waffle charts (recommended that the number be a multiple of 10)
label_size = 3 # font size for labels
label_percent_round = 1 # number of decimals in percent labels
percentage_breaks = 11 # number of breaks on percentage axis


library(ggplot2)

# daæ parametr switch - ¿eby przestawic miedzy soba zmienne na osiach

data <- diamonds

# parameters
#data # data frame or tibble (obligatory parameter)
#factor_var <- "color" # 1st factor variable (obligatory parameter)
#title <- "QUANTITY BAR PLOT"
#caption <- "ABC"
#set_seed <- TRUE
#seed_value <- 42
#data_fraction <- 0.5
# factor_levels <- c("D", "E", "F", "G")
label_fontface <- 2
label_size <- 4
label_padding <- 0.25
title_size <- 10
title_horizontal_position <- 0.5
title_vertical_position <- 0.5
text_size <- 8
