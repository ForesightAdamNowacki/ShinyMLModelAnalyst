# ---------------------------------------------------------------------------- #
data_sampler <- function(data,
                         data_fraction = 0.5,
                         set_seed = TRUE,
                         seed_value = 42){
  
  if(set_seed == TRUE){set.seed(seed = seed_value)}
  data <- dplyr::as_tibble(data) %>% 
    dplyr::sample_frac(size = data_fraction) 
  
  return(data)
}
# Use case:
# data_sampler(data = diamonds,
#              data_fraction = 0.1,
#              set_seed = TRUE,
#              seed_value = 1) %>%
#   count(cut)

# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
factor_count_histogram_plot <- function(data,
                                        factor_var,
                                        title = "",
                                        caption = "",
                                        factor_axis = "",
                                        count_axis = "",
                                        repel_label = FALSE,
                                        expansion_multiplier = 0.05,
                                        label_padding = 0.25,
                                        label_size = 4,
                                        title_size = 10,
                                        title_horizontal_position = 0.5,
                                        title_vertical_position = 0.5,
                                        text_size = 4){
  
  # Variables:
  factor_var <- rlang::sym(factor_var)
  factor_var <- dplyr::enquo(factor_var)                    
  factor_var_name <- dplyr::quo_name(factor_var)
  
  # Names:
  if(count_axis == ""){count_axis <- "COUNT"}
  if(factor_axis == ""){factor_axis <- stringr::str_to_upper(factor_var_name)}
  if(title == ""){title <- "QUANTITY BAR PLOT"}
  if(caption == ""){caption <- "SOURCE: UNKNOWN"}
  
  # Data summary:
  data_summary <- data %>%
    dplyr::select(!!factor_var) %>%
    dplyr::group_by(!!factor_var) %>%
    dplyr::summarise(count = dplyr::n(),
                     .groups = "drop") %>%
    dplyr::mutate(mean_count = sum(count)/nrow(.))
  
  # Plot:
  plot <- data_summary %>%
    ggplot2::ggplot(data = .,
                    mapping = ggplot2::aes(x = !!factor_var,
                                           y = count, 
                                           fill = !!factor_var, 
                                           label = count)) +
    ggplot2::geom_hline(yintercept = data_summary$mean_count) +
    ggplot2::geom_bar(stat = "identity",
                      position = "identity",
                      color = "black") +
    ggplot2::labs(x = factor_axis,
                  y = count_axis,
                  caption = caption,
                  title = title) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = expansion_multiplier)) +
    ggplot2::theme(legend.position = "none",
                   plot.title = ggplot2::element_text(size = title_size, color = "black", face = "bold", hjust = title_horizontal_position, vjust = title_vertical_position),
                   axis.text.x = ggplot2::element_text(size = text_size, color = "black", face = "plain"),
                   axis.title.x = ggplot2::element_text(size = text_size, color = "black", face = "bold"),
                   axis.text.y = ggplot2::element_text(size = text_size, color = "black", face = "plain"),
                   axis.title.y = ggplot2::element_text(size = text_size, color = "black", face = "bold"),
                   panel.grid.major.x = ggplot2::element_line(color = "black", linetype = "dotted"),
                   panel.grid.minor.x = ggplot2::element_line(linetype = "blank"),
                   panel.grid.major.y = ggplot2::element_line(color = "black", linetype = "dotted"),
                   panel.grid.minor.y = ggplot2::element_line(linetype = "blank"),
                   axis.ticks = ggplot2::element_line(size = 1, color = "black", linetype = "dotted"),
                   axis.ticks.length = ggplot2::unit(0.1, "cm"),
                   plot.background = ggplot2::element_rect(fill = "gray80", color = "black", linetype = "solid", size = 1.5),
                   panel.background = ggplot2::element_rect(fill = "gray90", color = "black", linetype = "solid"),
                   plot.caption = ggplot2::element_text(size = text_size, color = "black", face = "bold", hjust = 1)) +
    ggplot2::scale_fill_manual(values = RColorBrewer::brewer.pal(nrow(data_summary), "Greys"))
  
  if(repel_label == TRUE){
    plot <- plot +
      ggrepel::geom_label_repel(color = "black", 
                                fill = "white",
                                fontface = 1,
                                size = label_size, 
                                label.padding = ggplot2::unit(label_padding, "lines"),
                                label.r = ggplot2::unit(0, "lines"))
  } else {
    plot <- plot +
      ggplot2::geom_label(color = "black", 
                          fill = "white",
                          fontface = 1,
                          size = label_size, 
                          label.padding = ggplot2::unit(label_padding, "lines"),
                          label.r = ggplot2::unit(0, "lines"))
  }
  
  return(plot)
}

# Use case:
# factor_count_histogram_plot(data = diamonds,
#                             factor_var = "color",
#                             factor_axis = "")
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
select_box_input <- function(vector){
  
  vector <- sort(unique(vector))
  return_ <- list()
  for(i in seq_along(vector)){
    return_[[i]] <- vector[i]
    names(return_)[i] <- vector[i]}
  
  return(return_)
}
# Use case:
# select_box_input(vector <- c("A", "B", "C"))
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
factor_percentage_histogram_plot <- function(data,
                                             factor_var,
                                             title = "",
                                             caption = "",
                                             factor_axis = "",
                                             percentage_axis = "",
                                             repel_label = FALSE,
                                             expansion_multiplier = 0.05,
                                             label_padding = 0.25,
                                             label_size = 4,
                                             title_size = 10,
                                             title_horizontal_position = 0.5,
                                             title_vertical_position = 0.5,
                                             text_size = 4,
                                             percentage_breaks = 11){
  
  # Variables:
  factor_var <- rlang::sym(factor_var)
  factor_var <- dplyr::enquo(factor_var)                    
  factor_var_name <- dplyr::quo_name(factor_var)
  
  # Names:
  if(percentage_axis == ""){percentage_axis <- "PERCENTAGE"}
  if(factor_axis == ""){factor_axis <- stringr::str_to_upper(factor_var_name)}
  if(title == ""){title <- "PERCENTAGE BAR PLOT"}
  if(caption == ""){caption <- "SOURCE: UNKNOWN"}
  
  # Data summary:
  data_summary <- data %>%
    dplyr::select(!!factor_var) %>%
    dplyr::group_by(!!factor_var) %>%
    dplyr::summarise(percentage = dplyr::n()/nrow(.),
                     .groups = "drop") %>%
    dplyr::mutate(mean_percentage = 1/nrow(.))
  
  # Plot:
  plot <- data_summary %>%
    ggplot2::ggplot(data = .,
                    mapping = ggplot2::aes(x = !!factor_var,
                                           y = percentage, 
                                           fill = !!factor_var, 
                                           label = paste0(round(100 * percentage, 1), "%"))) +
    ggplot2::geom_hline(yintercept = data_summary$mean_percentage) +
    ggplot2::geom_bar(stat = "identity",
                      position = "identity",
                      color = "black") +
    ggplot2::labs(x = factor_axis,
                  y = percentage_axis,
                  caption = caption,
                  title = title) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = expansion_multiplier),
                                labels = scales::percent_format(accuracy = 1), 
                                breaks = base::seq(from = 0, to = 1, length.out = percentage_breaks)) +
    ggplot2::theme(legend.position = "none",
                   plot.title = ggplot2::element_text(size = title_size, color = "black", face = "bold", hjust = title_horizontal_position, vjust = title_vertical_position),
                   axis.text.x = ggplot2::element_text(size = text_size, color = "black", face = "plain"),
                   axis.title.x = ggplot2::element_text(size = text_size, color = "black", face = "bold"),
                   axis.text.y = ggplot2::element_text(size = text_size, color = "black", face = "plain"),
                   axis.title.y = ggplot2::element_text(size = text_size, color = "black", face = "bold"),
                   panel.grid.major.x = ggplot2::element_line(color = "black", linetype = "dotted"),
                   panel.grid.minor.x = ggplot2::element_line(linetype = "blank"),
                   panel.grid.major.y = ggplot2::element_line(color = "black", linetype = "dotted"),
                   panel.grid.minor.y = ggplot2::element_line(linetype = "blank"),
                   axis.ticks = ggplot2::element_line(size = 1, color = "black", linetype = "dotted"),
                   axis.ticks.length = ggplot2::unit(0.1, "cm"),
                   plot.background = ggplot2::element_rect(fill = "gray80", color = "black", linetype = "solid", size = 1.5),
                   panel.background = ggplot2::element_rect(fill = "gray90", color = "black", linetype = "solid"),
                   plot.caption = ggplot2::element_text(size = text_size, color = "black", face = "bold", hjust = 1)) +
    ggplot2::scale_fill_manual(values = RColorBrewer::brewer.pal(nrow(data_summary), "Greys"))
  
  if(repel_label == TRUE){
    plot <- plot +
      ggrepel::geom_label_repel(color = "black", 
                                fill = "white",
                                fontface = 1,
                                size = label_size, 
                                label.padding = ggplot2::unit(label_padding, "lines"),
                                label.r = ggplot2::unit(0, "lines"))
  } else {
    plot <- plot +
      ggplot2::geom_label(color = "black", 
                          fill = "white",
                          fontface = 1,
                          size = label_size, 
                          label.padding = ggplot2::unit(label_padding, "lines"),
                          label.r = ggplot2::unit(0, "lines"))
  }
  
  return(plot)
}

# Use case:
# factor_percentage_histogram_plot(data = diamonds,
#                                  factor_var = "color")
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
factor_vs_factor_count_plot <- function(data,
                                        factor_var_1,
                                        factor_var_2,
                                        factor_axis_1 = "",
                                        factor_axis_2 = "",
                                        repel_label = FALSE,
                                        title = "",
                                        caption = "",
                                        label_padding = 0.25,
                                        label_size = 4,
                                        title_size = 10,
                                        title_horizontal_position = 0.5,
                                        title_vertical_position = 0.5,
                                        text_size = 4){
  
  # Variables:
  factor_var_1 <- rlang::sym(factor_var_1)
  factor_var_2 <- rlang::sym(factor_var_2)
  factor_var_1 <- dplyr::enquo(factor_var_1)
  factor_var_2  <- dplyr::enquo(factor_var_2)
  factor_var_1_name <- dplyr::quo_name(factor_var_1)
  factor_var_2_name <- dplyr::quo_name(factor_var_2)
  
  # Names:
  if(factor_axis_1 == ""){factor_axis_1 <- stringr::str_to_upper(factor_var_1_name)}
  if(factor_axis_2 == ""){factor_axis_2 <- stringr::str_to_upper(factor_var_2_name)}
  if(title == ""){title <- "QUANTITY TILE PLOT"}
  if(caption == ""){caption <- "SOURCE: UNKNOWN"}
  
  # Summary:
  data_summary <- data %>%
    dplyr::group_by(!!factor_var_1, !!factor_var_2) %>%
    dplyr::count() %>%
    dplyr::ungroup() %>%
    tidyr::complete(!!factor_var_1, !!factor_var_2, fill = list(n = 0))
  
  # Plot:
  plot <- data_summary %>%
    ggplot2::ggplot(data = .,
                    mapping = ggplot2::aes(x = !!factor_var_1,
                                           y = !!factor_var_2,
                                           fill = n,
                                           label = n)) +
    ggplot2::geom_tile(color = "black") +
    ggplot2::scale_fill_gradientn(colours = c("white", "black"), values = c(0, 1)) +
    ggplot2::labs(x = factor_axis_1,
                  y = factor_axis_2,
                  caption = caption,
                  title = title) +
    ggplot2::theme(legend.position = "none",
                   plot.title = ggplot2::element_text(size = title_size, color = "black", face = "bold", hjust = title_horizontal_position, vjust = title_vertical_position),
                   axis.text.x = ggplot2::element_text(size = text_size, color = "black", face = "plain"),
                   axis.title.x = ggplot2::element_text(size = text_size, color = "black", face = "bold"),
                   axis.text.y = ggplot2::element_text(size = text_size, color = "black", face = "plain"),
                   axis.title.y = ggplot2::element_text(size = text_size, color = "black", face = "bold"),
                   panel.grid.major.x = ggplot2::element_line(linetype = "blank"),
                   panel.grid.minor.x = ggplot2::element_line(linetype = "blank"),
                   panel.grid.major.y = ggplot2::element_line(linetype = "blank"),
                   panel.grid.minor.y = ggplot2::element_line(linetype = "blank"),
                   axis.ticks = ggplot2::element_line(size = 1, color = "black", linetype = "dotted"),
                   axis.ticks.length = ggplot2::unit(0.1, "cm"),
                   plot.background = ggplot2::element_rect(fill = "gray80", color = "black", linetype = "solid", size = 1.5),
                   panel.background = ggplot2::element_rect(fill = "gray90", color = "black", linetype = "solid"),
                   plot.caption = ggplot2::element_text(size = text_size, color = "black", face = "bold", hjust = 1))
  
  if(repel_label == TRUE){
    plot <- plot +
      ggrepel::geom_label_repel(color = "black", 
                                fill = "white",
                                fontface = 1,
                                size = label_size, 
                                label.padding = ggplot2::unit(label_padding, "lines"),
                                label.r = ggplot2::unit(0, "lines"))
  } else {
    plot <- plot +
      ggplot2::geom_label(color = "black", 
                          fill = "white",
                          fontface = 1,
                          size = label_size, 
                          label.padding = ggplot2::unit(label_padding, "lines"),
                          label.r = ggplot2::unit(0, "lines"))
  }
  
  return(plot)
}

# Use case:
# factor_vs_factor_count_plot(data = diamonds,
#                             factor_var_1 = "cut",
#                             factor_var_2 = "color")
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
factor_vs_factor_percentage_plot <- function(data,
                                             factor_var_1,
                                             factor_var_2,
                                             factor_axis_1 = "",
                                             factor_axis_2 = "",
                                             repel_label = FALSE,
                                             title = "",
                                             caption = "",
                                             label_padding = 0.25,
                                             label_size = 4,
                                             title_size = 10,
                                             title_horizontal_position = 0.5,
                                             title_vertical_position = 0.5,
                                             text_size = 4){
  
  # Variables:
  factor_var_1 <- rlang::sym(factor_var_1)
  factor_var_2 <- rlang::sym(factor_var_2)
  factor_var_1 <- dplyr::enquo(factor_var_1)
  factor_var_2  <- dplyr::enquo(factor_var_2)
  factor_var_1_name <- dplyr::quo_name(factor_var_1)
  factor_var_2_name <- dplyr::quo_name(factor_var_2)
  
  # Names:
  if(factor_axis_1 == ""){factor_axis_1 <- stringr::str_to_upper(factor_var_1_name)}
  if(factor_axis_2 == ""){factor_axis_2 <- stringr::str_to_upper(factor_var_2_name)}
  if(title == ""){title <- "PERCENTAGE TILE PLOT"}
  if(caption == ""){caption <- "SOURCE: UNKNOWN"}
  
  # Summary:
  data_summary <- data %>%
    dplyr::group_by(!!factor_var_1, !!factor_var_2) %>%
    dplyr::count() %>%
    dplyr::ungroup() %>%
    tidyr::complete(!!factor_var_1, !!factor_var_2, fill = list(n = 0)) %>%
    dplyr::mutate(percentage = 100 * n/sum(n))
  
  # Plot:
  plot <- data_summary %>%
    ggplot2::ggplot(data = .,
                    mapping = ggplot2::aes(x = !!factor_var_1,
                                           y = !!factor_var_2,
                                           fill = percentage,
                                           label = paste0(round(percentage, 1), "%"))) +
    ggplot2::geom_tile(color = "black") +
    ggplot2::scale_fill_gradientn(colours = c("white", "black"), values = c(0, 1)) +
    ggplot2::labs(x = factor_axis_1,
                  y = factor_axis_2,
                  caption = caption,
                  title = title) +
    ggplot2::theme(legend.position = "none",
                   plot.title = ggplot2::element_text(size = title_size, color = "black", face = "bold", hjust = title_horizontal_position, vjust = title_vertical_position),
                   axis.text.x = ggplot2::element_text(size = text_size, color = "black", face = "plain"),
                   axis.title.x = ggplot2::element_text(size = text_size, color = "black", face = "bold"),
                   axis.text.y = ggplot2::element_text(size = text_size, color = "black", face = "plain"),
                   axis.title.y = ggplot2::element_text(size = text_size, color = "black", face = "bold"),
                   panel.grid.major.x = ggplot2::element_line(linetype = "blank"),
                   panel.grid.minor.x = ggplot2::element_line(linetype = "blank"),
                   panel.grid.major.y = ggplot2::element_line(linetype = "blank"),
                   panel.grid.minor.y = ggplot2::element_line(linetype = "blank"),
                   axis.ticks = ggplot2::element_line(size = 1, color = "black", linetype = "dotted"),
                   axis.ticks.length = ggplot2::unit(0.1, "cm"),
                   plot.background = ggplot2::element_rect(fill = "gray80", color = "black", linetype = "solid", size = 1.5),
                   panel.background = ggplot2::element_rect(fill = "gray90", color = "black", linetype = "solid"),
                   plot.caption = ggplot2::element_text(size = text_size, color = "black", face = "bold", hjust = 1))
  
  if(repel_label == TRUE){
    plot <- plot +
      ggrepel::geom_label_repel(color = "black", 
                                fill = "white",
                                fontface = 1,
                                size = label_size, 
                                label.padding = ggplot2::unit(label_padding, "lines"),
                                label.r = ggplot2::unit(0, "lines"))
  } else {
    plot <- plot +
      ggplot2::geom_label(color = "black", 
                          fill = "white",
                          fontface = 1,
                          size = label_size, 
                          label.padding = ggplot2::unit(label_padding, "lines"),
                          label.r = ggplot2::unit(0, "lines"))
  }
  
  return(plot)
}

# Use case:
# factor_vs_factor_percentage_plot(data = diamonds,
#                                  factor_var_1 = "cut",
#                                  factor_var_2 = "color")
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
factor_vs_factor_percentage_group_tile_plot <- function(data,
                                                        factor_var_1,
                                                        factor_var_2,
                                                        factor_axis_1 = "",
                                                        factor_axis_2 = "",
                                                        repel_label = FALSE,
                                                        title = "",
                                                        caption = "",
                                                        label_padding = 0.25,
                                                        label_size = 4,
                                                        title_size = 10,
                                                        title_horizontal_position = 0.5,
                                                        title_vertical_position = 0.5,
                                                        text_size = 4){
  
  # Variables:
  factor_var_1 <- rlang::sym(factor_var_1)
  factor_var_2 <- rlang::sym(factor_var_2)
  factor_var_1 <- dplyr::enquo(factor_var_1)
  factor_var_2  <- dplyr::enquo(factor_var_2)
  factor_var_1_name <- dplyr::quo_name(factor_var_1)
  factor_var_2_name <- dplyr::quo_name(factor_var_2)
  
  # Names:
  if(factor_axis_1 == ""){factor_axis_1 <- stringr::str_to_upper(factor_var_1_name)}
  if(factor_axis_2 == ""){factor_axis_2 <- stringr::str_to_upper(factor_var_2_name)}
  if(title == ""){title <- "GROUP PERCENTAGE TILE PLOT"}
  if(caption == ""){caption <- "SOURCE: UNKNOWN"}
  
  # Summary:
  data_summary <- data %>%
    dplyr::group_by(!!factor_var_1, !!factor_var_2) %>%
    dplyr::count() %>%
    dplyr::ungroup() %>%
    tidyr::complete(!!factor_var_1, !!factor_var_2, fill = list(n = 0)) %>%
    dplyr::group_by(!!factor_var_1) %>%
    dplyr::mutate(group_n = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(scaled_percentage = 100 * n/group_n)
  
  # Plot:
  plot <- data_summary %>%
    ggplot2::ggplot(data = .,
                    mapping = ggplot2::aes(x = !!factor_var_1,
                                           y = !!factor_var_2,
                                           fill = scaled_percentage,
                                           label = paste0(round(scaled_percentage, 1), "%"))) +
    ggplot2::geom_tile(color = "black") +
    ggplot2::scale_fill_gradientn(colours = c("white", "black"), values = c(0, 1)) +
    ggplot2::labs(x = factor_axis_1,
                  y = factor_axis_2,
                  caption = caption,
                  title = title) +
    ggplot2::theme(legend.position = "none",
                   plot.title = ggplot2::element_text(size = title_size, color = "black", face = "bold", hjust = title_horizontal_position, vjust = title_vertical_position),
                   axis.text.x = ggplot2::element_text(size = text_size, color = "black", face = "plain"),
                   axis.title.x = ggplot2::element_text(size = text_size, color = "black", face = "bold"),
                   axis.text.y = ggplot2::element_text(size = text_size, color = "black", face = "plain"),
                   axis.title.y = ggplot2::element_text(size = text_size, color = "black", face = "bold"),
                   panel.grid.major.x = ggplot2::element_line(linetype = "blank"),
                   panel.grid.minor.x = ggplot2::element_line(linetype = "blank"),
                   panel.grid.major.y = ggplot2::element_line(linetype = "blank"),
                   panel.grid.minor.y = ggplot2::element_line(linetype = "blank"),
                   axis.ticks = ggplot2::element_line(size = 1, color = "black", linetype = "dotted"),
                   axis.ticks.length = ggplot2::unit(0.1, "cm"),
                   plot.background = ggplot2::element_rect(fill = "gray80", color = "black", linetype = "solid", size = 1.5),
                   panel.background = ggplot2::element_rect(fill = "gray90", color = "black", linetype = "solid"),
                   plot.caption = ggplot2::element_text(size = text_size, color = "black", face = "bold", hjust = 1))
  
  if(repel_label == TRUE){
    plot <- plot +
      ggrepel::geom_label_repel(color = "black", 
                                fill = "white",
                                fontface = 1,
                                size = label_size, 
                                label.padding = ggplot2::unit(label_padding, "lines"),
                                label.r = ggplot2::unit(0, "lines"))
  } else {
    plot <- plot +
      ggplot2::geom_label(color = "black", 
                          fill = "white",
                          fontface = 1,
                          size = label_size, 
                          label.padding = ggplot2::unit(label_padding, "lines"),
                          label.r = ggplot2::unit(0, "lines"))
  }
  
  return(plot)
}

# Use case:
# factor_vs_factor_percentage_scaled_plot(data = diamonds,
#                                         factor_var_1 = "color",
#                                         factor_var_2 = "cut")
# ---------------------------------------------------------------------------- #



# ---------------------------------------------------------------------------- #
factor_vs_factor_percentage_group_bar_plot <- function(data,
                                                       factor_var_1,
                                                       factor_var_2,
                                                       factor_axis_1 = "",
                                                       factor_axis_2 = "",
                                                       title = "",
                                                       caption = "",
                                                       label_padding = 0.25,
                                                       label_size = 4,
                                                       title_size = 10,
                                                       title_horizontal_position = 0.5,
                                                       title_vertical_position = 0.5,
                                                       text_size = 4,
                                                       percentage_breaks = 11,
                                                       display_legend = TRUE,
                                                       legend_position = "right",
                                                       legend_direction = "vertical"){
  
  # Variables:
  factor_var_1 <- rlang::sym(factor_var_1)
  factor_var_2 <- rlang::sym(factor_var_2)
  factor_var_1 <- dplyr::enquo(factor_var_1)
  factor_var_2  <- dplyr::enquo(factor_var_2)
  factor_var_1_name <- dplyr::quo_name(factor_var_1)
  factor_var_2_name <- dplyr::quo_name(factor_var_2)
  
  # Names:
  if(factor_axis_1 == ""){factor_axis_1 <- stringr::str_to_upper(factor_var_1_name)}
  if(factor_axis_2 == ""){factor_axis_2 <- stringr::str_to_upper(factor_var_2_name)}
  if(title == ""){title <- "GROUP PERCENTAGE BAR PLOT"}
  if(caption == ""){caption <- "SOURCE: UNKNOWN"}
  
  # Group levels:
  factor_levels <- data %>%
    dplyr::select(!!factor_var_2) %>%
    dplyr::distinct() %>%
    dplyr::pull() %>%
    length(.)
  
  # Plot:
  plot <- data %>%
    ggplot2::ggplot(data = .,
                    mapping = aes(x = !!factor_var_1,
                                  fill = !!factor_var_2)) +
    ggplot2::geom_bar(position = "fill",
                      color = "black") +
    ggplot2::scale_fill_manual(values = RColorBrewer::brewer.pal(factor_levels, "Greys")) +
    ggplot2::labs(x = factor_axis_1,
                  y = factor_axis_2,
                  fill = factor_axis_2,
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
                   plot.caption = ggplot2::element_text(size = text_size, color = "black", face = "bold", hjust = 1)) +
    ggplot2::scale_y_continuous(limits = base::c(0, 1), 
                                labels = scales::percent_format(accuracy = 1), 
                                breaks = base::seq(from = 0, to = 1, length.out = percentage_breaks))
  
  if(display_legend == TRUE){
    plot <- plot +
      ggplot2::theme(legend.position = legend_position,
                     legend.direction = legend_direction,
                     legend.text = ggplot2::element_text(size = text_size, color = "black", face = "plain"),
                     legend.title = ggplot2::element_text(size = text_size, color = "black", face = "bold"),
                     legend.key = ggplot2::element_rect(colour = "gray90"),
                     legend.box.background = ggplot2::element_rect(color = "black", linetype = "solid"),
                     legend.background = ggplot2::element_rect(fill = "gray90", linetype = "solid", color = "black"),
                     legend.box.spacing = ggplot2::unit(0.25, "cm"))
  } else {
    plot <- plot +
      ggplot2::theme(legend.position = "none")
  }
  
  return(plot)
}

# Use case:
factor_vs_factor_percentage_group_bar_plot(data = diamonds,
                                           factor_var_1 = "cut",
                                           factor_var_2 = "color")


# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
factor_circle_plot <- function(data,
                               factor_var,
                               title = "",
                               caption = "",
                               repel_label = FALSE,
                               label_padding = 0.25,
                               label_size = 4,
                               title_size = 10,
                               title_horizontal_position = 0.5,
                               title_vertical_position = 0.5,
                               text_size = 4){
  
  # Variables:
  factor_var <- rlang::sym(factor_var)
  factor_var <- dplyr::enquo(factor_var)                    
  factor_var_name <- dplyr::quo_name(factor_var)
  
  # Names:
  if(title == ""){title <- "CIRCLE PLOT"}
  if(caption == ""){caption <- "SOURCE: UNKNOWN"}
  
  # Summary:
  data_summary <- data %>%
    dplyr::group_by(!!factor_var) %>%
    dplyr::count() %>%
    dplyr::ungroup() %>%
    dplyr::mutate(percentage = 100 * n/sum(n))
  
  # Plot
  packing <- packcircles::circleProgressiveLayout(data_summary$n, sizetype = 'area')
  data_packing <- cbind(data_summary, packing)
  ggplot_circle_data <- packcircles::circleLayoutVertices(packing, npoints = 10000)
  
  plot <- ggplot2::ggplot() +
    ggplot2::geom_polygon(data = ggplot_circle_data,
                          mapping = ggplot2::aes(x = x, 
                                                 y = y,
                                                 group = id,
                                                 fill = as.factor(id)), 
                          color = "black") +
    ggplot2::scale_fill_manual(values = RColorBrewer::brewer.pal(nrow(data_summary), "Greys")) +
    ggplot2::labs(caption = caption,
                  title = title) +
    ggplot2::theme(legend.position = "none",
                   plot.title = ggplot2::element_text(size = title_size, color = "black", face = "bold", hjust = title_horizontal_position, vjust = title_vertical_position),
                   axis.text.x = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_line(linetype = "blank"),
                   panel.grid.minor.x = ggplot2::element_line(linetype = "blank"),
                   panel.grid.major.y = ggplot2::element_line(linetype = "blank"),
                   panel.grid.minor.y = ggplot2::element_line(linetype = "blank"),
                   axis.ticks = ggplot2::element_blank(),
                   plot.background = ggplot2::element_rect(fill = "gray80", color = "black", linetype = "solid", size = 1.5),
                   panel.background = ggplot2::element_rect(fill = "gray90", color = "black", linetype = "solid"),
                   plot.caption = ggplot2::element_text(size = text_size, color = "black", face = "bold", hjust = 1))
  
  if(repel_label == TRUE){
    plot <- plot +
      ggrepel::geom_label_repel(data = data_packing,
                                mapping = ggplot2::aes(x = x,
                                                       y = y, 
                                                       label = !!factor_var),
                                color = "black", 
                                fill = "white",
                                fontface = 1,
                                size = label_size, 
                                label.padding = ggplot2::unit(label_padding, "lines"),
                                label.r = ggplot2::unit(0, "lines"))
  } else {
    plot <- plot +
      ggplot2::geom_label(data = data_packing,
                          mapping = ggplot2::aes(x = x,
                                                 y = y, 
                                                 label = !!factor_var),
                          color = "black", 
                          fill = "white",
                          fontface = 1,
                          size = label_size, 
                          label.padding = ggplot2::unit(label_padding, "lines"),
                          label.r = ggplot2::unit(0, "lines"))
  }
  
  return(plot)
}

# Use case:
# factor_vs_factor_percentage_group_bar_plot(data = diamonds,
#                                            factor_var = "color")  
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
factor_waffle_plot <- function(data,
                               factor_var,
                               factor_axis = "",
                               grid_size = 10,
                               title = "",
                               caption = "",
                               title_size = 10,
                               title_horizontal_position = 0.5,
                               title_vertical_position = 0.5,
                               text_size = 4,
                               display_legend = TRUE,
                               legend_position = "right",
                               legend_direction = "horizontal"){
  
  # Variables:
  factor_var <- rlang::sym(factor_var)
  factor_var <- dplyr::enquo(factor_var)                    
  factor_var_name <- dplyr::quo_name(factor_var)
  
  # Names:
  if(factor_axis == ""){factor_axis <- stringr::str_to_upper(factor_var_name)}
  if(title == ""){title <- "WAFFLE PLOT"}
  if(caption == ""){caption <- "SOURCE: UNKNOWN"}
  
  # Dimension check:
  selected_factor_var <- data %>%
    dplyr::select(!!factor_var)
  unrounded_squares <- table(selected_factor_var) * (grid_size^2/nrow(selected_factor_var))
  rounded_squares <- round(unrounded_squares)
  factor_levels <- length(rounded_squares)
  
  if(sum(rounded_squares) == grid_size^2){
    rounded_squares <- rounded_squares
  }
  
  if(sum(rounded_squares) < grid_size^2){
    
    diff <- grid_size^2 - sum(rounded_squares)
    names <- tibble::tibble(names = names(unrounded_squares),
                            values = as.vector(unrounded_squares),
                            values_ceiling = ceiling(values),
                            ceiling_diff = values_ceiling - values) %>%
      dplyr::arrange(ceiling_diff) %>%
      dplyr::slice(1:diff) %>%
      dplyr::pull(names)
    
    for(i in seq_along(names)){
      id <- which(names(rounded_squares) == names[i])
      rounded_squares[id] <- rounded_squares[id] + 1
    }
  }
  
  if(sum(rounded_squares) > grid_size^2){
    
    diff <- sum(rounded_squares) - grid_size^2
    values <- tibble::tibble(names = names(unrounded_squares),
                             values = as.vector(unrounded_squares),
                             values_half = (floor(values) + ceiling(values))/2,
                             values_half_diff = values - values_half)
    names <- dplyr::bind_rows(values %>%
                                dplyr::filter(values_half_diff > 0) %>%
                                dplyr::arrange(values_half_diff),
                              values %>%
                                dplyr::filter(values_half_diff < 0) %>%
                                dplyr::arrange(values_half_diff)) %>%
      dplyr::slice(1:diff) %>%
      dplyr::pull(names)
    
    for(i in seq_along(names)){
      id <- which(names(rounded_squares) == names[i])
      rounded_squares[id] <- rounded_squares[id] - 1
    }
  }
  
  # Plot
  plot <- tidyr::expand_grid(y = 1:grid_size, x = 1:grid_size) %>%
    dplyr::mutate(factor_category = rep(names(rounded_squares), times = as.vector(rounded_squares)),
                  factor_category = factor(factor_category, levels = names(rounded_squares), ordered = TRUE)) %>%
    ggplot2::ggplot(data = ., aes(x = y, y = x, fill = factor_category)) +
    ggplot2::geom_tile(color = "black") +
    ggplot2::scale_x_continuous(expand = base::c(0, 0)) +
    ggplot2::scale_y_continuous(expand = base::c(0, 0)) +
    ggplot2::labs(fill = factor_axis,
                  caption = caption,
                  title = title) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = title_size, color = "black", face = "bold", hjust = title_horizontal_position, vjust = title_vertical_position),
                   axis.text.x = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_line(linetype = "blank"),
                   panel.grid.minor.x = ggplot2::element_line(linetype = "blank"),
                   panel.grid.major.y = ggplot2::element_line(linetype = "blank"),
                   panel.grid.minor.y = ggplot2::element_line(linetype = "blank"),
                   axis.ticks = ggplot2::element_blank(),
                   plot.background = ggplot2::element_rect(fill = "gray80", color = "black", linetype = "solid", size = 1.5),
                   panel.background = ggplot2::element_rect(fill = "gray90", color = "black", linetype = "solid"),
                   plot.caption = ggplot2::element_text(size = text_size, color = "black", face = "bold", hjust = 1)) +
    ggplot2::scale_fill_manual(values = RColorBrewer::brewer.pal(factor_levels, "Greys"))
  
  if(display_legend == TRUE){
    plot <- plot +
      ggplot2::theme(legend.position = legend_position,
                     legend.direction = legend_direction,
                     legend.text = ggplot2::element_text(size = text_size, color = "black", face = "plain"),
                     legend.title = ggplot2::element_text(size = text_size, color = "black", face = "bold"),
                     legend.key = ggplot2::element_rect(colour = "gray90"),
                     legend.box.background = ggplot2::element_rect(color = "black", linetype = "solid"),
                     legend.background = ggplot2::element_rect(fill = "gray90", linetype = "solid", color = "black"),
                     legend.box.spacing = ggplot2::unit(0.25, "cm"))
  } else {
    plot <- plot +
      ggplot2::theme(legend.position = "none")
  }
  
  return(plot)
}

# Use case:
# factor_waffle_plot(data = diamonds,
#                    factor_var = "clarity")
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
factor_vs_factor_parallel_plot <- function(data,
                                           factor_var_1,
                                           factor_var_2,
                                           title = "",
                                           caption = "",
                                           repel_label = FALSE,
                                           label_size = 4,
                                           text_size = 10,
                                           title_size = 10,
                                           title_horizontal_position = 0.5,
                                           title_vertical_position = 0.5,
                                           label_padding = 0.25){
  
  # Variables:
  factor_var_1 <- rlang::sym(factor_var_1)
  factor_var_2 <- rlang::sym(factor_var_2)
  factor_var_1 <- dplyr::enquo(factor_var_1)
  factor_var_2  <- dplyr::enquo(factor_var_2)
  factor_var_1_name <- dplyr::quo_name(factor_var_1)
  factor_var_2_name <- dplyr::quo_name(factor_var_2)
  
  # Names:
  if(title == ""){title <- "PARALLEL PLOT"}
  if(caption == ""){caption <- "SOURCE: UNKNOWN"}
  
  # Summary data
  data_summary <- data %>%
    dplyr::group_by(!!factor_var_1, !!factor_var_2) %>%
    dplyr::count() %>%
    dplyr::ungroup() %>%
    tidyr::complete(!!factor_var_1, !!factor_var_2, fill = list(n = 0)) %>%
    as.data.frame()
  
  # Factor levels:
  levels_factor_var_1 <- data_summary %>%
    dplyr::select(!!factor_var_1) %>%
    dplyr::distinct() %>%
    dplyr::pull() %>%
    as.vector()
  levels_factor_var_2 <- data_summary %>%
    dplyr::select(!!factor_var_2) %>%
    dplyr::distinct() %>%
    dplyr::pull() %>%
    as.vector()
  
  # Plot
  plot_1 <- ggparallel::ggparallel(vars = list(c(factor_var_1_name, factor_var_2_name)),
                                   data = data_summary,
                                   weight = "n",
                                   method = "parset",
                                   text.angle = 0, 
                                   label = FALSE, 
                                   order = 0) +
    ggplot2::scale_fill_manual(values = rep("gray60", times = length(levels_factor_var_1) + length(levels_factor_var_2))) +
    ggplot2::scale_colour_manual(values = rep("black", times = length(levels_factor_var_1) + length(levels_factor_var_2)))
  
  plot_1_data <- ggplot2::ggplot_build(plot_1)$data[[2]] %>%
    tibble::as_tibble(.) %>%
    dplyr::select(ymin, ymax, xmin, xmax) %>%
    dplyr::mutate(x = (xmin + xmax)/2,
                  y = (ymin + ymax)/2,
                  label = c(rev(levels_factor_var_1), rev(levels_factor_var_2)))
  
  plot <- plot_1 +
    ggplot2::labs(title = title,
                  caption = caption) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = title_size, color = "black", face = "bold", hjust = title_horizontal_position, vjust = title_vertical_position),
                   axis.text.x = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_line(linetype = "blank"),
                   panel.grid.minor.x = ggplot2::element_line(linetype = "blank"),
                   panel.grid.major.y = ggplot2::element_line(linetype = "blank"),
                   panel.grid.minor.y = ggplot2::element_line(linetype = "blank"),
                   axis.ticks = ggplot2::element_blank(),
                   plot.background = ggplot2::element_rect(fill = "gray80", color = "black", linetype = "solid", size = 1.5),
                   panel.background = ggplot2::element_rect(fill = "gray90", color = "black", linetype = "solid"),
                   plot.caption = ggplot2::element_text(size = text_size, color = "black", face = "bold", hjust = 1),
                   legend.position = "none")
  
  if(repel_label == TRUE){
    plot <- plot +
      ggrepel::geom_label_repel(data = plot_1_data,
                                mapping = ggplot2::aes(x = x,
                                                       y = y, label = label),
                                color = "black", 
                                fill = "white",
                                fontface = 1,
                                size = label_size, 
                                label.padding = ggplot2::unit(label_padding, "lines"),
                                label.r = ggplot2::unit(0, "lines"))
  } else {
    plot <- plot +
      ggplot2::geom_label(data = plot_1_data,
                          mapping = ggplot2::aes(x = x,
                                                 y = y, label = label),
                          color = "black", 
                          fill = "white",
                          fontface = 1,
                          size = label_size, 
                          label.padding = ggplot2::unit(label_padding, "lines"),
                          label.r = ggplot2::unit(0, "lines"))
  }
  
  return(plot)
}

# Use case:
# factor_vs_factor_parallel_plot(data = diamonds,
#                                factor_var_1 = "clarity",
#                                factor_var_2 = "cut")
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
 


