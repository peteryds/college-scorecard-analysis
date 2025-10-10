# R/visualizations.R

# A shared color palette for consistency
GROUP_PALETTE <- c(
  "Ivy League" = "#E4572E",
  "Florida Public" = "#17BEBB",
  "New College of Florida" = "#7F95D1",
  "Private Nonprofit (National)" = "#76B041",
  "Public (National)" = "#F1C40F",
  "Other" = "#7D7D7D"
)

#' Create a box plot of SAT scores by group
#' @param df The analysis-ready data frame.
#' @return A ggplot object.
plot_sat_boxplot <- function(df) {
  sat_df <- df %>% dplyr::filter(!is.na(sat_avg), !is.na(group_label))
  sat_order <- sat_df %>%
    dplyr::group_by(group_label) %>%
    dplyr::summarize(med_sat = median(sat_avg, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(desc(med_sat)) %>%
    dplyr::pull(group_label)
  
  ggplot2::ggplot(
    sat_df %>% dplyr::mutate(group_label = forcats::fct_relevel(group_label, sat_order)),
    ggplot2::aes(x = group_label, y = sat_avg, fill = group_label)
  ) +
    ggplot2::geom_boxplot(outlier.shape = NA, alpha = 0.85) +
    ggplot2::geom_jitter(width = 0.15, alpha = 0.22, size = 1.2) +
    ggplot2::scale_fill_manual(values = GROUP_PALETTE, guide = "none") +
    ggplot2::labs(
      title = "Distribution of SAT Scores by Comparison Group",
      x = NULL, y = "Average SAT Score"
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 16, hjust = 1))
}

#' Create a bar chart of median net price by group
#' @param df The analysis-ready data frame.
#' @return A ggplot object.
plot_net_price_barchart <- function(df) {
  price_df <- df %>%
    dplyr::filter(!is.na(net_price), !is.na(group_label)) %>%
    dplyr::group_by(group_label) %>%
    dplyr::summarize(median_net_price = median(net_price, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(group_label = forcats::fct_reorder(group_label, median_net_price))
  
  ggplot2::ggplot(price_df, ggplot2::aes(x = group_label, y = median_net_price, fill = group_label)) +
    ggplot2::geom_col(alpha = 0.9) +
    ggplot2::geom_text(ggplot2::aes(label = scales::dollar(median_net_price)),
                       vjust = -0.4, size = 3.7, fontface = "bold") +
    ggplot2::scale_fill_manual(values = GROUP_PALETTE, guide = "none") +
    ggplot2::scale_y_continuous(labels = scales::dollar_format(), expand = ggplot2::expansion(mult = c(0.05, 0.12))) +
    ggplot2::labs(
      title = "Median Net Price by Comparison Group",
      x = NULL, y = "Median Net Price (Cost after aid)"
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 16, hjust = 1))
}

#' Create a bar chart of mean graduation rates by group
#' @param df The analysis-ready data frame.
#' @return A ggplot object.
plot_grad_rate_barchart <- function(df) {
  grad_bar_df <- df %>%
    dplyr::filter(!is.na(grad_rate), !is.na(group_label)) %>%
    dplyr::group_by(group_label) %>%
    dplyr::summarize(mean_gr = mean(grad_rate, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(group_label = forcats::fct_reorder(group_label, mean_gr, .desc = TRUE))
  
  ggplot2::ggplot(grad_bar_df, ggplot2::aes(x = group_label, y = mean_gr, fill = group_label)) +
    ggplot2::geom_col(alpha = 0.9) +
    ggplot2::geom_text(ggplot2::aes(label = scales::percent(mean_gr, accuracy = 0.1)),
                       vjust = -0.4, size = 3.7, fontface = "bold") +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                                expand = ggplot2::expansion(mult = c(0.05, 0.12))) +
    ggplot2::scale_fill_manual(values = GROUP_PALETTE, guide = "none") +
    ggplot2::labs(
      title = "Mean Graduation Rate by Comparison Group",
      x = NULL, y = "Mean Graduation Rate"
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 16, hjust = 1))
}

#' Create a scatter plot of earnings vs. net price
#' @param df The analysis-ready data frame.
#' @return A ggplot object.
plot_earnings_scatterplot <- function(df) {
  earn_df <- df %>%
    dplyr::filter(!is.na(earn10), !is.na(net_price), !is.na(group_label), !is.na(grad_rate))
  
  highlight_df <- earn_df %>% dplyr::filter(is_ivy | is_ncf)
  
  ggplot2::ggplot(earn_df, ggplot2::aes(x = net_price, y = earn10, color = group_label, size = grad_rate)) +
    ggplot2::geom_point(alpha = 0.65) +
    ggrepel::geom_text_repel(data = highlight_df, ggplot2::aes(label = name),
                             size = 3.2, max.overlaps = 50, show.legend = FALSE) +
    ggplot2::scale_color_manual(values = GROUP_PALETTE) +
    ggplot2::scale_size_continuous(range = c(2, 8), labels = scales::percent) +
    ggplot2::scale_x_continuous(labels = scales::dollar) +
    ggplot2::scale_y_continuous(labels = scales::dollar) +
    ggplot2::labs(
      title = "Post-Graduation Earnings vs. Net Price",
      subtitle = "Point size is scaled by graduation rate",
      x = "Median Net Price", y = "Median Earnings 10-Yrs After Entry",
      color = "Group", size = "Graduation Rate"
    ) +
    ggplot2::theme_minimal(base_size = 13)
}