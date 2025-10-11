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
#' 
plot_earnings_scatterplot <- function(df) {
  library(dplyr)
  library(stringr)
  library(ggplot2)
  library(ggrepel)
  library(grid)
  
  earn_df <- df %>%
    filter(!is.na(earn10), !is.na(net_price), !is.na(group_label), !is.na(grad_rate))
  
  # Always label NCF
  ncf_df <- earn_df %>%
    filter(grepl("new college of florida", name, ignore.case = TRUE)) %>%
    slice_head(n = 1)
  
  # Categorize for top-3 selection
  pool <- earn_df %>%
    mutate(
      .cat = case_when(
        str_detect(str_to_lower(group_label), "florida") ~ "Florida Public",
        str_detect(str_to_lower(group_label), "private") ~ "Private",
        str_detect(str_to_lower(group_label), "public")  ~ "Public",
        TRUE ~ NA_character_
      )
    )
  
  # Top-3 per category (Public, Private, Florida Public)
  top3_main <- pool %>%
    filter(.cat %in% c("Public", "Private", "Florida Public")) %>%
    group_by(.cat) %>%
    slice_max(order_by = earn10, n = 3, with_ties = FALSE) %>%
    ungroup() %>%
    select(-.cat)
  
  # ✅ Correct Ivy selection (vectorized & robust)
  ivy_rows <- earn_df %>%
    filter(
      # prefer boolean flag if present
      (is.logical(is_ivy) & !is.na(is_ivy) & is_ivy) |
        # fallback: group label contains "ivy"
        str_detect(str_to_lower(group_label), "ivy")
    )
  
  top3_ivy <- ivy_rows %>%
    slice_max(order_by = earn10, n = 3, with_ties = FALSE)
  
  # Labels to draw = top3 per bucket + top3 Ivy (EXCLUDING NCF to avoid duplicate)
  label_df_main <- bind_rows(top3_main, top3_ivy) %>%
    filter(!grepl("new college of florida", name, ignore.case = TRUE)) %>%
    distinct(name, .keep_all = TRUE)
  
  ggplot(
    earn_df,
    aes(x = net_price, y = earn10, color = group_label, size = grad_rate)
  ) +
    geom_point(alpha = 0.65) +
    
    # Labels for selected schools (group-colored fill @ 50% alpha, black text)
    ggrepel::geom_label_repel(
      data = label_df_main,
      aes(label = name, fill = group_label),
      color = "black",
      label.size = 0.25,
      label.r = unit(0.1, "lines"),
      label.padding = unit(0.15, "lines"),
      size = 3.2,
      max.overlaps = 200,
      show.legend = FALSE
    ) +
    
    # Distinct NCF point + bold label (same group-colored fill @ 50% alpha)
    geom_point(
      data = ncf_df,
      shape = 21, stroke = 0.8, fill = "white", size = 3, inherit.aes = FALSE,
      aes(x = net_price, y = earn10, color = group_label)
    ) +
    ggrepel::geom_label_repel(
      data = ncf_df,
      aes(label = name, fill = group_label),
      color = "black",
      fontface = "bold",
      label.size = 0.3,
      label.r = unit(0.12, "lines"),
      label.padding = unit(0.18, "lines"),
      max.overlaps = Inf,
      min.segment.length = 0,
      size = 3.6,
      show.legend = FALSE
    ) +
    
    # color = points; fill = labels (same palette, but labels are alpha=0.5)
    scale_color_manual(values = GROUP_PALETTE) +
    scale_fill_manual(values = scales::alpha(GROUP_PALETTE, 0.5)) +
    
    scale_size_continuous(range = c(0.2, 0.8), labels = scales::percent) +
    scale_x_continuous(labels = scales::dollar) +
    scale_y_continuous(labels = scales::dollar) +
    labs(
      title = "Post-Graduation Earnings vs. Net Price",
      subtitle = "Top-3 labeled per category (Public, Private, Florida Public, Ivy) + New College of Florida",
      x = "Median Net Price", y = "Median Earnings 10-Yrs After Entry",
      color = "Group", size = "Graduation Rate"
    ) +
    coord_cartesian(clip = "off") +
    theme_minimal(base_size = 13) +
    theme(plot.margin = margin(10, 20, 10, 10))
}

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(scales)
  library(usmap)
})

# --- Helper: coerce state to 2-letter abbreviations (accepts full names or abbr) ---
to_abbr <- function(x) {
  x <- trimws(x)
  out <- toupper(x)
  full_lower <- tolower(x)
  mi <- match(full_lower, tolower(state.name))
  out[!is.na(mi)] <- state.abb[mi[!is.na(mi)]]
  out[grepl("^washington\\s*d\\.?c\\.?$|^district of columbia$", full_lower)] <- "DC"
  out
}

# --- Ensure the input exists and metric is present ---
if (!exists("df_final")) stop("df_final not found in environment.")
if (!"earnings_to_net" %in% names(df_final)) stop("df_final$earnings_to_net not found.")

# --- Rank by earnings_to_net and take Top 50 ---
top50 <- df_final %>%
  filter(!is.na(earnings_to_net)) %>%
  slice_max(order_by = earnings_to_net, n = 50, with_ties = FALSE)

# Graceful no-data fallback
if (nrow(top50) == 0) {
  plot.new(); text(0.5, 0.5, "No rows with earnings_to_net found.", cex = 1.2)
} else {
  # Find the state column
  state_col <- dplyr::case_when(
    "state"        %in% names(top50) ~ "state",
    "school_state" %in% names(top50) ~ "school_state",
    "school.state" %in% names(top50) ~ "school.state",
    TRUE ~ NA_character_
  )
  if (is.na(state_col)) stop("No state column found (expected one of: state, school_state, school.state).")
  
  # Count Top-50 per state; usmap needs a column literally named 'state' or 'fips'
  state_counts <- top50 %>%
    mutate(state = to_abbr(.data[[state_col]])) %>%
    filter(!is.na(state), state %in% c(state.abb, "DC")) %>%
    count(state, name = "num_top50")
  
  # If nothing matched valid states, show a friendly note
  if (nrow(state_counts) == 0) {
    plot.new(); text(0.5, 0.5, "No Top-50 rows mapped to valid US states.", cex = 1.2)
  } else {
    # Build and PRINT the plot (ensures it renders in R Markdown)
    p_map <- plot_usmap(data = state_counts, values = "num_top50", color = "white") +
      scale_fill_continuous(
        low = "lightblue", high = "darkblue",
        na.value = "gray95",
        name = "Top-50 (earnings/net)", labels = comma
      ) +
      labs(
        title = "Where the Top 50 Colleges Are (by Earnings-to-Net-Price)",
        subtitle = "Count of top-50 colleges by state, ranked using df_final$earnings_to_net",
        x = NULL, y = NULL
      ) +
      theme(legend.position = "right")
    
    print(p_map)  # <-- explicit print so it shows up in outputs
  }
}
