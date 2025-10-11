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

# State locations of TOP 25 colleges 
# Self-contained code to plot where the Top-25 colleges are (by 10-yr earnings)
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(scales)
})

# Ensure map data is available
if (!requireNamespace("maps", quietly = TRUE)) install.packages("maps")
library(maps)  # ggplot2::map_data("state") relies on this

# ---- 1) Pick the ranking metric and take Top 25 ----
ranking_metric <- "earn10"

stopifnot(ranking_metric %in% names(df_final))
top25 <- df_final %>%
  filter(!is.na(.data[[ranking_metric]])) %>%
  slice_max(order_by = .data[[ranking_metric]], n = 25, with_ties = FALSE)

# ---- 2) Normalize state names (map US postal → full state name) ----
# Determine which column holds the 2-letter state code
state_col <- if ("state" %in% names(top25)) "state" else if ("school_state" %in% names(top25)) "school_state" else "school.state"
if (!state_col %in% names(top25)) stop("Could not find a state column ('state', 'school_state', or 'school.state').")

state_xwalk <- tibble::tibble(
  abbr = state.abb,
  full = tolower(state.name)
) %>%
  # Optionally include DC if present in your data (map_data('state') does not include DC)
  bind_rows(tibble::tibble(abbr = "DC", full = "district of columbia"))

state_counts <- top25 %>%
  mutate(state_abbr = toupper(.data[[state_col]])) %>%
  left_join(state_xwalk, by = c("state_abbr" = "abbr")) %>%
  # Drop rows that didn't match to a state polygon (e.g., PR/DC not in map_data('state'))
  filter(!is.na(full)) %>%
  count(full, name = "num_top25")

# ---- 3) Build choropleth dataset ----
us_polys <- ggplot2::map_data("state") %>% as_tibble()  # columns: long, lat, group, order, region, subregion
plot_df <- us_polys %>%
  left_join(state_counts, by = c("region" = "full"))

# ---- 4) Plot choropleth: where the Top-25 are located ----
ggplot(plot_df, aes(long, lat, group = group, fill = num_top25)) +
  geom_polygon(color = "white", linewidth = 0.3) +
  coord_fixed(1.3) +
  scale_fill_gradient(
    low = "lightblue", high = "darkblue",
    na.value = "gray95",
    name = "Top-25 Colleges",
    labels = label_number(accuracy = 1)
  ) +
  labs(
    title = "Where the Top 25 Colleges Are Located",
    subtitle = "Count of Top-25 colleges by state (ranked by median 10-year earnings)",
    x = NULL, y = NULL
  ) +
  theme_void(base_size = 12) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )
