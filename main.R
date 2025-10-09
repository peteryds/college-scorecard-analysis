# ------------------------------------------------------------
# College Scorecard Research Project — R Pipeline
# (no plot; adds Ivy League, Florida public, New College of Florida,
#  Private Nonprofit (National), and Public (National) groups)
# ------------------------------------------------------------

library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(factoextra)
library(ggplot2)
library(tidyverse)
library(conflicted)
library(stringr)

# =======================
# 1. API 
# =======================
api_key <- ""   # <- put your api.data.gov key here

# Prefer dplyr variants of common conflicted functions
suppressMessages({
  conflict_prefer("filter", "dplyr")
  conflict_prefer("lag", "dplyr")
  conflict_prefer("rename", "dplyr")
})

# Fields per College Scorecard data dictionary
fields <- c(
  "id", "school.name", "school.state", "school.city",
  "school.ownership",                               # <-- needed for public/private-nonprofit groups
  "latest.admissions.sat_scores.average.overall",
  "latest.cost.tuition.in_state",
  "latest.cost.tuition.out_of_state",
  "latest.cost.avg_net_price.overall",              # aggregate net price (correct field)
  "latest.completion.rate_suppressed.overall",
  "latest.earnings.10_yrs_after_entry.median",
  "latest.aid.median_debt.completers.overall",
  "latest.academics.program_percentage.computer",
  "latest.academics.program_percentage.engineering",
  "latest.academics.program_percentage.mathematics"
)

fields_str <- paste(fields, collapse = ",")

# =======================
# 2. Collect the data
# =======================
get_all_schools <- function(api_key, fields_str, per_page = 100) {
  base_url <- "https://api.data.gov/ed/collegescorecard/v1/schools.json"
  page <- 0
  results <- list()
  
  repeat {
    page <- page + 1
    url <- paste0(
      base_url,
      "?api_key=", api_key,
      "&fields=", fields_str,
      "&per_page=", per_page,
      "&page=", page
    )
    
    res <- GET(url)
    stop_for_status(res)
    data_json <- content(res, as = "text", encoding = "UTF-8")
    data_list <- fromJSON(data_json)
    
    if (length(data_list$results) == 0) break
    results[[page]] <- data_list$results
  }
  
  bind_rows(results)
}

df_raw <- get_all_schools(api_key, fields_str, per_page = 100)

# =======================
# 3. Data Cleaning
# =======================
colnames(df_raw)

df <- dplyr::rename(df_raw,
                    name = school.name,
                    state = school.state,
                    city = school.city,
                    ownership = school.ownership,
                    sat_avg = latest.admissions.sat_scores.average.overall,
                    tuition_in = latest.cost.tuition.in_state,
                    tuition_out = latest.cost.tuition.out_of_state,
                    grad_rate = latest.completion.rate_suppressed.overall,
                    earn10 = latest.earnings.10_yrs_after_entry.median,
                    debt_mdn = latest.aid.median_debt.completers.overall,
                    pcip_cs = latest.academics.program_percentage.computer,
                    pcip_eng = latest.academics.program_percentage.engineering,
                    pcip_math = latest.academics.program_percentage.mathematics,
                    net_price_overall = latest.cost.avg_net_price.overall
) %>%
  mutate(across(c(
    ownership, sat_avg, tuition_in, tuition_out, grad_rate, earn10, debt_mdn,
    pcip_cs, pcip_eng, pcip_math, net_price_overall
  ), ~ suppressWarnings(as.numeric(.x)))) %>%
  mutate(
    # Use overall net price (fallback to in-state tuition)
    net_price_overall = coalesce(net_price_overall, tuition_in),
    # Keep your original names for compatibility
    net_price_low    = net_price_overall,
    net_price_middle = net_price_overall,
    net_price_upper  = net_price_overall,
    net_price        = net_price_middle,
    stem_share       = rowSums(across(c(pcip_cs, pcip_eng, pcip_math), ~ coalesce(.x, 0)), na.rm = TRUE),
    earnings_to_net  = earn10 / net_price,
    earnings_adj_grad = (earn10 * grad_rate) / net_price
  )

# =======================
# 4. Comparison Groups
# =======================

# Ivy League (canonical Scorecard names + light pattern backup)
ivy_names <- c(
  "Brown University",
  "Columbia University in the City of New York",
  "Cornell University",
  "Dartmouth College",
  "Harvard University",
  "University of Pennsylvania",
  "Princeton University",
  "Yale University"
)
ivy_patterns <- c(
  "brown university",
  "columbia university( in the city of new york)?",
  "cornell university",
  "dartmouth college",
  "harvard university",
  "university of pennsylvania",
  "princeton university",
  "yale university"
)

# Florida public universities (State University System of Florida)
fl_public_patterns <- c(
  "university of florida$",
  "florida state university$",
  "university of south florida",         # allow campus suffixes
  "university of central florida$",
  "florida international university$",
  "florida atlantic university$",
  "florida gulf coast university$",
  "university of north florida$",
  "florida agricultural and mechanical university$",  # FAMU
  "florida polytechnic university$",
  "new college of florida$",              # also singled out below
  "university of west florida$"
)

# Ownership codes per data dictionary:
# 1 = Public, 2 = Private nonprofit, 3 = Private for-profit
df <- df %>%
  mutate(
    name_norm = str_squish(str_to_lower(name)),
    is_ivy = name %in% ivy_names |
      str_detect(name_norm, str_c("^(", str_c(ivy_patterns, collapse = "|"), ")$")),
    is_fl_public = str_detect(name_norm, str_c("(", str_c(fl_public_patterns, collapse = "|"), ")")),
    is_ncf = name_norm == "new college of florida",
    is_public_nat = ownership == 1,
    is_private_np_nat = ownership == 2,
    # Mutually-exclusive label for easy group summaries:
    # Priority: NCF > Ivy > Florida Public > Private Nonprofit (National) > Public (National) > Other
    group_label = case_when(
      is_ncf ~ "New College of Florida",
      is_ivy ~ "Ivy League",
      is_fl_public ~ "Florida Public",
      is_private_np_nat ~ "Private Nonprofit (National)",
      is_public_nat ~ "Public (National)",
      TRUE ~ "Other"
    )
  )

# Quick sanity checks (printed to console)
ivies_found <- df %>% filter(is_ivy) %>% distinct(name) %>% arrange(name)
message("Ivies found (", nrow(ivies_found), "):\n - ", paste(ivies_found$name, collapse = "\n - "))

fl_public_found <- df %>% filter(is_fl_public) %>% distinct(name) %>% arrange(name)
message("Florida public universities found (", nrow(fl_public_found), "):\n - ",
        paste(fl_public_found$name, collapse = "\n - "))

ncf_found <- df %>% filter(is_ncf) %>% distinct(name)
message("New College of Florida present? ", ifelse(nrow(ncf_found) > 0, "Yes", "No"))

priv_np_found <- df %>% filter(is_private_np_nat) %>% distinct(name) %>% arrange(name)
message("Private Nonprofit (National) count: ", nrow(priv_np_found))

public_nat_found <- df %>% filter(is_public_nat) %>% distinct(name) %>% arrange(name)
message("Public (National) count: ", nrow(public_nat_found))

# =======================
# 5. Descriptive Analysis
# =======================
summary_tbl <- df %>%
  select(name, state, net_price, earn10, grad_rate, sat_avg,
         stem_share, debt_mdn, earnings_to_net, earnings_adj_grad, group_label)

print(head(summary_tbl, 10))

# Grouped means across your ROI metrics
compare_vars <- c("net_price","earn10","grad_rate","sat_avg","debt_mdn",
                  "stem_share","earnings_to_net","earnings_adj_grad")

group_means <- df %>%
  group_by(group_label) %>%
  summarize(across(all_of(compare_vars), ~ mean(.x, na.rm = TRUE)), .groups = "drop")
print(group_means)

# Robust distribution summary (median / IQR)
group_robust <- df %>%
  group_by(group_label) %>%
  summarize(
    across(all_of(compare_vars),
           list(median = ~median(.x, na.rm = TRUE),
                p25 = ~quantile(.x, 0.25, na.rm = TRUE),
                p75 = ~quantile(.x, 0.75, na.rm = TRUE)),
           .names = "{.col}_{.fn}"),
    .groups = "drop"
  )
print(group_robust)

# Focus tables
ivy_table <- df %>%
  filter(is_ivy) %>%
  select(name, state, net_price, earn10, grad_rate, sat_avg,
         stem_share, debt_mdn, earnings_to_net, earnings_adj_grad) %>%
  arrange(name)
print(ivy_table)

fl_public_table <- df %>%
  filter(is_fl_public) %>%
  select(name, state, net_price, earn10, grad_rate, sat_avg,
         stem_share, debt_mdn, earnings_to_net, earnings_adj_grad) %>%
  arrange(name)
print(fl_public_table)

ncf_table <- df %>%
  filter(is_ncf) %>%
  select(name, state, net_price, earn10, grad_rate, sat_avg,
         stem_share, debt_mdn, earnings_to_net, earnings_adj_grad)
print(ncf_table)

private_np_table <- df %>%
  filter(is_private_np_nat) %>%
  select(name, state, net_price, earn10, grad_rate, sat_avg,
         stem_share, debt_mdn, earnings_to_net, earnings_adj_grad) %>%
  arrange(name)
print(private_np_table)

public_nat_table <- df %>%
  filter(is_public_nat) %>%
  select(name, state, net_price, earn10, grad_rate, sat_avg,
         stem_share, debt_mdn, earnings_to_net, earnings_adj_grad) %>%
  arrange(name)
print(public_nat_table)

# =======================
# 6. Regression Analysis
# =======================
reg_df <- df %>%
  filter(!is.na(earn10), !is.na(net_price),
         !is.na(sat_avg), !is.na(grad_rate))

lm1 <- lm(earn10 ~ net_price + sat_avg + grad_rate + stem_share, data = reg_df)
print(summary(lm1))

# =======================
# 7. K-means
# =======================
cluster_df <- df %>%
  filter(!is.na(net_price), !is.na(earn10), !is.na(grad_rate), !is.na(debt_mdn)) %>%
  select(name, net_price, earn10, grad_rate, debt_mdn)

cluster_mat <- scale(cluster_df[, -1])
set.seed(123)
km <- kmeans(cluster_mat, centers = 4, nstart = 50)
cluster_df$cluster <- factor(km$cluster)

print(fviz_cluster(list(data = cluster_mat, cluster = km$cluster), geom = "point"))
# =======================
# Visualization — SAT Box Plot & Net Price Bar Chart by Comparison Group
# (Append this to the end of your script)
# =======================

library(scales)     # pretty axis labels (dollar / comma)
library(forcats)    # factor reordering helpers
library(ggplot2)
library(dplyr)
library(stringr)

# A friendly, high-contrast palette for groups (extend if you add more groups)
group_palette <- c(
  "Ivy League"                = "#E4572E",
  "Florida Public"            = "#17BEBB",
  "New College of Florida"    = "#7F95D1",
  "Private Nonprofit (National)" = "#76B041",
  "Public (National)"         = "#F1C40F",
  "Other"                     = "#7D7D7D"
)

# ------------------------------------------------------------------------------
# 1) SAT Box Plot by Group — with richer styling & annotations
# ------------------------------------------------------------------------------

sat_df <- df %>%
  filter(!is.na(sat_avg), !is.na(group_label))

# Sample sizes for subtitle
sat_counts <- sat_df %>%
  count(group_label, name = "n") %>%
  arrange(desc(n))

sat_subtitle <- paste0(
  "Groups ordered by median SAT (high → low) • n = ",
  paste0(sat_counts$group_label, " (", sat_counts$n, ")", collapse = "  ·  ")
)

# Order groups by median SAT (high → low)
sat_order <- sat_df %>%
  group_by(group_label) %>%
  summarize(med_sat = median(sat_avg, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(med_sat)) %>%
  pull(group_label)

overall_sat_median <- median(sat_df$sat_avg, na.rm = TRUE)

p_sat_box <- ggplot(
  sat_df %>% mutate(group_label = forcats::fct_relevel(group_label, sat_order)),
  aes(x = group_label, y = sat_avg, fill = group_label)
) +
  # a subtle reference line for overall median SAT
  geom_hline(yintercept = overall_sat_median, linetype = "dashed", linewidth = 0.4, color = "#555555") +
  annotate("text",
           x = Inf, y = overall_sat_median,
           label = paste0("Overall median: ", comma(overall_sat_median)),
           hjust = 1.05, vjust = -0.25, size = 3.2, color = "#555555") +
  geom_boxplot(outlier.shape = NA, alpha = 0.85, color = "#333333", width = 0.7) +
  # light jitter to show density
  geom_jitter(width = 0.15, alpha = 0.22, size = 1.2, color = "#2F2F2F") +
  # highlight group medians with a larger point
  stat_summary(fun = median, geom = "point", shape = 21, size = 2.8,
               fill = "white", color = "#111111", stroke = 0.6) +
  scale_fill_manual(values = group_palette, guide = "none") +
  labs(
    title = "SAT Scores by Comparison Group",
    subtitle = sat_subtitle,
    x = NULL,
    y = "Average SAT (overall)",
    caption = "Notes: Boxes show IQR with median line; dots are individual institutions. Dashed line marks the overall median SAT across groups."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 16, hjust = 1, vjust = 1, size = 11),
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(color = "#5A5A5A")
  )
print(p_sat_box)

# ------------------------------------------------------------------------------
# 2) Net Price Bar Chart by Group — median with IQR error bars & labels
#   (replaces the net price box plot for a clearer group comparison)
# ------------------------------------------------------------------------------

price_df <- df %>%
  filter(!is.na(net_price), !is.na(group_label)) %>%
  group_by(group_label) %>%
  summarize(
    n = n(),
    median_net_price = median(net_price, na.rm = TRUE),
    iqr = IQR(net_price, na.rm = TRUE),                 # interquartile range as a variability band
    .groups = "drop"
  )

# Order groups by median price (low → high for affordability)
price_df <- price_df %>%
  mutate(group_label = fct_reorder(group_label, median_net_price))

price_subtitle <- paste0(
  "Bars show group median net price (overall) with IQR error bars • n = ",
  paste0(price_df$group_label, " (", price_df$n, ")", collapse = "  ·  ")
)

p_net_bar <- ggplot(price_df, aes(x = group_label, y = median_net_price, fill = group_label)) +
  geom_col(alpha = 0.9, width = 0.72, color = "#333333") +
  geom_errorbar(aes(ymin = median_net_price - iqr / 2, ymax = median_net_price + iqr / 2),
                width = 0.18, color = "#2F2F2F", linewidth = 0.6) +
  # label bars with the median dollar amount
  geom_text(aes(label = dollar(median_net_price)),
            vjust = -0.4, size = 3.7, color = "#1E1E1E", fontface = "bold") +
  scale_fill_manual(values = group_palette, guide = "none") +
  scale_y_continuous(labels = dollar_format(), expand = expansion(mult = c(0.05, 0.12))) +
  labs(
    title = "Median Net Price by Comparison Group",
    subtitle = price_subtitle,
    x = NULL,
    y = "Median Net Price (overall)",
    caption = "Notes: Bars represent group medians; error bars show ±½ IQR as a compact measure of variability.\nNet price is the average annual cost after grants and scholarships."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 16, hjust = 1, vjust = 1, size = 11),
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(color = "#5A5A5A")
  )
print(p_net_bar)

# =======================
# Visualization — Graduation Rates by Comparison Group
# (Append this block to the end of your script)
# =======================

library(ggplot2)
library(dplyr)
library(forcats)
library(scales)
library(stringr)

# Consistent palette with your earlier visuals
group_palette <- c(
  "Ivy League"                   = "#E4572E",
  "Florida Public"               = "#17BEBB",
  "New College of Florida"       = "#7F95D1",
  "Private Nonprofit (National)" = "#76B041",
  "Public (National)"            = "#F1C40F",
  "Other"                        = "#7D7D7D"
)

# Filter once for clean inputs
grad_df <- df %>%
  filter(!is.na(grad_rate), !is.na(group_label)) %>%
  mutate(grad_rate = pmin(pmax(grad_rate, 0), 1))  # clamp to [0,1] if any oddities

# Sample sizes for subtitles
grad_counts <- grad_df %>%
  count(group_label, name = "n") %>%
  arrange(desc(n))

grad_counts_str <- paste0(grad_counts$group_label, " (", grad_counts$n, ")",
                          collapse = "  ·  ")

# ------------------------------------------------------------------------------
# 1) Graduation Rate — BOX PLOT (distribution-focused)
# ------------------------------------------------------------------------------

# Order groups by median grad rate (high → low)
grad_order_box <- grad_df %>%
  group_by(group_label) %>%
  summarize(med_gr = median(grad_rate, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(med_gr)) %>%
  pull(group_label)

overall_grad_median <- median(grad_df$grad_rate, na.rm = TRUE)

p_grad_box <- ggplot(
  grad_df %>% mutate(group_label = fct_relevel(group_label, grad_order_box)),
  aes(x = group_label, y = grad_rate, fill = group_label)
) +
  geom_hline(yintercept = overall_grad_median, linetype = "dashed",
             linewidth = 0.4, color = "#555555") +
  annotate("text", x = Inf, y = overall_grad_median,
           label = paste0("Overall median: ", percent(overall_grad_median, accuracy = 0.1)),
           hjust = 1.05, vjust = -0.25, size = 3.2, color = "#555555") +
  geom_boxplot(outlier.shape = NA, alpha = 0.85, color = "#333333", width = 0.7) +
  geom_jitter(width = 0.15, alpha = 0.22, size = 1.2, color = "#2F2F2F") +
  stat_summary(fun = median, geom = "point", shape = 21, size = 2.8,
               fill = "white", color = "#111111", stroke = 0.6) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_manual(values = group_palette, guide = "none") +
  labs(
    title = "Graduation Rates by Comparison Group",
    subtitle = paste0("Distribution view (median & IQR) • n = ", grad_counts_str),
    x = NULL,
    y = "Graduation Rate",
    caption = "Notes: Boxes show IQR with median line; dots are individual institutions. Dashed line marks the overall median across groups."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 16, hjust = 1, vjust = 1, size = 11),
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(color = "#5A5A5A")
  )
print(p_grad_box)

# ------------------------------------------------------------------------------
# 2) Graduation Rate — BAR CHART (average-focused with variability)
# ------------------------------------------------------------------------------

grad_bar_df <- grad_df %>%
  group_by(group_label) %>%
  summarize(
    n = n(),
    mean_gr = mean(grad_rate, na.rm = TRUE),
    sd_gr   = sd(grad_rate,  na.rm = TRUE),
    iqr_gr  = IQR(grad_rate, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Order by mean graduation rate (high → low)
  mutate(group_label = fct_reorder(group_label, mean_gr, .desc = TRUE))

p_grad_bar <- ggplot(grad_bar_df, aes(x = group_label, y = mean_gr, fill = group_label)) +
  geom_col(alpha = 0.9, width = 0.72, color = "#333333") +
  # Use IQR/2 as a compact variability band around the mean
  geom_errorbar(aes(ymin = pmax(mean_gr - iqr_gr/2, 0),
                    ymax = pmin(mean_gr + iqr_gr/2, 1)),
                width = 0.18, color = "#2F2F2F", linewidth = 0.6) +
  geom_text(aes(label = percent(mean_gr, accuracy = 0.1)),
            vjust = -0.4, size = 3.7, color = "#1E1E1E", fontface = "bold") +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0.05, 0.12))) +
  scale_fill_manual(values = group_palette, guide = "none") +
  labs(
    title = "Average Graduation Rate by Comparison Group",
    subtitle = paste0("Bars show group mean; error bars ±½ IQR • n = ", grad_counts_str),
    x = NULL,
    y = "Average Graduation Rate",
    caption = "Notes: Mean emphasizes overall level; IQR/2 error bars give a robust sense of within-group variability."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 16, hjust = 1, vjust = 1, size = 11),
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(color = "#5A5A5A")
  )
print(p_grad_bar)
