# ------------------------------------------------------------
# College Scorecard Research Project — R Pipeline
# ------------------------------------------------------------

library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(factoextra)
library(ggplot2)
library(tidyverse)
library(conflicted)

# =======================
# 1. API 
# =======================
api_key <- ""   # <- put your api.data.gov key here

# Explicitly choose which functions to prefer
suppressMessages({
  conflict_prefer("filter", "dplyr")
  conflict_prefer("lag", "dplyr")
  conflict_prefer("rename", "dplyr")   # <— added this line
})

fields <- c(
  "id", "school.name", "school.state", "school.city",
  "latest.admissions.sat_scores.average.overall",
  "latest.cost.tuition.in_state",
  "latest.cost.tuition.out_of_state",
  # Correct aggregate net price field:
  "latest.cost.avg_net_price.overall",
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
df <- dplyr::rename(df_raw,   # <— explicitly calls dplyr’s rename()
    name = school.name,
    state = school.state,
    city = school.city,
    sat_avg = latest.admissions.sat_scores.average.overall,
    tuition_in = latest.cost.tuition.in_state,
    tuition_out = latest.cost.tuition.out_of_state,
    grad_rate = latest.completion.rate_suppressed.overall,
    earn10 = latest.earnings.10_yrs_after_entry.median,
    debt_mdn = latest.aid.median_debt.completers.overall,
    pcip_cs = latest.academics.program_percentage.computer,
    pcip_eng = latest.academics.program_percentage.engineering,
    pcip_math = latest.academics.program_percentage.mathematics
  ) %>%
  mutate(across(c(sat_avg, tuition_in, tuition_out, grad_rate, earn10, debt_mdn,
                  pcip_cs, pcip_eng, pcip_math), ~ suppressWarnings(as.numeric(.x)))) %>%
  mutate(
    net_price_overall = coalesce(
      `latest.cost.avg_net_price.overall`,
      tuition_in
    ),
    net_price_low    = net_price_overall,
    net_price_middle = net_price_overall,
    net_price_upper  = net_price_overall,
    net_price        = net_price_middle,
    stem_share       = rowSums(across(c(pcip_cs, pcip_eng, pcip_math), ~ coalesce(.x, 0)), na.rm = TRUE),
    earnings_to_net  = earn10 / net_price,
    earnings_adj_grad = (earn10 * grad_rate) / net_price
  )

# =======================
# 4. Descriptive Analysis
# =======================
summary_tbl <- df %>%
  select(name, state, net_price, earn10, grad_rate, sat_avg,
         stem_share, debt_mdn, earnings_to_net, earnings_adj_grad)

head(summary_tbl, 10)

top_value <- df %>%
  filter(
    !is.na(earnings_to_net),
    !is.na(net_price),
    !is.na(earn10),
    earn10 > 90000
  ) %>%
  arrange(desc(earnings_to_net)) %>%
  select(name, state, net_price, earn10, earnings_to_net) %>%
  head(100)

print(top_value)

# =======================
# 5. Regression Analysis
# =======================
reg_df <- df %>%
  filter(!is.na(earn10), !is.na(net_price),
         !is.na(sat_avg), !is.na(grad_rate))

lm1 <- lm(earn10 ~ net_price + sat_avg + grad_rate + stem_share, data = reg_df)
summary(lm1)

# =======================
# 6. K-means
# =======================
cluster_df <- df %>%
  filter(!is.na(net_price), !is.na(earn10), !is.na(grad_rate), !is.na(debt_mdn)) %>%
  select(name, net_price, earn10, grad_rate, debt_mdn)

cluster_mat <- scale(cluster_df[, -1])
set.seed(123)
km <- kmeans(cluster_mat, centers = 4, nstart = 50)
cluster_df$cluster <- factor(km$cluster)

fviz_cluster(list(data = cluster_mat, cluster = km$cluster), geom = "point")

# =======================
# 7. Visualization
# =======================
plot_df <- df %>%
  filter(!is.na(earn10), !is.na(net_price), !is.na(grad_rate))

n_samp <- min(500L, nrow(plot_df))

if (n_samp >= 2L) {
  plot_df %>%
    dplyr::slice_sample(n = n_samp) %>%
    ggplot(aes(x = net_price, y = earn10, size = grad_rate, color = stem_share)) +
    geom_point(alpha = 0.6) +
    scale_size_continuous(range = c(1, 10), name = "Grad Rate") +
    scale_color_gradient(low = "lightblue", high = "darkblue", name = "STEM %") +
    labs(title = "College ROI: Earnings vs Cost",
         subtitle = "Bubble size = Graduation Rate, Color = STEM Share",
         x = "Net Price ($)", y = "10-Year Earnings ($)") +
    theme_minimal() +
    geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed")
} else {
  message("Not enough rows to plot after filtering (need at least 2).")
}
