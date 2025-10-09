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
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

# =======================
# 1. API 
# =======================
api_key <- ""   
# create api key here: https://api.data.gov/signup/ 

# Explicitly choose which filter to use
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

fields <- c(
  # ------------------------
  # Basic Identifiers
  # ------------------------
  "id",
  "school.name",
  "school.state",
  "school.city",
  "school.region_id",
  "school.carnegie_size_setting",
  "school.institutional_characteristics.level",

  # ------------------------
  # Admissions
  # ------------------------
  "latest.admissions.sat_scores.average.overall",
  "latest.admissions.act_scores.midpoint.cumulative",
  "latest.admissions.admission_rate.overall",

  # ------------------------
  # Cost & Tuition
  # ------------------------
  "latest.cost.tuition.in_state",
  "latest.cost.tuition.out_of_state",
  "latest.cost.tuition.program_year",
  "latest.cost.net_price.public",
  "latest.cost.net_price.private",
  "latest.cost.avg_net_price.overall",

  # --- Cost by Income Level (Public & Private) ---
  "latest.cost.net_price.public.by_income_level.0-30000",
  "latest.cost.net_price.public.by_income_level.30001-48000",
  "latest.cost.net_price.public.by_income_level.48001-75000",
  "latest.cost.net_price.public.by_income_level.75001-110000",
  "latest.cost.net_price.public.by_income_level.110001-plus",
  "latest.cost.net_price.private.by_income_level.0-30000",
  "latest.cost.net_price.private.by_income_level.30001-48000",
  "latest.cost.net_price.private.by_income_level.48001-75000",
  "latest.cost.net_price.private.by_income_level.75001-110000",
  "latest.cost.net_price.private.by_income_level.110001-plus",

  # ------------------------
  # Completion & Outcomes
  # ------------------------
  "latest.completion.rate_suppressed.overall",
  "latest.completion.consumer_rate.overall",
  "latest.completion.150_4yr_completion_rate_students.overall",
  "latest.completion.4yr_completion_rate.overall",
  "latest.completion.8yr_completion_rate.overall",

  # ------------------------
  # Earnings & Employment
  # ------------------------
  "latest.earnings.10_yrs_after_entry.median",
  "latest.earnings.6_yrs_after_entry.median",
  "latest.earnings.8_yrs_after_entry.median",
  "latest.earnings.combined_median_earnings",
  "latest.earnings.mean_earnings",
  "latest.earnings.median_earnings",
  "latest.earnings.count_earnings_gt_25k",

  # ------------------------
  # Aid & Debt
  # ------------------------
  "latest.aid.median_debt.completers.overall",
  "latest.aid.median_debt.noncompleters",
  "latest.aid.students_with_any_loan",
  "latest.aid.students_with_pell_grant",
  "latest.aid.default_rate",

  # ------------------------
  # Academics: Program Percentages
  # ------------------------
  "latest.academics.program_percentage.computer",
  "latest.academics.program_percentage.engineering",
  "latest.academics.program_percentage.mathematics",
  "latest.academics.program_percentage.physical_science",
  "latest.academics.program_percentage.business_marketing",
  "latest.academics.program_percentage.education",
  "latest.academics.program_percentage.health",
  "latest.academics.program_percentage.social_science",
  "latest.academics.program_percentage.visual_performing",
  "latest.academics.program_percentage.psychology",
  "latest.academics.program_percentage.communications_technology",
  "latest.academics.program_percentage.security_law_enforcement",
  "latest.academics.program_percentage.legal",
  "latest.academics.program_percentage.humanities",

  # ------------------------
  # Demographics
  # ------------------------
  "latest.student.size",
  "latest.student.demographics.men",
  "latest.student.demographics.women",
  "latest.student.demographics.first_generation",
  "latest.student.demographics.share_white",
  "latest.student.demographics.share_black",
  "latest.student.demographics.share_hispanic",
  "latest.student.demographics.share_asian",
  "latest.student.demographics.share_pacific_islander",
  "latest.student.demographics.share_two_or_more",
  "latest.student.demographics.share_non_resident_alien",

  # ------------------------
  # Misc
  # ------------------------
  "latest.school.ownership",
  "latest.school.degrees_awarded.predominant",
  "latest.school.locale"
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
    url <- paste0(base_url,
                  "?api_key=", api_key,
                  "&fields=", fields_str,
                  "&per_page=", per_page,
                  "&page=", page)
    
    res <- GET(url)
    stop_for_status(res)
    data_json <- content(res, as = "text")
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
df <- df_raw %>%
  rename(
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
                  pcip_cs, pcip_eng, pcip_math), as.numeric)) %>%
  # Control the net price by income level (0-48000)
mutate(
  net_price_low = coalesce(
    `latest.cost.net_price.public.by_income_level.0-48000`,
    `latest.cost.net_price.private.by_income_level.0-48000`,
    tuition_in
  ),
  net_price_middle = coalesce(
    `latest.cost.net_price.public.by_income_level.48001-75000`,
    `latest.cost.net_price.private.by_income_level.48001-75000`,
    tuition_in
  ),
  net_price_upper = coalesce(
    `latest.cost.net_price.public.by_income_level.75001-plus`,
    `latest.cost.net_price.private.by_income_level.75001-plus`,
    tuition_in
  ),
  # Default to middle-class net price
  net_price = net_price_middle,
  
  stem_share = rowSums(across(c(pcip_cs, pcip_eng, pcip_math), ~ coalesce(.x, 0))),
  earnings_to_net = earn10 / net_price,
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
# Earnings vs Net Price with graduation rate as bubble size
df %>%
  filter(!is.na(earn10), !is.na(net_price), !is.na(grad_rate)) %>%
  sample_n(500) %>%
  ggplot(aes(x = net_price, y = earn10, size = grad_rate, color = stem_share)) +
  geom_point(alpha = 0.6) +
  scale_size_continuous(range = c(1, 10), name = "Grad Rate") +
  scale_color_gradient(low = "lightblue", high = "darkblue", name = "STEM %") +
  labs(title = "College ROI: Earnings vs Cost",
       subtitle = "Bubble size = Graduation Rate, Color = STEM Share",
       x = "Net Price ($)", y = "10-Year Earnings ($)") +
  theme_minimal() +
  geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed")
