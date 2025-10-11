# main.R: Main script for the College Scorecard analysis pipeline

# ------------------------------------------------------------
# 1. Setup
# ------------------------------------------------------------
# Load required packages
suppressPackageStartupMessages({
  library(dotenv)
  library(httr)
  library(jsonlite)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(stringr)
  library(conflicted)
  library(ggrepel)
  library(scales)
})

# Handle function conflicts
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

# Load custom functions
source("R/load_data.R")
source("R/clean_data.R")
source("R/define_groups.R")
source("R/visualizations.R")

# Load environment variables (API_KEY) from .env file
load_dot_env()

# Set parameters
API_KEY <- Sys.getenv("API_KEY")
FIELDS <- c(
  "id", "school.name", "school.state", "school.city", "school.ownership",
  "latest.admissions.sat_scores.average.overall",
  "latest.cost.tuition.in_state", "latest.cost.tuition.out_of_state",
  "latest.cost.avg_net_price.overall",
  "latest.completion.rate_suppressed.overall",
  "latest.earnings.10_yrs_after_entry.median",
  "latest.aid.median_debt.completers.overall",
  "latest.academics.program_percentage.computer",
  "latest.academics.program_percentage.engineering",
  "latest.academics.program_percentage.mathematics"
)
FIELDS_STR <- paste(FIELDS, collapse = ",")


# ------------------------------------------------------------
# 2. Execute Analysis Pipeline
# ------------------------------------------------------------
# Step 1: Load data
df_raw <- load_scorecard_data(api_key = API_KEY, fields_str = FIELDS_STR)

# Step 2: Clean data
df_clean <- clean_scorecard_data(df_raw)

# Step 3: Define and add comparison groups
df_final <- add_comparison_groups(df_clean)

# ------------------------------------------------------------
# 3. Descriptive Analysis (Output to Console)
# ------------------------------------------------------------
message("\n--- Descriptive Analysis Summary ---")
# Calculate and print group means
group_means <- df_final %>%
  group_by(group_label) %>%
  summarize(
    across(
      c(net_price, earn10, grad_rate, sat_avg, debt_mdn, stem_share),
      ~ mean(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )
print(group_means)


# ------------------------------------------------------------
# 4. Generate and Save Visualizations
# ------------------------------------------------------------
message("\n--- Generating Plots ---")
# Ensure the 'output' directory exists
if (!dir.exists("output")) {
  dir.create("output")
}

# Generate plot objects
p_sat <- plot_sat_boxplot(df_final)
p_price <- plot_net_price_barchart(df_final)
p_grad <- plot_grad_rate_barchart(df_final)
p_scatter <- plot_earnings_scatterplot(df_final)
# p_map <- print(p_map)

# Save plots to file
ggsave("output/sat_scores_by_group.png", p_sat, width = 10, height = 7)
ggsave("output/net_price_by_group.png", p_price, width = 10, height = 7)
ggsave("output/grad_rate_by_group.png", p_grad, width = 10, height = 7)
ggsave("output/earnings_vs_price_scatter.png", p_scatter, width = 11, height = 8)
# ggsave("output/map.png", p_map, width = 11, height = 8)

message("✅ All plots have been saved to the 'output/' directory!")