# R/data_assessment.R

# --- Main Data Assessment Function ---

#' Performs a comprehensive data quality assessment on a dataframe.
#'
#' @param df The dataframe to be assessed.
#' @return A list containing the quality summary, quality issues, NA-only variables, and duplicate count.
perform_data_assessment <- function(df) {
   
# 1. Format and label the data 
   master_table <- df %>%
     rename(
       id = id,
       name = `school.name`,
       state = `school.state`,
       city = `school.city`,
       sat_avg = `latest.admissions.sat_scores.average.overall`,
       tuition_in = `latest.cost.tuition.in_state`,
       tuition_out = `latest.cost.tuition.out_of_state`,
       grad_rate = `latest.completion.rate_suppressed.overall`,
       earn10 = `latest.earnings.10_yrs_after_entry.median`,
       control = `school.ownership`
     ) %>%
     
# 2. Convert relevant columns to numeric
     mutate(across(c(
       sat_avg,
       tuition_in,
       tuition_out,
       grad_rate,
       earn10
     ), as.numeric)) %>%
     
# 3. Add readable institution type
     mutate(
       institution_type = case_when(
         control == 1 ~ "Public",
         control == 2 ~ "Private Nonprofit",
         control == 3 ~ "Private For-Profit",
         TRUE ~ "Other"
       )
     )
  
# 4. Check duplicates based on 'id'
   total_duplicates <- master_table %>%
     group_by(id) %>%
     filter(n() > 1) %>%
     summarise(Duplicates = n() - 1) %>%
     summarise(Total_Duplicates = sum(Duplicates)) %>%
     pull(Total_Duplicates)
   cat("Total duplicated rows in the dataset:", total_duplicates, "\n")
   
# 5. Identify categorical variables  columns
   categorical_cols <- master_table %>%
     select(where(~ is.character(.) | is.factor(.))) %>%
     colnames()
   categorical_cols
   
# 6. Completeness function
   calculate_completeness <- function(column) {
     total_values <- length(column)
     missing_values <- sum(is.na(column))
     completeness <- (1 - (missing_values / total_values)) * 100
     return(round(completeness, 2))
   }

# 7. Numeric consistency function
   check_consistency_numeric <- function(column, min_val = -Inf, max_val = Inf) {
     total_values <- length(column)
     invalid_values <- sum(column < min_val | column > max_val, na.rm = TRUE)
     consistency_pct <- (1 - (invalid_values / total_values)) * 100
     return(round(consistency_pct, 2))
   }
   
# 8. Categorical consistency function
   check_consistency_categorical <- function(column, allowed_values = NULL) {
     if (is.null(allowed_values)) return(NA_real_)
     total_values <- length(column)
     invalid_values <- sum(!column %in% allowed_values, na.rm = TRUE)
     consistency_pct <- (1 - (invalid_values / total_values)) * 100
     return(round(consistency_pct, 2))
   }
   
#  9. Outlier detection using IQR function
   check_outliers_iqr <- function(column) {
     if(!is.numeric(column)) return(NA_real_)
     Q1 <- quantile(column, 0.25, na.rm = TRUE)
     Q3 <- quantile(column, 0.75, na.rm = TRUE)
     IQR_val <- Q3 - Q1
     lower <- Q1 - 1.5 * IQR_val
     upper <- Q3 + 1.5 * IQR_val
     outliers <- sum(column < lower | column > upper, na.rm = TRUE)
     pct_outliers <- (outliers / length(column)) * 100
     return(round(pct_outliers, 2))
   }
   
# 10. Updated Numeric Ranges & expected values for categorical variables
   numeric_ranges <- list(
     tuition_in = c(-10000, 100000),        # small negative allowed for scholarships, grants and aid
     tuition_out = c(-10000, 150000),
     earn10 = c(0, 500000),
     grad_rate = c(0, 1),
     sat_avg = c(400, 1600),
     # Net price by income level (public)
     net_price_pub_0_30 = c(-10000, 100000),
     net_price_pub_30_48 = c(-10000, 100000),
     net_price_pub_48_75 = c(-10000, 100000),
     net_price_pub_75_110 = c(-10000, 100000),
     net_price_pub_110_plus = c(-10000, 100000),
     # Net price by income level (private)
     net_price_priv_0_30 = c(-10000, 100000),
     net_price_priv_30_48 = c(-10000, 100000),
     net_price_priv_48_75 = c(-10000, 100000),
     net_price_priv_75_110 = c(-10000, 100000),
     net_price_priv_110_plus = c(-10000, 100000)
   )
   
   expected_categories <- list(
     control = c(1, 2, 3), # diferent values for control
     state = state.abb #State comparison vs state abbreviations
   )
   
# 11. Create quality summary with statistical summary
   quality_summary <- master_table %>%
     summarise(across(everything(), ~ calculate_completeness(.))) %>%
     pivot_longer(
       cols = everything(),
       names_to = "Variable",
       values_to = "Completeness_Percentage"
     ) %>%
     rowwise() %>%
     mutate(
       
       # Adding consistency 
       Consistency = case_when(
         Variable %in% names(numeric_ranges) ~ check_consistency_numeric(
           master_table[[Variable]],
           min_val = numeric_ranges[[Variable]][1],
           max_val = numeric_ranges[[Variable]][2]
         ),
         Variable %in% names(expected_categories) ~ check_consistency_categorical(
           master_table[[Variable]],
           allowed_values = expected_categories[[Variable]]
         ),
         TRUE ~ NA_real_
       ),
       # Adding and calculate Outlier percentage
       Outlier_Percentage = ifelse(Variable %in% names(numeric_ranges),
                                   check_outliers_iqr(master_table[[Variable]]), NA_real_),
       # Final Statistical summaries
       Min = ifelse(Variable %in% names(numeric_ranges), min(master_table[[Variable]], na.rm = TRUE), NA_real_),
       Q1 = ifelse(Variable %in% names(numeric_ranges), quantile(master_table[[Variable]], 0.25, na.rm = TRUE), NA_real_),
       Median = ifelse(Variable %in% names(numeric_ranges), median(master_table[[Variable]], na.rm = TRUE), NA_real_),
       Mean = ifelse(Variable %in% names(numeric_ranges), mean(master_table[[Variable]], na.rm = TRUE), NA_real_),
       Q3 = ifelse(Variable %in% names(numeric_ranges), quantile(master_table[[Variable]], 0.75, na.rm = TRUE), NA_real_),
       Max = ifelse(Variable %in% names(numeric_ranges), max(master_table[[Variable]], na.rm = TRUE), NA_real_),
       SD = ifelse(Variable %in% names(numeric_ranges), sd(master_table[[Variable]], na.rm = TRUE), NA_real_),
       
     ) %>%
     ungroup() %>%
     arrange(desc(Completeness_Percentage))
   
   # Add average row
   average_row <- tibble(
     Variable = "Average",
     Completeness_Percentage = round(mean(quality_summary$Completeness_Percentage, na.rm = TRUE), 2),
     Consistency = round(mean(quality_summary$Consistency, na.rm = TRUE), 2),
     Outlier_Percentage = round(mean(quality_summary$Outlier_Percentage, na.rm = TRUE), 2),
     Min = NA_real_,
     Q1 = NA_real_,
     Median = NA_real_,
     Mean = NA_real_,
     Q3 = NA_real_,
     Max = NA_real_,
     SD = NA_real_,
     Mode = NA_character_
   )
   quality_summary <- bind_rows(quality_summary, average_row)
   
   # Print table
   print(quality_summary)
   
# 12. Quality issues report with NA
   quality_issues <- quality_summary %>%
     filter(Completeness_Percentage < 70 | Consistency < 90 | Outlier_Percentage > 5)
   if (!dir.exists("output")) dir.create("output")
   write.csv(quality_issues, file = file.path("output", "data_quality_issues.csv"), row.names = FALSE)
  
# 13. Identify variables that are completely empty
    na_only_vars <- master_table %>% # variables with NA
     summarise(across(everything(), ~ all(is.na(.)))) %>%  # TRUE of all are NA
     pivot_longer(cols = everything(), names_to = "Variable", values_to = "All_NA") %>%
     filter(All_NA == TRUE)
   print(na_only_vars)
   
# 14. Return all results as a list for easy access
  return(list(
    quality_summary = quality_summary,
    quality_issues = quality_issues,
    na_only_vars = na_only_vars,
    total_duplicates = total_duplicates
  ))
}