# R/data_assessment.R

# --- Helper Functions for Data Assessment ---

#' Calculate the percentage of non-missing values in a column.
calculate_completeness <- function(column) {
  completeness <- (1 - (sum(is.na(column)) / length(column))) * 100
  return(round(completeness, 2))
}

#' Check if numeric values fall within a specified range.
check_consistency_numeric <- function(column, min_val = -Inf, max_val = Inf) {
  # Exclude NAs from the consistency check denominator for accuracy
  non_na_count <- sum(!is.na(column))
  if (non_na_count == 0) return(100)
  
  invalids <- sum(column < min_val | column > max_val, na.rm = TRUE)
  consistency_pct <- (1 - (invalids / non_na_count)) * 100
  return(round(consistency_pct, 2))
}

#' Detect the percentage of outliers using the IQR method.
check_outliers_iqr <- function(column) {
  if (!is.numeric(column) || sum(!is.na(column)) < 2) return(NA_real_)
  Q1 <- quantile(column, 0.25, na.rm = TRUE)
  Q3 <- quantile(column, 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR_val
  upper <- Q3 + 1.5 * IQR_val
  outliers <- sum(column < lower | column > upper, na.rm = TRUE)
  
  # Calculate outliers as a percentage of non-NA values
  non_na_count <- sum(!is.na(column))
  if (non_na_count == 0) return(0)
  
  pct_outliers <- (outliers / non_na_count) * 100
  return(round(pct_outliers, 2))
}


# --- Main Data Assessment Function ---

#' Performs a comprehensive data quality assessment on a dataframe.
#'
#' @param df The dataframe to be assessed.
#' @return A list containing the quality summary, quality issues, NA-only variables, and duplicate count.
perform_data_assessment <- function(df) {
  
  # 1. Check for duplicates based on 'id'
  total_duplicates <- df %>%
    dplyr::group_by(id) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    dplyr::summarise(Duplicates = dplyr::n() - 1, .groups = 'drop') %>%
    dplyr::summarise(Total_Duplicates = sum(Duplicates)) %>%
    dplyr::pull(Total_Duplicates)

  # 2. Define expected ranges for numeric variables
  numeric_ranges <- list(
    sat_avg = c(400, 1600),
    tuition_in = c(-10000, 100000),
    tuition_out = c(-10000, 150000),
    grad_rate = c(0, 1),
    earn10 = c(0, 500000),
    net_price_pub_0_30k = c(-10000, 100000),
    net_price_pub_30_48k = c(-10000, 100000),
    net_price_pub_48_75k = c(-10000, 100000),
    net_price_priv_0_30k = c(-10000, 100000),
    net_price_priv_30_48k = c(-10000, 100000),
    net_price_priv_48_75k = c(-10000, 100000)
  )

  # 3. Generate the main quality summary table
  quality_summary <- tibble::tibble(Variable = colnames(df)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      Completeness = calculate_completeness(df[[Variable]]),
      Consistency = ifelse(Variable %in% names(numeric_ranges),
                           check_consistency_numeric(df[[Variable]], numeric_ranges[[Variable]][1], numeric_ranges[[Variable]][2]),
                           NA_real_),
      Outliers_Pct = check_outliers_iqr(df[[Variable]]),
      Mean = ifelse(is.numeric(df[[Variable]]), round(mean(df[[Variable]], na.rm = TRUE), 2), NA),
      SD = ifelse(is.numeric(df[[Variable]]), round(sd(df[[Variable]], na.rm = TRUE), 2), NA),
      Min = ifelse(is.numeric(df[[Variable]]), min(df[[Variable]], na.rm = TRUE), NA),
      Median = ifelse(is.numeric(df[[Variable]]), median(df[[Variable]], na.rm = TRUE), NA),
      Max = ifelse(is.numeric(df[[Variable]]), max(df[[Variable]], na.rm = TRUE), NA)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(Completeness)

  # 4. Filter for variables with potential quality issues
  quality_issues <- quality_summary %>%
    dplyr::filter(Completeness < 70 | Consistency < 90 | Outliers_Pct > 5)

  # 5. Identify variables that are completely empty
  na_only_vars <- df %>%
    dplyr::summarise(dplyr::across(everything(), ~ all(is.na(.)))) %>%
    tidyr::pivot_longer(cols = everything(), names_to = "Variable", values_to = "All_NA") %>%
    dplyr::filter(All_NA == TRUE)

  # 6. Return all results as a list for easy access
  return(list(
    quality_summary = quality_summary,
    quality_issues = quality_issues,
    na_only_vars = na_only_vars,
    total_duplicates = total_duplicates
  ))
}