# R/load_data.R

#' Fetch all school data from the College Scorecard API
#'
#' @param api_key Your api.data.gov API key.
#' @param fields_str A comma-separated string of fields to request.
#' @param per_page The number of items to request per page.
#' @return A data frame containing the raw school data.
get_all_schools <- function(api_key, fields_str, per_page = 100) {
  base_url <- "https://api.data.gov/ed/collegescorecard/v1/schools.json"
  page <- 0
  results <- list()
  
  repeat {
    url <- paste0(
      base_url,
      "?api_key=", api_key,
      "&fields=", fields_str,
      "&per_page=", per_page,
      "&page=", page
    )
    
    res <- httr::GET(url)
    httr::stop_for_status(res)
    data_json <- httr::content(res, as = "text", encoding = "UTF-8")
    data_list <- jsonlite::fromJSON(data_json)
    
    if (length(data_list$results) == 0) break
    results[[page + 1]] <- data_list$results
    page <- page + 1
  }
  
  dplyr::bind_rows(results)
}

#' Load College Scorecard data, prioritizing a local cache file
#'
#' @param api_key Your api.data.gov API key.
#' @param fields_str A comma-separated string of fields to request.
#' @return A data frame containing the raw school data.
load_scorecard_data <- function(api_key, fields_str) {
  # Ensure the 'data' directory exists
  if (!dir.exists("data")) {
    dir.create("data")
  }
  raw_file_path <- "data/college_scorecard_raw.csv"
  
  if (file.exists(raw_file_path)) {
    message("✅ Found existing CSV file. Loading from disk...")
    df_raw <- read.csv(raw_file_path, stringsAsFactors = FALSE)
  } else {
    message("⚙️  No local CSV found. Fetching data from API...")
    df_raw <- get_all_schools(api_key, fields_str, per_page = 100)
    
    # Save raw data for reproducibility
    write.csv(df_raw, raw_file_path, row.names = FALSE)
    message(paste0("💾 Data fetched and saved to '", raw_file_path, "'"))
  }
  return(df_raw)
}