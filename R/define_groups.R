# R/define_groups.R

#' Add comparison group labels to the school data
#'
#' @param df A cleaned data frame.
#' @return A data frame with new columns for identifying groups.
add_comparison_groups <- function(df) {
  ivy_names <- c(
    "Brown University", "Columbia University in the City of New York", "Cornell University",
    "Dartmouth College", "Harvard University", "University of Pennsylvania",
    "Princeton University", "Yale University"
  )
  
  fl_public_patterns <- c(
    "university of florida$", "florida state university$", "university of south florida",
    "university of central florida$", "florida international university$",
    "florida atlantic university$", "florida gulf coast university$",
    "university of north florida$", "florida agricultural and mechanical university$",
    "florida polytechnic university$", "new college of florida$",
    "university of west florida$"
  )
  
  df <- df %>%
    dplyr::mutate(
      name_norm = stringr::str_squish(stringr::str_to_lower(name)),
      is_ivy = name %in% ivy_names,
      is_fl_public = stringr::str_detect(name_norm, stringr::str_c(fl_public_patterns, collapse = "|")),
      is_ncf = name_norm == "new college of florida",
      is_public_nat = ownership == 1,
      is_private_np_nat = ownership == 2,
      group_label = dplyr::case_when(
        is_ncf ~ "New College of Florida",
        is_ivy ~ "Ivy League",
        is_fl_public ~ "Florida Public",
        is_private_np_nat ~ "Private Nonprofit (National)",
        is_public_nat ~ "Public (National)",
        TRUE ~ "Other"
      )
    )
  
  # Sanity check
  message("Group labeling complete. Counts per group:")
  print(table(df$group_label))
  
  return(df)
}