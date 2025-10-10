# R/clean_data.R

#' Clean and transform raw College Scorecard data
#'
#' @param df_raw The raw data frame loaded from the API or CSV.
#' @return A cleaned data frame with new calculated columns.
clean_scorecard_data <- function(df_raw) {
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
    dplyr::mutate(dplyr::across(c(
      ownership, sat_avg, tuition_in, tuition_out, grad_rate, earn10, debt_mdn,
      pcip_cs, pcip_eng, pcip_math, net_price_overall
    ), ~ suppressWarnings(as.numeric(.x)))) %>%
    dplyr::mutate(
      net_price = dplyr::coalesce(net_price_overall, tuition_in),
      stem_share = rowSums(dplyr::across(c(pcip_cs, pcip_eng, pcip_math), ~ dplyr::coalesce(.x, 0)), na.rm = TRUE),
      earnings_to_net = earn10 / net_price,
      earnings_adj_grad = (earn10 * grad_rate) / net_price
    )
  
  return(df)
}