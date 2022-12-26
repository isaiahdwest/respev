###############################################################################
#                                                                             #
#     Script for calculating term values for each vehicle                     #
#                                                                             #
###############################################################################

# User input repl year
# calculated based on age / term
# right now
# adjust vehicle price depreciation rate ---

# compare new vehicle of existing make model OR of a different vehicle

ann_fuel <- function(vmt, eff) {
  vmt / eff
}

add_annual_fuel <- function(.data) {

  if ("annual_fuel" %in% names(.data)) {
    .data
  } else {
    .data %>%
      dplyr::mutate(annual_fuel = ann_fuel(annual_vmt, efficiency))
  }

}

seq_years <- function(.data) {

  if ("years" %in% names(.data)) {
    .data
  } else {
    .data %>%
      dplyr::group_by(vin) %>%
      dplyr::mutate(years = paste0("year_", seq_len(term)) %>% list()) %>%
      tidyr::unnest(years) %>%
      dplyr::ungroup()
  }

}

cost_seq <- function(.data) {

  .data %>%
    add_annual_fuel() %>%
    seq_years() %>%
    dplyr::group_by(vin) %>%
    dplyr::mutate(
      residual_value = dplyr::case_when(
        type == "PURCHASE" ~ initial_cost * (1- depreciation_rate) ^ dplyr::row_number(),
        TRUE ~ 0
      ),
      initial_cost = dplyr::case_when(
        type == "PURCHASE" & years == "year_1" ~ initial_cost,
        type == "LEASE" ~ initial_cost,
        TRUE ~ 0
      ),
      cost_maint = annual_vmt * maint_rate,
      cost_fuel = fuel_cost * annual_fuel
    ) %>%
    dplyr::ungroup()

}

ghg_seq <- function(.data) {

  if ("annual_co2" %in% names(.data)) {
    .data
  } else {

    .data %>%
      add_annual_fuel() %>%
      seq_years() %>%
      dplyr::group_by(vin) %>%
      dplyr::mutate(annual_co2 = co2_rate * annual_fuel / 1000) %>%
      dplyr::ungroup()
  }

}

get_repl_year <- function(.data) {
  if ("repl_year" %in% names(.data)) {
    .data
  } else {
    .data %>%
      dplyr::group_by(vin) %>%
      dplyr::mutate(
        repl_year = model_year + term
      ) %>%
      dplyr::ungroup()
  }
}

add_years <- function(.data) {
  .data %>%
    get_repl_year() %>%
    dplyr::group_by(vin) %>%
    dplyr::mutate(year = purchase_year + dplyr::row_number() - 1) %>%
    dplyr::ungroup()
}

get_tco <- function(.data) {

  if ("tco_year" %in% names(.data)) {
    .data
  } else {

    .data %>%
      dplyr::group_by(vin) %>%
      dplyr::mutate(tco_year = dplyr::case_when(
        years == paste0("year_", term) & type == "PURCHASE" ~ initial_cost +  cost_maint + cost_fuel - residual_value,
        TRUE ~ initial_cost +  cost_maint + cost_fuel
      )) %>%
      dplyr::ungroup()
  }
}

get_cum_tco <- function(.data) {

  .data %>%
    get_tco() %>%
    dplyr::group_by(vin) %>%
    dplyr::mutate(tco = cumsum(tco_year)) %>%
    dplyr::ungroup()

}

get_cum_ghg <- function(.data) {

  .data %>%
    ghg_seq() %>%
    dplyr::group_by(vin) %>%
    dplyr::mutate(ghg_cum = cumsum(annual_co2)) %>%
    dplyr::ungroup()

}

#' Calculate GHG emissions and TCO of a vehicle(s) over its term
#' @param .data vehicle input data, one row per vehicle
#' @export
full_term <- function(.data) {
  .data %>%
    cost_seq() %>%
    ghg_seq() %>%
    add_years() %>%
    get_cum_tco() %>%
    get_cum_tco()

}

