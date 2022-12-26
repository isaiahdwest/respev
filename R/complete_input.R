
# figure out how to set configs, i.e. set rigid to TRUE for all

get_model_year <- function(.data) {
  .data %>%
    dplyr::mutate(model_year_fill = format(Sys.Date(), format = "%Y") %>% as.integer(),
                  model_year = dplyr::coalesce(model_year, model_year_fill)) %>%
    dplyr::select(-model_year_fill)
}

get_purchase_year <- function(.data) {
  .data %>%
    dplyr::mutate(purchase_year_fill = model_year,
                  purchase_year = dplyr::coalesce(purchase_year, purchase_year_fill)) %>%
    dplyr::select(-purchase_year_fill)
}

get_annual_vmt <- function(.data, rigid = FALSE) {

  join_func <- if (rigid) {
    dplyr::inner_join
  } else {
    dplyr::left_join
  }

  .data %>%
    join_func(annual_vmt_df,
              by = "duty") %>%
    dplyr::mutate(annual_vmt = dplyr::coalesce(annual_vmt, annual_vmt_fill)) %>%
    dplyr::select(-annual_vmt_fill)

}

# THIS IS MORE OF A CALC REPL YEAR FOR EXISTING VEHICLE
# Vehicles should be replaced at 200000
# get_term <- function(.data, max_vmt = 200000) {
#
#   .data %>%
#     mutate(years_to_repl = max_vmt %/% annual_vmt,
#            curr_yr = Sys.Date() %>% format(format = "%Y") %>% as.numeric(),
#            age = curr_yr - model_year,
#            term_fill)
#
# }

get_term <- function(.data, max_vmt = 200000) {
  .data %>%
    dplyr::mutate(term_fill = 200000 %/% annual_vmt,
                  term = dplyr::coalesce(term, term_fill)) %>%
    dplyr::select(-term_fill)
}

# filter to rows with missing data, fill in, bind back on

waterfall <- list(
  get_model_year,
  get_purchase_year,
  get_annual_vmt,
  get_term
)


