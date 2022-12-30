
# figure out how to set configs, i.e. set rigid to TRUE for all

# Should fill in with VIN decoder first, then use this
#' Filling in value for missing duty
#'
#' @description Fills in a value for duty for vehicle's missing it. Assumes
#' light duty.
#'
#' @param .data Dataframe of vehicle data
#'
#' @examples \dontrun{
#' get_duty(vehicle_data)
#' }
#' @export
get_duty <- function(.data) {
  # Assuming LD as this is a residential calculator
  .data %>%
    dplyr::mutate(duty_fill = "LD",
                  duty = dplyr::coalesce(duty, duty_fill)) %>%
    dplyr::select(-duty_fill)
}

#' Filling in Value for Missing Type
#'
#' @description Assigns a value for purchase type for vehicles missing a value,
#' assumes a default of "PURCHASE"
#' @param .data Dataframe of vehicle data
#'
#' @examples \dontrun{
#' get_type(vehicle_data)
#' }
#' @export
get_type <- function(.data) {
  # assuming purchased
  .data %>%
    dplyr::mutate(type_fill = "PURCHASE",
                  type = dplyr::coalesce(type, type_fill)) %>%
    dplyr::select(-type_fill)
}

#' Fill in Missing Cost
#'
#' @description Fills in value for vehicles missing purchase / leasing price,
#' uses the internal \code{initial_cost_df}.
#' @param .data Dataframe of vehicle data
#' @examples \dontrun{
#' get_initial_cost(vehicle_data)
#' }
#' @export
get_initial_cost <- function(.data) {

  .data %>%
    dplyr::left_join(initial_cost_df,
                     by = c("duty", "powertrain", "type")) %>%
    dplyr::mutate(initial_cost = dplyr::coalesce(initial_cost, initial_cost_fill)) %>%
    dplyr::select(-initial_cost_fill) %>%
    dplyr::left_join(initial_cost_df %>%
                       dplyr::group_by(duty) %>%
                       dplyr::summarise(initial_cost_fill =  mean(initial_cost_fill))
                     %>%
                       dplyr::ungroup(),
                     by = "duty") %>%
    dplyr::mutate(initial_cost = dplyr::coalesce(initial_cost, initial_cost_fill)) %>%
    dplyr::select(-initial_cost_fill)
}

#' Fill in Model Year
#' @description Adds a value for moddel year for vehicles missing this value,
#' assigns current year as the value
#' @param .data Dataframe of vehicle data
#' @examples \dontrun{
#' get_model_year(vehicle_data)
#' }
get_model_year <- function(.data) {
  .data %>%
    dplyr::mutate(model_year_fill = format(Sys.Date(), format = "%Y") %>% as.integer(),
                  model_year = dplyr::coalesce(model_year, model_year_fill)) %>%
    dplyr::select(-model_year_fill)
}

#' Fill in Purchase Year
#' @description Fill in a value for vehicles missing a purchase year, assumes
#' that it's the same as model year
#' @param .data Dataframe of vehicle data
#' @examples \dontrun{
#' get_purchase_year(vehicle_data)
#' }
#' @export
get_purchase_year <- function(.data) {
  .data %>%
    dplyr::mutate(purchase_year_fill = model_year,
                  purchase_year = dplyr::coalesce(purchase_year, purchase_year_fill)) %>%
    dplyr::select(-purchase_year_fill)
}

#' Fill in Annual Vehicle Milles Traveled
#' @export
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

#' Fill in Term
#' @export
get_term <- function(.data, max_vmt = 200000) {
  .data %>%
    dplyr::mutate(term_fill = 200000 %/% annual_vmt,
                  term = dplyr::coalesce(term, term_fill)) %>%
    dplyr::select(-term_fill)
}

# filter to rows with missing data, fill in, bind back oninp

#' Fill in All Missing Values
#' @export
complete_input <- function(.data,
                           waterfall  = list(
                             get_duty,
                             get_type,
                             get_model_year,
                             get_purchase_year,
                             get_annual_vmt,
                             get_term
                           )
) {

  apply_waterfall <- function(x, funcs = waterfall) {
    for(f in funcs) {
      x <- f(x)
    }
    x
  }
  .data %>%
    dplyr::mutate(rid = dplyr::row_number()) %>%
    dplyr::filter(dplyr::if_all(dplyr::everything(), ~ !is.na(.))) %>%
    rbind(
      .data %>%
        dplyr::mutate(rid = dplyr::row_number()) %>%
        dplyr::filter(dplyr::if_any(dplyr::everything(), ~ is.na(.))) %>%
        apply_waterfall()
    ) %>%
    dplyr::arrange(rid) %>%
    dplyr::select(-rid)

}


