
# figure out how to set configs, i.e. set rigid to TRUE for all

get_annual_vmt <- function(.data, rigid = FALSE) {

  join_func <- function(rigid) {
    if (rigid) {
      dplyr::inner_join
    } else {
      dplyr::left_join
    }
  }

  .data %>%
    join_func(annual_vmt_df,
              by = "duty")

}

# Vehicles should be replaced at 200000
get_term <- function(.data) {



}



}


