annual_vmt_df <- data.frame(
  duty = c("LD", "MD", "HD"),
  annual_vmt_fill = c(11550, 19000, 50000),
  stringsAsFactors = FALSE
)

initial_cost_df <- data.frame(
  duty = c(rep("LD", 5), rep("MD", 5), rep("HD", 5)),
  powertrain = c(rep("ICEV", 3), rep("BEV", 3), rep("PHEV", 3), rep("HEV", 3), rep("FCV", 3)),
  initial_cost_fill = c(rep(50000, 5), rep(75000, 5), rep(100000, 5)),
  stringsAsFactors = FALSE
)

usethis::use_data(annual_vmt_df,
                  initial_cost_df,
                  internal = TRUE, overwrite = TRUE)
