annual_vmt_df <- data.frame(
  duty = c("LD", "MD", "HD"),
  annual_vmt_fill = c(11550, 19000, 50000),
  stringsAsFactors = FALSE
)

usethis::use_data(annual_vmt_df, internal = TRUE, overwrite = TRUE)
