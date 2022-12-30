# library(ggplot2)
#
input <- tibble::tribble(
  ~vin,~make,~model,~duty,~powertrain,~model_year,~purchase_year,~initial_cost,~depreciation_rate,~annual_vmt,~efficiency,~fuel,~fuel_cost,~maint_rate,~co2_rate,~term,~type,
  "V59HLGJF","SUBARU","FORESTER","LD","ICEV",2019,2020,50000,.1,14000,32,"GAS",3.418,0.05,8.8,12,"PURCHASE",
  "HFG88068","HONDA","ODYSSEY","LD","ICEV",2018,2020,40000,.1,15000,26,"GAS",3.418,0.10,8.8,12,"PURCHASE",
  "LAS8Y923","HONDA","ODYSSEY","LD","ICEV",2018,2020,40000,.1,NA_integer_,26,"GAS",3.418,0.10,8.8,12,"PURCHASE",
  "DFHAHQ23","HONDA","ODYSSEY","LD","ICEV",NA_integer_,NA_integer_,40000,.1,NA_integer_,26,"GAS",3.418,0.10,8.8,12,NA_character_,
  "001","HONDA","ODYSSEY",NA_character_,"ICEV",NA_integer_,NA_integer_,40000,.1,NA_integer_,26,"GAS",3.418,0.10,8.8,12,NA_character_,
)
#
#
#
# input %>%
#   full_term() %>%
#   ggplot(aes(x = year, y = tco, fill = vin)) +
#   geom_bar(stat = "identity", position = position_dodge()) +
#   geom_text(aes(label = round(tco, 0)), fontface = "bold", vjust = 1.5,
#             position = position_dodge(.9), size = 4)
