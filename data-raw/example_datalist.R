datalist_imprimerie <- read_data("data-raw/datalist_imprimerie.xlsx")
datalist_gebaeudetechnik <- read_data("data-raw/datalist_gebaeudetechnik.xlsx")

usethis::use_data(datalist_imprimerie, datalist_gebaeudetechnik,
                  internal = TRUE, overwrite = TRUE)
