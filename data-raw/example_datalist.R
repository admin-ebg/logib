datalist_example <- read_data("data-raw/Example_data_sheet_M1.xlsx")
datalist_example$weekly_hours <- 40

usethis::use_data(datalist_example, overwrite = TRUE)
