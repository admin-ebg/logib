all_column_names <- list(
  code = c("personal_number", "age", "sex", "years_of_service", "training",
           "professional_function", "level_of_requirements", "professional_position",
           "activity_rate", "paid_hours", "basic_wage", "allowances",
           "monthly_wage_13", "special_payments", "weekly_hours",
           "annual_hours", "population", "comments", "supplement1",
           "supplement2", "supplement3", "supplement4", "supplement5"),
  datalist = list(
    de = names(readxl::read_excel("data-raw/datalist_de.xlsx", sheet = 2)),
    en = names(readxl::read_excel("data-raw/datalist_en.xlsx", sheet = 2)),
    fr = names(readxl::read_excel("data-raw/datalist_fr.xlsx", sheet = 2)),
    it = names(readxl::read_excel("data-raw/datalist_it.xlsx", sheet = 2))
  ),
  data_export = list(
    de = names(readxl::read_excel("data-raw/Exportfile_M1_de.xlsx", sheet = 3)),
    en = names(readxl::read_excel("data-raw/Exportfile_M1_en.xlsx", sheet = 3)),
    fr = names(readxl::read_excel("data-raw/Exportfile_M1_fr.xlsx", sheet = 3)),
    it = names(readxl::read_excel("data-raw/Exportfile_M1_it.xlsx", sheet = 3))
  )
)

usethis::use_data(all_column_names, internal = FALSE, overwrite = TRUE)
