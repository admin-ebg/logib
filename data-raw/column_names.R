all_column_names <- list(
  code = c("personal_number", "age", "sex", "years_of_service", "training",
           "professional_function", "skill_level", "professional_position",
           "activity_rate", "paid_hours", "basic_wage", "allowances",
           "monthly_wage_13", "special_payments", "weekly_hours",
           "annual_hours", "population", "comments", "supplement1",
           "supplement2", "supplement3", "supplement4", "supplement5"),
  datalist = list(
    de = names(readxl::read_excel("data-raw/Datalist_d.xlsx")),
    en = names(readxl::read_excel("data-raw/Datalist_e.xlsx")),
    fr = names(readxl::read_excel("data-raw/Datalist_f.xlsx")),
    it = names(readxl::read_excel("data-raw/Datalist_i.xlsx"))
  ),
  data_export = list(
    de = names(readxl::read_excel("data-raw/Exportfile_d.xlsx", sheet = 3)),
    en = names(readxl::read_excel("data-raw/Exportfile_e.xlsx", sheet = 3)),
    fr = names(readxl::read_excel("data-raw/Exportfile_f.xlsx", sheet = 3)),
    it = names(readxl::read_excel("data-raw/Exportfile_i.xlsx", sheet = 3))
  )
)

usethis::use_data(all_column_names, internal = TRUE, overwrite = TRUE)
