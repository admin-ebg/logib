# Helper function
min_r_version <- function(packages){
  list <- list()
  for(x in packages){
    list[x] <- regmatches(utils::packageDescription(x)$Depends,
                          regexec("R \\(>= ([0-9]+\\.[0-9]+(\\.[0-9]+)?)\\)",
                                  utils::packageDescription(x)$Depends))[[1]][2]
  }
  return(as.character(min(package_version(list))))
}

# Set up the DESCRIPTION file
usethis::use_description(fields = list(
  Package = "logib",
  Type = "Package",
  Title = "Salary Analysis by the Swiss Federal Office for Gender Equality",
  Version = "0.2.0",
  Description = paste(
    "Implementation of the Swiss Confederation's standard analysis model for salary analyses <https://www.ebg.admin.ch/en/equal-pay-analysis-with-logib> in R.",
    "The analysis is run at company-level and the model is intended for medium-sized and large companies. It can technically be used with 50 or more employees (apprentices, trainees/interns and expats are not included in the analysis).",
    "Employees with at least 100 employees are required by the Gender Equality Act to conduct an equal pay analysis.",
    "This package allows users to run the equal salary analysis in R, providing additional transparency with respect to the methodology and simple automation possibilities."),
  LazyData = "true",
  Depends = paste0("R (>= ", min_r_version(c("lubridate", "readxl", "testthat")), ")")),
  roxygen = TRUE)

# Add authors
usethis::use_author(
  given = "Marc",
  family = "Stöckli",
  email = "marc.stoeckli@ebg.admin.ch",
  role = c("aut", "cre")
)
usethis::use_author(
  given = "Jonathan",
  family = "Chassot",
  role = "aut"
)
usethis::use_author(
  given = "Jeremy",
  family = "Kolly",
  role = "ctb"
)
usethis::use_author(
  given = "Federal Office for Gender Equality of Switzerland",
  email = "ebg@ebg.admin.ch",
  role = c("cph", "fnd")
)

# Add license
usethis::use_gpl3_license()

# Add packages to Imports
use_package("lubridate")
use_package("readxl")
use_package("stats")
use_package("utils")

# Add packages to Suggests
use_package("testthat", type = "Suggests")
