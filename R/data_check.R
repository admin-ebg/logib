#' Check a dataframe
#'
#' \code{check_data} checks a dataframe (as produced by
#' \code{\link{read_data}}).
#'
#' This function checks a dataframe (as produced by \code{read_data}
#' for correctness and consistency)
#'
#' @param data data.frame to be checked
#'
#' @return a data.frame with information concerning each incorrect data point
#' in the \code{data} data.frame
#'
#' @keywords internal
#'
check_data <- function(data) {
  # Build a dataframe which will contain all errors found in the data as well
  # as an index vector for the row names
  errors <- data.frame()
  idx <- rownames(data)

  # ----- Missing values check -------------------------------------------------

  # Check for missing values of all obligatory columns
  obligatory_columns <- c("personal_number", "age", "sex", "years_of_service",
                          "training", "professional_function", "level_of_requirements",
                          "professional_position", "basic_wage", "population")
  for (col in obligatory_columns) {
    error_rows <- as.numeric(idx[is.na(data[, col])])
    errors <- rbind(
      errors,
      build_errors(error_rows, data$personal_number[error_rows],
                   rep(NA, length(error_rows)), col,
                   paste0(
                     "Missing '",
                     paste0(
                       toupper(substr(gsub("_", " ",
                                           col), 1, 1)),
                       tolower(substr(gsub("_", " ",
                                           col), 2, nchar(gsub("_", " ", col))))
                     ), "'"), 1)
    )
  }

  # ----- Correct values check -------------------------------------------------

  # Check for non-unique personal_number
  error_rows <- as.numeric(idx[duplicated(data$personal_number) |
                      duplicated(data$personal_number, fromLast = TRUE)])
  errors <- rbind(errors,
                  build_errors(error_rows, data$personal_number[error_rows],
                               data$personal_number[error_rows],
                               "personal_number", "Duplicate 'personal number'",
                               1))

  # Check for non-integer age
  error_rows <- as.numeric(idx[data$age != as.integer(data$age)])
  errors <- rbind(errors,
                  build_errors(error_rows, data$personal_number[error_rows],
                               data$age[error_rows], "age",
                               "'Age' is not a whole number", 1))

  # Check for age limits (>= 13, <= 100)
  error_rows <- as.numeric(idx[data$age < 13 | data$age > 100])
  errors <- rbind(errors,
                  build_errors(error_rows, data$personal_number[error_rows],
                               data$age[error_rows], "age",
                               "'Age' is not between 13 and 100", 1))

  # Check for plausible age limits (>= 15, <= 70)
  error_rows <- as.numeric(idx[data$age < 15 | data$age > 70])
  errors <- rbind(errors,
                  build_errors(error_rows, data$personal_number[error_rows],
                               data$age[error_rows], "age",
                               "'Age' is not between 15 and 70", 2))

  # Check for incorrect sex (neither F nor M)
  error_rows <- as.numeric(idx[!(data$sex %in% c("F", "M"))])
  errors <- rbind(errors,
                  build_errors(error_rows, data$personal_number[error_rows],
                               data$sex[error_rows], "sex",
                               "'Sex' is neither 'F' nor 'M'", 1))

  # Check for years of service limits (>= 0, <= 85)
  error_rows <- as.numeric(idx[data$years_of_service < 0 |
                                 data$years_of_service > 85])
  errors <- rbind(errors,
                  build_errors(error_rows, data$personal_number[error_rows],
                               data$years_of_service[error_rows],
                               "years_of_service",
                               "'Years of service' is not between 0 and 85", 1))

  # Check for plausible years of service limits (>= 0, <= 55)
  error_rows <- as.numeric(idx[data$years_of_service > 55 &
                                 data$years_of_service <= 85])
  errors <- rbind(errors,
                  build_errors(error_rows, data$personal_number[error_rows],
                               data$years_of_service[error_rows],
                               "years_of_service",
                               "'Years of service' is more than 55", 2))

  # Check for entry age
  error_rows <- as.numeric(idx[data$age - data$years_of_service < 13])
  errors <- rbind(errors,
                  build_errors(error_rows, data$personal_number[error_rows],
                               data$age[error_rows] - data$years_of_service[error_rows], "entry_age",
                               "Entry age ('Age' - 'Years of service') is less than 13", 1))

  # Check for implausible entry age
  error_rows <- as.numeric(idx[data$age - data$years_of_service < 16 &
                                 data$age - data$years_of_service >= 13])
  errors <- rbind(errors,
                  build_errors(error_rows, data$personal_number[error_rows],
                               data$age[error_rows] - data$years_of_service[error_rows], "entry_age",
                               "Entry age ('Age' - 'Years of service') is less than 16", 2))

  # Check for wrong training/education values
  error_rows <- as.numeric(idx[!(data$training %in% 1:8)])
  errors <- rbind(errors,
                  build_errors(error_rows, data$personal_number[error_rows],
                               data$training[error_rows],
                               "training", "'Training' is not between 1 and 8",
                               1))

  # Check for FTE limits (0 - 150% or 0 - 300 hours)
  error_rows <- as.numeric(idx[data$activity_rate > 150 | data$activity_rate < 0])
  errors <- rbind(errors,
                  build_errors(error_rows, data$personal_number[error_rows],
                               data$activity_rate[error_rows], "activity_rate",
                               "'Activity rate' is not between 0% and 150%", 1))
  error_rows <- as.numeric(idx[data$activity_rate > 100 & data$activity_rate <= 150])
  errors <- rbind(errors,
                  build_errors(error_rows, data$personal_number[error_rows],
                               data$activity_rate[error_rows], "activity_rate",
                               "'Activity rate' is above 100%", 2))
  error_rows <- as.numeric(idx[data$paid_hours > 300 | data$paid_hours < 0])
  errors <- rbind(errors,
                  build_errors(error_rows, data$personal_number[error_rows],
                               data$paid_hours[error_rows], "paid_hours",
                               "'Paid hours' is not between 0 and 300", 1))
  error_rows <- as.numeric(idx[data$paid_hours >= 220 & data$paid_hours <= 300])
  errors <- rbind(errors,
                  build_errors(error_rows, data$personal_number[error_rows],
                               data$paid_hours[error_rows], "paid_hours",
                               "'Paid hours' is more than 220", 2))

  # Check for activity_rate and paid_hours to be both non-zero
  error_rows <- as.numeric(idx[data$activity_rate > 0 & data$paid_hours > 0])
  errors <- rbind(errors,
                  build_errors(error_rows, data$personal_number[error_rows],
                               data$paid_hours[error_rows], "paid_hours",
                               "'Activity rate' and 'paid hours' are both non-zero", 1))

  # Check for non-positive basic wage
  error_rows <- as.numeric(idx[data$basic_wage < 0])
  errors <- rbind(errors,
                  build_errors(error_rows, data$personal_number[error_rows],
                               data$basic_wage[error_rows], "basic_wage",
                               "'Basic wage' is negative", 1))
  error_rows <- as.numeric(idx[data$basic_wage == 0 & (data$activity_rate > 0 |
                                                         data$paid_hours > 0)])
  errors <- rbind(errors,
                  build_errors(error_rows, data$personal_number[error_rows],
                               data$basic_wage[error_rows], "basic_wage",
                               "'Basic wage' is zero", 1))

  # Check for negative allowances
  error_rows <- as.numeric(idx[data$allowances < 0])
  errors <- rbind(errors,
                  build_errors(error_rows, data$personal_number[error_rows],
                               data$allowances[error_rows], "allowances",
                               "'Allowances' are negative", 1))

  # Check for negative 13th monthly wage
  error_rows <- as.numeric(idx[data$monthly_wage_13 < 0])
  errors <- rbind(errors,
                  build_errors(error_rows, data$personal_number[error_rows],
                               data$monthly_wage_13[error_rows],
                               "monthly_wage_13",
                               "'13th monthly wage' is negative", 1))

  # Check for 13th monthly wage exceeding 25% of the basic wage
  error_rows <- as.numeric(idx[data$monthly_wage_13 > 0.25*data$basic_wage])
  errors <- rbind(errors,
                  build_errors(error_rows, data$personal_number[error_rows],
                               data$monthly_wage_13[error_rows],
                               "monthly_wage_13",
                               paste0("'13th monthly wage' exceeds 25% of the ",
                                      "'basic wage'"), 2))

  # Check for 13th monthly wage undercutting 8% of the basic wage
  error_rows <- as.numeric(idx[data$monthly_wage_13 < 0.08*data$basic_wage])
  errors <- rbind(errors,
                  build_errors(error_rows, data$personal_number[error_rows],
                               data$monthly_wage_13[error_rows],
                               "monthly_wage_13",
                               paste0("'13th monthly wage' is less than 8% of the ",
                                      "'basic wage'"), 2))

  # Check for negative special payments
  error_rows <- as.numeric(idx[data$special_payments < 0])
  errors <- rbind(errors,
                  build_errors(error_rows, data$personal_number[error_rows],
                               data$special_payments[error_rows],
                               "special_payments",
                               "'Special payments' are negative", 1))

  # Check for weekly hours range (>= 1, <= 100)
  error_rows <- as.numeric(idx[data$weekly_hours < 1 | data$weekly_hours > 100])
  errors <- rbind(errors,
                  build_errors(error_rows, data$personal_number[error_rows],
                               data$weekly_hours[error_rows],
                               "weekly_hours",
                               "'Weekly hours' are not between 1 and 100", 1))

  # Check for plausible weekly hours range (>= 1, <= 50)
  error_rows <- as.numeric(idx[data$weekly_hours > 50])
  errors <- rbind(errors,
                  build_errors(error_rows, data$personal_number[error_rows],
                               data$weekly_hours[error_rows],
                               "weekly_hours", "'Weekly hours' are above 50",
                               2))

  # Check for wrong population values
  error_rows <- as.numeric(idx[!(data$population %in% 1:5)])
  errors <- rbind(errors,
                  build_errors(error_rows, data$personal_number[error_rows],
                               data$population[error_rows],
                               "population",
                               "'Population' is not between 1 and 5", 1))

  # Check for combinations of level_of_requirements and professional_position
  error_rows <- as.numeric(idx[data$level_of_requirements %in% 6:8 &
                                 data$professional_position %in% 1:2])
  errors <- rbind(errors,
                  build_errors(error_rows, data$personal_number[error_rows],
                               data$level_of_requirements[error_rows],
                               "level_of_requirements",
                               paste("Unusual combination: high professional",
                                      "position and low level of requirements"), 2))

  # Check for combinations of level_of_requirements and training
  error_rows <- as.numeric(idx[data$level_of_requirements %in% 1:3 &
                                 data$training %in% 7:8])
  errors <- rbind(errors,
                  build_errors(error_rows, data$personal_number[error_rows],
                               data$level_of_requirements[error_rows],
                               "level_of_requirements",
                               paste("Unusual combination: high level of",
                                     "requirements and low training"), 2))

  # Check for incorrect level or requirements
  error_rows <- as.numeric(idx[!(data$level_of_requirements %in% 1:8)])
  errors <- rbind(errors,
                  build_errors(error_rows, data$personal_number[error_rows],
                               data$level_of_requirements[error_rows],
                               "level_of_requirements",
                               paste("Level of requirements is not an integer",
                                     "between 1 and 8"), 1))

  # Check for incorrect professional position
  error_rows <- as.numeric(idx[!(data$professional_position %in% 1:5)])
  errors <- rbind(errors,
                  build_errors(error_rows, data$personal_number[error_rows],
                               data$professional_position[error_rows],
                               "professional_position",
                               paste("Professional position is not an integer",
                                     "between 1 and 5"), 1))

  if(nrow(errors) > 0){
    return(errors[order(errors$row), ])
  }
  if(nrow(errors) == 0){
    return(errors)
  }
}

#' Builds a dataframe of errors
#'
#' \code{build_errors} builds a dataframe of errors as used by the function
#' \code{check_data}.
#'
#' @param rows a vector of numbers representing the rows which contain an error
#' @param pers_id a vector of strings of the personal ID which contain an error
#' @param vals a vector of the erroneous values
#' @param description the description of the error occurring
#' @param column the name of the column containing the error
#' @param importance the importance of the error occurring
#'
#' @return a dataframe of errors with the columns \code{column},
#' \code{description}, \code{importance}
#'
#' @keywords internal
#'
build_errors <- function(rows, pers_id, vals, column, description, importance) {
  n_rows <- length(rows)
  if (n_rows == 0) return(NULL)
  data.frame(row = rows, pers_id = pers_id, value = vals,
             column = rep(column, n_rows),
             description = rep(description, n_rows),
             importance = rep(importance, n_rows))
}
