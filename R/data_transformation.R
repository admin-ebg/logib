#' Compute age values
#'
#' Computes the age given a birthyear or a birthdate
#'
#' @param x a string or number vector to be transformed
#' @param age_spec a string indicating the age specification, can be one of
#' \code{NULL}, \code{"age"}, \code{"birthyear"}, or \code{"date_of_birth"}. If
#' this parameter is set to \code{NULL}, the function automatically tries to
#' infers the specification
#' @param reference_year a number indicating the reference year in order to
#' compute the age from a birthyear or birthdate. If \code{age_spec} is
#' \code{"age"}, this parameter can be ignored.
#'
#' @return a numeric vector of ages
#'
#' @keywords internal
compute_age <- function(x, age_spec = NULL, reference_year = NULL) {
  if (is.null(age_spec)) {
    if (is.numeric(x)) {
      # If x is numeric, it must be either age or birthyear
      digits <- nchar(trunc(x))
      if (all(digits == 4)) {
        age <- compute_age(x, "birthyear", reference_year)
      } else if (all(digits < 4)) {
        age <- x
      } else {
        stop(paste0("The format of the 'age' column could not be identified, ",
                    "please use 'age_spec' to specify the correct format."))
      }
    } else {
      age <- compute_age(x, "date_of_birth", reference_year)
    }
  } else {
    if (age_spec == "age") {
      age <- x
    } else if (age_spec == "birthyear") {
      if (is.null(reference_year)) {
        stop("Please specify a 'reference_year' for the age computation.")
      }
      age <- reference_year - x
    } else if (age_spec == "date_of_birth") {
      if (is.null(reference_year)) {
        stop("Please specify a 'reference_year' for the age computation.")
      }
      # Try to infer the date format
      age <- reference_year - lubridate::year(
        lubridate::parse_date_time(x, orders = c("dmy", "mdy", "ymd")))
    }
  }
  age
}

#' Compute years_of_service value
#'
#' Computes the years of service given an entry date or entry year
#'
#' @param x a string or number vector to be transformed
#' @param entry_date_spec a string indicating the entry_date specification, can
#' be one of \code{NULL}, \code{"years"}, \code{"entry_year"}, or
#' \code{"entry_date"}. If this parameter is set to \code{NULL}, the function
#' automatically tries to infers the specification
#' @param reference_year a number indicating the reference year in order to
#' compute the years of service from an entry date. If \code{entry_date_spec} is
#' \code{"years"}, this parameter can be ignored.
#' @param reference_month a number indicating the reference month in order to
#' compute the years of service from an entry date. If \code{entry_date_spec} is
#' \code{"years"} or \code{"entry_years"}, this parameter can be ignored.
#'
#' @return a numeric vector of years of service
#'
#' @keywords internal
compute_years_of_service <- function(x, entry_date_spec = NULL,
                                     reference_year = NULL,
                                     reference_month = NULL) {
  if (is.null(entry_date_spec)) {
    if (is.numeric(x)) {
      # If x is numeric, it must be either years or entry_year
      digits <- nchar(trunc(x))
      if (all(digits == 4)) {
        yos <- compute_years_of_service(x, "entry_year", reference_year)
      } else if (all(digits < 4)) {
        yos <- x
      } else {
        stop(paste0("The format of the 'entry_date' column could not be ",
                    "identified, please use 'entry_date_spec' to specify the ",
                    "correct format."))
      }
    } else {
      yos <- compute_years_of_service(x, "entry_date", reference_year)
    }
  } else {
    if (entry_date_spec == "years") {
      yos <- x
    } else if (entry_date_spec == "entry_year") {
      if (is.null(reference_year)) {
        stop(paste0("Please specify a 'reference_year' for the years of ",
                    "service computation."))
      }
      yos <- reference_year - x
    } else if (entry_date_spec == "entry_date") {
      if (is.null(reference_year)) {
        stop(paste0("Please specify a 'reference_year' for the years of ",
                    "service computation."))
      }
      ref_date <- ifelse(reference_month == 12,
        as.Date(paste0(reference_year + 1, "-01-01")),
        as.Date(paste0(reference_year, "-", reference_month + 1, "-01")))
      # Infer date format and compute YEARFRAC as in Excel
      x <- lubridate::parse_date_time(x, orders = c("dmy", "mdy", "ymd"))
      yos <- sapply(x, function(y) yearfrac(y, ref_date))
    }
  }
  yos
}

#' Transform a data.frame according to the requirements of the model
#'
#' Transforms specific columns of a data.frame to match the requirements of
#' the standard analysis model.
#'
#' @param data a dataframe object as produced by \code{read_data} which is to
#' be transformed
#' @param reference_year a number indicating the reference year of the analysis
#' @param female_spec a string or number indicating the way females are
#' specified in the dataset.
#' @param male_spec a string or number indicating the way males are
#' specified in the dataset
#' @param age_spec a string indicating the age specification, can be one of
#' \code{NULL}, \code{"age"}, \code{"birthyear"}, or \code{"date_of_birth"}. If
#' this parameter is set to \code{NULL}, the function automatically tries to
#' infers the specification
#' @param entry_date_spec a string indicating the entry_date specification, can
#' be one of \code{NULL}, \code{"years"}, \code{"entry_year"}, or
#' \code{"entry_date"}. If this parameter is set to \code{NULL}, the function
#' automatically tries to infers the specification
transform_data <- function(data, reference_year, female_spec = "F",
                           male_spec = "M", age_spec = NULL,
                           entry_date_spec = NULL) {
  # At this stage, the specifications must be OK as they are being checked in
  # the prepare_data() function
  data$sex <- factor(data$sex, levels = c(male_spec, female_spec),
                     labels = c("M", "F"))
  data$skill_level <- factor(data$skill_level)
  data$professional_position <- factor(data$professional_position)

  # Transform NA salary components and workload components to zero
  for (col in c("basic_wage", "allowances", "monthly_wage_13",
                "special_payments", "activity_rate", "paid_hours")) {
    data[is.na(data[, col]), col] <- 0
  }
  # Compute age and years of service given the specifications
  data$age <- compute_age(data$age, age_spec, reference_year)
  data$years_of_service <- compute_years_of_service(data$years_of_service,
                                                    entry_date_spec,
                                                    reference_year)
  # Transform training category to years of training and create years of earning
  training_years <- c(17, 15, 14, 15, 13, 12, 11, 7)
  data$years_of_training <- training_years[data$training]
  data$years_of_earning <- sapply(data$age - data$years_of_training - 6,
                                  function(x) max(x, 0))
  data$years_of_earning2 <- data$years_of_earning^2

  # Get most frequent weekly workhours for standardization (highest in case of
  # equality)
  mfww <- max(as.numeric(names(which(table(data$weekly_hours) == max(table(
    data$weekly_hours))))))
  # Special case when there are no weekly hours and only hourly contracts
  if (mfww == 0) {
    mfww <- max(as.numeric(names(which(table(data$annual_hours) == max(table(
      data$annual_hours)))))) / 52
  }

  # Build an FTE column for salary standardization
  data$fte <- (data$weekly_hours * data$activity_rate) / (100 * mfww)
  data$fte[data$fte == 0] <- (12 * data$paid_hours[data$fte == 0]) / (52 * mfww)

  # Standardize and compute the total standardized salary
  data$standardized_basic_wage <- data$basic_wage / data$fte
  data$standardized_allowances <- data$allowances / data$fte
  data$standardized_monthly_wage_13 <- data$monthly_wage_13 / data$fte
  data$standardized_special_payments <- data$special_payments / data$fte
  data$standardized_salary <- data$standardized_basic_wage +
    data$standardized_allowances + data$standardized_monthly_wage_13 +
    data$standardized_special_payments

  data
}
