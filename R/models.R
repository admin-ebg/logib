#' Standard Analysis Model
#'
#' Estimates the Swiss Confederation standard analysis model (a linear
#' regression) for salary equality between women and men.
#'
#' The standard analysis model's formula is the following:
#'
#' \code{log(standardized_salary) ~ years_of_training + years_of_service +
#' years_of_earning + years_of_earning^2 + skill_level + professional_position +
#' sex}
#'
#' The \code{sex_neutral} parameter can be used to run the sex neutral model,
#' i.e. a linear regression without the sex coefficient.
#'
#' @param data data.frame as produced by \code{\link{prepare_data}}
#' @param sex_neutral boolean indicating whether the linear regression is to be
#' run using the sex_neutral model or the standard one.
#'
#' @return an object of \code{\link{class}} "\code{\link[stats]{lm}}"
#'
#' @keywords internal
run_standard_analysis_model <- function(data, sex_neutral = FALSE) {
  # Throw an error when the minimum requirements for the standard analysis are
  # not met (i.e. less than 50 employees or less than 1 woman or man)
  if (nrow(data) < 50) {
    print(data)
    stop(paste0("There must be at least 50 valid employees to run the ",
                "standard analysis model"))
  }
  if (abs(sum(data$sex == "F") - sum(data$sex == "M")) == nrow(data)) {
    stop(paste0("There must be at least 1 woman and 1 man in the valid ",
                "employees to run the standard analysis model"))
  }
  # Handle cases where skill level or professional position have 1 level only
  # by simply setting the column to a numeric 0 (thus it will be absorbed by
  # the intercept coefficient)
  if (length(levels(data$skill_level)) == 1) {
    data$skill_level <- 0
  }
  if (length(levels(data$professional_position)) == 1) {
    data$professional_position <- 0
  }
  # Run and return the linear regression according to the sex_neutral parameter
  if (sex_neutral) {
    stats::lm(log(standardized_salary) ~ years_of_training + years_of_service +
         years_of_earning + years_of_earning2 + skill_level +
         professional_position, data = data)
  } else {
    stats::lm(log(standardized_salary) ~ years_of_training + years_of_service +
         years_of_earning + years_of_earning2 + skill_level +
         professional_position + sex, data = data)
  }
}

#' Kennedy Estimator
#'
#' Computes the consistent and almost unbiased estimator for dummy variables in
#' semilogarithmic regressions proposed by Kennedy, P.E. (1981). Estimation
#' with correctly interpreted dummy variables in semilogarithmic equations.
#' American Economic Review, 71, 801.
#'
#' Given a semilogarithmic regression with a dummy variable and its estimated
#' coefficient \code{c} with a variance \code{v}, the consistent and almost
#' unbiased estimator proposed by Kennedy is computed as
#' \code{k = exp(c) / exp(v / 2) - 1}
#'
#' @param coefficient numeric value of the estimated coefficient for a dummy
#' variable in a semilogarithmic regression
#' @param variance numeric value of the variance of this estimated coefficient
#'
#' @return a numeric value representing the so-called "Kennedy estimator"
#'
#' @keywords internal
get_kennedy_estimator <- function(coefficient, variance) {
  exp(coefficient) / exp(variance / 2) - 1
}
