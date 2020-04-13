#' Run a Salary Analysis
#'
#' Runs a salary analysis according to the Swiss standard analysis model
#'
#' @param
#' TODO: specify params
#'
#' @return object of type \code{standard_analysis_model} with the following
#' elements
#' \itemize{
#'    \item{\code{original_data}: }{The original data passed by the user in the
#'    \code{data} parameter}
#'    \item{\code{clean_data}: }{The cleaned up data which was used for the
#'    analysis}
#'    \item{\code{results}: }{The result of the standard analysis model}
#'
#' }
#'
#' @examples
#' TODO: add examples
#'
#' @export
standard_analysis <- function(data, reference_year, female_spec = "F",
                              male_spec = "M", age_spec = NULL,
                              entry_date_spec = NULL,
                              ignore_plausibility_check = FALSE,
                              prompt_data_cleanup = TRUE) {
  original_data <- data
  data <- prepare_data(data, reference_year, female_spec, male_spec, age_spec,
                       entry_date_spec, ignore_plausibility_check,
                       prompt_data_cleanup)
  results <- estimate_standard_analysis_model(data)


  output <- list(original_data = original_data,
                 clean_data = data,
                 results = results)
  class(output) <- "standard_analysis_model"
  output

}

#' Summary of the Salary Analysis
#'
#' Summary of an estimated salary analysis object of class
#' \code{standard_analysis_model}
#'
#' TODO: Description
#'
#' @param object estimated salary analysis object of class
#' \code{standard_analysis_model}
#'
#' @examples
#' # Estimate standard analysis model
#' result <- standard_analysis(data, 2020)
#'
#' # Show summary of the salary analysis
#' summary(result)
#'
#' @export
summary.standard_analysis_model <- function(object) {
  # Compute Kennedy estimate
  coef_sexF <- object$results$coefficients[length(object$results$coefficients)]
  se_sexF   <- summary(object$results)$coefficients[nrow(
    summary(object$results)$coefficients), 2]
  kennedy_estimate <- get_kennedy_estimator(coef_sexF, se_sexF^2)

  # Print output
  cat("\nSummary of the Standard Analysis Model:\n", rep("=", 80), "\n\n",
      sep = "")
  cat("Under otherwise equal circumstances, women earn ",
      sprintf("%.1f%% ", abs(100 * kennedy_estimate)),
      ifelse(kennedy_estimate > 0, "more ", "less "),
      "than men.\n", rep("-", 80), "\n", sep = "")
  x <- summary(object$results)
  cat(rep("-", 80), "\n\n", sep = "")
}
