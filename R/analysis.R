#' Run a Salary Analysis
#'
#' Runs a salary analysis according to the Swiss standard analysis model
#'
#' @param data a data.frame of employees as produced by \code{read_data}
#' @param reference_year an integer representing the reference year, i.e. the
#' year for which we analyse the salaries
#' @param female_spec an optional string or numeric representing the way women
#' are encoded in the \code{data}
#' @param male_spec an optional string or numeric representing the way men are
#' encoded in the \code{data}
#' @param age_spec an optional string to specify the way \code{age} is encoded
#' in the data (\code{NULL} will try to automatically infer the age format,
#' \code{"age"} implies that the \code{age} is specified as the age of a person,
#' \code{"birthyear"} implies that the \code{age} is specified as the year of
#' birth of a person, and \code{"birthdate"} implies that the \code{age} is
#' specified as the date of birth of a person)
#' @param entry_date_spec an optional string to specify the way
#' \code{entry_date} is encoded in the data (\code{NULL} will try to
#' automatically infer the format, \code{"years"} implies that the
#' \code{entry_date} is specified as the number of years for which the person
#' has been in the company, \code{"entry_year"} implies that the
#' \code{entry_date} is specified as the year of the entry date of the person,
#' \code{"entry_date"} implies that the age is specified as the date of entry
#' of the person)
#' @param ignore_plausibility_check a boolean indicating whether the
#' plausibility of the data should be checked or whether all correct data is
#' considered plausible
#' @param prompt_data_cleanup a boolean indicating whether a prompt will pop up
#' to enforce cleaning the data until all data is correct
#'
#' @return object of type \code{standard_analysis_model} with the following
#' elements
#' \itemize{
#'    \item{\code{params}: }{The set of original parameters passed to the
#'    function}
#'    \item{\code{data_original}: }{The original data passed by the user in the
#'    \code{data} parameter}
#'    \item{\code{data_clean}: }{The cleaned up data which was used for the
#'    analysis}
#'    \item{\code{data_errors}: }{The list of errors which were found upon
#'    checking the data}
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
                              prompt_data_cleanup = FALSE) {
  params <- list(reference_year = reference_year, female_spec = female_spec,
                     male_spec = male_spec, age_spec = age_spec,
                     entry_date_spec = entry_date_spec)
  data_original <- data
  data_prepared <- prepare_data(data, reference_year, female_spec, male_spec,
                                age_spec, entry_date_spec,
                                ignore_plausibility_check, prompt_data_cleanup)
  results <- run_standard_analysis_model(data_prepared$data)
  output <- list(params = params,
                 data_original = data_original,
                 data_clean = data_prepared$data,
                 data_errors = data_prepared$errors,
                 results = results)
  class(output) <- "standard_analysis_model"
  output

}

#' Summary of the Salary Analysis
#'
#' Summary of an estimated salary analysis object of class
#' \code{standard_analysis_model}
#'
#' \code{summary.standard_analysis_model} provides a short summary of the wage
#' analysis according to the Standard Analysis Model. The summary describes the
#' number of records used for the analysis, the Kennedy estimate of the wage
#' difference under otherwise equal circumstances and the summary of the linear
#' regression.
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
  coef_sex_f <- object$results$coefficients[length(object$results$coefficients)]
  se_sex_f   <- summary(object$results)$coefficients[nrow(
    summary(object$results)$coefficients), 2]
  kennedy_estimate <- get_kennedy_estimator(coef_sex_f, se_sex_f^2)

  # Compute the number of employees total / valid for all, women and men only
  n_original <- nrow(object$data_original)
  n_f_original <- sum(object$data_original$sex == object$params$female_spec,
                      na.rm = TRUE)
  n_m_original <- sum(object$data_original$sex == object$params$male_spec,
                      na.rm = TRUE)
  n_clean <- nrow(object$data_clean)
  n_f_clean <- sum(object$data_clean$sex == "F")
  n_m_clean <- sum(object$data_clean$sex == "M")

  # Significance tests
  ratings <- c(
    paste0("The value is not statistically significant. The statistical ",
           "method does not allow a valid gender effect to be determined."),
    paste0("The value is statistically significant. The statistical method ",
           "allows a valid gender effect to be determined."),
    paste0("The value exceeds 5%, which is statistically significant. The ",
           "statistical method allows a major, valid gender effect to be ",
           "determined."))
  sig_level <- 0.05
  h0_threshold <- 0.05
  rating_level <- ifelse(
    2 * (1 - stats::pt(abs(coef_sex_f) / se_sex_f,
                df = object$results$df.residual)) > sig_level, 1,
    ifelse(
      1 - stats::pt((abs(coef_sex_f) - h0_threshold) / se_sex_f,
             df = object$results$df.residual) > sig_level, 2, 3))

  # Print standard analysis output
  cat("\nSummary of the Standard Analysis Model:\n", sep = "")
  cat(rep("=", 80), "\n\n", sep = "")
  cat("Number of employees: ", n_original, " of which ", n_f_original,
      sprintf(" (%.1f%%)", 100 * n_f_original / n_original), " women and ",
      n_m_original, sprintf(" (%.1f%%)", 100 * n_m_original / n_original),
      " men.\n", sep = "")
  cat("Number of employees included in the analysis: ", n_clean, " of which ",
      n_f_clean, sprintf(" (%.1f%%)", 100 * n_f_clean / n_clean), " women and ",
      n_m_clean, sprintf(" (%.1f%%)", 100 * n_m_clean / n_clean), " men.\n",
      rep("-", 80), "\n", sep = "")
  cat("Under otherwise equal circumstances, women earn ",
      sprintf("%.1f%% ", abs(100 * kennedy_estimate)),
      ifelse(kennedy_estimate > 0, "more ", "less "),
      "than men.\n\n", ratings[rating_level], "\n\n", sep = "")
  # Print linear regression output
  invisible(readline(prompt = paste0("Press [enter] to display the summary of ",
                                     "the linear regression...")))
  cat("\nSummary of the Linear Regression:\n", sep = "")
  cat(rep("=", 80), "\n", sep = "")
  cat(paste0(utils::capture.output(summary(object$results)), sep = "\n"),
      sep = "")
  cat("\n\n", sep = "")
}
