#' Run a Salary Analysis
#'
#' Runs a salary analysis according to the Swiss standard analysis model
#'
#' @param data a data.frame of employees as produced by \code{read_data}
#' @param reference_month an integer representing the reference month, i.e. the
#' month for which we analyze the salaries
#' @param reference_year an integer representing the reference year, i.e. the
#' year for which we analyze the salaries
#' @param usual_weekly_hours an optional numeric representing the usual weekly
#' working hours (missing values in \code{weekly_hours} are replaced by
#' \code{usual_weekly_hours}; if \code{NULL}, the missing values are not
#' replaced)
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
#'
#' @return object of type \code{analysis_model} with the following
#' elements
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
#' @examples
#' results <- analysis(data = datalist_example, reference_month = 1,
#'    reference_year = 2019, usual_weekly_hours = 40, female_spec = "F",
#'    male_spec = "M", age_spec = "age")
#'
#' @export
analysis <- function(data, reference_month, reference_year,
                     usual_weekly_hours = NULL, female_spec = "F", male_spec = "M",
                     age_spec = NULL, entry_date_spec = NULL) {
  params <- list(reference_month = reference_month,
                 reference_year = reference_year,
                 usual_weekly_hours = usual_weekly_hours,
                 female_spec = female_spec, male_spec = male_spec,
                 age_spec = age_spec, entry_date_spec = entry_date_spec)
  data_original <- data
  data_prepared <- prepare_data(data, reference_month, reference_year,
                                usual_weekly_hours, female_spec, male_spec,
                                age_spec, entry_date_spec)
  results <- run_standard_analysis_model(data_prepared$data)
  output <- list(params = params,
                 data_original = data_original,
                 data_clean = data_prepared$data,
                 data_errors = data_prepared$errors,
                 results = results)
  class(output) <- "analysis_model"
  output

}

#' Summary of the Salary Analysis
#'
#' Summary of an estimated salary analysis object of class
#' \code{analysis_model}
#'
#' \code{summary.analysis_model} provides a short summary of the wage
#' analysis according to the Standard Analysis Model. The summary describes the
#' number of records used for the analysis, the Kennedy estimate of the wage
#' difference under otherwise equal circumstances and the summary of the linear
#' regression.
#'
#' @param object estimated salary analysis object of class
#' \code{analysis_model}
#' @param ... further arguments passed to or from other methods
#'
#' @return Nothing
#'
#' @examples
#' # Estimate standard analysis model
#' results <- analysis(data = datalist_example, reference_month = 1,
#'    reference_year = 2019, usual_weekly_hours = 40, female_spec = "F",
#'    male_spec = "M", age_spec = "age")
#'
#' # Show summary of the salary analysis
#' summary(results)
#'
#' @export
summary.analysis_model <- function(object, ...) {
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
    paste0("This pay difference is not statistically significant. ",
           "The pay difference may solely be explained by \n",
           "the interaction of various objective factors such as ",
           "age, education and professional position."),
    paste0("This pay difference is statistically significant. ",
           "The pay difference may not or not solely be \n",
           "explained by the interaction of various objective factors such as ",
           "age, education and professional position."),
    paste0("This pay difference is statistically significant. ",
           "The pay difference may not or not solely be \n",
           "explained by the interaction of various objective factors such as ",
           "age, education and professional position.\n\n",
           "The target value of 2.5% has been exceeded. ",
           "The target value is a voluntary benchmark aimed at motivating \n",
           "employers to consistently avoid unexplained pay differences."),
    paste0("This pay difference is statistically significant. ",
           "The pay difference may not or not solely be \n",
           "explained by the interaction of various objective factors such as ",
           "age, education and professional position.\n\n",
           "The limit value of 5% has been exceeded. ",
           "With this pay difference you do not satisfy the requirements applied to \n",
           "various contexts, in particular the requirements of the Gender ",
           "Equality Act and the participation requirements \n",
           "for public procurement of the Confederation regarding equal treatment between ",
           "women and men in terms of salary."))
  sig_level <- 0.05
  target_value <- 0.025
  limit_value <- 0.05
  rating_level <- ifelse(
    2 * (1 - stats::pt(abs(coef_sex_f) / se_sex_f,
                df = object$results$df.residual)) > sig_level, 1,
    ifelse(
      floor(abs(kennedy_estimate)*1000)/1000 <= target_value, 2,
      ifelse(floor(abs(kennedy_estimate)*1000)/1000 <= limit_value, 3, 4)))
  # Infer the print size for methodology metrics from the degrees of freedom
  np <- ceiling(log(object$results$df.residual, 10))


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
  cat(rep("-", 80), "\n\n", sep = "")
  # Print methodology metrics
  cat("Methodology Metrics:\n", sep = "")
  cat(rep("=", 80), "\n\n", sep = "")
  cat("Regression results\n", sep = "")
  cat(rep("-", 80), "\n", sep = "")
  cat(sprintf(paste0("%-48s: %", np + 4, ".3f\n"), "Gender coefficient",
                     coef_sex_f), sep = "")
  cat(sprintf(paste0("%-48s: %", np + 4, ".3f\n"),
                     "Standard error of the gender coefficient", se_sex_f),
              sep = "")
  cat(sprintf(paste0("%-48s: %", np, "d\n"), "Degrees of freedom",
              object$results$df.residual), sep = "")
  cat(sprintf(paste0("%-48s: %", np + 4, ".3f\n\n"), "R-squared",
              summary(object$results)$r.squared))
  cat(paste0("Test to see whether the wage difference differs significantly ",
             "from zero\n"), sep = "")
  cat(rep("-", 80), "\n", sep = "")
  cat("H0: Wage diff. = 0%; HA: Wage diff. <> 0%\n", sep = "")
  cat(sprintf(paste0("%-48s: %", np + 4, ".3f\n"), "Critical t-value",
              stats::qt(.975, object$results$df.residual)))
  cat("(Alpha = 5%, two-sided, N = degrees of freedom)\n", sep = "")
  cat(sprintf(paste0("%-48s: %", np + 4, ".3f\n"), "Test statistic t",
              stats::qt(stats::pt(abs(coef_sex_f) / se_sex_f,
                                  object$results$df.residual),
                        object$results$df.residual)), sep = "")
  cat(sprintf("%-48s: %s\n\n", "Significance", ifelse(rating_level == 1, "No",
                                                   "Yes")))
}
