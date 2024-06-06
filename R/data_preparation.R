#' Build column name mappings
#'
#' \code{build_custom_mapping} creates a vector of column name mappings for the
#' user to read her or his custom dataframe
#'
#' Builds a mapping from the custom column names of a given data.frame to the
#' variable names used in the standard analysis model. If \code{prompt_mapping}
#' is set to \code{TRUE}, the function prompts the mapping for each column
#' of the data.frame. If \code{prompt_mapping} is set to \code{FALSE}, the
#' mapping is built using the order of the columns of the given data.frame.
#'
#' @param data the custom dataframe for which the user wants to build a custom
#' mapping
#' @param language a character string representing the language in which the
#' columns will be displayed during the mapping prompt (\code{"de"} or
#' \code{"fr"} or \code{"it"} or \code{"en"})
#' @param prompt_mapping a boolean indicating whether the function prompts the
#' user for the exact mapping of his dataframe or whether the columns are
#' mapped automatically by order
#'
#' @return
#' A named vector of characters, where the name indicates the column name in the
#' original data.frame and the value indicates the column name as used by the
#' standard analysis model.
#'
#' @export
build_custom_mapping <- function(data, language = "de", prompt_mapping = TRUE) {
  language <- tolower(language)
  if (!(language %in% c("de", "en", "fr", "it"))) {
    stop("The language must be either 'de', 'fr', 'it', or 'en'.")
  }
  col_code <- all_column_names[["code"]]
  col_datalist <- gsub("\r\n", " ", all_column_names[["datalist"]][[language]])
  # Make sure the data has headers
  if (is.null(names(data))) {
    names(data) <- paste0("column_", seq_len(ncol(data)))
  }
  if (prompt_mapping) {
    # Prompt the user to map the column of her or his file one by one
    choices <- paste0(col_code, " - [", col_datalist, "]")
    #choices <- c(paste0("DO NOT MAP THIS COLUMN ('", col, "')"), choices)
    custom_map <- c()
    for (col in names(data)) {
      x <- utils::menu(c(paste0("Do NOT map the column '", col, "'"), choices),
                       title = paste0("Please choose the corresponding column for '",
                                      col, "'."))
      # Save choice and update as to not map the same column twice
      if (x > 1) {
        custom_map <- c(custom_map, col_code[x - 1])
        col_code <- col_code[- (x - 1)]
        names(custom_map)[length(custom_map)] <- col
        choices <- choices[-x]
      }
    }
  } else {
    if (length(col_code) != length(names(data))) {
      stop("The number of columns in 'data' does not match the number of ",
           "columns required by the official model. Please call the function ",
           "with 'prompt_mapping = TRUE'.")
    }
    # Simply match the columns sequentially without prompting
    names(col_code) <- names(data)
    custom_map <- col_code
  }
  custom_map
}

#' Prepares a dataframe for the analysis
#'
#' Prepares a dataframe for the analysis in three steps:
#' \itemize{
#'   \item{Checks whether \code{sex}, \code{age}, and \code{entry_date} have the
#'   correct format and whether their specifications are plausible}
#'   \item{Build the dataframe used for the analysis}
#'   \item{Check each row of the dataframe for correctness and plausibility}
#' }
#'
#' @param data a dataframe object as produced by \code{read_data} which is to
#' be used in the analysis
#' @param reference_month a number indicating the reference month of the
#' analysis
#' @param reference_year a number indicating the reference year of the analysis
#' @param usual_weekly_hours an optional numeric representing the usual weekly
#' working hours
#' @param female_spec a string or number indicating the way females are
#' specified in the dataset
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
#'
#' @return a data.frame which has no incorrect rows left and can be used to
#' estimate the standard analysis model
#'
#' @keywords internal
prepare_data <- function(data, reference_month, reference_year,
                         usual_weekly_hours,
                         female_spec = "F", male_spec = "M", age_spec = NULL,
                         entry_date_spec = NULL) {
  # Make sure the specification parameters are correct
  if (!(reference_month %in% 1:12)) {
    stop(simpleWarning("The 'reference_month' must be an integer between 1 and 12."))
  }
  if (any(is.na(data$weekly_hours)) && is.null(usual_weekly_hours)) {
    stop(simpleWarning(paste0("'weekly_hours' has missing values, please specify ",
                              "'usual_weekly_hours'.")))
  }
  if (female_spec == male_spec) {
    stop(simpleWarning("The 'female_spec' and 'male_spec' arguments must differ."))
  }
  if (!is.null(age_spec)) {
    if (!(age_spec %in% c("age", "birthyear", "date_of_birth"))) {
      stop(simpleWarning(paste0("The 'age_spec' parameter must be one of 'age', 'birthyear'",
                                ", or 'date_of_birth'")))
    }
  }
  if (!is.null(entry_date_spec)) {
    if (!(entry_date_spec %in% c("years", "entry_year", "entry_date"))) {
      stop(simpleWarning(paste0("The 'entry_date_spec' parameter must be one of 'years', ",
                                "'entry_years', or 'entry_date'.")))
    }
  }

  # Make sure the formats for sex match the specification
  sex_levels <- levels(factor(data$sex))
  if (length(sex_levels) > 2) {
    stop(simpleWarning(paste0("There are more than 2 sexes represented in the data, ",
                              "there must be exactly 2 sexes for the analysis to work.")))
  }
  if (!(all(sex_levels %in% c(female_spec, male_spec)))) {
    stop(simpleWarning(paste0("The 'female_spec' and/or 'male_spec' parameters do not ",
                              "match the values in the data.")))
  }
  # Build the dataframe for the analysis (this will also check whether the
  # formats for age and entry_date match the specification)
  data <- transform_data(data, reference_year, usual_weekly_hours,
                         female_spec, male_spec, age_spec, entry_date_spec)
  # Check the data for correctness and plausibility
  errors <- check_data(data)
  # We discard all data with an error code of 1 (incorrect data)
  invalid_rows <- unique(errors$row[errors$importance == 1])
  if (length(invalid_rows) > 0) {
    if(length(invalid_rows) == 1){
      answer <- utils::menu(
        c("Yes", "No"), title =
          paste("There is", length(invalid_rows), "invalid observation.",
                "Invalid observations will be discarded from the analysis.",
                "Do you want to continue and run the analysis without",
                "this observation?")
      )
    }
    if(length(invalid_rows) > 1){
      answer <- utils::menu(
        c("Yes", "No"), title =
          paste("There are", length(invalid_rows), "invalid observations.",
                "Invalid observations will be discarded from the analysis.",
                "Do you want to continue and run the analysis without",
                "these observations?")
      )
    }
    if(answer == 1){
      data <- data[-invalid_rows, ]
    }
    if(answer == 2){
      stop(simpleWarning("The analysis was aborted by the user."))
    }
  }
  # Identify implausible data
  implausible_rows <- unique(errors$row[errors$importance == 2])
  implausible_rows <- setdiff(implausible_rows, invalid_rows)

  if (length(invalid_rows) == 1) {
    errors_str <- utils::capture.output(print(errors[errors$importance == 1,
                                                     c("pers_id",
                                                       "description")]))
    warning(simpleWarning(paste(c(paste(length(invalid_rows), "observation is",
                                        "invalid and has been discarded:"),
                                  errors_str, ""), collapse = "\n")))
  }
  if (length(invalid_rows) > 1) {
    errors_str <- utils::capture.output(print(errors[errors$importance == 1,
                                                     c("pers_id",
                                                       "description")]))
    warning(simpleWarning(paste(c(paste(length(invalid_rows), "observations are",
                                        "invalid and have been discarded:"),
                                  errors_str, ""), collapse = "\n")))
  }
  if (length(implausible_rows) == 1) {
    errors_str <- utils::capture.output(print(errors[errors$importance == 2,
                                                     c("pers_id",
                                                       "description")]))
    warning(simpleWarning(paste(c(paste(length(implausible_rows), "observation is",
                                        "implausible. This observation will be kept in the",
                                        "analysis:"),
                                  errors_str), collapse = "\n")))
  }
  if (length(implausible_rows) > 1) {
    errors_str <- utils::capture.output(print(errors[errors$importance == 2,
                                                     c("pers_id",
                                                       "description")]))
    warning(simpleWarning(paste(c(paste(length(implausible_rows), "observations are",
                                        "implausible. These observations will be kept in the",
                                        "analysis:"),
                                  errors_str), collapse = "\n")))
  }
  list(data = data, errors = errors)
}
