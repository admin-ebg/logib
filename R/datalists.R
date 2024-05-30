#' Download official Excel datalists
#'
#' Downloads an empty version of the latest official excel datalist in the
#' specified language to the given \code{path}.
#'
#' @param file a character string representing the file path to which the
#' downloaded datalist will be saved.
#' @param language a character string representing the language of the datalist
#' to be download (\code{"de"} or \code{"fr"} or \code{"it"} or \code{"en"}).
#'
#' @return None
#'
#' @export
download_datalist <- function(file, language = "de") {
  language <- tolower(language)

  # Make sure the given language and the file extension are valid
  if (!(language %in% c("de", "en", "fr", "it"))) {
    stop("The language must be either 'de', 'fr', 'it', or 'en'.")
  }
  if (substr(tolower(file), nchar(file) - 4, nchar(file)) != ".xlsx") {
    stop("The destination file must end in .xlsx")
  }

  # Build the URL according to the given language
  url <- paste0("https://logib.admin.ch/assets/Data/Datalist_",
                substr(language, 1, 1), ".xlsx")

  utils::download.file(url, file, mode = "wb")
}

#' Read official datalist or data_export Excel file
#'
#' Reads an official datalist or data_export file into a dataframe object.
#'
#' @param path a character string indicating the path of the Excel file to be
#'   read
#'
#' @return a dataframe with the contents of the datalist or data_export
#'
#' @keywords internal
read_official_excel <- function(path) {
  col_code <- all_column_names[["code"]]
  # If the file has 2 sheets it should be a datalist, if it has 4 it should be an
  # export file, otherwise halt the process
  sheet_names <- readxl::excel_sheets(path)
  n_sheets <- length(sheet_names)
  if (n_sheets == 2) {
    sheet_to_read <- intersect(sheet_names, c("Vorlage_Datenblatt",
                                              "mod\u00E8le_de_feuille_de_donn\u00E9es",
                                              "modello_del_foglio_di_dati",
                                              "data_sheet_template"))
    if(!(sheet_to_read %in% sheet_names)){
      stop(paste("The chosen file does not match any of the official files:",
                 "Excel sheet", sheet_to_read, "is missing"))
    }
    data <- readxl::read_excel(path, sheet = sheet_to_read)
    data_origin <- "datalist"
  } else if (n_sheets == 4) {
    sheet_to_read <- intersect(sheet_names, c("Individuelle_Angaben",
                                              "Donn\u00E9es_individuelles",
                                              "Dati_individuali",
                                              "Individual_information"))
    if(!(sheet_to_read %in% sheet_names)){
      stop(paste("The chosen file does not match any of the official files:",
                 "Excel sheet", sheet_to_read, "is missing"))
    }
    data <- readxl::read_excel(path, sheet = sheet_to_read)
    data_origin <- "data_export"
  } else {
    stop("The chosen file does not match any of the official files.")
  }
  # Make sure the columns correspond to that of an official Excel
  for (lang in c("de", "en", "fr", "it")) {
    col_data <- all_column_names[[data_origin]][[lang]]
    if (length(names(data)) == length(col_data)) {
      # gsub removes the carriage return, \r, used by Windows
      if (all(gsub("\r\n", "\n", names(data)) == gsub("\r\n", "\n", col_data))) {
        # Map column names to the 'code' names and return the dataframe
        data <- data[, 1:23]
        names(data) <- col_code
        # Transform specific columns to numerical values for the Exportfile
        if (data_origin == "data_export") {
          for (col_name in c("age", "years_of_service", "training",
                             "level_of_requirements", "professional_position",
                             "activity_rate", "paid_hours", "basic_wage",
                             "allowances", "monthly_wage_13",
                             "special_payments", "weekly_hours",
                             "annual_hours", "population")) {
            data[, col_name] <- as.numeric(unlist(data[, col_name]))
          }
        }
        return(data.frame(data))
      }
    }
  }
  # If no match happened above, the datafile doesn't match the required format
  stop(paste0("The chosen file does not match any of the official files. ",
              "Please make sure you did not add or remove columns from the ",
              "official file."))
}

#' Create the dataframe object used for the standard analysis model
#'
#' Reads either a custom dataframe object or an official Excel file (datalist
#' or data export) and transforms it to a dataframe object which can be used
#' for the standard analysis model
#'
#' Exactly one of \code{data_path} or \code{custom_data} must be \code{NULL}.
#'
#' @param data_path a string indicating the path for an official Excel file,
#' if this parameter is set to \code{NULL}, the function reads the dataframe
#' object provided in the parameter \code{custom_data} instead
#' @param custom_data a dataframe which was imported by the user beforehand,
#' if this parameter is set to \code{NULL}, the function import the data from
#' the path provided in the parameter \code{data_path} instead
#' @param prompt_mapping a boolean indicating whether the function prompts the
#' user for the exact mapping of his dataframe or whether the columns are
#' mapped automatically by order. This parameter is only relevant when
#' \code{custom_data} is not set to \code{NULL}
#' @param language a character string representing the language in which the
#' columns will be displayed during the mapping prompt (\code{"de"} or
#' \code{"fr"} or \code{"it"} or \code{"en"}). This parameter is only relevant
#' when \code{custom_data} is not set to \code{NULL}
#'
#' @return a dataframe which can be used to compute the standard analysis model
#'
#' @export
read_data <- function(data_path = NULL, custom_data = NULL,
                      prompt_mapping = TRUE, language = "de") {
  if (is.null(data_path) & is.null(custom_data)) {
    stop("At least one of 'data_path' and 'custom_data' must not be NULL")
  }
  if (!(is.null(data_path)) & !(is.null(custom_data))) {
    stop("At least one of 'data_path' and 'custom_data' must be NULL")
  }
  if (is.null(custom_data)) {
    read_official_excel(data_path)
  } else {
    custom_map <- build_custom_mapping(custom_data, language, prompt_mapping)
    # Drop all columns which aren't used in the custom map and map the data
    data <- custom_data[, names(custom_data) %in% names(custom_map)]
    names(data) <- custom_map[names(data)]
    data
  }
}
