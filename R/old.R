
#' Reads a dataframe and maps it columns
#'
#' \code{map_data} reads a dataframe and maps it columns to match the ones
#' used in the salary analysis.
#'
#' Renames the columns a data.frame to the column names used in the standard
#' analysis model. The mapping is based either on the origin of the data (i.e.
#' official datalist or data export from the webtool) or using a custom map
#' as produced by \code{build_custom_mapping}
#'
#' @param data data.frame to be transformed
#' @param data_origin a string indicating the origin of the data, can be one of
#' \code{datalist}, \code{data_export}, or \code{custom}
#' @param custom_map a character vector as produced by
#' \code{build_custom_mapping}
#'
#' @return a data.frame with the column names as used in the code
#'
#' @keywords internal
#'
map_data <- function(data, data_origin = "datalist", custom_map = NULL) {
  col_code <- all_column_names[["code"]]
  if (data_origin == "datalist" | data_origin == "data_export") {
    # Make sure the column from the files match the 'real' columns
    for (lang in c("de", "en", "fr", "it")) {
      col_data <- all_column_names[[data_origin]][[lang]]
      if (length(names(data)) == length(col_data)) {
        if (all(names(data) == col_data)) {
          names(data) <- col_code
          return(data)
        }
      }
    }
    # If no match happened above, the datafile doesn't match the required format
    stop(paste0("The chosen '", data_origin, "' does not match any of the ",
                "official '", data_origin, "'. Please make sure you did not ",
                "add or remove columns from the official '", data_origin, "'. ",
                "If your data comes from a custom file,  make sure you ",
                "set the 'data_origin' parameter to 'custom'."))
  } else if (data_origin == "custom") {
    if (is.null(custom_map)) {
      stop("When choosing data_origin='custom', a 'custom_map' must also be ",
           "provided in the arguments. You can build this using the ",
           "'build_custom_map' function.")
    }
    # Make sure the custom_map only has names from the provided dataframe
    if (!all(names(custom_map) %in% names(data))) {
      stop("The provided 'custom_map' and the provided 'data' do not ",
           "coincide. Please make sure you build the 'custom_map' using the ",
           "correct dataframe.")
    }
    # Drop all columns which aren't used in the custom map and map the data
    data <- data[, names(data) %in% names(custom_map)]
    names(data) <- custom_map[names(data)]
    return(data)
  } else {
    # data_origin parameter does not match the possible choices
    stop(paste0("The 'data_origin' parameter must be either 'datalist', ",
                "'data_export' or 'custom'."))
  }
}
