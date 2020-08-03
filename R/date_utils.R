#' Check if the interval between two dates is less than a year
#'
#' @param date1 the first date by chronological order
#' @param date2 the second date by chronological order
#'
#' @return a boolean
#'
#' @keywords internal
within_a_year <- function(date1, date2) {
  if (lubridate::year(date1) == lubridate::year(date2)) {
    return(TRUE)
  }
  if (lubridate::year(date1) + 1 == lubridate::year(date2)) {
    n1 <- lubridate::month(date1) * 100 + lubridate::day(date1)
    n2 <- lubridate::month(date2) * 100 + lubridate::day(date2)
    if (n1 >= n2) {
      return(TRUE)
    }
  }
  FALSE
}


#' Check if the interval between two dates contains the 29th February
#'
#' @param date1 the first date by chronological order
#' @param date2 the second date by chronological order
#'
#' @return a boolean
#'
#' @keywords internal
feb29_between <- function(date1, date2) {
  # This is only called when within_a_year(date1, date2) is true, thus we can
  # simplify the logic
  yr1 <- lubridate::year(date1)
  yr2 <- lubridate::year(date2)
  if (yr1 == yr2) {
    if (!lubridate::leap_year(date1)) {
      return(FALSE)
    } else {
      dt29 <- as.Date(paste0(lubridate::year(date1), "-02-29"))
      return((date1 <= dt29) & (date2 >= dt29))
    }
  } else {
    m1 <- lubridate::month(date1)
    m2 <- lubridate::month(date2)
    if (m1 > 2) {
      if (m2 < 2) {
        return(FALSE)
      } else if (m2 == 2) {
        return(lubridate::day(date2) == 29)
      } else {
        return(TRUE)
      }
    } else {
      return(lubridate::leap_year(date1))
    }
  }
}


#' Time difference between two dates in fractional year terms
#'
#' Computes the time difference between \code{date1} and \code{date2} in
#' fractional year terms. This is equivalent to the YEARFRAC() method used in
#' Excel, with the parameter "Actual/Actual"
#'
#' @param date1 the first date
#' @param date2 the second date
#'
#' @return fractional years between \code{date1} and \code{date2}
#'
#' @keywords internal
yearfrac <- function(date1, date2) {
  # Make sure date1 comes before date2
  if (date1 > date2) {
    date_temp <- date1
    date1 <- date2
    date2 <- date_temp
  }
  if (date1 == date2) {
    return(0)
  }
  yr1 <- lubridate::year(date1)
  yr2 <- lubridate::year(date2)
  if (within_a_year(date1, date2)) {
    if((yr1 == yr2) & lubridate::leap_year(date1)) {
      year_len <- 366
    } else if (feb29_between(date1, date2)) {
      year_len <- 366
    } else {
      year_len <- 365
    }
    return(round(as.numeric(date2 - date1) / year_len, digits = 5))
  } else {
    num_years <- yr2 - yr1 + 1
    days_in_year <- as.numeric(as.Date(paste0(yr2 + 1, "-01-01")) - as.Date(
      paste0(yr1, "-01-01")))
    return(round(as.numeric(date2 - date1) * (num_years / days_in_year),
                 digits = 5))
  }
}
