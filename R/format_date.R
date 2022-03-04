#' Format a date
#'
#' A wrapper for \code{\link[base]{format}} that removes leading zeros on day
#' numbers, and has useful defaults.
#'
#' @param date The date to format. Defaults to today's date. Must be of class
#'   "Date", see \code{\link[base]{as.Date}} to convert a character string to a
#'   date.
#' @param foramt String for the format to use see \code{\link[base]{strptime}}.
#'   Defaults to "Month DD, YYYY".
#' @export
format_date <- function(date = base::Sys.Date(), format = "%B %d, %Y") {
  date_out <- base::format(date, format)
  # remove leading zeros in day number
  date_out <- base::gsub("(?<![0-9])0+", "", date_out, perl = TRUE)
  return(date_out)
}
