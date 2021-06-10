#' Check if a date is valid.
#'
#' @description Check if the date is valid in the format %d/%m/%y.
#'       shamelessly ripped from https://stackoverflow.com/questions/48542804/r-date-format-check
#'
#' @param mydate the input date.
#' @param date.format the format to check against.
#'
#' @export
#'

is_date <- function(mydate, date.format = "%d/%m/%y"){
  tryCatch(!is.na(as.Date(mydate, date.format)),
           error = function(err) {FALSE})
}
