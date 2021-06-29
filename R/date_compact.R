#' Check if a date is valid, and return a compact version.
#'
#' @description Perform an is_date() check, and, if the date is valid under the prescribed format,
#'       return a compact version.
#'
#' @param example An example parameter
#'
#' @export
#'

date_compact <- function(date, date.format = "%d/%m/%y"){
  if(isTRUE(is_date(mydate = date))){
    date <- as.Date(date, format = date.format)
    date_of_run_compact <- format(date, "%d%m%y")
  } else if(isTRUE(is_date(mydate = date, date.format = "%d%m%y"))){
    date_of_run_compact <- date
  } else {
    stop("please enter a valid date of format dd/mm/yy")
  }
  return(date_of_run_compact)
}

