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
    message("Date format detected as dd/mm/yy.","\n",
            "Generating run sheet for ",date,".")
  } else{
    stop("please enter a valid date of format dd/mm/yy")
  }
  date_of_run_compact <- format(Date_b, "%d%m%y")
  return(date_of_run_compact)
}
