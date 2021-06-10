#' Get the names of all the run sheets in the run sheet directory.
#'
#' @description Returns a list of all the files in the specified directory, and then indexes them
#'         for the run_sheet_string.
#'
#' @param run_sheet_directory the directory where the user has stored their run sheets.
#' @param full_names TRUE/FALSE return full path of the files
#' @param run_sheet_string what is the common string in the file name of your run sheets?
#'
#' @export
#'

get_run_sheet_names <- function(run_sheet_directory = SampleQ_run_sheet_folder,full_names = FALSE, run_sheet_string = "run sheet"){
  files <- list.files(path = run_sheet_directory, full.names = full_names, recursive = FALSE)
  files[grepl(run_sheet_string, files, fixed = FALSE)]
}
