#' Plot a tree of all run sheets.
#'
#' @description Plots a tree of all the run sheets.
#'
#' @param run_sheet_directory where are the run sheets?
#' @param run_sheet_string what is the common string in run sheet file names?
#'
#' @export
#'

run_sheet_tree <- function(run_sheet_directory = SampleQ_run_sheet_folder, run_sheet_string = "run sheet"){
  run_sheets <- get_run_sheets(run_sheet_directory = run_sheet_directory, full_names = TRUE, run_sheet_string = "run sheet")
  run_sheets <- str_replace(string = run_sheets,
                            pattern = run_sheet_directory,
                            replacement = "Path:/SampleQ run sheets/")
  run_sheet_tree <- data.tree::as.Node(data.frame(pathString = run_sheets))
  print(run_sheet_tree)
}
