#' List all the folders inside the export directory.
#'
#' @description A simple wrapper of list.dirs - lists all the folders inside the user-defined
#'        export directory.
#'
#' @param export_directory the directory containing folders that will contain exported SampleQ data files.
#' @param full_names TRUE/FALSE list the full path of folders inside the export directory?
#'
#' @export
#'

get_export_folder_names <- function(export_directory = SampleQ_export_folder,full_names = FALSE){
  list.dirs(path = export_directory, full.names = full_names, recursive = TRUE)
}
