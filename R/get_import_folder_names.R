#' List all the folders inside the import directory.
#'
#' @description A simple wrapper of list.dirs - lists all the folders inside the user-defined
#'        import directory.
#'
#' @param import_directory the directory containing imported folders from the aqualog host computer.
#' @param full_names TRUE/FALSE list the full path of folders inside the import directory?
#'
#' @export
#'

# Get the import folders
get_import_folder_names <- function(import_directory = SampleQ_import_folder, full_names = FALSE){
  list.dirs(path = import_directory, full.names = full_names, recursive = FALSE)
}
