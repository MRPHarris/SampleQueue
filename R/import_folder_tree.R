#' Plot a tree of all import folders.
#'
#' @description Plots a folder tree of all the folders inside the import directory.
#'
#' @param import_folder_path where are the import folders?
#'
#' @export
#'

import_folder_tree <- function(import_folder_path = SampleQ_import_folder){
  import_folders <- get_import_folders(import_directory = import_folder_path, full_names = TRUE)
  import_folders <- str_replace(string = import_folders,
                                pattern = import_folder_path,
                                replacement = "Path:/SampleQ import folder/")
  import_tree <- data.tree::as.Node(data.frame(pathString = import_folders))
  print(import_tree)
}
