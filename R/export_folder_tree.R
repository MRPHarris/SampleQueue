#' Plot a tree of all export folders.
#'
#' @description Plots a folder tree of all the folders inside the export directory.
#'
#' @param export_folder_path where are the export folders?
#'
#' @export
#'

export_folder_tree <- function(export_folder_path = SampleQ_export_folder){
  export_folders <- get_export_folders(export_directory = export_folder_path, full_names = TRUE)
  export_folders <- str_replace(string = export_folders,
                                pattern = export_folder_path,
                                replacement = "Path:/SampleQ export folder/")
  export_tree <- data.tree::as.Node(data.frame(pathString = export_folders))
  print(export_tree)
}
