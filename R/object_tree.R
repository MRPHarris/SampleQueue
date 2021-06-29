#' Print a tree of files or folders in the specified directory
#'
#' @description A wrapper for get_names(). Prints a tree of the paths returned.
#'
#' @param directory the folder within which to plot the tree.
#' @param type string of either "files" or "folders".
#'
#' @export
#'

object_tree <- function(directory,
                        type = "folders"){
  ## input check type
  if((!isTRUE(type == "files")) && (!isTRUE(type == "folders"))){
    stop("Please specify type 'files' or 'folders'")
  }
  ## main fn
  if(type == "files"){
    files <- get_names(directory = directory,
                       type = type,
                       full_names = TRUE)
    files <- files[!file.info(files)$isdir]
    files <- str_replace(string = files,
                         pattern = directory,
                         replacement = paste0("Path:/",sapply(strsplit(directory,"/"),"[",length(strsplit(directory,"/")[[1]]))))
    tree <- data.tree::as.Node(data.frame(pathString = files))
  } else if(type == "folders"){
    folders <- get_names(directory = directory,
                         type = type,
                         full_names = TRUE,
                         recursive = TRUE)
    folders <- str_replace(string = folders,
                           pattern = directory,
                           replacement = paste0("Path:/",sapply(strsplit(directory,"/"),"[",length(strsplit(directory,"/")[[1]]))))
    tree <- data.tree::as.Node(data.frame(pathString = folders))
  }
  print(tree)
}
