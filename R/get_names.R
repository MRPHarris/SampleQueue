#' Get object (file or folder) names from a given directory
#'
#' @description A wrapper for list.dirs and list.files. Obtains names of objects within the
#'       specified directory.
#'
#' @param directory the full path to the folder
#' @param type either "files" or "folders"
#' @param string optional string to index the files or folders.
#' @param full_names TRUE/FALSE to obtain full paths for each object.
#' @param recursive list.dirs and list.files option. Should the listing recuse into directories?
#'
#' @export
#'

get_names <- function(directory,
                      type = "files",
                      string = NULL,
                      full_names = TRUE,
                      recursive = FALSE){
  ## input check type
  if((!isTRUE(type == "files")) && (!isTRUE(type == "folders"))){
    stop("Please specify type 'files' or 'folders'")
  }
  ## main fn
  if(type == "files"){
    # File handling
    names <- list.files(path = directory, full.names = TRUE, recursive = FALSE)
    names <- names[!file.info(names)$isdir]
    names <- names[!is.na(names)]
    if(length(names) == 0){
      stop("No files in the given directory. Try recursive?")
    }
    if(!is.null(string)){
      names <- names[grepl(string, names, fixed = FALSE)]
    }
  } else if(type == "folders"){
    # Folder handling
    names <- list.dirs(path = directory, full.names = full_names, recursive = recursive)
    # double slash handling - not necessarily needed, but why not?
    if(any(str_detect(names,"//"))){
      names <- str_replace(string = names, pattern = "//",replacement = "/")
    }
    if(!is.null(string)){
      names <- names[grepl(string, names, fixed = FALSE)]
    }
  }
  if(!isTRUE(full_names)){
    names <- str_replace(string = names,
                         pattern = directory,
                         replacement = "")
    if(any(str_detect(names,"/"))){ # slash handling - not necessarily needed, but why not?
      names <- str_replace(string = names, pattern = "/",replacement = "")
    }
  }
  return(names)
}

