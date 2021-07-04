# General purpose utility functions for the SampleQueue package. These functions are almost all internal, i.e. not exported.
# The exception is get_names(), which is useful for assessing folder contents, and thus whether the package is operating as it should.

#' Check if a date is valid.
#'
#' @description Check if the date is valid in the format dd/mm/yy. Adapted from https://stackoverflow.com/questions/48542804/r-date-format-check
#'
#' @param mydate the input date.
#' @param date.format the format to check against.
#'
#' @noRd
#
is_date <- function(mydate, date.format = "%d/%m/%y"){
  tryCatch(!is.na(as.Date(mydate, date.format)),
           error = function(err) {FALSE})
}

#' Check if a date is valid, and return a compact version.
#'
#' @description Perform an is_date() check, and, if the date is valid under the prescribed format, return in format ddmmyy.
#'
#' @param date the date to convert to DDMMYY
#' @param date.format the format of the imported date.
#'
#' @noRd
#
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

#' Check if a compact date is present in a given string.
#'
#' @description Interrogate a string to determine if a compact date (%d%m%y) is present. If it is, return it. If it isn't, give NULL.
#'
#' @param string A string to be examined
#'
#' @noRd
#
compact_date_present <- function(string){
  # check if a period is present in the given string.
  periodpresent <- str_detect(string,"[.]")
  if(isTRUE(periodpresent)){
    string_noperiod <- unlist(strsplit(string,"[.]"))[1]
  } else{
    string_noperiod <- string
  }
  # Dismantle at spaces.
  string_spacesep <-  unlist(strsplit(string_noperiod,"\\s+"))
  # check for dates in project file name strings.
  check_string_date <- is_date(mydate = string_spacesep, date.format = "%d%m%y")
  # Getting the date from the string, if it exists. First one.
  if(isTRUE(any(check_string_date))){
    # So, there's a datestring.
    string_date <- string_spacesep[first(which(check_string_date))]
  } else {
    # No datestring! NULL.
    string_date <- NULL
  }
  string_date
}

#' Insert a single row into a dataframe.
#'
#' @description A less clunky row insert function than an rbind method. Shamelessy borrowed from Ari B. Friedman's response in https://stackoverflow.com/questions/11561856/add-new-row-to-dataframe-at-specific-row-index-not-appended
#'
#' @param df The data frame within which the row is to be inserted
#' @param row The row to be inserted
#' @param index the index at which the row will be inserted. All rows after will be shunted "down" 1.
#'
#' @noRd
#
insert_row <- function(df, row, index) {
  df <- as.data.frame(df)
  if(index <= nrow(df)){
    df[seq(index+1,nrow(df)+1),] <- df[seq(index,nrow(df)),]
    df[index,] <- row
    df
  } else if(index > nrow(df)){# check if index > nrow. If so, tack it on the end.
    df[nrow(df)+1,] <- row
    df
  }
}

#' Detect and return the extension of one or more given filenames.
#'
#' @description Split a string at the period, and return the characters after it. In filenames (on windows at least), this is the extension, assuming nothing funky is going on.
#'
#' @param filenames a character string containing one or more filenames.
#'
#' @noRd
#
ext_detect <- function(filenames){
  splits <- str_split(filenames,"[.]")
  splits <- sapply(splits,function(x) x[length(x)])
  ext <- splits
  return(ext)
}

#' Get object (file or folder) names from a given directory
#'
#' @description A wrapper for list.dirs and list.files. Obtains names of objects within the
#'       specified directory.
#'
#' @param directory full path to the folder
#' @param type character string string, either "files" or "folders".
#' @param string either NULL or an optional character string to index the files or folders.
#' @param full_names TRUE/FALSE to obtain full paths for each object.
#' @param recursive list.dirs and list.files option. Should the listing recurse into directories?
#'
#' @export
#
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

#' Print a tree of files or folders in the specified directory
#'
#' @description A cosmetic wrapper for get_names(). Prints a tree of the paths returned into the console window.
#'
#' @param directory the full path to the folder within which a tree will be generated.
#' @param type character string string, either "files" or "folders".
#'
#' @noRd
#
object_tree <- function(directory, type = "folders"){
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
                         replacement = paste0("Path:/",sapply(strsplit(directory,"/"),"[",length(strsplit(directory,"/")[[1]])),"/"))
    tree <- data.tree::as.Node(data.frame(pathString = files))
  } else if(type == "folders"){
    folders <- get_names(directory = directory,
                         type = type,
                         full_names = TRUE,
                         recursive = TRUE)
    folders <- str_replace(string = folders,
                           pattern = directory,
                           replacement = paste0("Path:/",sapply(strsplit(directory,"/"),"[",length(strsplit(directory,"/")[[1]])),"/"))
    tree <- data.tree::as.Node(data.frame(pathString = folders))
  }
  print(tree)
}

#' Crudely append plurals of a given set of characters, to the same vector of characters.
#'
#' @description Check each set of characters within a vector of strings, detecting if an 's' is present at the end. Add one to all instances where one is not present.
#'
#' @param strings A vector of strings/characters.
#'
#' @noRd
#
add_plurals <- function(strings){
  if(!is.character(strings)){
    stop("Please provide a character vector")
  }
  plurals <- vector(mode = "character", length = 0)
  it_list <- vector(mode = "list", length = length(strings))
  for(c in seq_along(it_list)){
    string <- strings[c]
    last_character <- substr(string, nchar(string), nchar(string))
    if(last_character != "s"){
      # Make a copy
      string_plural <- paste0(string,"s")
      plurals <- c(plurals, string_plural)
    }
  }
  strings_plusplurals <- c(strings,plurals)
  strings_plusplurals
}

#' Remove the file path from a given file or folder name.
#'
#' @description Removes the file or folder path from a given file/folder name by splitting the path at every "/", and getting rid of all but the last set of characters. If given a short filename (no path), the same string will just be spat out the other end unchanged.
#'
#' @param filnames a string containing the full file path.
#'
#' @noRd
#
trim_path <- function(filenames){
  if(length(filenames) > 1){
    it_list <- vector(mode = "list", length = length(filenames))
    trimmed_filenames <- vector(mode = "character", length = length(filenames))
    for(f in seq_along(filenames)){
      filename_trimmed <- unlist(strsplit(filenames[f],"/"))[length(unlist(strsplit(filenames[f],"/")))]
      trimmed_filenames[f] <- filename_trimmed
    }
    trimmed_filenames
  } else if(length(filenames) == 1){
    filename <- filenames
    filename_trimmed <- unlist(strsplit(filename,"/"))[length(unlist(strsplit(filename,"/")))]
    filename_trimmed
  } else{
    message("Empty object; no path to trim.")
  }
}
