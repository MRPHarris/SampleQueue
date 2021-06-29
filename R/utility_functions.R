# Utility/internal functions for the SampleQueue package. These functions are not exported.

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
    string_date <- projfile_components_spacesep[first(which(check_string_date))]
  } else {
    # No datestring! NULL.
    string_date <- NULL
  }
  string_date
}

#' Create a dummy list that imitates the desired SampleQueue folder framework. Long, clunky.
#'
#' @description Write a log file to the specified destination.
#'
#' @noRd
#
default_folder_layout <- function(){
  folderx3 <- vector(mode = "list", length = 3)
  folderx4 <- vector(mode = "list", length = 4)
  folderx5 <- vector(mode = "list", length = 5)
  folderx6 <- vector(mode = "list", length = 6)
  # Highest level folders
  SampleQueueFolders <- folderx3
  names(SampleQueueFolders) = c("1 imported files", "2 renamed files", "3 exported files")
  # First tier export folders
  SampleQueueFolders[[3]] <- folderx6
  names(SampleQueueFolders[[3]]) <- c("milliq blanks","sampleq blanks","samples","standards","project files","logs")
  # Second tier export folders
  SampleQueueFolders[[3]][[1]] <- folderx3
  names(SampleQueueFolders[[3]][[1]]) <- c("ABS","raw EEMs","workbooks")
  SampleQueueFolders[[3]][[2]] <- vector(mode = "list", length = 2)
  names(SampleQueueFolders[[3]][[2]]) <- c("blank files","workbooks")
  SampleQueueFolders[[3]][[3]] <- folderx4
  names(SampleQueueFolders[[3]][[3]]) <- c("ABS","milliq blank subtracted EEMs","raw EEMs","workbooks")
  SampleQueueFolders[[3]][[4]] <- folderx4
  names(SampleQueueFolders[[3]][[4]]) <- c("ABS","milliq blank subtracted EEMs","raw EEMs","workbooks")
  return(SampleQueueFolders)
}

#' Detect and return the extension of one or more given filenames.
#'
#' @description Split a string at the period, and return the characters after it. In filenames, this is the extension.
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
