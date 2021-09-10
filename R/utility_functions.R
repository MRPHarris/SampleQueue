# General purpose utility functions for the SampleQueue package. These functions are almost all internal, i.e. not exported.
# The exception is get_names(), which is useful for assessing folder contents, and thus whether the package is operating as it should.


#' Get object names from a given directory
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

#' Check if a date is valid.
#'
#' @description Check if the date is valid in the format dd/mm/yy. Adapted from https://stackoverflow.com/questions/48542804/r-date-format-check
#'
#' @param mydate the input date.
#' @param date.format the format to check against.
#'
#' @noRd
#'
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
#'
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
#'
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
#'
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
#'
ext_detect <- function(filenames){
  splits <- str_split(filenames,"[.]")
  splits <- sapply(splits,function(x) x[length(x)])
  ext <- splits
  return(ext)
}

#' Print a tree of files or folders in the specified directory
#'
#' @description A cosmetic wrapper for get_names(). Prints a tree of the paths returned into the console window.
#'
#' @param directory the full path to the folder within which a tree will be generated.
#' @param type character string string, either "files" or "folders".
#'
#' @noRd
#'
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
#'
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
#'
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


#' Read multiple Aqualog eem .dat files. An unwrapped version of eemR::eem_read targeted specifically at Aqualog .dat files, with simple multiple-file handling.
#'
#' @description Read multiple Aqualog .dat PEM files into an EEM/eemR/staRdom compliant eemlist object. Directories not supported.
#'
#' @importFrom tools file_path_sans_ext
#'
#' @param file one or more long filenames (i.e. path included) of ASCII .dat PEM files from an Aqualog.
#'
#' @noRd
#'
eem_read_mod <- function (file){
  # This import function is eemR's eem_import_aqualog.
  f <- function(file) {
    data <- readLines(file)

    eem <- stringr::str_extract_all(data, "-?\\d+(?:\\.\\d*)?(?:[eE][+\\-]?\\d+)?")

    ex <- sort(as.numeric(eem[[1]]))

    n_col <- lapply(eem, length)
    n_col <- unlist(n_col)
    expected_col <- as.numeric(names(sort(-table(n_col)))[1])

    eem[n_col != expected_col] <- NULL
    eem <- lapply(eem, as.numeric)
    eem <- do.call(rbind, eem)

    em <- eem[, 1]
    eem <- eem[, -1]
    eem <- as.matrix(eem[, ncol(eem):1])

    l <- list(
      file = file,
      x = eem,
      em = em,
      ex = ex
    )

    return(l)
  }
  # This is the eem constructor from eemR.
  eemf <- function(data) {
    if (!all(c("file", "x", "em", "ex") %in% names(data))) {
      stop("Your custom function should return a named list with four components: file, x, ex, em")
    }
    file_path_sans_ext <- function(x, compression = FALSE) {
      if (compression)
        x <- sub("[.](gz|bz2|xz)$", "", x)
      sub("([^.]+.+)\\.[[:alnum:]]+$", "\\1", x)
    }
    res <- list(
      file = data$file,
      sample = ifelse(
        is.null(data$sample),
        file_path_sans_ext(basename(data$file)),
        data$sample
      ),
      x = data$x,
      ex = data$ex,
      em = data$em,
      location = dirname(data$file)
    )
    class(res) <- "eem"
    attr(res, "is_blank_corrected") <- FALSE
    attr(res, "is_scatter_corrected") <- FALSE
    attr(res, "is_ife_corrected") <- FALSE
    attr(res, "is_raman_normalized") <- FALSE
    return(res)
  }
  # multiple file handling
  if(length(file) > 1){
    # eemlist init
    eemlist_add <- vector(mode = "list", length = length(file))
    class(eemlist_add) <- "eemlist"
    # Loop for eem processing and addition to eemlist_add
    for(i in seq_along(eemlist_add)){
      # get file for this iteration
      file_it <- file[i]
      # stop ifs
      stopifnot(file.exists(file_it))
      # Directory checking. Unsure how this section will integrate with the loop to be honest.
      #isdir <- file.info(file_it)$isdir
      #if (isdir) {
      #  file_it <- list.files(file_it, full.names = TRUE, recursive = recursive,
      #                        no.. = TRUE, include.dirs = FALSE, pattern = "*.txt|*.dat|*.csv",
      #                        ignore.case = TRUE)
      #  file_it <- file_it[!file.info(file_it)$isdir]
      #}
      # Now do function application, etc.
      res <- lapply(file_it, f) # this applies the import function to the file.
      res <- lapply(res, eemf) # this applies the eem constructor
      res <- unlist(res, recursive = FALSE)
      class(res) <- "eem"
      res[unlist(lapply(res, is.null))] <- NULL # Remnoves unreadable EEMs.
      eemlist_add[[i]] <- res
    }
    return(eemlist_add)
  } else if(length(file) == 1){
    # stop if
    stopifnot(file.exists(file))
    # removing directory handling
    #isdir <- file.info(file)$isdir
    #if (isdir) {
    #  file <- list.files(file, full.names = TRUE, recursive = recursive,
    #                     no.. = TRUE, include.dirs = FALSE, pattern = "*.txt|*.dat|*.csv",
    #                     ignore.case = TRUE)
    #  file <- file[!file.info(file)$isdir]
    #}
    res <- lapply(file, f) # this applies the import function to the file.
    res <- lapply(res, eemf) # this applies the eem constructor
    class(res) <- "eemlist"
    res[unlist(lapply(res, is.null))] <- NULL # Remnoves unreadable EEMs.
    return(res)
  } else if(length(file) < 1){
    stop("Files parameter is empty; no files to read.")
  }
}

#' Set all negative values within a group of EEMs to 0.
#'
#' @description Negative fluorescence is not possible, and typically indicates
#'        bad processing, noise, or artefacts. This function will set all negative
#'        values within one or more EEMs to 0.
#'
#' @param eemlist A list of EEMs in a format compliant with eemR/staRdom.
#' @param outputfolder optional; either NULL or a path to the folder where the new eemlist will be sent.
#'
#' @noRd
#'
eemlist_neg_to_0 <- function(eemlist, outputfolder = NULL){
  EEMs_NoNeg <- vector(mode = "list", length = length(eemlist))
  class(EEMs_NoNeg) <- "eemlist"
  for(i in seq_along(EEMs_NoNeg)){                                         # main for loop
    eem_it <- eemlist[[i]]
    file_it <- eem_it[['file']]
    sample_it <- eem_it[['sample']]
    location_it <- eem_it[['location']]
    eem_ungathered <- as.data.frame(eem_it, gather = FALSE)       # extract EEM, don't gather
    eem_ungathered[,][eem_ungathered[,] <0] <- 0                        # set all values less than 0 in EEM to 0
    eem_df <- eemdf_to_eem(eemdf = eem_ungathered,
                           file = file_it,
                           sample = sample_it,
                           location = location_it)
    class(eem_df) <- "eem"
    EEMs_NoNeg[[i]] <- eem_df
    if(!is.null(outputfolder)){
      write.csv(eem_ungathered, file = paste0(outputfolder,EEMs_NoNeg[[i]][["sample"]],"_noneg.csv"), row.names = TRUE) # Export EEM with iterative naming scheme.
    }
  }
  return(EEMs_NoNeg)
}

#' Set all negative values within a group of EEMs to 0.
#'
#' @description Negative fluorescence is not possible, and typically indicates
#'        bad processing, noise, or artefacts. This function will set all negative
#'        values within one or more EEMs to 0.
#'
#' @param eemlist A list of EEMs in a format compliant with eemR/staRdom.
#' @param outputfolder optional; either NULL or a path to the folder where the new eemlist will be sent.
#'
#' @noRd
#'
eem_neg_to_0 <- function(eem, outputfolder = NULL){
  file_it <- eem[['file']]
  sample_it <- eem[['sample']]
  location_it <- eem[['location']]
  eem_ungathered <- as.data.frame(eem, gather = FALSE)       # extract EEM, don't gather
  eem_ungathered[,][eem_ungathered[,] <0] <- 0                        # set all values less than 0 in EEM to 0
  eem_new <- eemdf_to_eem(eemdf = eem_ungathered,
                          file = file_it,
                          sample = sample_it,
                          location = location_it)
  class(eem_new) <- "eem"
  if(!is.null(outputfolder)){
    write.csv(eem_ungathered, file = paste0(outputfolder,eem[['sample']],"_noneg.csv"), row.names = TRUE) # Export EEM with iterative naming scheme.
  }
  return(eem_new)
}

#' Average a set of EEMs.
#'
#' @description Take a set of EEMs and average them together. The EEMs must be the same size.
#'
#' @param eemlist A list of EEMs in a format compliant with eemR/staRdom.
#'
#' @noRd
#'
average_eems <- function(eemlist){
  if(length(eemlist) == 1){
    message("1 EEM passed to average_eems() for averaging. Returning unchanged.")
    new_eemlist <- eemlist
    return(new_eemlist)
  } else if(length(eemlist) > 1){
    ungathered_list <- vector(mode = "list",length = length(eemlist))
    for(i in seq_along(eemlist)){
      eem_it <- eemlist[[i]]
      file_it <- eem_it[['file']]
      sample_it <- eem_it[['sample']]
      location_it <- eem_it[['location']]
      eem_ungathered <- as.data.frame(eemlist[[i]], gather = FALSE)       # extract EEM, don't gather
      ungathered_list[[i]] <- eem_ungathered
    }
    # Average it
    averaged <- Reduce("+",ungathered_list)/length(ungathered_list)
    # Now back to eem
    averaged_eem <- eemdf_to_eem(averaged,
                                 file = "",
                                 sample = "averaged_eem",
                                 location = "")
    new_eemlist <- vector(mode = "list",length = 1)
    class(new_eemlist) <- "eemlist"
    message(paste0(length(eemlist)," EEMs passed to average_eems() for averaging."))
    new_eemlist[[1]] <- averaged_eem
    return(new_eemlist)
  }

}

#' Takes a data frame and attempts to coerce it to an EEM object of the style used by EEM/eemR/staRdom.
#'
#' @description An alternative to eemR's eem constructor.
#'
#' @param eemdf the dataframe to be coerced to an EEM object.
#' @param file filename of the EEM, if applicable.
#' @param sample the samplename of the EEM, if applicable.
#' @param location the location of the EEM file, if applicable.
#'
#'@noRd
eemdf_to_eem <- function(eemdf,
                         file,
                         sample,
                         location){
  # code adapted from staRdom's .eem_csv importer.
  x <- eemdf
  ex <- colnames(x)[] %>% as.numeric()
  em <- rownames(x) %>% as.numeric()
  x <- x[,] %>% as.matrix() %>% unname()
  x <- x[!is.na(em),!is.na(ex)]
  ex <- ex[!is.na(ex)]
  em <- em[!is.na(em)]
  l <- list(
    file = file,
    sample = sample,
    x = x,
    ex = ex,
    em = em,
    location = location
  )
  class(l) <- "eem"
  return(l)
}

#' Subtracts one eem (the subtrahend) from an eemlist (the minuends).
#'
#' @description A function for subtracting one eem from multiple other eems. For use in blank subtraction. All eems must be of matching dimensions
#'
#' @param eems_minuend an eemR/staRdom compliant eemlist object.
#' @param eem_subtrahend a single eem (not an eemlist) to be used as the subtrahend during subtraction.
#'
#'@noRd
#'
eemlist_subtract <- function(eems_minuend,
                             eem_subtrahend){
  ## input checks
  if(class(eems_minuend) != "eemlist"){
    stop("object eems_minuend must be of class eemlist")
  }
  if(class(eem_subtrahend) == "eemlist"){
    eem_subtrahend = eem_subtrahend[[1]] # Pull out just the eem.
  }
  ## new eemlist to add processed eems to.
  eemlist_sub <- vector(mode = "list", length = length(eems_minuend))
  class(eemlist_sub) <- "eemlist"
  ## coerce subtrahend to data.frame
  eem_subtrahend_df = as.data.frame(eem_subtrahend, gather = FALSE)
  ## Main subtraction loop
  for(e in seq_along(eems_minuend)){
    ## coerce minuend for this iteration to data frame.
    eem_minuend_it <- eems_minuend[[e]]
    eem_minuend_df_it <- as.data.frame(eem_minuend_it, gather = FALSE)
    ## check that the dimensions are the same.
    if(!isTRUE(nrow(eem_minuend_df_it) == nrow(eem_subtrahend_df) & ncol(eem_minuend_df_it) == ncol(eem_subtrahend_df))){
      stop("EEMs minuend and subtrahend are of differing dimensions.")
    }
    ## FEATURE UPDATE: ADD extend2largest() OPTION HERE
    ## Subtraction
    eem_minuend_df_new <- eem_minuend_df_it - eem_subtrahend_df
    ## Return to eem class object
    eem_minuend_new <- eemdf_to_eem(eemdf = eem_minuend_df_new,
                                    file = eem_minuend_it[['file']],
                                    sample = eem_minuend_it[['sample']],
                                    location = eem_minuend_it[['location']])
    ## Add processed eem from this iteration to the new eemlist
    eemlist_sub[[e]] <- eem_minuend_new
  }
  return(eemlist_sub)
}
