# Internal functions used to process files that relate to the run sheet (i.e not .txt, project, or other files).
# These functions are utilised by process_sample_queue to identify and match files with their destination folders.

#' Return the matching foldername (short form).
#'
#' @description Return the matching foldername (short form) when supplied with a number of names and possible strings to match.
#'
#' @param foldernames a character vector containing one or more folder names.
#' @param possible_strings a character vector containing one or more strings to be matched with the folder names.
#'
#' @noRd
#'
matching_folder <- function(foldernames,
                            possible_strings){
  it_list <- vector(mode = "list", length = length(foldernames))
  for(f in seq_along(it_list)){
    # get name for this it
    foldername <- foldernames[f]
    if(str_detect(foldername,"/")){ # in case long names were used
      foldername <- unlist(strsplit(foldername,"/"))[length(unlist(strsplit(foldername,"/")))]
    }
    # have to allow for plurals!
    # adding plurals
    poss_strings_plurals <- add_plurals(possible_strings)
    possible_strings <- c(possible_strings,poss_strings_plurals)
    # Collapsing to index across whole string.
    possible_collapse <- paste(possible_strings, collapse = " ")
    if(str_detect(possible_collapse,foldername)){
      matching_name <- foldername
    }
  }
  matching_name
}

#' Return the type folder.
#'
#' @description Given a single row from a run sheet, where are these files destined to go? Operates by type - i.e. sampleq blank, milliq blank, sample, etc.
#'
#' @param run_sheet_row single-row dataframe. A row from a run sheet.
#' @param export_dir the export directory.
#'
#' @noRd
#'
get_type_folder <- function(run_sheet_row,
                            export_dir){
  input_type <- run_sheet_row$Type
  # Get the supported types
  types <- type_strings()
  whichtype <- vector(mode = "character", length = 1)
  for(t in seq_along(types)){
    type_collapse <- paste(types[[t]], collapse = " ")
    if(str_detect(type_collapse,input_type)){
      whichtype <- names(types[t])
      whichtype_otherstrings <- types[[t]]
    }
  }

  # folder should contain any of the strings in the matching types() list.
  possible_folders <- get_names(export_dir,type = "folders", full_names = FALSE)
  dest_folder_short <- matching_folder(foldernames = possible_folders, possible_strings = whichtype_otherstrings)
  # which long folder matches this short one?
  long_folders <- get_names(export_dir,type = "folders", full_names = TRUE)
  matching_full_folder <- long_folders[which(str_detect(long_folders, dest_folder_short))]
  matching_full_folder
}

#' Return the row sample type.
#'
#' @description This fn returns gives the type - in the form of the title from the matching list element from "type_strings"
#'
#' @param run_sheet_row single-row dataframe. A row from a run sheet.
#'
#' @noRd
#'
get_type <- function(run_sheet_row){
  input_type <- run_sheet_row$Type
  types <- type_strings()
  whichtype <- vector(mode = "character", length = 1)
  for(t in seq_along(types)){
    type_collapse <- paste(types[[t]], collapse = " ")
    if(str_detect(type_collapse,input_type)){
      whichtype <- names(types[t])
      whichtype_otherstrings <- types[[t]]
    }
  }
  whichtype
}

#' Given a set of files of type "Sample Queue Blank", what are the matching subfolders for each of the given file names?
#'
#' @description Accesses the given sampleq blank folder in the export directory, returning a character vector of the matching folders for each file.
#'
#' @param type_folder the type folder.
#' @param filenames a character vector of filenames to be matched with sub-folders within the type folder.
#'
#' @noRd
#'
get_sqblank_folders <- function(type_folder,
                                filenames){
  type_destfolders <- get_names(directory = type_folder,
                                type = "folders",
                                full_names = TRUE)
  if(!is.character(filenames)){
    stop("filenames must be a character vector.")
  }
  types <- extension_strings(type = "Sample Queue Blank")
  matching_folders <- vector(mode = "character", length = length(filenames))
  it_list <- vector(mode = "list", length = length(filenames))
  for(f in seq_along(it_list)){
    # get the extension for this file.
    file_extension <- ext_detect(filenames[f])
    if(file_extension == "blank"){
      # .blank file handling
      # Get the folder in the sampleq folder that matches this file type.
      input_ext <- file_extension
      whichtype <- vector(mode = "character", length = 1)
      for(t in seq_along(types)){
        type_collapse <- paste(types[[t]], collapse = " ")
        if(str_detect(type_collapse,input_ext)){
          whichtype <- names(types[t])
          whichtype_otherstrings <- types[[t]]
        }
      }
      # folder should contain any of the strings in the matching types() list.
      possible_folders <- type_destfolders
      dest_folder_short <- matching_folder(foldernames = possible_folders, possible_strings = whichtype_otherstrings)
      # which long folder matches this short one?
      long_folders <- type_destfolders
      match_folder <- long_folders[which(str_detect(long_folders, dest_folder_short))]
      matching_folders[f] <- match_folder
    } else if(file_extension == "ogw"){
      # workbook file handling
      # Get the folder in the sampleq folder that matches this file type.
      input_ext <- "workbook"
      whichtype <- vector(mode = "character", length = 1)
      for(t in seq_along(types)){
        type_collapse <- paste(types[[t]], collapse = " ")
        if(str_detect(type_collapse,input_ext)){
          whichtype <- names(types[t])
          whichtype_otherstrings <- types[[t]]
        }
      }
      # folder should contain any of the strings in the matching types() list.
      possible_folders <- type_destfolders
      dest_folder_short <- matching_folder(foldernames = possible_folders, possible_strings = whichtype_otherstrings)
      # which long folder matches this short one?
      long_folders <- type_destfolders
      match_folder <- long_folders[which(str_detect(long_folders, dest_folder_short))]
      matching_folders[f] <- match_folder
    } else if(file_extension == "dat"){
      message("This file is of Type: Sample Queue Blank, but has an ASCII file extension (.dat). This may be a categorisation error.")
    } else{
      message(paste0("Unknown file extension: .",file_extension,"\n",
                     "Skipping file ",sq_match_filenames_short[f]))
    }
  }
  matching_folders
}

#' Given a set of filenames, what are the matching folders they belong to, within the given 'type' folder? For use in all file type cases except type "Sample Queue Blank".
#'
#' @description A spin-off of get_sqblank_folders(), for use with the other types. This could probably be merged with that function in future.
#'
#' @param type_folder the type folder.
#' @param filenames a character vector of filenames to be matched with sub-folders within the type folder.
#' @param input_type one of the acceptable input sample types. See type_strings().
#'
#' @noRd
#'
get_normal_folders <- function(type_folder,
                               filenames,
                               input_type){
  type_rename <- input_type
  type_destfolders <- get_names(directory = type_folder,
                                type = "folders",
                                full_names = TRUE)
  # So, we've got a normal file. They're all going to be either .dat or .ogw.
  matching_folders <- vector(mode = "character", length = length(filenames))
  # Iterate over matching files.
  it_list <- vector(mode = "list", length = length(filenames))
  for(f in seq_along(it_list)){
    # What's the extension of this file?
    file <- filenames[f]
    file_extension <- ext_detect(filenames[f])
    if(file_extension == "ogw"){
      # workbook file handling
      # Get the folder in the sampleq folder that matches this file type.
      types <- extension_strings(type = type_rename)
      input_ext <- "workbook"
      whichtype <- vector(mode = "character", length = 1)
      for(t in seq_along(types)){
        type_collapse <- paste(types[[t]], collapse = " ")
        if(str_detect(type_collapse,input_ext)){
          whichtype <- names(types[t])
          whichtype_otherstrings <- types[[t]]
        }
      }
      # folder should contain any of the strings in the matching types() list.
      possible_folders <- type_destfolders
      dest_folder_short <- matching_folder(foldernames = possible_folders, possible_strings = whichtype_otherstrings)
      # which long folder matches this short one?
      long_folders <- type_destfolders
      match_folder <- long_folders[which(str_detect(long_folders, dest_folder_short))]
      matching_folders[f] <- match_folder
    } else if(file_extension == "dat"){
      # .dat file handling here.
      data_type <- ASCII_data_type(file)
      types <- ASCII_strings()
      whichtype <- vector(mode = "character", length = 1)
      for(t in seq_along(types)){
        type_collapse <- paste(types[[t]], collapse = " ")
        if(str_detect(type_collapse,data_type)){
          whichtype <- names(types[t])
          whichtype_otherstrings <- types[[t]]
        }
      }
      # folder should contain any of the strings in the matching types() list.
      possible_folders <- type_destfolders
      # match folder string.
      dest_folder_short <- matching_folder(foldernames = possible_folders, possible_strings = whichtype_otherstrings)
      # which long folder matches this short one?
      long_folders <- type_destfolders
      short_folders <- trim_path(type_destfolders)

      # Matching folder/s
      match_folder <- long_folders[which(str_detect(long_folders, dest_folder_short))]
      # is there more than one
      if(length(match_folder) > 1){
        # This block removes the target string from shortened versions of the multiple match folders, and identifies which one was the perfect match.
        match_folder_short <- trim_path(match_folder)
        stripped_strings <- str_remove(match_folder_short,dest_folder_short)
        pure_strings <- match_folder[which(stripped_strings == "")]
        # retain perfect match.
        match_folder <- pure_strings
      } else {
        # carry on
      }
      matching_folders[f] <- match_folder

    }
  }
  matching_folders
}

#' Return the export filenames.
#'
#' @description Essentially a tortuous str_replace wrapper. Replaces the sq_name in the supplied filenames with the real_name, generating a set of new filenames.
#'
#' @param filenames a character vector of filenames.
#' @param real_name the "Real_Name" from the run sheet.
#' @param sq_name the "SampleQ_Name" from the run sheet.
#'
#' @noRd
#'
get_real_filenames <- function(filenames,
                               real_name,
                               sq_name){
  if(any(str_detect(filenames,"/"))){
    filenames <- trim_path(filenames)
  }
  #Remove extensions. For one or more files.
  if(length(filenames) > 1){
    it_list <- vector(mode = "list", length = length(filenames))
    new_filenames <- vector(mode = "character", length = length(filenames))
    for(e in seq_along(it_list)){
      #filenames_noext <- unlist(strsplit(filenames[e],"[.]"))[1] # Get the short filename, but remove the extension.
      new_filenames[e] <- str_replace(filenames[e],sq_name,real_name)
    }
  } else{
    #filenames_noext <- strsplit(filenames,"[.]")[1]
    new_filenames <- str_replace(filenames,sq_name,real_name)
  }
  new_filenames
}

#' Returns the ASCII data type from a given file name.
#'
#' @description Single file names only. Extracts the 3-character signifier for SampleQ ASCII data types. Returns NULL if it fails. E.g. "PEM" for XYY Processed Sample-Blank EEM data.
#'
#' @param filename a single file name.
#'
#' @noRd
#'
ASCII_data_type <- function(filename){
  # does the file have an extension?
  extn_check <- str_detect(filename,"[.]")
  if(isTRUE(str_detect(filename,"[.]"))){
    ext <- ext_detect(filename)
  } else{
    ext <- NULL
  }
  if(str_detect(filename,"/")){
    filename <- trim_path(filename)
  }
  # remove extension
  filename_noext <- unlist(strsplit(filename,"[.]"))[1] # Get the short filename, but remove the extension.
  # ASCII type determination
  ASCII_type <- substr(filename_noext,nchar(filename_noext)-2,nchar(filename_noext))
  # Is this type all letters?
  if(ext != "dat" && !is.null(ext)){
    message(filename," doesn't appear to be an ASCII file.")
    ASCII_type <- NULL
  } else if(is.null(ext)){
    if(!isTRUE(grepl("^[A-Za-z]+$", ASCII_type, perl = T))){
      message(filename," is an ASCII file, but there are numbers in the 3-letter ASCII file code (",ASCII_type,"). Is it a supported file type?")
      ASCII_type <- NULL
    }
  } else if(ext == "dat"){
    if(!isTRUE(grepl("^[A-Za-z]+$", ASCII_type, perl = T))){
      message(filename," is an ASCII file, but there are numbers in the 3-letter ASCII file code (",ASCII_type,"). Is it a supported file type?")
      ASCII_type <- NULL
    }
  }
  ASCII_type
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
eem_neg_to_0 <- function(eemlist, outputfolder = NULL){
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
