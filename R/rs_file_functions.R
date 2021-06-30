# Internal functions used to process files that relate to the run sheet (i.e not .txt, project, or other files).

#' Return the matching foldername (short form).
#'
#' @description Return the matching foldername (short form) when supplied with a number of names and possible strings to match.
#'
#' @param foldernames a character vector containing one or more folder names.
#' @param possible_strings a character vector containing one or more strings to be matched with the folder names.
#'
#' @noRd
#
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
#
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
#
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
#
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
#
get_normal_folders <- function(type_folder,
                               filenames,
                               input_type){
  type_rename = input_type
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
      dest_folder_short <- matching_folder(foldernames = possible_folders, possible_strings = whichtype_otherstrings)
      # which long folder matches this short one?
      long_folders <- type_destfolders
      match_folder <- long_folders[which(str_detect(long_folders, dest_folder_short))]
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
#
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
#
ASCII_data_type <- function(filename){
  ext <- ext_detect(filename)
  if(str_detect(filename,"/")){
    filename <- trim_path(filename)
  }
  # remove extension
  filename_noext <- unlist(strsplit(filename,"[.]"))[1] # Get the short filename, but remove the extension.
  # ASCII type determination
  ASCII_type <- substr(filename_noext,nchar(filename_noext)-2,nchar(filename_noext))
  # Is this type all letters?
  if(ext != "dat"){
    message(filename," doesn't appear to be an ASCII file.")
    ASCII_type <- NULL
  } else if(ext == "dat"){
    if(!isTRUE(grepl("^[A-Za-z]+$", ASCII_type, perl = T))){
      message(filename," is an ASCII file, but there are numbers in the 3-letter ASCII file code (",ASCII_type,"). Is it a supported file type?")
      ASCII_type <- NULL
    }
  }
  ASCII_type
}
