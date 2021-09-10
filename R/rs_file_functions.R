# Internal functions used to process files that relate to the run sheet (i.e not .txt, project, or other files).
# These functions are utilised by process_sample_queue to identify and match files with their destination folders.

#' Return the matching folder name (short form).
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

#' Return the run sheet row sample type.
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

#' Perform various processing functions on applicable samples using the internal log.
#'
#' @description Uses the internal file log to perform milliq blank subtraction, a negative-to-0 operation, and dilution calculations for all applicable samples/standards/replicates.
#'
#' @param run_sheet A data.frame matching the format of a run sheet. For info on how to format a run sheet, see the package documentation.
#' @param log_file the internal log data frame generated by process_sample_queue, documenting file locations and sample types.
#' @param blank_subtract TRUE/FALSE average any milliq blanks in the run, and subtract the resulting EEM from all samples/standards/replicates. Defaults to TRUE.
#' @param neg_to_0 TRUE/FALSE to set all instances of negative fluorescence in EEMs to 0 prior to export. Defaults to TRUE.
#' @param dilution TRUE/FALSE where dilution factor values are given in the run sheet, perform a corresponding multiplication to the fluorescence intensity values of those EEMs. Defaults to TRUE.
#'
#'@noRd
#'
postprocess_PEM <- function(run_sheet,
                            log_file,
                            blank_subtract = TRUE,
                            neg_to_0 = TRUE,
                            dilution = TRUE){
  message("## Processing starting for run-sheet EEMs")
  log <- log_file
  # Initial trimming of sqblanks and workbooks
  if(length(which(log$type == "Sample Queue Blank")) > 0){
    log <- log[-which(log$type == "Sample Queue Blank"),] # sqblank exclusion if any present
  }
  log_extensions <- ext_detect(log$`exported files`) # check extensions
  if(length(which(log_extensions == "ogw")) > 0){
    log <- log[-which(log_extensions == "ogw"),] # workbook exclusion if any present
  }
  # The log now only contains files associated with samples, replicates, blanks and standards.
  # Select for the PEM files.
  stripped_export_files <- trim_path(log$`exported files`)
  stripped_export_files <- unlist(strsplit(stripped_export_files,"[.]"))
  stripped_export_files <- stripped_export_files[-which(stripped_export_files == "dat")]
  types <- unlist(lapply(stripped_export_files,ASCII_data_type))
  log_PEM <- log[which(types == "PEM"),]
  # logs
  PEM_mqblank_log <- log_PEM[which(log_PEM$type == "MilliQ Water Blank"),]
  PEM_sample_log <- log_PEM[which(log_PEM$type == "Sample" | log_PEM$type == "Replicate" | log_PEM$type == "Standard"),]
  # Dilution Handling setup
  if(isTRUE(dilution)){
    PEM_sample_log$dilution <- NA
    # get sample names without extensions
    names <- trim_path(PEM_sample_log$`exported files`)
    names <- unlist(lapply(str_split(names,"[.]"),"[",1))
    names <- str_remove(names,"PEM")
    # Get samples which have dilution info
    dilfac_samples <- run_sheet$Real_Name[which(!is.na(run_sheet$Dilution_Factor))]
    quick_it_list <- vector(mode = "list", length = length(dilfac_samples))
    for(q in seq_along(quick_it_list)){
      it_row <- as.numeric(which(names == dilfac_samples[q]))
      PEM_sample_log$dilution[it_row] <- run_sheet$Dilution_Factor[which(!is.na(run_sheet$Dilution_Factor))][q]
    }
  }
  # filenames
  PEM_mqblanks <- as.character(log_PEM[which(log_PEM$type == "MilliQ Water Blank"),]$`exported files`)
  PEM_samples <- as.character(log_PEM[which(log_PEM$type == "Sample" | log_PEM$type == "Replicate" | log_PEM$type == "Standard"),]$`exported files`)
  # Average the blank EEMs, if applicable
  if(isTRUE(any(log$type == "MilliQ Water Blank"))){
    if(isTRUE(blank_subtract)){
      blank_eemlist <- eem_read_mod(file = PEM_mqblanks)
      blank_eemlist <- average_eems(eemlist = blank_eemlist)
      blank_eem <- blank_eemlist[[1]]
    }
  }
  ## New code
  #Iterating along sample types, doing the work on each in turn.
  sample_types <- unique(PEM_sample_log$type)
  type_itlist <- vector(mode = "list", length = length(sample_types))
  for(t in seq_along(type_itlist)){
    # sample type for this iteration
    type_it <- sample_types[t]
    # sample details from log
    PEM_samples_it_log <- PEM_sample_log[which(PEM_sample_log$type == type_it),]
    PEM_samples_it <- as.character(PEM_samples_it_log$`exported files`)
    if(isTRUE(dilution)){
      # Do any of these samples need dilution?
      dilfac_indices <- which(!is.na(PEM_samples_it_log$dilution))
      if(length(dilfac_indices) > 0){
        names <- trim_path(PEM_samples_it_log$`exported files`)
        names <- unlist(lapply(str_split(names,"[.]"),"[",1))
        names <- str_remove(names,"PEM")
      }
    }
    # get the sample names for future use.
    names_short <- trim_path(PEM_samples_it)
    names_short_noext <- sapply(strsplit(names_short,"[.]"), "[[", 1)
    # folder for this type
    type_folder <- strsplit(PEM_samples_it_log$`exported files`,"/")[[1]]
    parent_typefolder <- paste0(paste(type_folder[1:(length(strsplit(PEM_samples_it_log$`exported files`,"/")[[1]])-2)],collapse = "/"),"/")
    # long and short folder names within the parent type folder.
    parent_fnames <- get_names(parent_typefolder,type = "folders")
    parent_fnames_short <- trim_path(parent_fnames)
    target_folder <- paste0(parent_fnames[which(str_detect(parent_fnames_short,"updated") == TRUE)],"/")
    # get the samples for this type.
    sample_eemlist <- eem_read_mod(file = PEM_samples_it)
    ## Processing steps!
    # 1) Blank subtraction
    if(!isTRUE(any(log$type == "MilliQ Water Blank"))){
      message("No milliq blanks. Skipping blank subtraction.")
      eems_subtracted <- sample_eemlist
    } else{
      if(isTRUE(blank_subtract)){
        # perform subtraction
        eems_subtracted <- eemlist_subtract(eems_minuend = sample_eemlist,
                                            eem_subtrahend = blank_eem)
        message(paste0("Blank subtraction performed for ",type_it," EEMs"))
      }
    }
    # 2) negative to NA
    if(isTRUE(neg_to_0)){
      eems_subtracted <- eemlist_neg_to_0(eems_subtracted)
      message(paste0("Negative fluorescence intensity values set to 0 for ",type_it," EEMs"))
    }
    # 3) Dilution processing
    if(isTRUE(dilution)){
      if(length(dilfac_indices) > 0){
        message("Dilution processing to be performed for sample/s ",paste(names[dilfac_indices], collapse = ", "))
        # Which samples have dilution data?
        names <- trim_path(PEM_samples_it_log$`exported files`)
        names <- unlist(lapply(str_split(names,"[.]"),"[",1))
        # Dilution iterations
        q_it_list <- vector(mode = "list", length = length(dilfac_indices))
        for(z in seq_along(q_it_list)){
          # Pull out the EEM for this iteration for sample
          eem_names <- unlist(lapply(eems_subtracted,"[[",'sample'))
          # Where is the target EEM in the eemlist?
          which_eem <- as.numeric(which(eem_names == names[which(!is.na(PEM_samples_it_log$dilution))][z]))
          target_eem <- eems_subtracted[which_eem][[1]]
          # Do the dilution calculation
          dilfac <- as.numeric(PEM_samples_it_log$dilution[dilfac_indices[z]])
          target_eem_df <- as.data.frame(target_eem, gather = FALSE)
          target_eem_df_proc <- target_eem_df*dilfac
          # Coerce back to data.frame
          target_eem_res <- data_frame_to_eem(x = target_eem_df_proc,
                                              file = target_eem$file,
                                              sample = target_eem$sample,
                                              location = target_eem$location)
          # Insert it back into the eemlist
          eems_subtracted[[which_eem]] <- target_eem_res
        }
        message(paste0("Dilution intensity correction performed for ",type_it," EEMs"))
      } else{
        message("No samples on run sheet of type ",type_it," have dilution factors listed; skipping dilution.")
      }
    }
    # export for this type
    message("Saving updated EEMs for type: ",type_it)
    eemUtils::save_eemlist_csvs(eemlist = eems_subtracted,
                                outputfolder = target_folder)
  }
}
