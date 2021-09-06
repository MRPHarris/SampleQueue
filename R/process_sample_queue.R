#' Rename and collate Aqualog sample queue output files.
#'
#' @description Copies, renames and sorts output files from the Aqualog sample queueing system based upon a supplied run sheet. Also creates a log file using generate_logfile() and copies, renames .opj project files using transfer_project_files(). Options for simple post-processing (e.g. blank subtraction)
#'
#' @param folder The folder within the import directory containing all the files from the sample queue run related to the supplied run sheet.
#' @param export_dir The export directory containing the 'type' subfolders.
#' @param run_date Date of format dd/mm/yy on which the run was completed on the Aqualog.
#' @param run_sheet A data.frame matching the format of a run sheet. For info on how to format a run sheet, see the package documentation.
#' @param write_over TRUE/FALSE file.copy parameter. Write over identically named files in the destination folders?
#' @param mqblank_sub TRUE/FALSE. Imports sample types and subtracts an average of all milli-q blanks in the run from each. Exports .csv outputs to the "blank subtracted PEM" folder in each sample type folder.
#' @param dilution_process TRUE/FALSE where dilution factor values are given in the run sheet, perform a corresponding multiplication to the fluorescence intensity values of those EEMs. Defaults to TRUE.
#' @param neg_to_0 TRUE/FALSE to set all instances of negative fluorescence in EEMs to 0 prior to export. Defaults to TRUE.
#' @param dry_run TRUE/FALSE to skip all file copying and return a log of all file import and export paths. Simulates a 'real' run.
#'
#' @export
#'
process_sample_queue <- function(folder,
                                 export_dir,
                                 run_date,
                                 run_sheet,
                                 write_over = TRUE,
                                 mqblank_sub = TRUE,
                                 dilution_process = TRUE,
                                 neg_to_0 = TRUE,
                                 dry_run = FALSE){
  if(isTRUE(dry_run)){
    message("Dry run. Skipping log file creation and project file transfer.")
  } else if(!isTRUE(dry_run)){
    # Create a log file, store it in the sub-folder containing "logs".
    generate_logfile(run_sheet = run_sheet,
                     destination = paste0(export_dir,"logs/"),
                     folder = folder)
    # Copy and re-name the project file/s, if there is one.
    transfer_project_files(foldername = folder,
                           run_date = run_date,
                           export_directory = export_dir)
  }
  # Process the import folder using a run sheet.
  file_names_full <- get_names(directory = folder,
                               type = "files",
                               full_names = TRUE)
  file_names_full <- file_names_full[which(ext_detect(file_names_full) != "txt")]
  file_names_full <- file_names_full[which(ext_detect(file_names_full) != "opj")]
  file_names_short <- trim_path(file_names_full)
  # init the file log.
  file_log <- data.frame(matrix(NA,nrow = length(file_names_full),ncol = 3))
  colnames(file_log) <- c("imported files","exported files","type")
  fn_it_list <- vector(mode = "list", length = nrow(run_sheet))
  for(r in seq_along(fn_it_list)){
    row_it <- run_sheet[r,]
    # read in row attributes.
    input_sq_name <- row_it$SampleQ_Name
    input_real_name <- row_it$Real_Name
    type_it <- get_type(row_it)
    # What files match this row?
    sq_match_filenames_full <- file_names_full[str_detect(file_names_full,input_sq_name)]
    sq_match_filenames_short <- file_names_short[str_detect(file_names_short,input_sq_name)]
    # How many files in this loop it?
    if(r == 1){
      log_rowstart <- 1
    }
    # Main distinguishing elements.
    if(type_it == "Sample Queue Blank"){
      ## SQ BLANK FILES
      # This is the folder where the files will end up.
      row_type_folder <- get_type_folder(run_sheet_row = row_it,
                                         export_dir = export_dir)
      # possible folders. Defunct line?
      #type_destfolders <- get_names(directory = row_type_folder,
      #                              type = "folders",
      #                              full_names = TRUE)
      # Original, full filenames. The "to" part of the eventual file.copy() call.
      filenames_from <- sq_match_filenames_full
      # These are the sub-folders, matched to the filenames.
      dest_folders <- get_sqblank_folders(type_folder = row_type_folder,
                                          filenames = filenames_from)
      # Here are the real sample names using a str_replace() wrapper function.
      files_to_short <- get_real_filenames(filenames = filenames_from,
                                           real_name = input_real_name,
                                           sq_name = input_sq_name)
      # Generating the filenames_to
      it_list <- vector(mode = "list",length = length(files_to_short))
      filenames_to <- vector(mode = "character",length = length(files_to_short))
      for(f in seq_along(it_list)){
        filenames_to[f] <- paste0(dest_folders[f],"/",files_to_short[f])
      }
      # Add the files to the log
      file_log[c((log_rowstart):(log_rowstart+(length(filenames_to)-1))),1] <- filenames_from
      file_log[c((log_rowstart):(log_rowstart+(length(filenames_to)-1))),2] <- filenames_to
      file_log[c((log_rowstart):(log_rowstart+(length(filenames_to)-1))),3] <- type_it
      log_rowstart <- log_rowstart+(length(filenames_to))
      # Ok, now do the file.copy call! Or not, if it's a dry run.
      if(!isTRUE(dry_run)){
        check <- file.copy(from = filenames_from, to = filenames_to, overwrite = write_over)
        # Completion message
        if(all(check)){
          message("Files related to run-sheet row ",r," (",type_it,")"," were processed successfully.")
        } else{
          message("There was an issue copying the file/s from run sheet row ",r,".")
        }
      } else if(isTRUE(dry_run)){
        message("Dry run. Files related to run-sheet row ",r," (",type_it,")"," were added to the file log.")
      }
    } else if(type_it == "Sample" || type_it == "MilliQ Water Blank" || type_it == "Replicate" || type_it == "Standard"){
      ## "NORMAL" FILES
      # This is the type folder where the files will end up.
      row_type_folder <- get_type_folder(run_sheet_row = row_it,
                                         export_dir = export_dir)
      # These are the possible sub-folders. Defunct?
      #type_destfolders <- get_names(directory = row_type_folder,
      #                              type = "folders",
      #                              full_names = TRUE)
      # Original, full filenames. The "to" part of the eventual file.copy() call.
      filenames_from <- sq_match_filenames_full
      dest_folders <- get_normal_folders(type_folder = row_type_folder,
                                         filenames = filenames_from,
                                         input_type = type_it)
      # Here are the real sample names using a str_replace() wrapper function.
      files_to_short <- get_real_filenames(filenames = filenames_from,
                                           real_name = input_real_name,
                                           sq_name = input_sq_name)
      # Generating the filenames_to
      it_list <- vector(mode = "list",length = length(files_to_short))
      filenames_to <- vector(mode = "character",length = length(files_to_short))
      for(f in seq_along(it_list)){
        filenames_to[f] <- paste0(dest_folders[f],"/",files_to_short[f])
      }
      # Add the files to the log
      file_log[c((log_rowstart):(log_rowstart+(length(filenames_to)-1))),1] <- filenames_from
      file_log[c((log_rowstart):(log_rowstart+(length(filenames_to)-1))),2] <- filenames_to
      file_log[c((log_rowstart):(log_rowstart+(length(filenames_to)-1))),3] <- type_it
      log_rowstart <- log_rowstart+(length(filenames_to))
      # Ok, now do the file.copy call! Or not, if it's a dry run.
      if(!isTRUE(dry_run)){
        check <- file.copy(from = filenames_from, to = filenames_to, overwrite = write_over)
        # Completion message
        if(all(check)){
          message("Files related to run-sheet row ",r," (",type_it,")"," were processed successfully.")
        } else{
          message("There was an issue copying the file/s from run sheet row ",r,".")
        }
      } else if(isTRUE(dry_run)){
        # completion message for dry run.
        message("Dry run. Files related to run-sheet row ",r," (",type_it,")"," were added to the file log.")
      }
    }
  }
  # Post processing query
  if(!isTRUE(dry_run)){
    # Any post processing to
    pprocess <- any(isTRUE(mqblank_sub) || isTRUE(dilution_process) || isTRUE(neg_to_0))
    if(isTRUE(pprocess)){
      postprocess_PEM(run_sheet = run_sheet,
                      log_file = file_log,
                      neg_to_0 = neg_to_0,
                      dilution = dilution_process,
                      blank_subtract = mqblank_sub)
    }
  } else if(isTRUE(dry_run)){
    return(file_log)
  }
}


