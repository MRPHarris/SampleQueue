#' The main function of the SampleQueue package. Automatically processes Aqualog sample queue output files when given a set of parameters including a run sheet.
#'
#' @description Copies, renames and sorts output files from the Aqualog sample queueing system based upon a supplied run sheet. Also creates a log file using generate_logfile() and copies, renames .opj project files using transfer_project_files().
#'
#' @param folder The folder within the import directory containing all the files from the sample queue run related to the supplied run sheet.
#' @param export_dir The export directory containing the 'type' subfolders.
#' @param run_date Date of format %d/%m/%y on which the run was completed on the Aqualog.
#' @param run_sheet A data.frame matching the format of a run sheet. For info on how to format a run sheet, see the package documentation.
#' @param write_over TRUE/FALSE file.copy parameter. Write over identically named files in the destination folders?
#'
#' @export
#
process_sample_queue <- function(folder,
                                 export_dir,
                                 run_date,
                                 run_sheet,
                                 write_over = TRUE){
  # 1) create a log file, store it in the sub-folder containing "logs".
  generate_logfile(run_sheet = run_sheet,
                   destination = paste0(export_dir,"logs/"),
                   folder = folder)

  # 2) Copy and re-name the project file.
  transfer_project_files(foldername = folder,
                         run_date = run_date,
                         export_directory = export_dir)

  # 3) Process the import folder using a run sheet.
  file_names_full <- get_names(directory = folder,
                               type = "files",
                               full_names = TRUE)
  file_names_full <- file_names_full[which(ext_detect(file_names_full) != "txt")]
  file_names_full <- file_names_full[which(ext_detect(file_names_full) != "opj")]
  file_names_short <- trim_path(file_names_full)
  # init the file log. This will eventually be used for the blank subtraction. Now it's just... sitting there.
  file_log <- data.frame(matrix(NA,nrow = length(file_names_full),ncol = 2))
  colnames(file_log) <- c("exported files","type")
  fn_it_list <- vector(mode = "list", length = nrow(run_sheet))
  for(r in seq_along(fn_it_list)){
    row_it <- run_sheet[r,]
    # read in row attributes.
    input_sq_name <- row_it$SampleQ_Name
    input_real_name <- row_it$Real_Name
    input_type <- get_type(row_it)
    # What files match this row?
    sq_match_filenames_full <- file_names_full[str_detect(file_names_full,input_sq_name)]
    sq_match_filenames_short <- file_names_short[str_detect(file_names_short,input_sq_name)]
    # How many files in this loop it?
    if(r == 1){
      log_rowstart <- 1
    }
    # Main distinguishing elements.
    if(input_type == "Sample Queue Blank"){
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
      file_log[c((log_rowstart):(log_rowstart+(length(filenames_to)-1))),1] <- filenames_to
      file_log[c((log_rowstart):(log_rowstart+(length(filenames_to)-1))),2] <- input_type
      log_rowstart <- log_rowstart+(length(filenames_to))
      # Ok, now do the file.copy call!
      check <- file.copy(from = filenames_from, to = filenames_to, overwrite = write_over)
      # Completion message
      if(all(check)){
        message("Files related to run-sheet row ",r," (",input_type,")"," were processed successfully.")
      } else{
        message("There was an issue copying the file/s from run sheet row ",r,".")
      }
    } else if(input_type == "Sample" || input_type == "MilliQ Water Blank" || input_type == "Replicate" || input_type == "Standard"){
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
      file_log[c((log_rowstart):(log_rowstart+(length(filenames_to)-1))),1] <- filenames_to
      file_log[c((log_rowstart):(log_rowstart+(length(filenames_to)-1))),2] <- input_type
      log_rowstart <- log_rowstart+(length(filenames_to))
      # Ok, now do the file.copy call!
      check <- file.copy(from = filenames_from, to = filenames_to, overwrite = write_over)
      # Completion message
      if(all(check)){
        message("Files related to run-sheet row ",r," (",input_type,")"," were processed successfully.")
      } else{
        message("There was an issue copying the file/s from run sheet row ",r,".")
      }
    }

  }
}
