# Functions used to create log files. All bar generate_logfile() are not exported.

#' Save a log file as a .txt using write.table.
#'
#' @description Write a log file to the specified destination.
#'
#' @param run_sheet The run sheet for this folder.
#' @param destination A full path to the export directory.
#' @param folder
#'
#' @export
#
generate_logfile <- function(run_sheet,
                             destination,
                             folder){
  target_run_sheet <- run_sheet # prevent recursive because my naming conventions leave a lot to be desired
  target_folder <- folder # "
  file_names_full <- get_names(directory = target_folder, type = "files", full_names = TRUE)
  file_names_short <- get_names(directory = target_folder, type = "files", full_names = FALSE)
  # Get the extensions for all the files in the folder.
  extensions <- ext_detect(file_names_short)
  # Pull out the text files - short and long form.
  txt_files <- file_names_short[which(extensions == "txt")]
  txt_files_full <- file_names_full[which(extensions == "txt")]
  # Create a blank logfile.
  datetime <- Sys.time()
  folder_name_short <- unlist(strsplit(target_folder, "/"))
  folder_name_short <- folder_name_short[length(folder_name_short)]
  logfile <- blank_log_file(datetime = datetime, folder_name_short = folder_name_short)
  # what's it called? Where is it going to go?
  logfile_destination <- destination
  logfile_name <- paste0("log file ",folder_name_short)
  # Add the runsheet
  runsheet_compressed <- format_runsheet_for_log(target_run_sheet, target_folder)
  logfile <- append_txt_to_log(log_file = logfile, txt_file_df = runsheet_compressed)
  # Loop through other text files, adding them to the end.
  if(length(txt_files) > 0){
    # So, there's text files.
    it_list = vector(mode = "list", length = length(txt_files))
    for(i in seq_along(it_list)){
      # import the file
      txt_file_name <- txt_files[i]
      txt_file <- readLines(con = txt_files_full[i], warn=FALSE)
      # format it (compress, add spacing and title rows)
      txt_file_df <- format_txt_for_log(txt_file = txt_file, txt_file_name = txt_file_name)
      # tack the text file onto the blank log file.
      logfile <- append_txt_to_log(log_file = logfile, txt_file_df = txt_file_df)
    }
  }
  # Now, send the log file to the logs folder.
  save_log(log_file = logfile,
           destination = logfile_destination,
           name = logfile_name,
           append_date = datetime)
  message("Logfile ",logfile_name," generated successfully.")
}

#' Create a blank log file.
#'
#' @description Create a simple dataframe comprised of three lines of text to start off a log file.
#'
#' @param datetime Either NULL or a provided Sys.time call. If set to NULL, one will be generated within the function.
#' @param folder_name_short Character string: the name of the folder containing the target files, without its path.
#'
#' @noRd
#
blank_log_file <- function(datetime = NULL, folder_name_short){
  if(is.null(datetime)){
    datetime <- Sys.time()
  }
  blank_log <- data.frame(matrix(NA,4,1))
  blank_log[1,] <- paste0("This is an automatically generated log file for the folder ", "[",folder_name_short,"].")
  blank_log[2,] <- paste0("All text files from the folder are collated below, along with the run sheet.")
  blank_log[3,] <- paste0("Log file generated on ", format(datetime, "%d/%m/%y"), " at ", format(datetime, "%H:%M:%S."))
  blank_log[4,] <- c(" ")
  blank_log
}

#' Format a .txt file to be added to a log file.
#'
#' @description read the lines in a text file into a simple, one-column data frame, and add title lines.
#'
#' @param txt_file A text file, imported with readLines().
#' @param txt_file_name Character string: the short-form name (i.e. no path attached) of the above text file.
#'
#' @noRd
#
format_txt_for_log <- function(txt_file, txt_file_name){
  df <- data.frame(matrix(NA,nrow = length(txt_file),ncol = 1))
  df[,] <- txt_file
  # Prepare the intermediate rows
  rows_insert <- data.frame(matrix(NA,4,1))
  rows_insert[1,] <- c(" ")
  rows_insert[2,]<- paste0("File: ",txt_file_name)
  rows_insert[3,] <- c(" ")
  rows_insert[4,] <- paste0("===== NEW FILE =====")
  # insert the intermediate rows
  ins_list <- vector(mode = "list", length = nrow(rows_insert))
  for(z in seq_along(ins_list)){
    df <- insert_row(df,rows_insert[z,1],1)
  }
  txt_file_df <- insert_row(df," ",nrow(df)+1)
  txt_file_df
}

#' Format a run sheet to be added to a log file.
#'
#' @description Similar to format_txt_for_log, but for the run sheet.
#'
#' @param run_sheet The run sheet for this folder's samples.
#' @param folder Character string: the folder containing the samples related to this run sheet. Either short or long form filename.
#'
#' @noRd
#
format_runsheet_for_log <- function(run_sheet, folder){
  # Name line
  name <- paste0("Input run sheet for ",unlist(strsplit(folder,split ="/"))[length(unlist(strsplit(folder,split = "/")))])
  run_sheet_compressed <- data.frame(matrix(NA,nrow = nrow(run_sheet)+1, ncol = 1)) # init
  run_sheet_compressed[1,] <- paste(colnames(run_sheet), collapse = " ") # header row
  run_sheet_compressed[2:nrow(run_sheet_compressed),1] <- apply(run_sheet[,c(1:ncol(run_sheet))], 1, paste, collapse = " ") # other rows
  df <- run_sheet_compressed
  rows_insert <- data.frame(matrix(NA,4,1))
  rows_insert[1,] <- c(" ")
  rows_insert[2,]<- paste0("File: ",name)
  rows_insert[3,] <- c(" ")
  rows_insert[4,] <- paste0("===== NEW FILE =====")
  # insert the intermediate rows
  ins_list <- vector(mode = "list", length = nrow(rows_insert))
  for(z in seq_along(ins_list)){
    df <- insert_row(df,rows_insert[z,1],1)
  }
  run_sheet_forlog <- insert_row(df," ",nrow(df)+1)
  run_sheet_forlog
}

#' Attach a compressed .txt file to a log file dataframe.
#'
#' @description Attach a compressed .txt file to an existing log file.
#'
#' @param log_file The log file
#' @param txt_file_df The formatted .txt file to be attached. An output of format_txt_for_log().
#'
#' @noRd
#
append_txt_to_log <- function(log_file,txt_file_df){
  names(txt_file_df) <- names(log_file) # make sure the names are the same. Doesn't matter what they are; just have to be the same.
  log_file_app <- rbind(log_file,txt_file_df) # bind
  log_file_app # return
}

#' Save a log file as a .txt using write.table.
#'
#' @description Write a log file to the specified destination.
#'
#' @param log_file The log file.
#' @param destination A full path to the log file folder. Log file will be saved here.
#' @param name The name of the log file.
#' @param append_date either NULL or a date of format %d/%m/%y or %d%m%y.
#'
#' @noRd
#
save_log <- function(log_file,
                     destination,
                     name,
                     append_date = NULL){
  if(!is.null(append_date)){
    name <- paste0(name," gen", date_compact(format(append_date, "%d/%m/%y")))
  }
  write.table(log_file, paste0(destination, name, ".txt"), col.names = FALSE, quote = FALSE, row.names = FALSE)
}


