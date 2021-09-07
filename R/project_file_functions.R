# Functions used to rename and save Aqualog project files (.opj files).

#' Transfer project files (.opj extension) to the project file folder, with some renaming.
#'
#' @description Takes all the project files in the target folder and moves them to the project file folder inside the export directory.
#'
#' @param foldername Name of the folder containing the project file and other target samples.
#' @param run_date The date of the run in format ddmmyy.
#' @param export_directory A path to the export directory.
#'
#' @export
#'
transfer_project_files <- function(foldername, run_date, export_directory){
  folder <- foldername
  rundate <- run_date
  # Get the file names again
  file_names_full <- get_names(directory = folder, type = "files", full_names = TRUE)
  file_names_short <- get_names(directory = folder, type = "files", full_names = FALSE)
  # Get the extensions for all the files in the folder.
  extensions <- ext_detect(file_names_short)
  # Pull out the text files - short and long form.
  project_files_short <- file_names_short[which(extensions == "opj")]
  project_files_full <- file_names_full[which(extensions == "opj")]
  if(length(project_files_short) > 0){
    # So, there's at least one project file.
    it_list <- vector(mode = "list", length = length(project_files_short))
    for(p in seq_along(it_list)){
      # Set up a looped file.copy.
      # get the destination
      projfile = project_files_short[p]
      projfile_destination <- paste0(export_directory,"project files/")
      # projfile new short name
      projfile_name_short <- projectfile_rename(project_file = projfile, folder_name = folder, rundate = run_date)
      # name from
      projfile_name_from <- project_files_full[p]
      projfile_name_to <- paste0(projfile_destination,projfile_name_short)
      # copy the file.
      file.copy(from = projfile_name_from,
                to = projfile_name_to)
    }
  }
  message("Project files transferred.")
}

#' Rename a given project file name. Output will be a short file name, with info from folder name.
#'
#' @description Create a simple dataframe comprised of three lines of text to start off a log file.
#'
#' @param project_file A project file name. Either short-form or long.
#' @param folder_name Name of the folder containing the project file and other target samples.
#' @param rundate The date of the run in format ddmmyy.
#'
#' @noRd
#'
projectfile_rename <- function(project_file, folder_name, rundate){
  # Are there backslashes present? If yes, break apart and pick last string, which should be the projfile short name.
  checkslash <- str_detect(project_file,"/")
  if(isTRUE(checkslash)){
    project_file <- unlist(strsplitproject_file)[length(unlist(strsplit(project_file,"/")))]
  }
  # Pick apart the name. Remove extension, and split by spaces.
  projfile_datecheck <- compact_date_present(string = project_file)
  if(!is.null(projfile_datecheck)){
    # There's a date present. Use it.
    projfile_date <- projfile_datecheck
  } else{
    # There isn't a date present. Use the run_date.
    projfile_date <- rundate
  }
  # How about the foldername?
  folder_name_short <- unlist(strsplit(folder_name, "/"))
  folder_name_short <- folder_name_short[length(folder_name_short)]
  folder_datecheck <- compact_date_present(string = folder_name_short)
  if(!is.null(folder_datecheck)){
    # There is a date present. Remove it.
    folder_name_spacesplit <- unlist(strsplit(folder_name_short,"\\s+"))
    folder_name_nodate <- folder_name_spacesplit[which(folder_name_spacesplit != folder_datecheck)]
    folder_name_nodate <- paste(folder_name_nodate, collapse = " ")
  } else{
    # There isn't a date present. Just use the folder name then.
    folder_name_nodate <- folder_name_short
  }
  projfile_newname_short <- paste0(projfile_date," ",folder_name_nodate,".opj")
  projfile_newname_short
}

