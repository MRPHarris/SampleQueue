# Functions used to set up the folders/directories used in the SampleQueue package.

#' Create the sample queue folder framework.
#'
#' @description Uses default_folder_layout() to create a set of nested folders that are used by the file handling functions. It does not overwrite; you can "fill out" an incomplete folder set.
#'
#' @param parent_directory the folder within which all the folders will be created
#' @param type one of "all", "import" or "export". "All" will create all the folders, and the other two options will only fill out those respective folders.
#'
#' @export
#'
create_queue_folders <- function(parent_directory,
                                 type = "all"){
  ## type input checks
  if((!isTRUE(type == "all")) && (!isTRUE(type == "import")) && (!isTRUE(type == "export"))){
    stop("Please specify type as one of 'all', 'import' or 'export'")
  }
  # get the imported folder layout
  folders <- default_folder_layout()
  if(type == "all"){
    ## big ol' nested loop
    for(l1 in seq_along(folders)){
      ## FIRST LEVEL: MAIN FOLDERS
      dir_path_l1 = paste0(parent_directory,names(folders[l1]),"/")
      dir.create(path = dir_path_l1, showWarnings = FALSE)
      # is there anything in this level?
      if(!is.null(folders[[l1]])){
        for(l2 in seq_along(folders[[l1]])){
          # If yes: SECOND LEVEL: SAMPLES/BLANKS/STANDARDS
          dir_path_l2 = paste0(dir_path_l1,names(folders[[l1]][l2]),"/")
          dir.create(path = dir_path_l2, showWarnings = FALSE)
          # is there anything in this level?
          if(!is.null(folders[[l1]][[l2]])){
            # If yes: THIRD LEVEL: FILE TYPE
            for(l3 in seq_along(folders[[l1]][[l2]])){
              dir_path_l3 = paste0(dir_path_l2,names(folders[[l1]][[l2]])[l3],"/")
              dir.create(path = dir_path_l3, showWarnings = FALSE)
            }
          }
        }
      }
    }
  }
  if(type == "import"){
    l1 = 1
    dir_path_l1 = paste0(parent_directory,names(folders[1]),"/")
    dir.create(path = dir_path_l1, showWarnings = FALSE)
    # is there anything in this level?
    if(!is.null(folders[[1]])){
      for(l2 in seq_along(folders[[l1]])){
        # If yes: SECOND LEVEL: SAMPLES/BLANKS/STANDARDS
        dir_path_l2 = paste0(dir_path_l1,names(folders[[l1]][l2]),"/")
        dir.create(path = dir_path_l2, showWarnings = FALSE)
        # is there anything in this level?
        if(!is.null(folders[[l1]][[l2]])){
          # If yes: THIRD LEVEL: FILE TYPE
          for(l3 in seq_along(folders[[l1]][[l2]])){
            dir_path_l3 = paste0(dir_path_l2,names(folders[[l1]][[l2]])[l3],"/")
            dir.create(path = dir_path_l3, showWarnings = FALSE)
          }
        }
      }
    }
  }
  if(type == "export"){
    l1 = 2
    dir_path_l1 = paste0(parent_directory,names(folders[2]),"/")
    dir.create(path = dir_path_l1, showWarnings = FALSE)
    # is there anything in this level?
    if(!is.null(folders[[2]])){
      for(l2 in seq_along(folders[[l1]])){
        # If yes: SECOND LEVEL: SAMPLES/BLANKS/STANDARDS
        dir_path_l2 = paste0(dir_path_l1,names(folders[[l1]][l2]),"/")
        dir.create(path = dir_path_l2, showWarnings = FALSE)
        # is there anything in this level?
        if(!is.null(folders[[l1]][[l2]])){
          # If yes: THIRD LEVEL: FILE TYPE
          for(l3 in seq_along(folders[[l1]][[l2]])){
            dir_path_l3 = paste0(dir_path_l2,names(folders[[l1]][[l2]])[l3],"/")
            dir.create(path = dir_path_l3, showWarnings = FALSE)
          }
        }
      }
    }
  }
  message("Folder framework creation attempt complete. Folder tree below.")
  object_tree(directory = parent_directory,
              type = "folders")
}

#' The default folder layout used by the SampleQueue package.
#'
#' @description A list, containing further nested lists mirroring the folder layout used by the SampleQueue package. Used by create_queue_folders() to create the folder framework used by this package.
#'
#' @noRd
#'
default_folder_layout <- function(){
  folderx2 <- vector(mode = "list", length = 2)
  folderx3 <- vector(mode = "list", length = 3)
  folderx4 <- vector(mode = "list", length = 4)
  folderx7 <- vector(mode = "list", length = 7)
  # Highest level folders
  SampleQueueFolders <- folderx2
  names(SampleQueueFolders) = c("1 imported files", "2 exported files")
  # First tier export folders
  SampleQueueFolders[[2]] <- folderx7
  names(SampleQueueFolders[[2]]) <- c("milliq blanks","sampleq blanks","samples","standards","project files","logs","replicates")
  # Second tier export folders
  SampleQueueFolders[[2]][["milliq blanks"]] <- folderx3
  names(SampleQueueFolders[[2]][["milliq blanks"]]) <- c("ABS","PEM","workbooks")
  SampleQueueFolders[[2]][["sampleq blanks"]] <- vector(mode = "list", length = 2)
  names(SampleQueueFolders[[2]][["sampleq blanks"]]) <- c("blank files","workbooks")
  SampleQueueFolders[[2]][["samples"]] <- folderx4
  names(SampleQueueFolders[[2]][["samples"]]) <- c("ABS","updated PEM","PEM","workbooks")
  SampleQueueFolders[[2]][["standards"]] <- folderx4
  names(SampleQueueFolders[[2]][["standards"]]) <- c("ABS","updated PEM","PEM","workbooks")
  SampleQueueFolders[[2]][["replicates"]] <- folderx4
  names(SampleQueueFolders[[2]][["replicates"]]) <- c("ABS","updated PEM","PEM","workbooks")
  return(SampleQueueFolders)
}
