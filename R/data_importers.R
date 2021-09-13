## This script contains various data importers for data types churned out by SampleQueue.
# Where possible, they have been adapted from existing R packages to ensure compliance with the
# eemR/staRdom/EEM framework. Please reference the necessary packages if using these fns.

#' Read in ABS files.
#'
#' @description Read in one or more Absorbance (ABS) ASCII .dat files from a given directory. This function is a direct
#'    modification of absorbance_read() from the staRdom package, with adjustments made to the filename
#'    fetching lines (get_names instead of list.files) to ensure .dat files are correctly fetched.
#'
#' @param ABS_path full path to either (1) a folder containing multiple ABS .dat files, or (2) a single absorbance file.
#' @param order TRUE/FALSE to order data according to wavelength.
#' @param recursive TRUE/FALSE to recurse into sub-directories.
#' @param str_match Use simple string-matching to ensure the indexed absorbance files contain the string "ABS", as standard in Absorbance files from the Aqualog Sample Queue system.
#' @param dec see ?staRdom::absorbance_read(). Table decimal separator testing.
#' @param sep see ?staRdom::absorbance_read(). Optional or automatic separator determination.
#' @param verbose TRUE/FALSE to provide additional information whilst running.
#' @param cores number of CPU cores used for parallel computation. Defaults to maximum, detected automatically.
#'
#' @export
#'
ABS_read <- function (ABS_path, order = TRUE, recursive = FALSE, dec = NULL, str_match = TRUE,
                      sep = NULL, verbose = TRUE, cores = parallel::detectCores(logical = FALSE))
{
  if(dir.exists(ABS_path)){
    if(isTRUE(str_match)){
      abs_data <- get_names(ABS_dir, type = "files", string = "ABS")
    } else {
      abs_data <- get_names(ABS_dir, type = "files")
    }
  } else if (file.exists(ABS_path)) {
    abs_data <- ABS_path
  } else stop("Absorbance data was not found!")
  cl <- parallel::makeCluster(min(cores, length(abs_data)), type = "PSOCK")
  parallel::clusterExport(cl, c("dec", "sep", "verbose"),
                          envir = environment())
  parallel::clusterEvalQ(cl, require(dplyr))
  parallel::clusterEvalQ(cl, require(stringr))
  abs_data <- parLapply(cl, abs_data, function(tab) {
    tryCatch({
      rawdata <- readLines(tab)
      data <- rawdata %>% sapply(str_remove, pattern = "([^0-9]*$)")
      first_number <- min(which((substr(data, 1, 1) %>%
                                   grepl("[0-9]", .))))
      last_number <- max(which((substr(data, 1, 1) %>%
                                  grepl("[0-9]", .))))
      if (is.null(sep) | is.null(dec)) {
        nsepdec <- data[first_number] %>% str_extract_all("[^-0-9eE]") %>%
          unlist()
        example_number <- data[first_number] %>% str_extract("([-]?[0-9]+[.,]?[0-9]+[eE]?[-0-9]+)$")
        if (is.null(dec) & length(nsepdec) > 1)
          dec <- example_number %>% str_replace("([-0-9eE]+)([.,]?)([-0-9eE]*)",
                                                "\\2")
        if (is.null(sep))
          sep <- gsub(pattern = dec, replacement = "",
                      x = data[first_number], fixed = TRUE) %>%
          str_extract(paste0("[^-0-9eE", dec,
                             "]"))
        if (verbose)
          warning("processing", tab, ": using",
                  sep, "as field separator and", dec,
                  "as decimal separator.", fill = TRUE)
      }
      data <- str_split(data, sep)
      table <- data[(first_number):last_number] %>% unlist() %>%
        matrix(ncol = length(data[[first_number]]), byrow = TRUE) %>%
        data.frame(stringsAsFactors = FALSE) %>% mutate_all(gsub,
                                                            pattern = ifelse(dec != "", dec, "."),
                                                            replacement = ".", fixed = TRUE)
      table <- table %>% mutate_all(as.numeric)
      attr(table, "location") <- rep(tab, ncol(table) -
                                       1)
      if (ncol(table) == 2) {
        samples <- tab %>% basename() %>% str_replace_all(regex(".txt$|.csv$",
                                                                ignore_case = TRUE), "")
      }
      else {
        samples <- rawdata[[1]] %>% str_split(sep) %>%
          unlist() %>% matrix(ncol = length(.), byrow = TRUE) %>%
          data.frame(stringsAsFactors = FALSE) %>% .[-1]
      }
      table <- table %>% setNames(c("wavelength",
                                    samples))
    }, error = function(err) {
      stop("Error while reading ", tab, ": ",
           err)
    })
  })
  parallel::stopCluster(cl)
  locations <- lapply(abs_data, function(tab) {
    attr(tab, "location")
  }) %>% unlist()
  if((length(abs_data) == 1)){
    abs_data <- abs_data[[1]] %>% as.data.frame()
  } else {
    abs_data <- abs_data %>% list_join(by = "wavelength")
  }
  if (order){
    abs_data <- abs_data %>% arrange(wavelength)
  }
  attr(abs_data, "location") <- locations
  abs_data
}

#' Read in PCT files.
#'
#' @description Read in one or more Percent Transmission (PCT) ASCII .dat files from a given directory. This function is a direct
#'    modification of absorbance_read() from the staRdom package, with adjustments made to the filename
#'    fetching lines (get_names instead of list.files) to ensure .dat files are correctly fetched.
#'
#' @param ABS_path full path to either (1) a folder containing multiple PCT .dat files, or (2) a single absorbance file.
#' @param order TRUE/FALSE to order data according to wavelength.
#' @param recursive TRUE/FALSE to recurse into sub-directories.
#' @param str_match Use simple string-matching to ensure the indexed absorbance files contain the string "PCT", as standard in Percent Transmission files from the Aqualog Sample Queue system.
#' @param dec see ?staRdom::absorbance_read(). Table decimal separator testing.
#' @param sep see ?staRdom::absorbance_read(). Optional or automatic separator determination.
#' @param verbose TRUE/FALSE to provide additional information whilst running.
#' @param cores number of CPU cores used for parallel computation. Defaults to maximum, detected automatically.
#'
#' @export
#'
PCT_read <- function (PCT_path, order = TRUE, recursive = FALSE, dec = NULL, str_match = TRUE,
                      sep = NULL, verbose = TRUE, cores = parallel::detectCores(logical = FALSE))
{
  if(dir.exists(PCT_path)){
    if(isTRUE(str_match)){
      abs_data <- get_names(PCT_path, type = "files", string = "PCT")
    } else {
      abs_data <- get_names(PCT_path, type = "files")
    }
  } else if (file.exists(PCT_path)) {
    abs_data <- PCT_path
  } else stop("Absorbance data was not found!")
  cl <- parallel::makeCluster(min(cores, length(abs_data)), type = "PSOCK")
  parallel::clusterExport(cl, c("dec", "sep", "verbose"),
                          envir = environment())
  parallel::clusterEvalQ(cl, require(dplyr))
  parallel::clusterEvalQ(cl, require(stringr))
  abs_data <- parLapply(cl, abs_data, function(tab) {
    tryCatch({
      rawdata <- readLines(tab)
      data <- rawdata %>% sapply(str_remove, pattern = "([^0-9]*$)")
      first_number <- min(which((substr(data, 1, 1) %>%
                                   grepl("[0-9]", .))))
      last_number <- max(which((substr(data, 1, 1) %>%
                                  grepl("[0-9]", .))))
      if (is.null(sep) | is.null(dec)) {
        nsepdec <- data[first_number] %>% str_extract_all("[^-0-9eE]") %>%
          unlist()
        example_number <- data[first_number] %>% str_extract("([-]?[0-9]+[.,]?[0-9]+[eE]?[-0-9]+)$")
        if (is.null(dec) & length(nsepdec) > 1)
          dec <- example_number %>% str_replace("([-0-9eE]+)([.,]?)([-0-9eE]*)",
                                                "\\2")
        if (is.null(sep))
          sep <- gsub(pattern = dec, replacement = "",
                      x = data[first_number], fixed = TRUE) %>%
          str_extract(paste0("[^-0-9eE", dec,
                             "]"))
        if (verbose)
          warning("processing", tab, ": using",
                  sep, "as field separator and", dec,
                  "as decimal separator.", fill = TRUE)
      }
      data <- str_split(data, sep)
      table <- data[(first_number):last_number] %>% unlist() %>%
        matrix(ncol = length(data[[first_number]]), byrow = TRUE) %>%
        data.frame(stringsAsFactors = FALSE) %>% mutate_all(gsub,
                                                            pattern = ifelse(dec != "", dec, "."),
                                                            replacement = ".", fixed = TRUE)
      table <- table %>% mutate_all(as.numeric)
      attr(table, "location") <- rep(tab, ncol(table) -
                                       1)
      if (ncol(table) == 2) {
        samples <- tab %>% basename() %>% str_replace_all(regex(".txt$|.csv$",
                                                                ignore_case = TRUE), "")
      }
      else {
        samples <- rawdata[[1]] %>% str_split(sep) %>%
          unlist() %>% matrix(ncol = length(.), byrow = TRUE) %>%
          data.frame(stringsAsFactors = FALSE) %>% .[-1]
      }
      table <- table %>% setNames(c("wavelength",
                                    samples))
    }, error = function(err) {
      stop("Error while reading ", tab, ": ",
           err)
    })
  })
  parallel::stopCluster(cl)
  locations <- lapply(abs_data, function(tab) {
    attr(tab, "location")
  }) %>% unlist()
  if((length(abs_data) == 1)){
    abs_data <- abs_data[[1]] %>% as.data.frame()
  } else {
    abs_data <- abs_data %>% list_join(by = "wavelength")
  }
  if (order){
    abs_data <- abs_data %>% arrange(wavelength)
  }
  attr(abs_data, "location") <- locations
  abs_data
}
