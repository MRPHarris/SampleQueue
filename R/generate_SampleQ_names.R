#' Generate SampleQ names.
#'
#' @description Generate the predicted/actual sample names of a given sample queue run, for
#'    use in creating a custom run sheet. Alternatively, use the infolist functions.
#'
#' @param blank_group_prefix the overall prefix
#' @param blank_base the name for blanks
#' @param sample_prefix the name for samples
#' @param n_blank_groups how many groups of samples + SampleQ blanks are being run?
#' @param samples_per_blank how many sampels are in each group?
#' @param blank_group_start_num not implemented: resuming a run at which blank?
#' @param sample_start_num not implemented: resuming a run at which sample?
#'
#' @export
#'

generate_SampleQ_names <- function(
  blank_group_prefix = "Test",
  blank_base = "Blank",
  sample_prefix = "Sample",
  n_blank_groups = 1,
  samples_per_blank = 25,
  blank_group_start_num = 1,
  sample_start_num = 1
){
  total_files_perblank <- (1 + samples_per_blank)
  total_files <- n_blank_groups * total_files_perblank
  SampleQ_Names <- data.frame(matrix(nrow = total_files, ncol = 1, NA))
  it_list_samples <- vector(mode = "list", length = total_files_perblank)
  it_list_blanks <- vector(mode = "list", length = n_blank_groups)
  for(b in seq_along(it_list_blanks)){
    starting_row <- (b * total_files_perblank) - samples_per_blank
    for(i in seq_along(it_list_samples)){
      if(i == 1){
        # Blank naming components
        prefix <- blank_group_prefix
        blanknumber <- formatC(b, width = 3, format = "d", flag = "0")
        name <- blank_base
        # Now concatenate
        run_blank_name <- paste0(prefix,blanknumber,name)
        row <- starting_row
        SampleQ_Names[row,1] <- run_blank_name
      } else if(i != 1) {
        prefix <- blank_group_prefix
        blanknumber <- formatC(b, width = 3, format = "d", flag = "0")
        name <- sample_prefix
        samplenumber <- i-1
        samplenumber <- formatC(samplenumber, width = 4, format = "d", flag = "0")
        # Now concatenate
        sample_name <- paste0(prefix,blanknumber,name,samplenumber)
        row <- starting_row + (i-1)
        SampleQ_Names[row,1] <- sample_name
      }
    }
  }
  colnames(SampleQ_Names) <- c("SampleQ_Name")
  return(SampleQ_Names)
}
