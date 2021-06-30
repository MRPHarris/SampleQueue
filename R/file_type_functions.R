# Internal functions used to store file type definitions.

#' Return a list containing the character strings used to define the different file "types" on the run sheet.
#'
#' @description Called internally in many functions to return a list containing the character strings used to define the different file "types" on the run sheet. Accessed by get_type().
#'
#' @noRd
#
type_strings <- function(){
  # There are broadly 5 types.
  type_string_list <- vector(mode = "list", length = 5)
  names(type_string_list) <- c("Sample Queue Blank", "MilliQ Water Blank", "Sample","Standard","Replicate")
  type_string_list[["Sample Queue Blank"]] <- c("sqblank","sq","sampleqblank","sampleq blank","SampleQ","SampleQBlank","SQ","SampleQ Blank", "samplpe queue blank","Sample Queue Blank")
  type_string_list[["MilliQ Water Blank"]] <- c("mqblank","milliq","milliqblank","milliq blank","MilliQ","MilliQBlank","MQ","mq","MilliQ Blank","milli-q blank","Milli-Q Blank","milli-q water blank","Milli-Q Water Blank")
  type_string_list[["Sample"]] <- c("Sample","sample")
  type_string_list[["Standard"]] <- c("STD","std","Standard","standard")
  type_string_list[["Replicate"]] <- c("Replicate","replicate","rep","Rep","Repeat","repeat","Rpt","rpt")
  type_string_list
}

#' Return the acceptable folder names for the extensions .blank and .ogw. Supply type of "Sample Queue Blank" or one of the 'normal' types.
#'
#' @description Called in a couple of the folder finding functions. Probably redundant, at least in part.
#'
#' @param type one of "Sample Queue Blank", "Sample", "MilliQ Water Blank", "Replicate", "Standard"
#'
#' @noRd
#
extension_strings <- function(type){
  # Returns a list object containing match strings for: .ogw, .blank,
  if(type == "Sample Queue Blank"){
    strings_list <- vector(mode = "list", length = 2)
    names(strings_list) <- c("Blank File", "Workbook File")
    strings_list[["Blank File"]] <- c("blank files","blank file","Blank Files","Blank File")
    strings_list[["Workbook File"]] <- c("workbook","workbooks","Workbook","Workbooks","Workbook File","workbook file","Workbook Files","workbook files")
    strings_list
  } else if(type == "Sample" || type == "MilliQ Water Blank" || type == "Replicate" || type == "Standard"){
    strings_list <- vector(mode = "list", length = 2)
    names(strings_list) <- c("Workbook File","ASCII Data File")
    strings_list[["Workbook File"]] <- c("workbook","workbooks","Workbook","Workbooks","Workbook File","workbook file","Workbook Files","workbook files")
    strings_list[["ASCII Data File"]] <- NULL # As far as I know there is no instance wherein this is returned.
    strings_list
  }
  strings_list
}

#' Serves two purposes: gives acceptable folder names for given ASCII data types, and matches the ASCII type 3-character signifier. I.e. ABS for Absorbance Data. If adding support for more ASCII types, start here then go and modify the folders.
#'
#' @description  Return the acceptable folder names for the supported Aqualog ASCII data types, and visa versa for file ASCII data type detection purposes.
#'
#' @noRd
#
ASCII_strings <- function(){
  strings_list <- vector(mode = "list", length = 2)
  names(strings_list) <- c("Absorbance File","Processed EEM")
  strings_list[["Absorbance File"]] <- c("Absorbance Data","Absorbance","absorbance data","absorbance","ABS","abs")
  strings_list[["Processed EEM"]] <- c("raw EEM","PEM","pem","Raw EEM data","PEM data","pem data","Excitation Emission Matrix","Excitation Emission Matrice",
                                       "excitation emission matrix","excitation emission matrice","Processed EEM","processed eem")
  strings_list
}
