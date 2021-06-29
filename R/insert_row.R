#' Insert a row into a dataframe
#'
#' @description A less clunky row insert function than an rbind method. Shamelessy ripped
#'        from Ari B. Friedman's response in https://stackoverflow.com/questions/11561856/add-new-row-to-dataframe-at-specific-row-index-not-appended
#'
#' @param df The data frame within which the row is to be inserted
#' @param row The row to be inserted
#' @param index the location of the row
#'
#' @export
#'

insert_row <- function(df, row, index) {
  df <- as.data.frame(df)
  if(index <= nrow(df)){
    df[seq(index+1,nrow(df)+1),] <- df[seq(index,nrow(df)),]
    df[index,] <- row
    df
  } else if(index > nrow(df)){# check if index > nrow. If so, tack it on the end.
    df[nrow(df)+1,] <- row
    df
  }
}
