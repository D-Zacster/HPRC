#' Function to drop columns that have only NAs.
#'
#' This function takes a data.frame object and outputs the data.frame with only columns that contain at least 1 non-NA value. Best when used with clean_strings() to make sure empty cells are appropriately identified.
#' @param input_dataframe A data.frame object
#' @export
#' @examples
#' drop_all_na_col()
drop_all_na_col = function(input_dataframe) {
  
  output_dataframe = input_dataframe %>%
    select_if(~ !all(is.na(.)))
 
}