#' Function to trim then convert empty strings in data.frame into NAs.
#'
#' This function takes a data.frame object, trims all character strings, coverts empty strings like '' and "" into NA, then outputs the cleaned data.frame.
#' @param input_dataframe A data.frame object
#' @export
#' @examples
#' clean_strings()
clean_strings = function(input_dataframe) {

  output_dataframe = input_dataframe %>%
    mutate(across(where(is.character), str_trim)) %>%
    mutate(across(where(is.character), ~na_if(.,"N/A"))) %>%
    mutate_if(is.character, list(~na_if(.,"")))

}

