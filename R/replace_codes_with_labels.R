#' Replace codes in a haven_labelled vector to the true values.
#'
#' This function replaces the codes in a haven_labelled vector with the values associated with the code. Can be used in a mutate call from dplyr or on any appropriate vector.
#' @param labelled_vector An object of class haven_labelled. Can confirm using: `is(vector,"haven_labelled")`
#' @param label_string String fed into the `which` argument of the attr() function. Defaults to "labels".
#' @export
#' @examples
#' haven_loaded_data_frame %>% 
#'   mutate(new_column = replace_codes_with_labels(haven_labelled_vector))
replace_codes_with_labels = function(labelled_vector, label_string="labels") {
  vector_values_and_labels = attr(labelled_vector,label_string)
  label_table = data.frame(Code = unname(vector_values_and_labels), 
                           Value = names(vector_values_and_labels))
  
  new_vector = c()
  for (i in 1:length(labelled_vector)) {
    new_vector = c(new_vector,
                   label_table$Value[label_table$Code==labelled_vector[i]])
  }
  new_vector
}