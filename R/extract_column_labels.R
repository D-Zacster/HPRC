#' Extract labels for each column in a data frame
#'
#' This function returns a list of the labels for all columns in a dataframe.
#' @param input_df A data.frame() object.
#' @param which_string String fed into the `which` argument of the attr() function. Defaults to "label".
#' @export
extract_column_labels = function(input_df,which_string="label") {
  label_list = c()
  for (variable in names(input_df)) {
    label_str = unname(attr(input_df[[variable]],which_string))
    if (is.null(label_str)) {
      label = NA
    } else {
      label = label_str
    }
    label_list = c(label_list,label)
  }
  label_list
}