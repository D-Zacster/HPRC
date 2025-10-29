#' Extract labels for each column in a data frame
#'
#' This function returns a data.frame() object of the names and labels for all columns in a dataframe.
#' @param input_df A data.frame() object.
#' @param which_string String fed into the `which` argument of the attr() function. Defaults to "label".
#' @export
extract_column_labels = function(input_df,which_string="label") {
  label_list = c()
  variables = names(input_df)
  for (variable in variables) {
    label_str = unname(attr(input_df[[variable]],which_string))
    if (is.null(label_str)) {
      label = NA
    } else {
      label = label_str
    }
    label_list = c(label_list,label)
  }
  label_table = data.frame(Variable = variables, Label = label_list)
  label_table
}
