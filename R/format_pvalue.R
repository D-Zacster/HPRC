#' Function to make p-values prettier for reports.
#'
#' This function rounds individual p-values to 4 digits and returns them to the user.
#' @param pvalue A single, numeric p-value.
#' @param out_format Specifies what will be returned. Defaults to strings. Indicating numeric will return 0 for values less than 0.0001.
#' @export
#' @examples
#' > p_value = 0.000000345
#' > format_pvalue(p_value)
#' [1] "<0.0001"
#'
#' ################
#'
#' > p_value = 0.000000345
#' > format_pvalue(p_value,"numeric")
#' [1] 0
format_pvalue = function(pvalue,out_format="string") {
  options(scipen=999)
  out_pval = round(pvalue,digits = 4)
  if (out_format=="string") {
    if (out_pval<0.0001) {
      out_pval = "<0.0001"
    } else {
      out_pval = as.character(out_pval)
    }
  } else if (out_format=="numeric") {
    if (out_pval<0.0001) {
      out_pval = 0
    }
  }
  return(out_pval)
}
