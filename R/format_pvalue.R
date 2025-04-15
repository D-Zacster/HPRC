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
