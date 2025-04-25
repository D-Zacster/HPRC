#' Update the HPRC file from github
#'
#' This function downloads the most recent HPRC package file from GitHub.
#' @export
#' @examples
#' update_HPRC()

update_HPRC = function() {
  require("devtools")
  tryCatch(expr = {detach("package:HPRC", unload=TRUE)},
           error = function(cond){print("HPRC package not loaded, continuing update.")})
  devtools::install_github("D-Zacster/HPRC")
}
