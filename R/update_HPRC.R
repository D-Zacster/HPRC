#' Update the HPRC file from github
#'
#' This function downloads the most recent HPRC package file from GitHub.
#' @export
#' @examples
#' update_HPRC()

update_HPRC = function() {
  require("devtools")
  detach("package:HPRC", unload=TRUE)
  devtools::install_github("D-Zacster/HPRC")
}
