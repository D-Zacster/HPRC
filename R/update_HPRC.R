#' Update the HPRC file from github
#'
#' This function downloads the most recent HPRC package file from GitHub.
#' @export
#' @examples
#' update_HPRC()

update_HPRC = function() {
  require("devtools")
  devtools::install_github("D-Zacster/HPRC")
}
