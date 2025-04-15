#' A library loading function.
#'
#' This function loads a list of libraries. If any of the libraries are missing, it installs them first.
#' @param libraries A vector of library names as strings. Defaults to "tidyverse","magrittr","haven", and "ggplot2"
#' @keywords library
#' @export
#' @examples
#' load_libraries()
load_libraries = function(libraries=c("tidyverse","magrittr","haven","ggplot2")) {
  missing_packages = setdiff(libraries, rownames(installed.packages()))
  if(length(missing_packages)>0) {
    print(paste0("Missing the following package(s): ",paste(missing_packages,collapse = ", "),". Installing missing package(s)!"))
    install.packages(missing_packages)
  }
  sapply(libraries,library,character = T)
  print(paste0("The following packages were loaded: ",paste(libraries,collapse = ", "),"."))
}
