#' A library loading function.
#'
#' This function loads a list of libraries. If any of the libraries are missing, it installs them first.
#' @param libraries A vector of library names as strings. Defaults to "tidyverse","magrittr","haven", and "ggplot2"
#' @param update TRUE/FALSE. Indicates whether you want all the packages to be updated. Defaults to FALSE.
#' @keywords library
#' @export
#' @examples
#' load_libraries()
load_libraries = function(libraries=c("tidyverse","magrittr","haven","ggplot2"),update=F) {
  if(update==TRUE) {
    packages_to_be_installed = libraries
  } else {
    packages_to_be_installed = setdiff(libraries, rownames(installed.packages()))
  }
  if(length(packages_to_be_installed)>0) {
    print(paste0("Installing the following package(s): ",paste(packages_to_be_installed,collapse = ", "),"."))
    install.packages(packages_to_be_installed)
  }
  sapply(libraries,library,character = T)
  print(paste0("The following packages were loaded: ",paste(libraries,collapse = ", "),"."))
}
