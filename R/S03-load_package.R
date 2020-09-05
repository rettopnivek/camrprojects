#' Installs and Loads an R Package
#'
#' Checks if a package is installed, and if not, sees if it
#' can be installed. Then, loads the package for easy use.
#'
#' @param package_name A character string with the package name.
#' @param from Where to download the package from; options include...
#' \enumerate{
#'   \item 'CRAN'
#'   \item 'Github'
#' }
#' @param repo An optional character string with the Github
#'   repository name (in the form 'username/repository').
#'   If \code{NULL}, assumes the username is 'rettopnivek'
#'   and that the repository is 'package_name'.
#' @param ... Additional parameters for the 'devtools'
#'   installation functions.
#'
#' @author Kevin Potter
#'
#' @examples
#' # Install/load package from CRAN
#' load_package( 'dplyr' )
#'
#' # Install/load package from author's github repository
#' load_package( 'utilityf', from = 'Github' )
#'
#' @export

load_package <- function( package_name,
                          from = 'CRAN',
                          repo = NULL,
                          ... ) {

  # List of packages that are installed
  installed_packages <- installed.packages()[,1]

  if ( !( package_name %in% installed_packages ) ) {

    # Install package via 'devtools' package

    # Check if 'devtools' is installed
    if ( !( 'devtools' %in% installed_packages ) ) {
      install.packages( 'devtools' )
    }

    # Installs package from CRAN repository
    if ( from == 'CRAN' ) {
      devtools::install_cran( package_name, ... )
    }

    # Installs package from Github repository
    if ( from == 'Github' ) {
      if ( is.null( repo ) ) {
        repo = paste0( "rettopnivek/", package_name )
      }
      devtools::install_github( repo, ... )
    }

  }

  # Load installed package for use
  library( package_name, character.only = T )

}

