#' Determine Git Version for Project
#'
#' FORTHCOMING.
#'
#' @return A character string giving the version number, in the
#'   format of MAJOR.MINOR.PATCH.
#'
#' @author Willam Schmitt
#'
#' @export

pull_git_version <- function() {

  # Default output
  out <- '1.0.0'

  # Current working directory
  cur_dir <- getwd()

  # Attempt to determine version number
  if ( exists( 'folder_pathways', envir = .GlobalEnv ) ) {

    # Go to project directory
    go_to('Project')

    # Read in json
    if ( 'package.json' %in% dir() ) {

      pckg_json <- jsonlite::read_json( 'package.json' )
      out <- pckg_json$version

    } else {

      warning( "The 'package.json' file was not found" )
      out <- c( '0.0.0' )

    }

  } else {
  }

  # Return to current directory
  setwd( orig_dir )

  return( out )
}

