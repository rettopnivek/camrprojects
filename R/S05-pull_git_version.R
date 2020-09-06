#' Determine Git Version for Project
#'
#' A function to extract the git version for an R project
#' that has auto-generated change logs.
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

    }

  } else {
    warning( paste0(
      "The list 'folder_pathways' with the paths and associated ",
      "labels was not found in the global environment - please ",
      "create this list via the 'create_folder_pathways' function"
    ), call. = F )
  }

  # Return to current directory
  setwd( cur_dir )

  return( out )
}

