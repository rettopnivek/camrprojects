#' Create Standardized Filename
#'
#' Generates a standardized file name following the format
#' TXX-description-dd_mm_yyyy-v.X.X.X.extension, giving
#' a file tag and counter (e.g., 'F01', 'D02'), a
#' human-readable description and date for version control,
#' a machine-readable version number, and the file
#' extension.
#'
#' @param description A human-readable brief description of
#'   the file content.
#' @param extension The file extension (e.g., 'docx', 'RData').
#' @param tag A single upper-case letter for file organization.
#' @param number A character string with a two-digit number
#'   for file organization.
#' @param file_date A character string with the date
#'   in the format 'mm_dd_yyyy'.
#' @param project_version The git version for the project
#'   in the format 'v.X.X.X'.
#'
#' @return A character string.
#'
#' @author Kevin Potter
#'
#' @examples
#' # Word document
#' print( create_standardized_filename(
#'   'Results', 'docx', project_version = 'v.1.0.0'
#' )
#' # Image file
#' print( create_standardized_filename(
#'   'Figure', 'png', project_version = 'v.1.0.0'
#' )
#'
#' @export

create_standardized_filename <- function( description,
                                          extension,
                                          tag = NULL,
                                          number = NULL,
                                          file_date = NULL,
                                          project_version = NULL ) {

  # Determine files in directory
  all_files <- dir()

  # If not specified, auto-generate file tag
  # based on extension
  if ( is.null( tag ) ) {

    # Word document
    if ( extension == 'docx' ) {
      tag <- 'D'
    }
    # Standard figure extentions
    if ( extension %in% c( 'pdf', 'jpg', 'jpeg', 'png' ) ) {
      tag <- 'F'
    }
    # R data file
    if ( extension %in% c( 'RData' ) ) {
      tag <- 'R'
    }
    # R script file
    if ( extension %in% c( 'R' ) ) {
      tag <- 'S'
    }

  }

  # If not specified, auto-generate file_date
  if ( is.null( file_date ) ) {
    file_date = format(
      Sys.Date(),
      '%m_%d_%Y'
    )
  }

  # If not specified, auto-generate version
  # number associated with gitlab project
  if ( is.null( project_version ) ) {
    project_version <- paste0( 'v.', pull_git_version() )
  }

  # Check for matching tags and descriptions for
  # files present in folder
  no_files <- TRUE
  if ( length( all_files ) > 0 ) {

    matching_tags <-
      stringr::str_sub( all_files, start = 1, end = 1 ) == tag &
      stringr::str_detect( all_files, stringr::fixed( '.' ) ) & # Exclude folders
      all_files != 'Placeholder.txt' # Exclude placeholder file

    matching_description <-
      stringr::str_detect( all_files, stringr::fixed( paste0('-', description, '-')) ) &
      stringr::str_detect( all_files, stringr::fixed( '.' ) ) & # Exclude folders
      all_files != 'Placeholder.txt' # Exclude placeholder file

    matching_extension <-
      stringr::str_detect( all_files, stringr::str_c(extension, '$') ) &
      stringr::str_detect( all_files, stringr::fixed('.') ) & # Exclude folders
      all_files != 'Placeholder.txt'

    no_files <- FALSE
  }

  # If not specified, auto-generate file number
  if ( is.null( number ) ) {

    if ( no_files ) {
      number <- 1
    } else {

      # Increment file number
      if ( any( matching_description & matching_tags & matching_extension ) ) {
        number <- stringr::str_sub(
          all_files[ matching_description & matching_tags & matching_extension ],
          start = 2, end = 3 )
      } else {
        number <- sum( matching_tags  ) + 1
      }
    }

    # Make sure number is a double-digit and
    # convert to character string
    nc <- nchar( number )
    if ( nc == 1 ) {
      number <- paste0( '0', number )
    } else {
      number <- as.character( number )
    }

  }

  # Generate file name
  filename <- paste0(
    tag,
    number,
    '-',
    description,
    '-',
    file_date,
    '-',
    project_version,
    '.',
    extension
  )

  return( filename )
}

