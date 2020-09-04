#' Create Standardized Filename for Output
#'
#' FORTHCOMING.
#'
#' @param description
#' @param extension
#' @param tag
#' @param number
#' @param file_date
#' @param project_version
#'
#' @return
#'
#' @author Kevin Potter
#'
#' @export

create_filename_for_output = function( description,
                                       extension,
                                       tag = NULL,
                                       number = NULL,
                                       file_date = NULL,
                                       project_version = NULL ) {

  # Determine files in directory
  all_files = dir()

  # If not specified, auto-generate file tag
  # based on extension
  if ( is.null( tag ) ) {

    # Word document
    if ( extension == 'docx' ) {
      tag = 'D'
    }
    # Standard figure extentions
    if ( extension %in% c( 'pdf', 'jpg', 'jpeg', 'png' ) ) {
      tag = 'F'
    }
    # R data file
    if ( extension %in% c( 'RData' ) ) {
      tag = 'R'
    }
    # R script file
    if ( extension %in% c( 'R' ) ) {
      tag = 'S'
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
    project_version = paste0( 'v.', pull_git_version() )
  }

  # Check for matching tags and descriptions for
  # files present in folder
  no_files = TRUE
  if ( length( all_files ) > 0 ) {

    matching_tags =
      stringr::str_sub( all_files, start = 1, end = 1 ) == tag &
      stringr::str_detect( all_files, stringr::fixed( '.' ) ) & # Exclude folders
      all_files != 'Placeholder.txt' # Exclude placeholder file

    matching_description =
      stringr::str_detect( all_files, stringr::fixed( description ) ) &
      stringr::str_detect( all_files, stringr::fixed( '.' ) ) & # Exclude folders
      all_files != 'Placeholder.txt' # Exclude placeholder file

    no_files = FALSE
  }

  # If not specified, auto-generate file number
  if ( is.null( number ) ) {

    if ( no_files ) {
      number = 1
    } else {

      # Increment file number
      if ( any( matching_description & matching_tags ) ) {
        number = stringr::str_sub(
          all_files[ matching_description & matching_tags ],
          start = 2, end = 3 )
      } else {
        number = sum( matching_tags  ) + 1
      }
    }

    # Make sure number is a double-digit and
    # convert to character string
    nc = nchar( number )
    if ( nc == 1 ) {
      number = paste0( '0', number )
    } else {
      number = as.character( number )
    }

  }

  # Generate file name
  filename = paste0(
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

