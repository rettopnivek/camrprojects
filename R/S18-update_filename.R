#' Create Standardized Filename and Remove Previous File
#'
#' Generates a standardized file name following the format
#' TXX-description-dd_mm_yyyy-v.X.X.X.extension, giving
#' a file tag and counter (e.g., 'F01', 'D02'), a
#' human-readable description and date for version control,
#' a machine-readable version number, and the file
#' extension. Furthermore, checks if a previous version of
#' the file (i.e., with earlier dates or project versions)
#' exists in the current directory - if it does, removes this
#' file to avoid duplicate files.
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
#' @return A character string, and deletes previous versions of
#' the file.
#'
#' @author Kevin Potter
#'
#' @export

update_filename = function( description,
                            extension,
                            tag = NULL,
                            number = NULL,
                            file_date = NULL,
                            project_version = NULL ) {

  # Create new file name
  new_file = create_standardized_filename(
    description = description,
    extension = extension,
    tag = tag,
    number = number,
    file_date = file_date,
    project_version = project_version
  )

  # Check for previous versions of file

  tag_and_desc = strsplit( new_file, split = '-' )[[1]]
  tag_and_desc = paste0(
    tag_and_desc[1], '-',
    tag_and_desc[2]
  )

  if ( file_present( tag_and_desc ) ) {
    old_file = file_present( tag_and_desc, output = 'name' )
    file.remove( old_file )
  }

  return( new_file )
}

