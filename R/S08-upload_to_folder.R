#' Upload File to Folder
#'
#' A convenience function that attempts to a) upload a file
#' to a specified folder (and optionally a sub-folder), b)
#' check if previous versions of the file exist and copy
#' them to a back-up folder.
#'
#' @param filename The file name of the file to upload
#'   (must exist in the current working directory)
#' @param primary_path The primary folder where the file should be
#'   uploaded, either...
#' \itemize{
#'   \item An absolute path to the primary folder;
#'   \item A label that can be passed to the \code{go_to} function.
#' }
#' @param sub_path An optional path to a sub-folder in the primary folder.
#' @param folder_for_previous An optional folder name where previous
#'   versions of the file should be copied.
#' @param filename_is_standardized Logical; if \code{TRUE} the filename
#'   is assumed to follow a standardized format, see
#'   \code{\link{create_standardized_filename}}.
#'
#' @author Kevin Potter
#'
#' @return A logical value, \code{TRUE} if file was successfully uploaded.
#'
#' @export

upload_to_folder <- function( filename,
                              primary_path = 'Dropbox',
                              sub_path = NULL,
                              folder_for_previous = 'Previous_versions',
                              filename_is_standardized = TRUE ) {

  # Path to original file
  filename_path <- paste0(
    getwd(), '/', filename
  )

  # Path to current working directory
  current_folder <- getwd()

  # Attempt to upload file
  uploaded <- tryCatch( {

    # Check if primary path is a label used by the 'go_to'
    # function
    if ( exists( 'folder_pathways', envir = .GlobalEnv ) ) {

      if ( primary_path %in% folder_pathways$Path_labels ) {
        # Navigate to folder via 'go_to' function
        go_to( primary_path )
      } else {
        # Navigate to folder via standard R function
        setwd( primary_path )
      }

    } else {
      # Navigate to folder via standard R function
      setwd( primary_path )
    }

    # Optionally, navigate to sub-folder
    if ( !is.null( sub_path ) ) setwd( sub_path )

    # If a folder is specified to copy previous versions
    # of file
    if ( !is.null( folder_for_previous ) ) {

      # Extract all file names in folder
      all_files <- dir()

      # Check if folder to store previous versions is
      # available
      if ( folder_for_previous %in% all_files ) {

        if ( filename_is_standardized ) {

          # Split file into separate components
          # [1] tag + number (e.g., 'F01')
          # [2] description (e.g., 'Data')
          # [3] date as mm_dd_yyyy
          # [4] version as v.X.X.X and extension
          file_components <- strsplit(
            filename,
            split = '-',
            fixed = T
          )[[1]]

          # Check for previous versions of the file
          prev_files <-
            # tag + number
            grepl( file_components[1], all_files, fixed = T ) &
            # description
            grepl( file_components[2], all_files, fixed = T )

        } else {

          # Check for previous versions of the file (exact match)
          prev_files <-
            filename %in% all_files

        }

        # If previous files exisit
        if ( any( prev_files ) ) {

          for ( i in 1:sum( prev_files ) ) {

            # Copy files into folder for previous version
            file.copy(
              from = all_files[ prev_files ][i],
              to = paste0(
                folder_for_previous, '/', all_files[ prev_files ][i]
              ),
              overwrite = TRUE
            )

          }

          # Remove original files
          for ( i in 1:sum( prev_files ) ) {
            file.remove( all_files[ prev_files ][i] )
          }

        }

      }

    }

    # Upload file
    file.copy(
      from = filename_path,
      to = filename,
      overwrite = T
    )

    # Indicate that file was successfully upload
    NULL
  },
  # Otherwise return error message
  error = function(e) return( e )
  )

  # Throw warning if uploading process failed
  upload_worked <- TRUE
  if ( !is.null( uploaded ) ) {
    warning( paste0(
      'File was not uploaded to dropbox folder - errors returned: ',
      uploaded )
    )
    upload_worked <- FALSE
  }

  # Return to current working directory
  setwd( current_folder )

  return( upload_worked )
}

