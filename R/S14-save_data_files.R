#' Save Data Files
#'
#' A convenience function that will save a
#' set of data frames in the global environment
#' as external files using the specified extension
#' and filenames.
#'
#' @param data_frames_to_save A character vector with the names
#'   of the data frames in the global environment that should
#'   be saved
#' @param extensions A character vector with the file extensions
#'   to output. Currently supports 'RData' and 'csv'.
#' @param filenames The filename(s) for the output, either a
#'   single character string or a vector matching in length
#'   to \code{extension}.
#' @param filename_modifiers A character vector of modifiers to the
#'   filenames When saving multiple data frames as .csv files.
#' @param tag An optional argument for the \code{create_standardized_filename}
#'   controlling the beginning tag at the start of the filename.
#' @param number An optional argument for the \code{create_standardized_filename}
#'   specifying the file number.
#' @param use_standard_names Logical; if \code{TRUE} uses
#'   the \code{create_standardized_filename} to generate filenames in
#'   a standardized format.
#'
#' @author Kevin Potter
#'
#' @export

save_data_files = function( data_frames_to_save,
                            extensions,
                            filenames,
                            filename_modifiers = NULL,
                            tag = NULL,
                            number = NULL,
                            use_standard_names = TRUE ) {

  ### Setup

  # Determine number of file types to save
  n_ext = length( extensions )

  # Match length of file names to number of extensions
  if ( length( filenames ) == 1 ) {
    filenames = rep( filenames, n_ext )
  }

  # if no user-supplied number, start file numbering at 1
  if ( is.null( number ) ) {
    current_number = 1
  } else {
    current_number = number
  }

  # If no user-supplied tag, use default
  if ( is.null( tag ) ) {
    tag = 'R'
  }
  # Match length of tags to number of extensions
  if ( length( tag ) == 1 ) {
    tag = rep( tag, n_ext )
  }

  # Function to create a two-digit character string
  # based on supplied number
  two_digits = function( n ) {

    n = as.character( n )
    if ( nchar( n ) == 1 ) {
      n = paste0( '0', n )
    }

    return( n )
  }

  ### Save .RData files

  if ( 'RData' %in% extensions ) {

    file_index = ( extensions %in% 'RData' )

    if ( use_standard_names ) {
      # If specified to use standardized naming scheme

      fname = create_standardized_filename(
        filenames[ file_index ],
        'RData',
        tag = tag[ file_index ],
        number = two_digits( current_number )
      )
      # Increment file number
      current_number = current_number + 1

    } else {
      # Otherwise use supplied filename

      fname = paste0( filenames[ file_index ], '.RData' )

    }

    # Save specified R objects located in global environment
    save(
      list = data_frames_to_save,
      envir = .GlobalEnv,
      file = fname
    )

  }

  ### Save .csv files

  if ( 'csv' %in% extensions ) {

    file_index = ( extensions %in% 'csv' )

    # Determine number of objects to save
    n_obj = length( data_frames_to_save )

    # Defaults for filename modifiers
    if ( is.null( filename_modifiers ) ) {
      filename_modifiers = rep( '', n_obj )
    }

    # Loop over R objects
    for ( i in 1:n_obj ) {

      if ( use_standard_names ) {
        # If specified to use standardized naming scheme

        fname = create_standardized_filename(
          paste0( filenames[ file_index ], filename_modifiers[i] ),
          'csv',
          tag = tag[ file_index ],
          number = two_digits( current_number )
        )

      } else {
        # Otherwise use supplied filename

        if ( filename_modifiers[i] == '' ) {
          # If no modifier is provided, to prevent files
          # being overwritten, include a file number

          fname = paste0( filenames[ file_index ],
                          '_', current_number,
                          '.csv' )
        } else {
          # Otherwise include modifier

          fname = paste0( filenames[ file_index ],
                          filename_modifiers[i],
                          '.csv' )
        }

      }

      # Save .csv file
      write.csv(
        get( data_frames_to_save[i], envir = .GlobalEnv),
        file = fname,
        row.names = F,
        quote = T
      )

      # Increment file number
      current_number = current_number + 1

    }

  }

}
