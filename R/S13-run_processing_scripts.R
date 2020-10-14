#' Run Data Processing Scripts
#'
#' A convenience function that navigates to the folder
#' with the set of data processing R scripts and
#' sources the specified subset of files. The portable
#' component of a \code{create_cleaned_data} function
#' typical for CAM R projects.
#'
#' @param processing_files A vector of either...
#'   \itemize{
#'     \item Numeric indices denoting which processing
#'       files to include;
#'     \item A character vector in the form of 'DPXX'
#'       indicating which files to include;
#'     \item A character vector with the full file names
#'       for the files to include.
#'   }
#' @param required_scripts A vector of (complete) file names
#'   giving the processing scripts that must be included.
#' @param reload Logical; if \code{TRUE}, re-downloads data
#'   from REDCap.
#' @param debug_code Logical; if \code{TRUE}, displays progess at
#'   specified cut-points for each processing script.
#'
#' @author Kevin Potter
#'
#' @export

run_processing_scripts = function( processing_files,
                                   required_scripts,
                                   reload,
                                   debug_code ) {

  # Load in REDCap data

  go_to( 'R' )
  update_data <<- reload
  debug_code <<- debug_code

  fname = file_present( 'Download_REDCap_data', output = 'name' )
  source( fname )

  # Run data processing scripts

  # Begin data processing/cleaning
  go_to( 'Processing' )

  # Determine all current processing files
  all_processing_files = dir()
  all_processing_files =
    all_processing_files[
      all_processing_files %>% str_detect( fixed( 'DP' ) ) &
        all_processing_files %>% str_detect( fixed( '.R' ) )
      ]

  # Check whether specifc processing files were specified
  if ( is.null( processing_files ) ) {

    # By default, include all processing files in folder
    processing_files = all_processing_files

  }

  # Indicator for when processing filenames are ready to
  # by passed on
  processing_filenames_ready = FALSE

  # Check if integer indices were provided
  if ( !processing_filenames_ready ) {
    if ( all( is.numeric( processing_files ) ) ) {

      # Include required scripts
      processing_files = c(
        ( required_scripts %>%
            str_sub( start = 3, end = 4 ) %>%
            as.numeric ),
        processing_files ) %>%
        unique

      # Check for any mismatches
      check = !processing_files %in% 1:length( all_processing_files )
      if ( any( check ) ) {
        warning( paste0(
          "Some indices listed in 'processing_files' not found in ",
          "in processing folder:\n",
          "Check ",
          paste( processing_files[ check ], collapse = ', ' ),
          "\n"
        ), call. = FALSE )
      }

      # Convert to full filenames
      processing_files = all_processing_files[ processing_files ]

      processing_filenames_ready = TRUE
    }
  }

  # Check if partial character strings (e.g., 'DP01', 'DP02') were provided
  if ( !processing_filenames_ready ) {
    if ( all( is.character( processing_files ) ) &
         all( nchar( processing_files ) == 4 ) ) {

      # Include required scripts
      processing_files = c(
        ( required_scripts %>%
            str_sub( start = 1, end = 4 ) ),
        processing_files ) %>%
        unique

      # Match initial tags (e.g., 'DP01', 'DP02')
      tags = str_sub( all_processing_files, start = 1, end = 4 )

      # Check for any mismatches
      check = !processing_files %in% tags
      if ( any( check ) ) {
        warning( paste0(
          "Some tags listed in 'processing_files' not found in ",
          "in processing folder:\n",
          "Check ",
          paste( processing_files[ check ], collapse = ', ' ),
          "\n"
        ), call. = FALSE )
      }

      matches = which( tags %in% processing_files )
      processing_files = all_processing_files[ matches ]

      processing_filenames_ready = TRUE
    }
  }

  # Check if full filenames were provided
  if ( !processing_filenames_ready ) {
    if ( all( is.character( processing_files ) ) ) {

      processing_files = c(
        required_scripts,
        processing_files
      ) %>% unique

      # Check for any mismatches
      check = !processing_files %in% all_processing_files
      if ( any( check ) ) {
        warning( paste0(
          "Some filenames listed in 'processing_files' not found in ",
          "in processing folder:\n",
          "Check ",
          paste( processing_files[ check ], collapse = ', ' ),
          "\n"
        ), call. = FALSE )
      }

      matches = which( all_processing_files %in% processing_files )
      processing_files = all_processing_files[ matches ]

      processing_filenames_ready = TRUE
    }
  }

  # Check if processing functions are already loaded
  any_processing_functions <-
    any( stringr::str_detect( ls( envir = .GlobalEnv ), fixed( 'process_' ) ) )
  if ( any_processing_functions ) {
    go_to( 'processing' )
    # Remove processing functions
    source( 'DP01_Functions.R' )
  }

  # Loop over provided processing files
  for ( i in 1:length( processing_files ) ) {

    go_to( 'processing' )
    check = tryCatch(
      {
        source( processing_files[i] )
        'Processing completed successfully'
      },
      error = function(e) return( as.character( e ) )
    )

    if ( check != 'Processing completed successfully' ) {
      message( '*** Error in script' )
      message( check )
      message( '*** Processing failed!' )

    }

  }

  # Once done, unload processing functions
  go_to( 'processing' )
  source( 'DP01_Functions.R' )

}

