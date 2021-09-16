# Legacy functions
# Written by...
#   Kevin Potter
#   William Schmitt
# email: kevin.w.potter@gmail.com
# Please email me directly if you
# have any questions or comments
# Last updated 2021-09-16

# Note: Legacy functions retained to ensure compatibility with
#       old CAM projects
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


#' Assign New Values Based on Partial or Exact Matching
#'
#' Assigns new values based on partial or exact matches with
#' values from an input vector.
#'
#' @param x A vector of values to match over.
#' @param matches A list of values in \code{x} to match over
#' @param new_values A vector of new values to assign based on
#'   matches to elements from \code{matches} (vector must be
#'   of equivalent length to \code{matches}).
#' @param type The type of matching, either 'partial' or 'exact'
#'   (uses \code{grepl} or \code{\%in\%}, respectively).
#' @param default Either a single value to assign in the absence of
#'   a match, or a vector equivalent in length to \code{x}.
#' @param replace_if An optional vector specifying the subset of
#'   default values when it is appropriate to assign new values.
#'
#' @return A new vector of equivalent length to \code{x}, with
#'   values assigned based on successful matches.
#'
#' @author Kevin Potter
#'
#' @examples
#' x <- c( 'Cat', 'Hat', 'Rat', 'Dog', 'Fog', 'Cog' )
#' # Partial matching
#' match_and_assign( x, list( 'at', 'og' ), c('A','B') )
#' # Exact matching
#' match_and_assign( x, list( 'Cat', c( 'Dog', 'Fog' ) ), c('A','B'),
#'                   default = '', type = 'exact' )
#' # Vector input for argument 'default'
#' x <- c( 'A', 'A', 'D', 'C', 'A', 'A', 'C', 'D' )
#' match_and_assign( x, list( 'C', 'D' ), c('B','B'), default = x )
#' # Using 'replace_if' for conditional assignment
#' x1 <- rep( LETTERS[1:4], each = 2 )
#' x2 <- rep( LETTERS[5:6], 4 )
#' match_and_assign( x2, list( 'E', 'F' ), c('1','2'),
#'                   default = x1, replace_if = c( 'A', 'B' ) )
#'
#' @export

match_and_assign <- function( x, matches, new_values, type = 'partial',
                              default = NA, replace_if = NULL ) {
  .Deprecated()

  # Number of observations
  No <- length( x )

  # Initialize output
  if ( length( default ) == No ) {
    output <- default
  } else {
    output <- rep( default[1], No )
  }

  # Number of values/elements to match over
  Nm <- length( matches )

  # Check that vector with new values has
  # same length as values to match over
  if ( length( new_values ) != Nm ) {
    stop( paste0(
      "Length of argument 'new_values' must be equivalent to ",
      "argument 'matches'"
    ), call. = F )
  }

  # Loop over values and match
  for ( i in 1:Nm ) {

    is_match <- rep( F, No )

    if ( type == 'partial' ) {
      is_match <- grepl( matches[[i]], x, fixed = T )
    }

    if ( type == 'exact' ) {
      is_match <- x %in% matches[[i]]
    }

    # If a vector of default values was provided
    # and user provided vector of values specifying
    # when replacement should occur
    if ( ( length( default ) == No ) &
         !is.null( replace_if ) ) {

      # Replace only if default values are in
      # subset appropriate for replacement
      output[ is_match & output %in% replace_if ] <- new_values[i]

    } else {
      # Update output
      output[ is_match ] <- new_values[i]
    }

  }

  return( output )
}

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
#' ))
#' # Image file
#' print( create_standardized_filename(
#'   'Figure', 'png', project_version = 'v.1.0.0'
#' ))
#'
#' @export

create_standardized_filename <- function( description,
                                          extension,
                                          tag = NULL,
                                          number = NULL,
                                          file_date = NULL,
                                          project_version = NULL ) {
  .Deprecated()
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
      '%Y-%m-%d'
    )
  }

  # If not specified, auto-generate version
  # number associated with gitlab project
  if ( is.null( project_version ) ) {
    project_version <- substr(git2r::last_commit()$sha, 0, 7)
  }

  st <- git2r::status(untracked=FALSE)
  if (length(c(st$unstaged, st$staged)) != 0) {
    project_version <- paste0(project_version, 'm')
    warning('Uncommited changes are present. Output files will have a version ending in "m".')
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

#' Source in Multiple R Scripts in a Folder
#'
#' A convenience function that loops through
#' and reads in code in .R files stored in a
#' folder located in the current working directory.
#'
#' @param files_to_include A vector of either...
#'   \itemize{
#'     \item Numeric indices denoting which files
#'       to include;
#'     \item A character string matching the initial
#'        set of letters across all relevant files (e.g., if
#'        all scripts of interest start with the letter 'S');
#'     \item A character vector with the full file names
#'       for the files to include.
#'   }
#' @param path The folder name with the scripts to source.
#'
#' @author Kevin Potter
#'
#' @export

source_scripts = function( files_to_include = NULL,
                           path = 'R' ) {
  .Deprecated()
  # Folders to load
  all_files <- dir(
    path = path
  )

  # Identify R scripts

  f <- function( x ) {
    grepl( x, all_files, fixed = T )
  }
  # Files must have extension .R
  r_files <-
    ( f( '.R' ) | f( '.r' ) ) &
    !( f( '.RData' ) | f( '.rdata' ) |
         f( '.Rmd' ) | f( '.rmd' ) |
         f( '.rda' )
    )

  # Isolate .R files
  if ( any( r_files ) ) {
    all_files <- all_files[ r_files ]
  } else {
    stop( 'No .R files found' )
  }

  # Check if subset of files should be included
  if ( !is.null( files_to_include ) ) {

    # If numeric indices were provided
    if ( is.numeric( files_to_include ) ) {
      files_to_source <- all_files[ files_to_include ]
    }

    # If a character vector was provided
    if ( is.character( files_to_include ) ) {

      # If a single character string with no '.R' extension
      # was provided
      if ( length( files_to_include ) == 1 &
           !any( grepl( '.R', files_to_include, fixed = T ) ) ) {

        n_letters <- nchar( files_to_include )

        to_check <- substr( all_files, start = 1, stop = n_letters )

        files_to_source <- all_files[
          files_to_include %in% to_check
        ]

      } else {
        # Exact matching to file names
        files_to_source <- all_files[ all_files %in% files_to_include ]
      }

    }
  } else {
    # Otherwise take all files in folder
    files_to_source <- all_files
  }

  # Source in all specified R scripts
  if ( length( files_to_source ) > 0 ) {
    sapply( 1:length( files_to_source ), function(i) {
      source( paste0( path, "/", files_to_source[i] ) )
    } )
  } else {
    stop( 'No files found matching given criteria' )
  }

}

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
  .Deprecated()

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

#' Process data from the COMM questionnaire
#'
#' This function scores the COMM questionnaire by summing the individual item
#' ratings and providing the subsequent indication. The function takes in the
#' processed data frame of REDCap data, subsets just its COMM variables, scores
#' the COMM (creating a sum and indication column), then adds these columns
#' back to the larger processing data frame.
#'
#' @param rc_dtf The unprocessed, direct from REDCap data frame. Required.
#'
#' @param dtf The processed data frame that you can assume has the
#' necessary session details. Required.
#'
#' @return `dtf` with new columns that have meta-data (i.e. attributes)
#' filled out.
#'
#' @author Eylul Akman
#'
#' @export
#' @md

# adds sum & indication columns for COMM score to data
process_comm = function( comm_dtf ) {
  .Deprecated()

  # sums all item ratings
  comm_dtf <- dplyr::transmute(
    comm_dtf,
    INV.INT.COMM.Sum = rowSums(dplyr::across(
      dplyr::starts_with('INV.INT.COMM.Item')))
  )

  # indicates positive or negative score based on sum
  comm_dtf$INV.CHR.COMM.Indication <- ifelse(
    comm_dtf$INV.INT.COMM.Sum >= 9,
    '+', '-'
  )

  return(comm_dtf)
}




