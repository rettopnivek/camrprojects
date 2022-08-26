# Legacy functions
# Written by...
#   Kevin Potter
#   William Schmitt
# email: kevin.w.potter@gmail.com
# Please email me directly if you
# have any questions or comments
# Last updated 2021-09-16

# Table of contents
# 1) save_data_files
# 2) match_and_assign
# 3) create_standardized_filename
# 4) source_scripts
# 5) pull_git_version
# 6) process_comm
# 7) path_by_os
# 8) upload_to_folder
# 9) elements
# 10) run_processing_scripts
# 11) add_meta_data_for_dictionary
# 12) notes_for_custom_function
# 13) go_to
# 14) create_folder_pathways
# 15) update_filename
# 16) file_present
# 17) load_package

# Note: Legacy functions retained to ensure compatibility with
#       old CAM projects

#### 1) save_data_files ####
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
#'   to `extension`.
#' @param filename_modifiers A character vector of modifiers to the
#'   filenames When saving multiple data frames as .csv files.
#' @param tag An optional argument for the `create_standardized_filename`
#'   controlling the beginning tag at the start of the filename.
#' @param number An optional argument for the `create_standardized_filename`
#'   specifying the file number.
#' @param use_standard_names Logical; if `TRUE` uses
#'   the `create_standardized_filename` to generate filenames in
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
  .Deprecated()
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

#### 2) match_and_assign ####
#' Assign New Values Based on Partial or Exact Matching
#'
#' Assigns new values based on partial or exact matches with
#' values from an input vector.
#'
#' @param x A vector of values to match over.
#' @param matches A list of values in `x` to match over
#' @param new_values A vector of new values to assign based on
#'   matches to elements from `matches` (vector must be
#'   of equivalent length to `matches`).
#' @param type The type of matching, either 'partial' or 'exact'
#'   (uses `grepl` or `\%in\%`, respectively).
#' @param default Either a single value to assign in the absence of
#'   a match, or a vector equivalent in length to `x`.
#' @param replace_if An optional vector specifying the subset of
#'   default values when it is appropriate to assign new values.
#'
#' @return A new vector of equivalent length to `x`, with
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

#### 3) create_standardized_filename ####
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

#### 4) source_scripts ####
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

#### 5) pull_git_version ####
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

#### 6) process_comm ####
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

#### 7) path_by_os ####
#' Navigate to Folder based on OS
#'
#' A function to navigate to different folders/paths
#' conditional on a user's operating system.
#'
#' @param paths A named vector consisting of...
#' \describe{
#'   \item{Windows}{The path for Windows-based operating systems}
#'   \item{Mac}{The path for Mac-based operating systems}
#'   \item{Linux}{The path for Linux-based operating systems}
#' }
#' @author Kevin Potter and William Schmitt
#'
#' @return A logical value, `TRUE` if the path
#' was successfully accessed.
#'
#' @examples
#' accessed_path <- path_by_os(
#'   c( Windows = 'C:/Users',
#'   Mac = '~/Users',
#'   Linux = '~' )
#' )
#' print( accessed_path ); print( getwd() )
#'
#' @export

path_by_os <- function( paths ) {
  .Deprecated()
  # Determine OS
  OS <- sessionInfo()

  # Attempt to navigate to path based on
  # operating system

  if ( stringr::str_detect( OS$running, 'Windows' ) ) {
    # For Windows machines
    path_to_try <- paths[ 'Windows' ]
  }
  if ( stringr::str_detect( OS$running, 'macOS' ) ) {
    # For Macs
    path_to_try <- paths[ 'Mac' ]
  }
  if ( stringr::str_detect( OS$running, 'unix' ) ) {
    # For Macs
    path_to_try <- paths[ 'Linux' ]
  }

  # Attempt to access path
  accessed_path <- tryCatch(
    {
      setwd( path_to_try )
      TRUE
    },
    error = function(e) return( FALSE )
  )

  return( accessed_path )
}

#### 8) upload_to_folder ####
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
#'   \item A label that can be passed to the `go_to` function.
#' }
#' @param sub_path An optional path to a sub-folder in the primary folder.
#' @param folder_for_previous An optional folder name where previous
#'   versions of the file should be copied.
#' @param filename_is_standardized Logical; if `TRUE` the filename
#'   is assumed to follow a standardized format, see
#'   [create_standardized_filename()].
#'
#' @author Kevin Potter
#'
#' @return A logical value, `TRUE` if file was successfully uploaded.
#'
#' @export

upload_to_folder <- function( filename,
                              primary_path = 'Dropbox',
                              sub_path = NULL,
                              folder_for_previous = 'Previous_versions',
                              filename_is_standardized = TRUE ) {
  .Deprecated()
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

      if ( primary_path %in% unlist( folder_pathways$Path_labels ) ) {
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

#### 9) elements ####
#' Extract Unique Elements of a Vector
#'
#' A convenience function that extracts the unique elements
#' of a vector, after optionally excluding/keeping specified
#' observations. The function can summarize the number of
#' unique values, or return the unique values as a vector.
#'
#' @param x A vector.
#' @param counts Logical; if `TRUE` returns a count of the number
#'   of unique values, otherwise returns a vector with the unique values.
#' @param include Optional logical vector of matching length to `x`
#'   indicating obserations to keep.
#' @param exclude Optional vector of types of observations to exclude
#'   (exact matches).
#' @param na.rm Logical; if `TRUE`, removes `NA` values.
#'
#' @author Kevin Potter
#'
#' @return A count of the number of unique values or a vector
#' of the unique values.
#'
#' @examples
#' vec <- c( rep( c( 'A', 'B', 'C', 'D' ), each = 3 ), NA )
#' # Number of unique elements - NA excluded by default
#' elements( vec )
#' # Unique elments as vector
#' elements( vec, count = F )
#' # Keep NA
#' elements( vec, na.rm = F )
#' elements( vec, na.rm = F, count = F )
#' # Exclude specific values
#' elements( vec, exclude = c( 'A', 'B' ) )
#' elements( vec, exclude = c( 'A', 'B' ), count = F )
#' elements( vec, exclude = c( 'A', 'B' ), count = F, na.rm = F )
#'
#' @export

elements = function( x, counts = T,
                     include = NULL, exclude = '',
                     na.rm = T ) {
  .Deprecated()
  # Number of observations
  n = length( x )

  # By default, include all observations
  if ( is.null( include ) ) {
    include = rep( T, n )
  }

  # Identify observations to exclude
  if ( !is.null( exclude ) ) {

    to_exclude = x %in% exclude

    if ( na.rm ) {
      to_exclude[ is.na( to_exclude ) ] = T
    } else {
      to_exclude[ is.na( to_exclude ) ] = F
    }
  } else {
    to_exclude = rep( F, n )
  }

  # If specified, identify missing data
  if ( na.rm ) {
    is_na = is.na( x )
  } else {
    is_na = rep( F, n )
  }

  entries =
    include &
    !to_exclude &
    !is_na

  if ( counts ) {
    out = length( unique( x[ entries ] ) )
  } else {
    out = unique( x[ entries ] )
  }

  return( out )
}

#### 10) run_processing_scripts ####
#' Run Data Processing Scripts
#'
#' A convenience function that navigates to the folder
#' with the set of data processing R scripts and
#' sources the specified subset of files. The portable
#' component of a `create_cleaned_data` function
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
#' @param reload Logical; if `TRUE`, re-downloads data
#'   from REDCap.
#' @param debug_code Logical; if `TRUE`, displays progess at
#'   specified cut-points for each processing script.
#'
#' @author Kevin Potter
#'
#' @export

run_processing_scripts = function( processing_files,
                                   required_scripts,
                                   reload,
                                   debug_code ) {
  .Deprecated()
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

#### 11) add_meta_data_for_dictionary ####
#' Add Meta-Data for Data Dictionaries
#'
#' A function that adds meta-data (see [base::attributes()])
#' to variables in a data frame or list that can be used for data
#' dictionary purposes. For standardized column names, many
#' fields can be auto-completed.
#'
#' @param Column_name The column name in the main data frame
#'   (e.g., IDS.CHR.Subject, SSS.CHR.Group).
#' @param Variable_category The category (i.e., associated
#'   processing script) for the variable. Can be determined
#'   from the initial three-letter abbreviation with
#'   standardized column names (change defaults via the
#'   argument `variable_categories`).
#' @param Data_type The data type of the values in the column.
#'   Can be determined from the second three-letter abbreviation
#'   with standardized column names (change defaults via the
#'   argument `data_types`).
#' @param Time_points_assessed For multi-session data, the list
#'   of all time points at which measure was collected, separated
#'   by the pipe symbol (e.g., 'Baseline|2nd time point|3rd time point').
#'   Can also inferred from inputs to the argument `time_points`.
#' @param Studies_assessed For data collected from multiple
#'   studies, or over multiple phases for a study (e.g., combination
#'   of data from pilot and active studies), the list of all
#'   studies (or phases) during which the measure was collected,
#'   separated by the pipe symbol (e.g., 'Pilot|Active').
#'   Can be inferred from inputs to the argument `studies`.
#' @param Groups_assessed For data with multiple randomization
#'   groups, the list of groups over which the measure was collected,
#'   separated by the pipe symbol (e.g., 'Group 1|Group 2|Group 3').
#'   Can be inferred from inputs to the argument `groups`.
#' @param Description Human-readable description of what the variable
#'   is and what it is for.
#' @param Values Internally stored values REDcap uses for paired
#'   values and labels (e.g., likert-scale type variables)
#'   separated by the pipe symbol (e.g., '1|2|3|4|5').
#' @param Labels_for_values The labels associated with internally
#'   stored values REDcap uses for paired values and labels (e.g.,
#'   likert-scale type variables) separated by the pipe symbol
#'   (e.g., 'Very low|Low|Medium|High|Very high'). Should match in
#'   length to the input for the argument `Values`.
#' @param Scale If applicable, the full name, abbreviation, and
#'   total number of items for a scale or inventory measure (e.g.,
#'   'Hospital Anxiety Depression SCale (HADS) - 14 items').
#' @param Subscale If applicable, the subscale name and its number
#'   of items (e.g., 'Anxiety - 7 items').
#' @param Range For numeric variables, the minimum and maximum
#'   observed values. Can be inferred based on input to the
#'   argument `x`.
#' @param Missing_data The values and/or labels used for missing data.
#'   Must be in the format '<Value 1|Value 2|...|Value N>'. For
#'   example, if missing values are represented by `NA`,
#'   `NAN`, " ", and "", this would be written as '<NA|NaN| |>'.
#' @param x The vector of values for the specified variable.
#' @param lst A list with the existing attributes/meta-data
#'   assigned to the variable.
#' @param time_points A vector of labels for the time points.
#' @param studies A vector of labels for the studies or study phases.
#' @param groups A vector of labels for the subject groups.
#' @param variable_categories A named vector matching 3-letter
#'   abbreviations with different variable categories.
#' @param data_types A named vector matching 3-letter
#'   abbreviations with different data types.
#' @param custom_attr An optional named list of lists, which allows
#'   users to specify additional meta-data entries
#'   besides the default ones. Each element in the list corresponds
#'   to a new entry, the name of the entry determined from the name of
#'   the element in the list. Additional information is given in the
#'   details and examples section.
#'
#' @details
#'
#' At a minimum, standardized column names follow the format:
#' 'XXX.YYY.Variable_Description'. Here, 'XXX' is a 3-letter abbreviation
#' specifying a general content category for a variable (e.g.,
#' variables for subject or patient identification, variables
#' that deal with experimental session details, variables for
#' inventory measures, etc.). Next, 'YYY' is a 3-letter abbreviation
#' specifying the data type of the variable (e.g., integer-based
#' values, double float precision numbers, character strings, etc.).
#' Finally, 'Variable_Description' is a brief human readable description
#' (e.g., 'Session_dates', or 'Time_points'). With this format,
#' auto-completion of several fields for meta-data (and by extension
#' future data dictionaries) become possible as per the
#' abbreviations and labels defined via the arguments
#' `variable_categories` and `data_types`.
#'
#' For cases that require additional meta-data entries not included
#' in the default arguments provided by `add_meta_data_for_dictionary`,
#' a user can define new entries via the `custom_attr` argument.
#' Furthermore, with careful formatting of the list passed to
#' the `custom_attr` argument, one can create custom meta-data
#' entries that auto-complete based on abbreviations included in
#' the column names, just like the `Variable_category` and
#' `Data_type` options. The final demonstration in the examples
#' sections details how this can be done.
#'
#' @author Kevin Potter
#'
#' @return A list with the completed meta-data fields.
#'
#' @examples
#' # Example data frame with standardized column names
#' df <- data.frame(
#'   IDS.CHR.Subject = rep( LETTERS[1:3], each = 3 ),
#'   SSS.INT.Time_point = rep( 1:3, 3 ),
#'   SSS.CHR.Group = rep( c( 'ACT', 'ACT', 'WLC' ), each = 3 ),
#'   SSS.DAT.Date = rep( Sys.Date(), 9 ),
#'   INV.DBL.Fake_scores = round( rnorm( 9 ), 1 ),
#'   stringsAsFactors = F
#' )
#' # Assume missing data for first time point
#' df$INV.DBL.Fake_scores[ seq( 1, 9, 3 ) ] = NA
#'
#' # Add meta-data for subject ID and time point columns
#' # Auto-completes variable category and data type
#' vrb <- colnames( df )[1]
#' attributes( df[[ vrb ]] ) <- add_meta_data_for_dictionary(
#'   Column_name = vrb,
#'   Description = 'Example subject identifier'
#' )
#' vrb <- colnames( df )[2]
#' attributes( df[[ vrb ]] ) <- add_meta_data_for_dictionary(
#'   Column_name = vrb,
#'   Description = 'Example study time points'
#' )
#'
#' # Add meta-data for details on randomization group
#' # Details on values and full group label included
#' vrb <- colnames( df )[3]
#' attributes( df[[ vrb ]] ) <- add_meta_data_for_dictionary(
#'   Column_name = vrb,
#'   Description = 'Example group membership',
#'   Values = 'ACT|WLC',
#'   Labels_for_values = 'Active group|Waitlist control'
#' )
#'
#' # Add meta-data for date data was collected
#' # Example of a variable with existing meta-data that
#' # should not be over-written
#' vrb <- colnames( df )[4]
#' attributes( df[[ vrb ]] ) <- add_meta_data_for_dictionary(
#'   Column_name = vrb,
#'   Description = 'Example date variable',
#'   # Prevent 'Date' class from being over-written
#'   prev_attributes = attributes( df[[ vrb ]] )
#' )
#'
#' # Add meta-data for inventory measure
#' # Specify scale details in format
#' # 'Inventory name (Abbreviation) - N items'
#' # Entries on time points, group, and range
#' # accurately completed by providing vector for scores,
#' # and columns defining time points and groups
#' vrb <- colnames( df )[5]
#' no_na <- !is.na( df[[ vrb ]] )
#' attributes( df[[ vrb ]] ) <- add_meta_data_for_dictionary(
#'   Column_name = vrb,
#'   Description = 'Example inventory scores',
#'   Scale = 'Fake Inventory (FI) - 10 items'
#'   x = df[[ vrb ]],
#'   time_points = unique( df$SSS.INT.Time_point[ no_na ] ),
#'   groups = unique( df$SSS.CHR.Group[ no_na ] )
#' )
#'
#' # --- Example for custom meta-data entries ---
#'
#' # Create example wide-form data set
#' df <- data.frame(
#'   IDS.CHR.AT.Subject = LETTERS[1:4],
#'   SSS.INT.AT.Group = rep( 1:2, each = 4 ),
#'   MED.INT.Y1.Took_meds = c( 0, 1, 1, 0 ),
#'   MED.INT.Y2.Took_meds = c( 0, 1, 1, 0 ),
#'   stringsAsFactors = F
#' )
#'
#' # Create named list with specific elements
#' # to allow auto-fill of time point info
#' # based on abbreviations in column names
#' custom_attr <- list(
#' # Name for meta-data entry
#' Study_year = list(
#'   # Content for entries
#'   Content = c( 'Year 1 data', 'Year 2 data' ),
#'   # Abbreviation to match in column name
#'   Abbreviation = c( 'Y1', 'Y2' ),
#'   # Start and end of abbreviation in column name
#'   Position = c( 9, 10 )
#' ) )
#'
#' # Loop over column names for the time points
#' # and auto-fill based on 'custom_attr' list
#' for ( i in 1:2 ) {
#'   vrb <- paste0( 'MED.INT.Y', i, '.Took_meds' )
#'   attributes( df[[vrb]] ) <- add_meta_data_for_dictionary(
#'     Column_name = vrb,
#'     Description = 'Did patient take meds',
#'     Values = '0|1',
#'     Labels_for_values = 'No|Yes',
#'     prev_attributes = attributes( df[[ vrb ]] ),
#'     groups = unique( df$SSS.INT.AT.Group ),
#'     custom_attr = custom_attr
#'   )
#' }
#'
#' @export

add_meta_data_for_dictionary = function( Column_name = ' ',
                                         Variable_category = ' ',
                                         Data_type = ' ',
                                         Time_points_assessed = 'N/A',
                                         Studies_assessed = 'N/A',
                                         Groups_assessed = 'N/A',
                                         Description = ' ',
                                         Values = ' ',
                                         Labels_for_values = ' ',
                                         Scale = ' ',
                                         Subscale = ' ',
                                         Range = ' ',
                                         Missing_data = '<NA|NaN|>',
                                         x = NULL,
                                         prev_attributes = NULL,
                                         time_points = NULL,
                                         studies = NULL,
                                         groups = NULL,
                                         variable_categories = c(
                                           IDS = 'Identifier',
                                           SSS = 'Session details',
                                           SBJ = 'Subject details',
                                           INV = 'Inventory measures',
                                           QTN = 'Inventory measures',
                                           TLF = 'Time-line follow-back measures',
                                           URN = 'Urine test data'
                                         ),
                                         data_types = c(
                                           INT = 'Integer',
                                           DBL = 'Double float',
                                           CHR = 'Character string',
                                           LGC = 'Logical',
                                           FCT = 'Enumerated type',
                                           DAT = 'R Date-class variable'
                                         ),
                                         custom_attr = NULL ) {
  .Deprecated()
  # If possible, automatic completion of variable category
  if ( nchar( Column_name ) >= 3 & Variable_category == ' ' ) {

    # Extract 3-letter abbreviation for variable category
    tag <- stringr::str_sub( Column_name, start = 1, end = 3 )

    # Extract existing 3-letter abbreviations with assigned
    # labels for categories
    nms <- names( variable_categories )

    # If tag is found in pre-defined abbreviations
    if ( tag %in% names( variable_categories ) ) {

      # Loop over categories
      for ( i in 1:length( nms ) ) {

        # When matched
        if ( tag == nms[i] ) {
          # Use pre-defined label for category
          Variable_category = as.character( variable_categories[i] )
        }

      }

    }

  }

  # If possible, automatic completion of data type
  if ( nchar( Column_name ) >= 7 & Data_type == ' ' ) {

    # Extract 3-letter abbreviation for data type
    tag = stringr::str_sub( Column_name, start = 5, end = 7 )

    # Extract existing 3-letter abbreviations with assigned
    # labels for data types
    nms <- names( data_types )

    # If tag is found in pre-defined abbreviations
    if ( tag %in% names( data_types ) ) {

      # Loop over categories
      for ( i in 1:length( nms ) ) {

        # When matched
        if ( tag == nms[i] ) {
          # Use pre-defined label for category
          Data_type = as.character( data_types[i] )
        }

      }

    }

  }

  # If raw vector of data is provided
  if ( !is.null( x ) ) {

    # Specify range using raw data
    Range = paste0(
      min( x[ !is.na( x ) ] ),
      ' to ',
      max( x[ !is.na( x ) ] )
    )

  }

  # If vector of time points is provided
  if ( !is.null( time_points ) ) {

    Time_points_assessed = paste(
      time_points,
      collapse = '|'
    )

  }

  if ( !is.null( studies ) ) {

    Studies_assessed = paste(
      studies,
      collapse = '|'
    )

  }

  if ( !is.null( groups ) ) {

    Groups_assessed = paste(
      groups,
      collapse = '|'
    )

  }

  if ( is.null( prev_attributes ) ) {
    # If variable does not have any
    # existing meta-data, create new list

    cur_attributes = list(
      Column_name = Column_name,
      Variable_category = Variable_category,
      Data_type = Data_type,
      Time_points_assessed = Time_points_assessed,
      Studies_assessed = Studies_assessed,
      Groups_assessed = Groups_assessed,
      Description = Description,
      Values = Values,
      Labels_for_values = Labels_for_values,
      Scale = Scale,
      Subscale = Subscale,
      Range = Range,
      Missing_data = Missing_data
    )

  } else {
    # Preserve existing attributes
    cur_attributes = prev_attributes

    # Add meta-data for data dictionary entries
    cur_attributes$Column_name = Column_name
    cur_attributes$Variable_category = Variable_category
    cur_attributes$Data_type = Data_type
    cur_attributes$Studies_assessed = Studies_assessed
    cur_attributes$Time_points_assessed = Time_points_assessed
    cur_attributes$Groups_assessed = Groups_assessed
    cur_attributes$Description = Description
    cur_attributes$Values = Values
    cur_attributes$Labels_for_values = Labels_for_values
    cur_attributes$Scale = Scale
    cur_attributes$Subscale = Subscale
    cur_attributes$Range = Range
    cur_attributes$Missing_data = Missing_data
  }

  # Add user-defined/project-specific meta-data entries
  if ( !is.null( custom_attr ) ) {

    # Loop over entries
    n_attr <- length( custom_attr )
    for ( k in 1:n_attr ) {

      # Extract name for entry
      attr_name <- names( custom_attr )[k]

      # Add content for entry
      if ( !is.null( custom_attr[[k]]$Abbreviation ) &
           !is.null( custom_attr[[k]]$Position ) ) {
        # If content is based on abbreviations in
        # column name

        # Extract tag
        tag <- stringr::str_sub(
          Column_name,
          start = custom_attr[[k]]$Position[1],
          end = custom_attr[[k]]$Position[2]
        )

        # Extract abbreviations
        lbls <- custom_attr[[k]]$Abbreviation

        # Initialize content
        cnt <- ' '
        # Loop over abbreviations and match with tag
        for ( j in 1:length( lbls ) ) {
          # Add content
          if ( tag %in% lbls ) {
            cnt <- custom_attr[[k]]$Content[j]
          }
        }
      } else {
        # Take content as is
        cnt <- custom_attr[[k]]$Content[1]
      }

      # Add meta-data entry
      cur_attributes <- c(
        cur_attributes,
        cnt
      )
      names( cur_attributes )[ length( cur_attributes ) ] <- attr_name

    }

  }

  # Loop through elements and check for special characters
  # that could cause encoding issues with a .csv file
  n_cases = length( cur_attributes )
  for ( i in 1:n_cases ) {

    check = any( grepl( ',', cur_attributes[[i]], fixed = T ) )
    if ( any( check ) ) {

      warning(
        paste0(
          'Please change meta-data to exclude commas to avoid ',
          'issues with .csv file'
        )
      )

      # Remove commas
      cur_attributes[[i]] = gsub( ',', ' - ', cur_attributes[[i]], fixed = T )

    }

  }

  return( cur_attributes )
}

#### 12) notes_for_custom_function ####
#' Display Documention for Custom R Function
#'
#' Extracts and displays documention for a custom R function,
#' given a documentation format of
#'
#' @param f An R function saved in the current workspace.
#'
#' @details Documentation start with the phrase '  # Purpose:'.
#' Each subsequent line that starts with '  #' will be displayed.
#'
#' Standard CAM procedure for documenting functions is to have
#' lines following the headers 'Purpose:', 'Arguments:', and
#' 'Returns:'.
#'
#' @author Kevin Potter
#'
#' @examples
#' # Example custom function with standardized documentation
#' f = function( x ) {
#'   # Purpose:
#'   # Displays the values of 'x'
#'   # Arguments:
#'   # x - An R object
#'   # Returns:
#'   # Output of the 'print' call
#'
#'   # Internal notes
#'   print( x )
#' }
#' notes_for_custom_function( f )
#'
#' @export

notes_for_custom_function = function( f ) {
  .Deprecated()
  f_details = capture.output( print( f ) )


  check_if_doc = any( grepl( "  # Purpose:", f_details, fixed = T ) )

  if ( check_if_doc ) {

    doc_start = which( grepl( "  # Purpose:", f_details, fixed = T ) )
    pound_sym = which( grepl( "  #", f_details, fixed = T ) )

    pound_sym =
      pound_sym[ c( 1, diff( pound_sym ) ) == 1 ]
    pound_sym = pound_sym[ pound_sym >= doc_start ]

    out = paste( f_details[ pound_sym ], collapse = '\n' )
    message( out )

  }

}

#### 13) go_to ####
#' Navigate to Folder in Project Directory
#'
#' A convenience function that allows a user to quickly navigate
#' to specified folders in the Project directory via an easily
#' readable command. Requires a list called `folder_pathways`
#' with the paths and associated labels to exist in the global environment
#' (see [create_folder_pathways()]).
#'
#' @param path The label associated with the desired folder to navigate to
#'   (e.g., 'Project', 'R', etc.).
#'
#' @author Kevin Potter
#'
#' @export

go_to <- function( path ) {
  .Deprecated()
  if ( !exists( 'folder_pathways', envir = .GlobalEnv ) ) {
    stop( paste0(
      "The list 'folder_pathways' with the paths and associated ",
      "labels was not found in the global environment - please ",
      "create this list via the 'create_folder_pathways' function"
      ), call. = F )
  }

  # Extract stored paths
  nms <- names( folder_pathways )
  nms <- nms[ nms != 'Path_labels' ]
  # Extract labels/abbreviations for each path
  lbls <- folder_pathways$Path_labels

  # Function to check if a string is
  # present in vector of strings
  is_in <- function( x, labels ) {
    return( x %in% labels )
  }

  location <- ""

  # Loop over possible paths
  for ( i in 1:length( nms ) ) {

    if ( path %in% lbls[[ nms[i] ]] ) {
      # Save path for future navigation
      location <- folder_pathways[[ nms[i] ]]
    }

  }

  if ( location == "" ) {
    stop( 'Label/abbreviation for folder path not available',
          call. = F )
  } else {
    setwd( location )
  }

}

#### 14) create_folder_pathways ####
#' Create or Update List with Paths for Project Directory Hierarchy
#'
#' Creates/update a list that contains paths for the folder hierarchy
#' for a standard CAM RStudio project. This list can then be accessed
#' by the [go_to()] function for fast and readable folder
#' navigation.
#'
#' @param folder_pathways An optional existing list with paths and labels
#'   that can then be updated with the `add_path` argument. If
#'   `NULL` (the default), automatically generates the list AND
#'   the standard folder hierarchy for a CAM RStudio project, where...
#'   \describe{
#'     \item{Project}{The project directory}
#'     \item{+ R}{The folder for R scripts}
#'     \item{+ Data}{}
#'     \item{++ Data_processing}{Processing scripts to clean the raw data}
#'     \item{++ Raw_data_files}{Files, such as downloads from REDCap, to
#'       be cleaned and processed}
#'   }
#' @param add_path An optional named list to add a new folder path and
#'   associated labels to an existing `folder_pathways` object.
#'   Must contain three elements:
#'   \describe{
#'     \item{path}{The absolute path to the folder to add;}
#'     \item{title}{The internal label for the slot for the folder path;}
#'     \item{labels}{The set of labels to use in the `go_to` call.}
#'   }
#' @param redcap Logical; if `TRUE`, when creating the
#'   `folder_pathways` list, will check for a folder one level
#'   above the project directory for an installation of the R package
#'   'REDCapR' version 0.9.8, which is compatible with current CAM
#'   projects as of 09/04/2020.
#' @param dropbox Logical; if `TRUE`, when creating the
#'   `folder_pathways` list, will check for a text file
#'   in the 'Raw_data_files' folder that contains the path to a
#'   dropbox folder on the local machine. If the file does not
#'   exist, a warning will be displayed and a file template
#'   will be created. The user can then copy the path into the
#'   file.
#' @param api Logical; if `TRUE`when creating the
#'   `folder_pathways` list, will check for a text file
#'   in the 'Raw_data_files' folder that contains the API token
#'   for a specified REDCap project. If the file does not
#'   exist, a warning will be displayed and a file template
#'   will be created. The user can then copy their API token
#'   into the file.
#' @return A list with slots for folder pathway of interest (e.g.,
#'   the project directory, the R scripts folder, etc.), and an
#'   additional list with labels (e.g., `c('Project','project')`)
#'   that can be used with the [go_to()] function to navigate
#'   to the associated folder.
#'
#' @author Kevin Potter
#'
#' @examples
#' # For example purposes, create simple
#' # list with path and labels
#' folder_pathways <- list(
#'   Project = getwd(),
#'   Path_labels = list( Project = c( 'Project', 'project' ) )
#' )
#'
#' # Add a new path and labels to the existing list
#' lst <- list(
#'   path = paste0( getwd(), '/R' ),
#'   title = 'R',
#'   labels = c( 'R', 'r' )
#' )
#' folder_pathways = create_folder_pathways( folder_pathways, add_path = lst )
#'
#' @export

create_folder_pathways <- function( folder_pathways = NULL,
                                    add_path = NULL,
                                    redcap = TRUE,
                                    dropbox = TRUE,
                                    api = TRUE ) {
  .Deprecated()
  # Initial creation of 'folder_pathways' list and
  # creation of directory structure for a standard
  # CAM project
  if ( is.null( folder_pathways ) ) {

    # Initialize list
    folder_pathways$Path_labels <- list(
      Project = c( 'Project', 'project' )
    )

    # Check if current folder has a file with the '.Rproj'
    # extension
    # - if TRUE, in Project directory
    # - if FALSE, assume to be in the 'R' directory,
    #   one level below the Project directory
    if ( !any( grepl( '.Rproj', dir(), fixed = T ) ) ) {
      setwd( '..' )
      # Check if assumptions are true and throw an error
      # if '.Rproj' extension not found
      if ( !any( grepl( '.Rproj', dir(), fixed = T ) ) ) {
        stop(
          paste0(
            "Initial generation of the 'folder_pathways' list ",
            "must be done either in the Project directory or ",
            "in the 'R' sub-directory one level below the ",
            "Project directory"
          ), call. = F
        )
      }
    }

    ### Folder for project directory
    folder_pathways$Project <- getwd()

    ### Folder for R scripts

    if ( !'R' %in% dir() ) {
      # If folder does not exist, create folder
      dir.create( 'R' )
    }
    setwd( 'R' )
    folder_pathways$R_scripts <- getwd()
    # Update path labels
    folder_pathways$Path_labels$R_scripts =
      c( 'R', 'r', 'R scripts', 'r scripts' )

    # Return to project directory
    setwd( folder_pathways$Project )

    ### Folder for data

    if ( !'Data' %in% dir() ) {
      # If folder does not exist, create folder
      dir.create( 'Data' )
    }
    setwd( 'Data' )
    folder_pathways$Data <- getwd()
    # Update path labels
    folder_pathways$Path_labels$Data <-
      c( 'Data', 'data' )

    ### Folder for raw data files

    if ( !'Raw_data_files' %in% dir() ) {
      # If folder does not exist, create folder
      dir.create( 'Raw_data_files' )
    }
    setwd( 'Raw_data_files' )
    folder_pathways$Raw_data_files <- getwd()
    # Update path labels
    folder_pathways$Path_labels$Raw_data_files <-
      c( 'Raw data', 'raw data', 'Raw', 'raw' )

    # If specified, check for file containing API token for
    # REDCap access
    if ( api ) {

      if ( !'API_token.txt' %in% dir() ) {
        warning(
          paste0(
            "The file 'API_token.txt' was not found - to download ",
            "data from REDCap, please get a project-specific token ",
            "and create a text file with the token in the 'Raw_data_files' ",
            "folder"
          ), call. = F
        )

        fileConn <- file("API_token.txt")
        writeLines(c("COPY API TOKEN HERE"), fileConn)
        close(fileConn)
      }

    }

    # If specified, check for file containing path to a
    # dropbox folder
    if ( dropbox ) {

      if ( !'Dropbox_folder_location.txt' %in% dir() ) {
        warning(
          paste0(
            "The file 'Dropbox_folder_location.txt' was not found - ",
            "to upload folders to a dropbox folder ",
            "please find the local path to the folder and add it ",
            "to the text file in the 'Raw_data_files' folder (e.g., ",
            "~/User/Dropbox_folder/)"
          ), call. = F
        )

        fileConn <- file("Dropbox_folder_location.txt")
        writeLines(c("COPY PATH TO DROPBOX FOLDER HERE"), fileConn)
        close(fileConn)

      } else {

        # Read in path for dropbox folder
        orig_dir <- getwd()
        setwd(
          scan( file = 'Dropbox_folder_location.txt',
                what = 'character', sep = '\n',
                quiet = T )
        )
        folder_pathways$Dropbox <- getwd()
        # Update path labels
        folder_pathways$Path_labels$Dropbox <-
          c( 'Dropbox', 'dropbox' )
        setwd( orig_dir )

      }

    }

    ### Folder for data processing scripts

    setwd( folder_pathways$Data )
    if ( !'Data_processing' %in% dir() ) {
      # If folder does not exist, create folder
      dir.create( 'Data_processing' )
    }
    setwd( 'Data_processing' )
    folder_pathways$Data_processing <- getwd()
    # Update path labels
    folder_pathways$Path_labels$Data_processing <-
      c( 'Processing', 'processing', 'Process', 'process',
         'Data processing', 'data processing',
         'Process data', 'process data',
         'Processing scripts', 'processing scripts' )

    ### Specific REDCapR package version

    # If specified, check if there is a folder for
    # the REDCapR package version 0.9.8
    if ( redcap ) {

      setwd( folder_pathways$Project )
      setwd( '..' )

      if ( !'REDCapR_previous_versions' %in% dir() ) {
        dir.create( 'REDCapR_previous_versions' )
        warning(
          paste0( 'Folder for REDCapR package version 0.9.8 ',
                  'created - make sure to install ',
                  'this version of the package (rather than ',
                  'most recent version)'
                ), call. = F
        )
      }
      setwd( 'REDCapR_previous_versions' )
      folder_pathways$REDCapR <- getwd()
      # Update path labels
      folder_pathways$Path_labels$REDCapR =
        c( 'REDCap', 'redcap', 'REDCapR', 'redcapr' )
      setwd( folder_pathways$R_scripts )

    }


  }

  # Update 'folder_pathways' list with new path and
  # associated labels
  if ( !is.null( add_path ) ) {

    # Check that necessary elements are included in
    # input
    if ( !all( names( add_path ) %in% c( 'path', 'title', 'labels' ) ) ) {
      stop( paste0(
        "To update 'folder_pathways' input with a new path and associated ",
        "labels, the 'add_path' list must have 3 named elements: ",
        "'path' with the folder path, 'title' with the internal list slot ",
        "name, and 'labels' with the labels for the path to use with the ",
        "'go_to' function"
      ), call. = F )
    }

    # Add path to list
    folder_pathways <- c( folder_pathways, add_path$path )

    # Re-title new path
    nms <- names( folder_pathways )
    nms[ nms == "" ] <- add_path$title
    names( folder_pathways ) <- nms

    # Add slot for path labels
    folder_pathways$Path_labels <- c(
      folder_pathways$Path_labels, list( add_path$labels ) )

    # Re-title slot for path labels
    nms <- names( folder_pathways$Path_labels )
    nms[ nms == "" ] <- add_path$title
    names( folder_pathways$Path_labels ) <- nms

  }

  return( folder_pathways )
}

#### 15) update_filename ####
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
  .Deprecated()
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

#### 16) file_present ####
#' Checks if a File is Present in a Folder
#'
#' Checks if a file is present in the current working directory.
#' Can check either regular file or file using MGH-CAM's standardized
#' file naming template: TXX-Description-MM_DD_YYYY-vX.X.X.ext
#'
#' @param string A file name or part of a file name to search for.
#' @param output Character string indicating the type of output to
#'   return, either...
#' \enumerate{
#'   \item 'Logical' or 'logical'
#'   \item 'Vector', 'vector', or 'vec'
#'   \item 'Index' or 'index'
#'   \item 'Name' or 'name'
#' }
#' @param std_name A logical indicating whether the file follows the
#'   standardized naming convention. If true, just matches the tag
#'   and description of the file.
#'
#' @return Either...
#' \enumerate{
#'   \item A logical value, `TRUE` if the file is present;
#'   \item A logical vector for all files in the folder;
#'   \item The index position for the file if it exists;
#'   \item The file name.
#' }
#'
#' @author Kevin Potter
#'
#' @export

file_present <- function( string,
                          output = 'Logical',
                          std_name = FALSE) {
  .Deprecated()
  # All files and folders present
  # in working directory
  all_files <- dir()

  # Determine if (standard) file name is present
  # in list of files/folders
  if (std_name) {
    fmatch <- regexpr('^\\w\\d{2}-[^-]*', string, perl = T)
    tag_and_desc <- regmatches(string, fmatch)
    check <- grepl(tag_and_desc, all_files, fixed = T)
  } else{
    check <- grepl( string, all_files, fixed = T )
  }

  # Output
  if ( output %in% c( 'Logical', 'logical' ) ) {
    return( any( check ) )
  }
  if ( output %in% c( 'Vector', 'vector', 'vec' ) ) {
    return( check )
  }
  if ( output %in% c( 'Index', 'index' ) ) {
    return( which( check ) )
  }
  if ( output %in% c( 'Name', 'name' ) ) {
    if ( any( check ) ) {
      return( all_files[ check ] )
    } else {
      return( NULL )
    }
  }

}

#### 17) load_package ####
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
#'   If `NULL`, assumes the username is 'rettopnivek'
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
  .Deprecated()

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
#' Create a global cache object.
#'
#' Creates a global object `camr` for storing information such as file paths in
#' an encapsulated manner. If a `.cache` Rds file can be found in the working
#' directory, its contents will be loaded.
#'
#' @author Michael Pascale
#'
#' @export
#' @md
camr_cache <- function () {
  .Deprecated()

  if (exists('camr', envir=.GlobalEnv))
    warning('Global cache object `camr` already initialized.')

  if (fs::file_exists('.cache'))
    camr <<- readRDS('.cache')
  else
    camr <<- list(origin=getwd())
}

#' Reset the global cache object. Delete the `.cache` file.
#'
#' @author Michael Pascale
#'
#' @export
#' @md
camr_reset <- function () {
  .Deprecated()
  if (fs::file_exists('.cache'))
    fs::file_delete('.cache')
  rm(camr, pos=1)
  camr_cache()
}

#' Save the global cache object to disk.
#'
#' The global cache is stored to a `.cache` Rds in the working directory.
#'
#' @author Michael Pascale
#'
#' @export
#' @md
camr_save <- function () {
  .Deprecated()
  saveRDS(camr, file=path(camr$origin, '.cache'))
}
#' Construct and check the existence of paths given the prefix directory.
#'
#' @author Michael Pascale
#'
#' @export
#' @md
camr_paths <- function () {
  .Deprecated()
  if (is.null(camr[['prefix']]))
    stop('Prefix not initialized.')

  folders <- c('Documents', 'Figures', 'Reports')
  expanded <- c()

  for (folder in folders) {
    fullpath <- path(camr$prefix, folder)

    if(!fs::dir_exists(fullpath) && askYesNo(stringr::str_glue('Directory "{fullpath}" does not exist. Create?')))
      fs::dir_create(fullpath)

    expanded <- append(expanded, fullpath)
  }

  camr$paths <<- setNames(as.list(expanded), folders)
  camr_save()
}


#' Set a miscellaneous item within the global `camr` object and save to disk.
#'
#' @param name The property to set.
#'
#' @param value The value to set.
#'
#' @author Michael Pascale
#'
#' @export
#' @md
camr_set <- function (name, value) {
  .Deprecated()
  camr[name] <<- value
  camr_save()
}

#' Get an item fromm the global `camr` object.
#'
#' @param name The property to get.
#'
#' @author Michael Pascale
#'
#' @export
#' @md
camr_get <- function (name) {
  .Deprecated()
  camr[name]
}
