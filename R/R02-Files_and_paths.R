# Functions for files and paths
# Written by...
#   Michael Pascale
#   Kevin Potter
# Maintained by...
#   Michael Pascale
#   Kevin Potter
# email:
#   mppascale@mgh.harvard.edu
#   kpotter5@mgh.harvard.edu
# Please email us directly if you
# have any questions or comments
# Last updated 2022-10-14

# Table of contents
# 1) File paths and names
#   1.1) camr_file_name
#   1.2) camr_filename
#   2.2) camr_file_path
# 2) Folders and directories
#   2.1) camr_pushd
#   2.2) camr_popd
# 3) camr_source_scripts
# 4) camr_load_from_RData

#### 1) File paths and names ####

#### 1.1) camr_file_name ####
#' Generate a Standardized File Name
#'
#' Function to generate a file name of the form
#' \code{project-description-date-time-commit.extension}.
#'
#' @param description A character string giving
#'   a brief description or human-readable name
#'   for the file (preferably in the format
#'   \code{File_name}).
#' @param extension An optional character string
#'   giving the file extension sans period. If
#'   \code{NULL} defaults to \code{RData}.
#' @param project An optional character string with
#'   the project label. If \code{NULL} will attempt to
#'   extract from an environment variable (CAMR_PROJECT)
#'   or a project-level config file.
#' @param date An optional character string giving
#'   the date (typically in the format YYYY_MM_DD).
#' @param time An optional character string giving
#'   the time (typically in the format HH_MM_SS).
#' @param datetime An optional character string giving
#'   both the date and time (typically in the format
#'   YYYY_MM_DD-HH_MM_SS).
#' @param git Logical; if \code{TRUE} includes the
#'   most recent commit number.
#'
#' @author Michael Pascale, Kevin Potter
#'
#' @returns A character string, a file name of the form
#' \code{project-description-date-time-commit.extension}.
#'
#' @export

camr_file_name <- function(
    description,
    extension = NULL,
    project = NULL,
    date = NULL,
    time = NULL,
    datetime = NULL,
    git = TRUE ) {

  # Check inputs
  checkmate::assert_character(description, pattern='^(\\w|\\.)+$')
  checkmate::assert_character(extension, pattern='^\\w+$', null.ok=TRUE)
  checkmate::assert_character(project, pattern='^\\w*$', null.ok=TRUE)

  # If file extension is not provided
  if ( is.null( extension ) ) {

    # If every file description includes an extension
    vlgl_has_extension <- grepl('\\.\\w{1,4}$', description)
    if (any(vlgl_has_extension)) {

      mchr_split <- str_match(description, '^(.*)\\.(\\w{1,4})$')

      description <- if_else(vlgl_has_extension, mchr_split[,2], description)
      extension <- if_else(vlgl_has_extension, mchr_split[,3], 'RData')

      # Close 'If every file description includes an extension'
    } else {
      extension <- 'RData'
      # Close else for 'If every file description includes an extension'
    }
    # Close 'If file extension is not provided'
  } else {
    checkmate::assert_character(description, pattern='^\\w+$')
    # Close else for 'If file extension is not provided'
  }

  # If label for project is not provided
  if ( is.null( project ) ) {

    # Attempt to find via environmental variable, then project config file
    project <- Sys.getenv('CAMR_PROJECT', NA) %??% tryCatch(config::get('project'), error=camr_pass) %??% ''

    # Close 'If label for project is not provided'
  }

  if (project != '') {
    # Double-check project label
    checkmate::assert_character(project, pattern='^\\w+$')

    project <- paste0(project, '-')
  }

  # If a date and time is not provided
  if ( is.null(datetime) ) {

    # If no date is provided
    if ( is.null(date) ) {

      date <- camrprojects::camr_ymd()

      # Close 'If no date is provided'
    }

    # If no time is provided
    if ( is.null(time) ) {

      time <- camrprojects::camr_hms()

      # Close 'If no time is provided'
    }

    # Create date and time for file name
    datetime <- paste0( date, '-', time )

    # Close 'If a date and time is not provided'
  }

  # Extract commit number
  if ( isTRUE(git) && git2r::in_repository() ) {

    commit <- substr(git2r::last_commit()$sha, 0, 7)
    status <- git2r::status(untracked = FALSE)

    # Check for uncommitted changes
    if ( length( c(status$unstaged, status$staged) ) != 0 ) {

      commit <- paste0(commit, 'm')
      warning( paste0(
        'Uncommited changes are present. Output files ',
        'will have a version ending in "m".'
      ) )

      # Close 'Check for uncommitted changes'
    }

    # Close 'Extract commit number'
  } else if ( is.character(git) ) {

    commit <- git

    # Close else for 'Extract commit number'
  }
  if ( commit != "" ) {
    commit <- paste0( "-", commit )
  }

  file_name <- paste0(
    project,
    paste(
      description,
      datetime,
      sep = '-'
    ),
    commit, '.', extension
  )

  camr_sanitize_path(file_name)
}

#### 1.2) camr_name_file ####

#' @rdname camr_file_name
#' @export

camr_name_file <- camr_file_name

#### 2.2) camr_file_path ####
#' Determine Absolute Path to a File
#'
#' Function to find the absolute path to a file given a
#' partial match to its name.
#'
#' @param partial_name A character string, the file name
#'   or part of the file name that can uniquely identify
#'   it from other files.
#' @param env_variable An optional character string, an
#'   environmental variable listed in a .env file
#'   loaded into the R environment via the [dotenv]
#'   package. If provided, will be used as the source
#'   folder assumed to contain the file.
#' @param folder An optional character string, the
#'   name of the folder (or part of the folder name
#'   that can uniquely identify it) containing the
#'   file. If the `env_variable` argument is provided,
#'   then the `folder` argument can be used to specify
#'   a subfolder containing the file.
#' @param latest Logical; if `TRUE` the function will
#'   sort all matching file names and return the final
#'   one (if file names contain dates, this means
#'   the function returns the most recent file).
#'
#' @return A character string, an absolute path to a file.
#'
#' @export
#' @md

camr_file_path <- function(
    partial_name,
    env_variable = NULL,
    folder = NULL,
    latest = TRUE ) {

  # Check inputs
  checkmate::assert_string( partial_name )
  checkmate::assert_logical( latest )

  # Initialize path to source folder
  path_to_source_folder <- ""

  # If environmental variable provided
  if ( !is.null( env_variable ) ) {

    # Check input
    checkmate::assert_string( env_variable )

    # Extract path to source folder
    path_to_source_folder <- Sys.getenv( env_variable )

    # If environmental variable not found
    if ( path_to_source_folder == "" ) {

      err_msg <- paste0(
        "Environmental variable '",
        env_variable,
        "' does not exist."
      )

      stop( err_msg)

      # Close 'If environmental variable not found'
    }

    # Close 'If environmental variable provided'
  }

  # Default to working directory
  if ( path_to_source_folder == "" ) {

    path_to_source_folder <- getwd()

    # Close 'Default to working directory'
  }

  # If a folder name is provided
  if ( !is.null( folder ) ) {

    # Check input
    checkmate::assert_string( folder )

    extended_path <- grepl( "/", folder, fixed = TRUE )

    if ( !extended_path ) {

      # Check that folder exists
      all_files_and_folders <-
        dir( path = path_to_source_folder )

      folder_not_found <- TRUE

      # If folder does not exist attempt partial matching
      if ( !( folder %in% all_files_and_folders ) ) {

        matches <-
          grepl( folder, all_files_and_folders, fixed = TRUE ) &
          !grepl( ".", all_files_and_folders, fixed = TRUE )

        # If partial match found
        if ( any( matches ) ) {

          folder_not_found <- FALSE

          matching_folders <- all_files_and_folders[ matches ]

          # Use most recent folder
          if ( latest ) {

            matching_folders <- sort(
              matching_folders, decreasing = TRUE
            )

            # Close 'Use most recent folder'
          }

          folder <- matching_folders[1]

          # Close 'If partial match found'
        }

        # Close 'If folder does not exist attempt partial matching'
      }

      # If no match
      if ( folder_not_found ) {

        err_msg <- paste0(
          "Unable to locate any folders containing the ",
          "label: ", folder
        )
        stop( err_msg )

        # Close 'If no match'
      }

    }

    # Create full path to source folder
    path_to_source_folder <- paste0(
      path_to_source_folder,
      '/', folder
    )

    # Close 'If a folder name is provided'
  }

  # Use working directory
  if ( path_to_source_folder == "" ) {

    path_to_source_folder <- getwd()

    # Close 'Use working directory'
  }

  # Extract files in source folder
  all_files <- dir(
    path = path_to_source_folder
  )

  # Find matches to file
  partial_matches <- grepl( partial_name, all_files, fixed = T )

  # Extract matches
  if ( any( partial_matches ) ) {

    matching_files <- all_files[ partial_matches ]

    # Close 'Extract matches'
  } else {

    matching_files <- c()

    # Close else for 'Extract matches'
  }

  # If no files found
  if (length(matching_files) == 0) {

    stop("File not found")

    # Close 'If no files found'
  }

  # Use most recent file
  if ( latest ) {

    matching_files <- sort(
      matching_files, decreasing = TRUE
    )

    # Close 'Use most recent file'
  }

  current_file <- matching_files[1]

  full_path <- paste0( path_to_source_folder, '/', current_file )

  return( full_path )
}

#### 2) Folders and directories ####

#### 2.1) camr_pushd ####
#' Push a path to the directory stack.
#'
#' @param path Optional. The path to save. Defaults to the working directory.
#'
#' @author Michael Pascale
#'
#' @export
#' @md

camr_pushd <- function(
    path = getwd() ) {

  if (!exists('.camr_stack')) {
    .camr_stack=NULL
  }

  path = fs::path_real(path)
  if (fs::dir_exists(path)) {
    .camr_stack <<- c(path, .camr_stack)
  } else {
    warning(
      paste0(
        'Path does not exist and has not been added to the directory stack.'
      )
    )
  }
}

#### 2.2) camr_popd ####
#' Pop the last path from the directory stack and switch into it.
#'
#' @author Michael Pascale
#'
#' @export
#' @md

camr_popd <- function() {

  if (!exists('.camr_stack')) {
    stop('The directory stack has not been initialized.')
  }

  if (length(.camr_stack)) {
    setwd(.camr_stack[1])
    .camr_stack <<- .camr_stack[-1]
  } else {
    warning('The directory stack is empty.')
  }

}

#### 3) camr_source_scripts ####
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

camr_source_scripts = function(
    files_to_include = NULL,
    path = 'R' ) {

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

#### 4) camr_load_from_RData ####
#' Load R Object From .RData File
#'
#' Loads a specified R object in from a \code{.RData} file.
#'
#' @param path_to_RData_file A character string, the
#'   file path to the \code{.RData} file.
#' @param object_name A character string, the specific
#'   R object to load from the \code{.RData} file.
#'
#' @return An R object loaded in from the \code{.RData} file.
#'
#' @export

camr_load_from_RData <- function(
    path_to_RData_file,
    object_name ) {

  # Load in .RData file
  load( path_to_RData_file )

  return( get( object_name ) )
}


#' Sanitize a filename by removing directory paths and invalid characters.
#'
#' This function is taken directly from `fs::path_sanitize` to avoid dependency.
#'
#' `path_sanitize()` removes the following:
#' - [Control characters](https://en.wikipedia.org/wiki/C0_and_C1_control_codes)
#' - [Reserved characters](https://kb.acronis.com/content/39790)
#' - Unix reserved filenames (`.` and `..`)
#' - Trailing periods and spaces (invalid on Windows)
#' - Windows reserved filenames (`CON`, `PRN`, `AUX`, `NUL`, `COM1`, `COM2`,
#'   `COM3`, COM4, `COM5`, `COM6`, `COM7`, `COM8`, `COM9`, `LPT1`, `LPT2`,
#'   `LPT3`, `LPT4`, `LPT5`, `LPT6`, LPT7, `LPT8`, and `LPT9`)
#' The resulting string is then truncated to [255 bytes in length](https://en.wikipedia.org/wiki/Comparison_of_file_systems#Limits)
#'
#' @param chr_filename A character vector to be sanitized.
#' @param chr_replacement A character vector used to replace invalid characters.
#'
#' @seealso <https://www.npmjs.com/package/sanitize-filename>, upon which this
#'   function is based.
#'
#' @return Character vector containing the sanitized path.
#' @export
#'
#' @examples
#' # potentially unsafe string
#' str <- "~/.\u0001ssh/authorized_keys"
#' path_sanitize(str)
#'
#' path_sanitize("..")
camr_sanitize_path <- function (chr_filename, chr_replacement = "")
{

  illegal <- "[/\\?<>\\:*|\":]"
  control <- "[[:cntrl:]]"
  reserved <- "^[.]+$"
  windows_reserved <- "^(con|prn|aux|nul|com[0-9]|lpt[0-9])([.].*)?$"
  windows_trailing <- "[. ]+$"
  chr_filename <- gsub(illegal, chr_replacement, chr_filename)
  chr_filename <- gsub(control, chr_replacement, chr_filename)
  chr_filename <- gsub(reserved, chr_replacement, chr_filename)
  chr_filename <- gsub(windows_reserved, chr_replacement, chr_filename,
                   ignore.case = TRUE)
  chr_filename <- gsub(windows_trailing, chr_replacement, chr_filename)
  chr_filename <- substr(chr_filename, 1, 255)
  if (chr_replacement == "") {
    return(chr_filename)
  }

  camr_sanitize_path(chr_filename, "")
}
