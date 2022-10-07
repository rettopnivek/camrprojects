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
# Last updated 2022-09-10

# Table of contents
# 1) File paths and names
#   1.1) camr_file_name
#   1.2) camr_filename
#   2.2) camr_file_path
# 2) Folders and directories
#   2.1) camr_pushd
#   2.2) camr_popd

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
#'   extract from an environment variable or a config file.
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
  checkmate::assert_character(description, pattern='^\\w+$')
  checkmate::assert_character(extension, pattern='^\\w+$', null.ok=TRUE)
  checkmate::assert_character(project, pattern='^\\w+$', null.ok=TRUE)

  # If file extension is provided
  if ( !is.null( extension ) ) {

    # Remove periods
    extension <-
      gsub( ".", "", extension, fixed = TRUE )

    # Close 'If file extension is provided'
  } else {

    extension <- 'RData'

    # Close else for 'If file extension is provided'
  }

  # If label for project is not provided
  if ( is.null( project ) ) {

    # Attempt to find via environmental variable
    project <- Sys.getenv( "PROJECT" )

    # If no environmental variable
    if ( project == "" ) {

      # Attempt to find via config file
      project <- config::get('project')

      # If no config file
      if ( is.null( project ) ) {

        err_msg <- 'Project not specified nor present in project config.'

        stop( err_msg )

        # Close 'If no config file'
      }

      # Close 'If no environmental variable'
    }

    # Double-check project label
    checkmate::assert_character(project, pattern='^\\w+$')

    # Close 'If label for project is not provided'
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

  separator <- ''
  if ( grepl( '-', datetime, fixed = TRUE ) ) {
    separator <- '-'
  }
  if ( grepl( '_', datetime, fixed = TRUE ) ) {
    separator <- '_'
  }
  if ( grepl( '.', datetime, fixed = TRUE ) ) {
    separator <- '.'
  }

  time_str <- paste0( '\\d{2}', separator, '\\d{2}', separator, '\\d{2}$' )

  n_digits <- stringr::str_count( pattern = "\\d")
  if ( n_digits == 12 ) {
    time_str <- paste0( '\\d{2}', separator, '\\d{2}$' )
  }

  checkmate::assert_character(
    datetime,
    pattern = paste0(
      '^\\d{4}', separator, '\\d{2}', separator, '\\d{2}',
      '-',
      time_str
    )
  )

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
  } else if ( is.character(git) && nchar(git) > 0 ) {

    commit <- git

    # Close else for 'Extract commit number'
  }

  file_name <- paste0(
    paste(
      project,
      description,
      datetime,
      sep = '-'
    ),
    commit, '.', extension
  )

  return( fs::path_sanitize(file_name) )
}

#### 1.2) camr_filename ####

#' Generates a Standard File Name
#'
#' @param description A character string. Preferably UpperCamelCase
#'   with no spaces.
#' @param extension An optional character string. The file extension(s)
#'   with no leading `.`.
#' @param project An optional character string. Name of the project.
#'   Defaults to that stored in config.yml if present.
#' @param date An optional character string for the date. Preferably in
#'   YYYY_MM_DD format - defaults to the current date.
#' @param time An optional character string for the time. Preferably in
#'   HH_MM_SS format - defaults to the current time in UTC.
#' @param git Optional logical. The project is a git repository. Defaults
#'   to `TRUE`.
#'
#' @return String. The generated file name.
#'
#' @author Michael Pascale
#'
#' @export
#' @md

camr_filename <- function(
    description,
    extension = NULL,
    project = config::get('project'),
    date = format(Sys.time(), '%Y_%m_%d', tz = "UTC"),
    time = format(Sys.time(), '%H_%M_%S', tz = "UTC"),
    git = TRUE ) {

  commit <- NULL

  project %??% stop('Project not specified nor present in project config.')

  checkmate::assert_character(description, pattern='^\\w+$')
  checkmate::assert_character(extension, pattern='^\\w+$', null.ok=TRUE)
  checkmate::assert_character(project, pattern='^\\w+$', null.ok=TRUE)
  checkmate::assert_character(date, pattern='^\\d{4}_\\d{2}_\\d{2}$')
  checkmate::assert_character(time, pattern='^\\d{2}_\\d{2}_\\d{2}$')

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
  } else if ( is.character(git) && nchar(git) > 0 ) {

    commit <- git

    # Close else for 'Extract commit number'
  }

  filename <- stringr::str_c(
    purrr::compact(c(project, description, date, time, commit)),
    collapse='-'
  )

  extension <- ifelse(
    is.null(extension),
    '',
    stringr::str_c('.', extension)
  )

  filename <- paste0(filename, extension)

  fs::path_sanitize(filename)
}

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
    warning('Path does not exist and has not been added to the directory stack.')
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

