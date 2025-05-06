# Functions for files and paths
# Written by...
#   Michael Pascale
#   Kevin Potter
#   Bryn Evohr
# Maintained by...
#   Kevin Potter
# email:
#   kpotter5@mgh.harvard.edu
# Please email us directly if you
# have any questions or comments
# Last updated 2025-05-05

# Table of contents
# 1) File paths and names
#   1.1) camr_file_name
#   1.2) camr_filename
#   2.2) camr_file_path
#   2.3) camr_find_file
# 2) Folders and directories
#   2.1) camr_pushd
#   2.2) camr_popd
# 3) camr_source_scripts
# 4) camr_load_from_RData
# 5) camr_copy_from_source

#### 1) File paths and names ####

#### 1.1) camr_name_file ####
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

camr_name_file <- function(
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
    project <- Sys.getenv(
      'CAMR_PROJECT', NA
    ) %??% tryCatch(config::get('project'), error=camr_pass) %??% ''

    # Close 'If label for project is not provided'
  }

  if (project != '') {
    # Double-check project label
    checkmate::assert_character(project, pattern='^\\w+$')

    project <- paste0(project, '-')
  } else {
    warning(
      'Project name not specified nor present in project-level configuration.'
    )
  }

  # If a date and time is not provided
  if ( is.null(datetime) ) {

    # If no date is provided
    if ( is.null(date) ) {

      date <- camrprojects::camr_ymd()

      # Close 'If no date is provided'
    } else if (is.Date(date)) {

      if (is.na(date)) {
        stop('Date cannot be missing, if provided.')
      }

      date <- date |> format('%Y_%m_%d')

      # Close first else for 'If no date is provided'
    } else {

      date <- lubridate::ymd(date, quiet=TRUE) |> format('%Y_%m_%d')

      if (is.na(date)) {
        stop('Invalid date format. Must by YYYY-MM-DD.')
      }

      # Close final else for 'If no date is provided'
    }

    # If no time is provided
    if ( is.null(time) ) {

      time <- camrprojects::camr_hms()

      # Close 'If no time is provided'
    } else {

      time <- time |>
        str_split('[^\\d]+', simplify=TRUE) |>
        str_pad(2, pad='0') |>
        paste(collapse='_') |>
        str_extract('^(0[0-9]|1[0-9]|2[0-3])_([0-5][0-9])(_([0-5][0-9]))?$')

      if (is.na(time)) {
        stop('Invalid time format. Must be HH:MM or HH:MM:SS with ',
             'each component delimited in some way.')
      }
      # Close else for 'If no date is provided'
    }

    # Create date and time for file name
    datetime <- paste0( date, '-', time )

    # Close 'If a date and time is not provided'
  } else {

    # If provided datetime is of type POSIXt
    if (is.POSIXt(datetime)) {

      if (is.na(datetime)) {
        stop('Datetime cannot be missing, if provided.')
      }

      datetime <- datetime |> format('%Y_%m_%d-%H_%M_%S')
      # Close 'If provided datetime is of type POSIXt'
    } else {
      datetime <- lubridate::ymd_hms(datetime, truncated=1, quiet=TRUE)

      if (is.na(datetime)) {
        stop('Invalid datetime format. Must be YYYY-MM-DD HH:MM or HH:MM:SS.')
      }

      datetime <- datetime |> format('%Y_%m_%d-%H_%M_%S')
      # Close else for 'If provided datetime is of type POSIXt'
    }

    # Close else for 'If a date and time is not provided'.
  }

  # Extract commit number
  if ( isTRUE(git) && git2r::in_repository() ) {

    commit <- substr(git2r::last_commit()$sha, 0, 7)
    status <- git2r::status(untracked = FALSE)

    # Check for uncommitted changes
    if ( length( c(status$unstaged, status$staged) ) != 0 ) {

      commit <- paste0(commit, 'm')
      warning( paste0(
        'Uncommited changes are present in this git repository. ',
        'Filename will have a commit hash with "m" appended.'
      ) )

      # Close 'Check for uncommitted changes'
    }

    # Close 'Extract commit number'
  } else if ( is.character(git) ) {

    commit <- git

    # Close else for 'Extract commit number'
  } else {
    commit <- ''
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

#### 1.2) camr_file_name ####

#' @rdname camr_name_file
#' @export

camr_file_name <- function(...) {
  .Deprecated('camr_name_file')
  camr_name_file(...)
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

#### 2.2) camr_find_file ####
#' Find Path to Most Recent File
#'
#' Function to find the absolute path to the most
#' up-to-date version of a file or directory given a
#' partial match to its name and the file location.
#'
#' @param nm_desc A character string, the file name
#'   or part of the file name that can uniquely identify
#'   it from other files.
#' @param chr_path A character string, the name of the
#'   file path to search in.
#' @param chr_project An optional character string, the
#'   name of the project.
#' @param chr_date An optional character string, the
#'   date of the file.
#' @param chr_commit An optional character string, the
#'   commit number of the file.
#' @param chr_date_tolerance An optional character string,
#'   margin of error for the date of the file.
#'   ('s', 'm', 'h', 'd').
#' @param chr_type An optional character string,
#'   file type to find.
#'   ('file', 'directory').
#' @param lgl_recurse Logical; if TRUE, the function
#'   will recurse fully.
#'
#' @import fs
#'
#' @author Michael Pascale, Bryn Evohr
#'
#' @return A character string, an absolute path to a file or directory.
#'
#' @export
#' @md

camr_find_file <- function(
    nm_desc,
    chr_path,
    chr_project=NULL,
    chr_date=NULL,
    chr_commit=NULL,
    chr_date_tolerance='s',
    chr_type='file',
    lgl_recurse=TRUE
) {

  chr_desc <- deparse(substitute(nm_desc))
  if (stringr::str_detect(chr_desc, '"')) {chr_desc <- nm_desc}

  assert_string(nm_desc, pattern='^\\w+(\\.\\w{1,5})?$')
  assert_string(chr_path)
  assert_string(chr_project, pattern='^\\w+$', null.ok=TRUE)
  assert_string(chr_date, pattern='^(\\d|\\.|-|_)+$', null.ok=TRUE)
  assert_choice(chr_date_tolerance, c('s', 'm', 'h', 'd'))
  assert_choice(chr_type, c('file', 'directory'))
  assert_logical(lgl_recurse, len=1, any.missing=FALSE)

  chr_name <- nm_desc |> str_extract('^\\w+')
  chr_ext <- nm_desc |> str_extract('\\.\\w+$')

  dtm_date <- ymd_hms(chr_date, quiet=TRUE, tz='UTC', truncated=3)

  if (chr_type == 'file') {

    dir_ls(
      path=chr_path,
      type='file',
      recurse=lgl_recurse,
      regexp=ifelse(is.na(chr_ext), '', str_glue('\\{chr_ext}$'))
    ) |>
      map_dfr(~ data.frame(
        filename=basename(.),
        fullpath=.
      )) |>
      extract(
        filename,
        c('project', 'description', 'timestamp', NA, NA, 'commit', 'extension'),
        '^(\\w+)-(\\w+)-(\\d{4}[\\.\\-_]?(\\d{2}[\\.\\-_]?){2,5})(-([a-z\\d]{8,9}))?(\\.\\w{1,5})?$',
        FALSE,
        TRUE
      ) |>
      drop_na(project, description, timestamp) |>
      mutate(
        project = str_to_lower(project),
        datetime = ymd_hms(timestamp, quiet=TRUE, tz='UTC', truncated=3),
        datediff = if (is.null(chr_date)) NA else difftime(datetime, dtm_date, units=chr_date_tolerance) |> abs() |> floor()
      ) |>
      filter(
        str_to_lower(description) == str_to_lower(chr_name),
        if (is.null(chr_project)) TRUE else str_to_lower(project) == str_to_lower(chr_project),
        if (is.null(chr_date)) TRUE else (startsWith(timestamp, chr_date) | datediff < 1),
        if (is.null(chr_commit)) TRUE else commit == chr_commit
      ) |>
      arrange(datetime) ->
      df_files_matching

    if (nrow(df_files_matching) > 1) {
      warning(
        'Matching file is ambiguous. Using the latest:\n    ',
        last(df_files_matching$filename)
      )
    } else if (nrow(df_files_matching) < 1) {
      stop('No files matching ', chr_name, ' in ', chr_path)
    }

    last(df_files_matching$fullpath)

  } else if (chr_type == 'directory') {

    dir_ls(
      path=chr_path,
      type='directory',
      recurse=lgl_recurse,
      regexp=ifelse(is.na(chr_ext), '', str_glue('\\{chr_ext}$'))
    ) |>
      map_dfr(~ data.frame(
        filename=basename(.),
        fullpath=.
      )) |>
      mutate(
        timestamp = str_extract(filename, '\\d{4}[\\.\\-_]?(\\d{2}[\\.\\-_]?){2,5}')
      ) |>
      drop_na(timestamp) |>
      mutate(
        datetime = ymd_hms(timestamp, quiet=TRUE, tz='UTC', truncated=3),
        datediff = if (is.null(chr_date)) NA else difftime(datetime, dtm_date, units=chr_date_tolerance) |> abs() |> floor()
      ) |>
      filter(
        if (is.null(chr_date)) TRUE else (startsWith(timestamp, chr_date) | datediff < 1),
      ) |>
      arrange(datetime) ->
      df_directories_matching

    if (nrow(df_directories_matching) > 1) {
      warning(
        'Matching directory is ambiguous. Using the latest:\n    ',
        last(df_directories_matching$filename)
      )
    } else if (nrow(df_directories_matching) < 1) {
      stop('No directory matching ', chr_name, ' in ', chr_path)
    }

    last(df_directories_matching$fullpath)

  }

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

#' Build a File/Directory Path
#' Optionally create a directory or apply `camr_name_file` to
#' the generated file path. Use shorthand "root" directories as
#' specified in the project `config.yml`.
#'
#' @param ...         Arguments to be passed to file.path.
#' @param chr_root    Directory named in `local-directories` of the
#'   project's `config.yml`.
#' @param lgl_verify  Verify that the path already exists?
#' @param lgl_create  Create a directory at the path if it does not exist?
#' @param lgl_name    Apply `camr_name_file` on the last argument?
#'
#' @return (Character) A vector of paths.
#' @export
#'
#' @examples
#' \dontrun{
#' write.csv(
#'   iris, camr_build_path('data', 'Example.csv', lgl_name=T, lgl_verify=F)
#' )
#'
#' camr_build_path(root='dropbox', 'fNIRS', 'Source')
#' }
camr_build_path <- function (
    ...,
    root=NULL,
    lgl_verify=TRUE,
    lgl_create=FALSE,
    lgl_name=FALSE) {

  # Validate inputs.
  checkmate::assert_string(root, null.ok=TRUE)
  checkmate::assert_logical(lgl_verify, len=1)
  checkmate::assert_logical(lgl_create, len=1)
  checkmate::assert_logical(lgl_name, len=1)

  if (lgl_verify && lgl_create)
    stop('Cannot specify both lgl_verify and lgl_create.')

  lst_dots <- list(...)
  checkmate::assert_list(lst_dots, names='unnamed', types='character')

  # Check the lengths of the dots arguments to avoid recycling issues.
  vint_lens <- lengths(lst_dots)

  if (length(lst_dots) > 1 &&
      any(vint_lens != 1 & vint_lens != max(vint_lens)))
    stop('Arguments must be of equal lengths (or of length 1).')

  # If a directory is specified, look it up in the project-level config.
  if (is.null(root)) {
    root <- getwd()
  } else {
    root <- config::get('local-directories')[[root]]
    if (is.null(root))
      stop('Could not read local directory list from project-level config.')
  }

  # Optionally call camr_name_file on the last argument.
  if (lgl_name) {
    lst_dots[[length(lst_dots)]] <-
      camr_name_file(lst_dots[[length(lst_dots)]])
  }

  # Build the complete path.
  vchr_paths <-
    normalizePath(do.call(file.path, c(root, lst_dots)), mustWork=FALSE)

  # Verify that the path exists.
  if (lgl_verify) {
    for (path in vchr_paths) {
      if (lgl_name)
        path <- dirname(path)
      if (!file.exists(path))
        stop('Path does not exist: ', path, '.')
    }
  }

  # Create a new directory.
  if (lgl_create) {
    for (path in vchr_paths)
      dir.create(path)
  }

  vchr_paths
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
#' The resulting string is then truncated to
#' [255 bytes in length](https://en.wikipedia.org/wiki/Comparison_of_file_systems#Limits)
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
camr_sanitize_path <- function (
    chr_filename,
    chr_replacement = "") {

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

#### 5) camr_copy_from_source ####
#' Copy Files From Source Folder
#'
#' Function to copy files from a subfolder
#' in a source folder to a new subfolder in
#' a user-defined source folder in the current
#' directory.
#'
#' @param chr_source_path A character string,
#'   the absolute path to the source folder.
#' @param chr_destination_path A character string,
#'   the path to the folder to which files should
#'   be copied - if blank, uses the current working
#'   directory.
#' @param chr_source_subfolder An optional character
#'   string, the full or partial name of a subfolder
#'   in source location with the files to copy.
#' @param chr_environment An optional character
#'   string, the environmental variable with
#'   the path to the source folder.
#' @param chr_config An optional character string,
#'   the input for [config::get()] to extract the
#'   path to the source folder from a config.yml file.
#'
#' @returns As a side effect copies files to a
#' to the specified destination folder.
#'
#' @export

camr_copy_from_source <- function(
    chr_source_path = '',
    chr_destination_path = '',
    chr_source_subfolder = '',
    chr_environment = 'FOLDER_SOURCE',
    chr_config = 'input-root-default' ) {

  # Path to source folder from environmental variable
  if ( chr_source_path == '' ) {

    chr_source_path <- Sys.getenv(
      chr_environment
    )

    # Close 'Path to source folder from environmental variable'
  }

  # Path to source folder from config variable
  if ( chr_source_path == '' ) {

    chr_source_path <- config::get(
      chr_config
    )

    # Close 'Path to source folder from config variable'
  }

  # If a subfolder is specified
  if ( chr_source_subfolder != '' ) {

    chr_subfolders <- dir(
      path = chr_source_path
    )
    chr_source_subfolder <- chr_subfolders[
      grepl(
        chr_source_subfolder,
        chr_subfolders,
        fixed = TRUE
      )
    ][1]

    chr_source_path <- paste0(
      chr_source_path, '/', chr_source_subfolder
    )

    # Close 'If a subfolder is specified'
  }

  # List files in source folder
  chr_files_and_folders_to_copy <- list.files(
    path = chr_source_path,
    recursive = TRUE,
    include.dirs = TRUE
  )

  # By default use current working directory
  if ( chr_destination_path == '' ) {

    chr_destination_path <- getwd()

    # Close 'By default use current working directory'
  }

  chr_new_path_for_copied_files_and_folders <- paste0(
    chr_destination_path, '/', chr_files_and_folders_to_copy
  )

  # Check if a subfolder is present
  lgc_is_directory <- !grepl(
    '.', chr_files_and_folders_to_copy, fixed = TRUE
  )

  # If subfolder is present
  if ( any( lgc_is_directory ) ) {

    # Create subfolder in new location
    lgc_success <- sapply(
      new_path_for_copied_files_and_folders[lgc_is_directory],
      function( chr_folder ) {
        dir.create(
          chr_folder,
          recursive = TRUE
        )
      }
    )

    # Close 'If subfolder is present'
  }

  # Copy files to local machine
  lgc_success <- file.copy(
    from = paste0(
      chr_source_path, '/',
      chr_files_and_folders_to_copy[!lgc_is_directory]
    ),
    to = chr_new_path_for_copied_files_and_folders[!lgc_is_directory]
  )

  # Error and warning messages
  if ( any(lgc_success) ) {

    if ( !all(lgc_success) ) {
      warning( 'Some files or folders were not copied' )
    }

    # Close 'Error and warning messages'
  } else {

    stop( 'Failed to copy files' )

    # Close else for 'Error and warning messages'
  }

}

