# Functions for files and paths
# Written by...
#   Michael Pascale
#   Kevin Potter
# Maintained by...
#   Michael Pascale
#   Kevin Potter
# email: mppascale@mgh.harvard.edu
#        kpotter5@mgh.harvard.edu
# Please email us directly if you
# have any questions or comments
# Last updated 2021-11-08

# Table of contents
# 1) camr_filename
# 2) camr_pushd
# 3) camr_popd
# 4) camr_processed_data_to_csv
# 5) camr_path

#### 1) camr_filename ####
#' Generates a standard filename.
#'
#' @param description String. Preferably UpperCamelCase with no spaces.
#'
#' @param extension Optional string. The file extension(s) with no leading `.`.
#'
#' @param project Optional string. Name of the project. Defaults to that stored
#' in config.yml if present.
#'
#' @param date Optional string. Date in YYYYMMDD format. Defaults to the current
#' date.
#'
#' @param time Optional string. Time in HHMMSS format. Defaults to the current
#' time in UTC.
#'
#' @param git Optional logical. The project is a git repository. Defaults to T.
#'
#' @return String. The generated filename.
#'
#' @author Michael Pascale
#'
#' @export
#' @md

camr_filename <- function(
  description,
  extension=NULL,
  project=config::get('project') %??% NULL,
  date=format(Sys.time(), '%Y_%m_%d', tz="UTC"),
  time=format(Sys.time(), '%H_%M_%S', tz="UTC"),
  git=TRUE
) {
  commit <- NULL

  checkmate::assert_character(description, pattern='^\\w+$')
  checkmate::assert_character(extension, pattern='^\\w+$', null.ok=TRUE)
  checkmate::assert_character(project, pattern='^\\w+$', null.ok=TRUE)
  checkmate::assert_character(date, pattern='^\\d{4}_\\d{2}_\\d{2}$')
  checkmate::assert_character(time, pattern='^\\d{2}_\\d{2}_\\d{2}$')

  if (isTRUE(git) && git2r::in_repository()) {
    commit <- substr(git2r::last_commit()$sha, 0, 7)
    status <- git2r::status(untracked=FALSE)
    if (length(c(status$unstaged, status$staged)) != 0) {
      commit <- paste0(commit, 'm')
      warning( paste0(
        'Uncommited changes are present. Output files ',
        'will have a version ending in "m".'
        ) )
    }
  } else if (is.character(git) && nchar(git) > 0) {
    commit <- git
  }

  filename <- stringr::str_c(
    purrr::compact(c(project, description, date, time, commit)),
    collapse='-'
  )

  extension <- ifelse(is.null(extension), '', stringr::str_c('.', extension))

  filename <- paste0(filename, extension)

  fs::path_sanitize(filename)
}

#### 2) camr_pushd ####
#' Push a path to the directory stack.
#'
#' @param path Optional. The path to save. Defaults to the working directory.
#'
#' @author Michael Pascale
#'
#' @export
#' @md
camr_pushd <- function (path = getwd()) {

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

#### 3) camr_popd ####
#' Pop the last path from the directory stack and switch into it.
#'
#' @author Michael Pascale
#'
#' @export
#' @md
camr_popd <- function () {

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
#### 4) camr_processed_data_to_csv ####
#' Convert Processed Data to .csv Files
#'
#' Convenience function that takes a data frame with
#' processed data and converts it to a .csv file along
#' with an additional .csv file with a data dictionary.
#'
#' @param dtf A data frame with column attributes that
#'   include a list of class \code{dictionary_meta_data}.
#' @param proj A character string, the abbreviation for
#'   the study (e.g., 'MMJ', 'ARCHES', 'PCORI', etc.).
#' @param desc A character string, the description of
#'   the file (e.g., 'Processed_data').
#' @param cur_time The date and time the file was created,
#'   preferably in the format YYYY_MM_DD-HH_MM.
#'   If not provided, computed automatically via the base
#'   R function \code{\link[base]{Sys.time}}.
#' @param commit_num A character string, the 7-digit alphanumeric
#'   commit reference - if not provided, automatically computed.
#' @param file_path An optional character string, the absolute
#'   path to the folder to save the files to - otherwise, saves
#'   to the current working directory.
#'
#' @return Creates two .csv files, one for the processed data
#' and another for the associated data dictionary (created
#' automatically via the \code{\link{data_frame_from_dictionary_meta_data}}
#' function).
#'
#' @export

camr_processed_data_to_csv <- function( dtf,
                                        proj,
                                        desc,
                                        cur_time = '',
                                        commit_num = NULL,
                                        file_path = '' ) {

  if ( is.null( commit_num ) ) {
    commit_num <- system("git rev-parse --short HEAD", intern = TRUE)
  }

  if ( cur_time == '' ) {
    cur_time <- format(
      Sys.time(),
      '%Y_%m_%d-%H_%M'
    )
  }

  fname <- paste0(
    # Project name
    proj, '-',
    # File description
    desc, '-',
    # Date and time
    cur_time, '-',
    # Gitlab commit reference
    commit_num,
    # Extension
    '.csv'
  )

  out <- paste0(
    getwd(), '/', fname
  )
  if ( file_path != '' ) {
    fname <- paste0(
      file_path, '/',
      fname
    )
    out <- fname
  }

  write.csv(
    dtf,
    file = fname,
    row.names = FALSE,
    quote = TRUE
  )

  dd <- data_frame_from_dictionary_meta_data(
    dtf[ , meta( dtf ) ]
  )

  fname <- paste0(
    # Project name
    proj, '-',
    # File description
    stringr::str_to_sentence(
      paste0( 'Data_dictionary_for_', desc )
    )[1], '-',
    # Date and time
    cur_time, '-',
    # Gitlab commit reference
    commit_num,
    # Extension
    '.csv'
  )

  if ( file_path != '' ) {
    fname <- paste0(
      file_path, '/',
      fname
    )
  }

  write.csv(
    dd,
    file = fname,
    row.names = FALSE,
    quote = TRUE
  )

  return( out )
}

#### 5) camr_path ####
#' Construct a Path Given Base Paths in Project Configuration File
#'
#' `r lifecycle::badge("experimental")`
#'
#' Expects the project `config.yml` to contain a named list of base paths,
#' `(?<prefix>\w+)-root`, and a default base path name,
#' `(?<prefix>\w+)-root-default`.
#'
#' @param prefix 'input' or 'output' corresponding to the prefix of the
#' corresponding parameter in `config.yml`.
#'
#' @param root Optional. Name of the base path to lookup.
#'
#' @param path Optional. Path tail to concatenate at the end of the base path.
#'
#' @param real Optional. Whether to check if the generated path exists. Defaults
#' to TRUE.
#'
#' @param real Optional. Whether to create a directory at the generated path.
#' Cannot be used with `real`. Defaults to FALSE.
#'
#' @author Michael Pascale
#' @keywords internal
#'
#' @export
#' @md
camr_path <- function (prefix, root=NULL, path=NULL, real=TRUE, create=FALSE) {
  checkmate::assert_choice(prefix, c('input', 'output'))
  checkmate::assert_string(root, null.ok=TRUE)
  checkmate::assert_string(path, null.ok=TRUE)
  checkmate::assert_logical(real, len=1)
  checkmate::assert_logical(create, len=1)
  checkmate::assert_false(real && create)

  config_root_default <- paste(prefix, 'root', 'default', sep='-')
  config_root_list <- paste(prefix, 'root', sep='-')

  if (is.null(root)) {
    root <- config::get(config_root_default) %??%
      stop('A default root was not detected in the project configuration.')
  }

  base <- config::get(config_root_list)[[root]] %??%
    stop('The specified root was not detected in the project configuration.')

  fullpath <- fs::path_join(c(base, path))

  if (!real) {
    if (create)
      fs::dir_create(fullpath)

    return(fullpath)
  }

  fs::path_real(fullpath)
}

# FIXME: Add function to determine the latest version of a file given a directory
# of files following the camr_filename format.
camr_latest <- function (path) {
  stop('This is a stub.')
}
