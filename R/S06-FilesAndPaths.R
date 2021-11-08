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
# 2) camr_processed_data_to_csv

#### 1) camr_filename ####
#' Generates a standard filename.
#'
#' @param description String. Preferably UpperCamelCase with no spaces.
#'
#' @param extension String. The file extension(s) with no leading `.`.
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
  description=stop('Must provide file description.'),
  extension=NULL,
  project=tryCatch(config::get('project'), error = function(cond){NULL}),
  date=format(Sys.time(), '%Y%m%d', tz="UTC"),
  time=format(Sys.time(), '%H%M%S', tz="UTC"),
  git=TRUE
) {
  commit <- NULL

  if (!stringr::str_detect(date, '\\d{8}') || !stringr::str_detect(date, '\\d{6}')) {
    warning('Invalid date/time provided.')
  }

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

#### 2) camr_processed_data_to_csv ####
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
    commit_num <- paste0(
      '-', substr(git2r::last_commit()$sha, 0, 7)
    )
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
    cur_time,
    '-',
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
    dtf
  )

  fname <- paste0(
    # Project name
    proj, '-',
    # File description
    stringr::str_to_sentence(
      paste0( 'Data_dictionary_for_', desc )
    ), '-',
    # Date and time
    cur_time,
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
