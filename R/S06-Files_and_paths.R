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
  project=config::get('project'),
  date=format(Sys.time(), '%Y_%m_%d', tz="UTC"),
  time=format(Sys.time(), '%H_%M_%S', tz="UTC"),
  git=TRUE
) {
  commit <- NULL

  project %??% stop('Project not specified nor present in project config.')

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
#'   include a list of class `dictionary_meta_data`.
#' @param proj A character string, the abbreviation for
#'   the study (e.g., 'MMJ', 'ARCHES', 'PCORI', etc.).
#' @param desc A character string, the description of
#'   the file (e.g., 'Processed_data').
#' @param cur_time The date and time the file was created,
#'   preferably in the format YYYY_MM_DD-HH_MM.
#'   If not provided, computed automatically via the base
#'   R function [base::Sys.time()].
#' @param commit_num A character string, the 7-digit alphanumeric
#'   commit reference - if not provided, automatically computed.
#' @param file_path An optional character string, the absolute
#'   path to the folder to save the files to - otherwise, saves
#'   to the current working directory.
#'
#' @return Creates two .csv files, one for the processed data
#' and another for the associated data dictionary (created
#' automatically via the [data_frame_from_dictionary_meta_data()]
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
      stop('Problem reading project config. More specifically, did not find a default directory root.')
  }

  base <- config::get(config_root_list)[[root]] %??%
    stop('Problem reading project config. More specifically, did not find the specified directory root.')

  fullpath <- fs::path_join(c(base, path))

  if (!real) {
    if (create)
      fs::dir_create(fullpath)

    return(fullpath)
  }

  fs::path_real(fullpath)
}


#### 6) camr_latest ####
#' Fetch the latest version a file given a description.
#'
#' `r lifecycle::badge("experimental")`
#'
#' Expects files named in the format output by `camr_filename`.
#'
#' @param prefix Passed to `camr_path`.
#'
#' @param root Optional. Passed to `camr_path`.
#'
#' @param path Optional. Base path of the file to be found.
#'
#' @param project Optional. Whether to expect the project name to prefix the
#' file description field. Defaults to TRUE.
#'
#' @param recurse Optional. Whether to search recursively. Defaults to TRUE.
#'
#' @author Michael Pascale
#' @keywords internal
#'
#' @export
#' @md
camr_latest <- function (prefix, description, extension, root=NULL, path=NULL, project=TRUE, recurse=TRUE) {
  checkmate::assert_string(description)
  checkmate::assert_string(path, null.ok=TRUE)
  checkmate::assert_logical(project, len=1)
  checkmate::assert_logical(recurse, len=1)

  chr_path_directory <- camr_path(prefix, root, path)

  chr_project = ifelse(
    project,
    config::get('project') %??% stop('Problem reading project config. More specifically, did not find the project name.'),
    '.*?'
  )

  fs::dir_ls(
    chr_path_directory,
    recurse=recurse,
    regexp=paste0('[/\\]', chr_project, '-', description, '-[^/\\]*\\.', extension, '$'),
    ignore.case=TRUE
  ) %>% unclass() %>% unname() -> vchr_paths_matching

  vint_idx <- order(vchr_paths_matching %>% str_remove("^.*/"), decreasing = TRUE)
  vchr_paths_matching[vint_idx][1]
}










#### 1.1) camr_match_to_files ####
#' Checks for Partial Matches Between a String and a Set of Files
#'
#' Checks if a file is present in the specified directory.
#' Can check either for regular files or files using MGH-CAM's
#' standardized file naming template:
#' TXX-Description-MM_DD_YYYY-HH_MM.ext.
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
#' @param ... Additional arguments passed to the
#'   [`dir()`][base::list.files] function.
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

camr_match_to_files <- function(
    string,
    output = 'Logical',
    std_name = FALSE,
    ... ) {

  # All files and folders present
  # in working directory
  all_files <- dir( ... )

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

#### 1.2) camr_file_paths ####
#' Returns File/Folder Paths
#'
#' Returns an absolute file or folder path.
#' Folder paths can be extracted from a
#' pre-specified environmental variable.
#'
#' @param file_name A character string, a
#'   partial match to the file of interest.
#' @param env_var A character string, the name for
#'   the environment variable.
#' @param path A character string, a relative or
#'   absolute path to a folder.
#' @param latest Logical; if `TRUE` returns only
#'   the latest version of a file whose name contains
#'   a date.
#'
#' @return A character string.
#'
#' @author Kevin Potter
#'
#' @export

camr_file_paths <- function(
    file_name = NULL,
    env_var = NULL,
    path = NULL,
    latest = TRUE ) {

  if ( !is.null( env_var ) ) {
    path = Sys.getenv( env_var )
    if ( path == '' ) {
      stop( 'Environmental variable for path not found' )
    }
  }

  if ( is.null( path ) ) {
    path <- getwd()
  }

  if ( !is.null( file_name ) ) {

    # All files and folders present
    # in working directory
    all_files <- dir( path = path )

    # Determine if file name is present
    # in list of files/folders
    check <- grepl(file_name, all_files, fixed = T)

    if ( any(check) ) {
      x <- all_files[check]
    } else {
      x <- NULL
    }

    if ( length( x ) == 0 ) {
      stop( 'File not found' )
    }

    if ( latest ) {
      return( paste0( path, '/', sort( x )[ length(x) ] ))
    } else {
      return( paste0( path, '/', sort( x ) ))
    }

  } else {
    return( path )
  }

}


