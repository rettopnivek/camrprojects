# library(fs)
# library(stringr)

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
  if (exists('camr', envir=.GlobalEnv))
    warning('Global cache object `camr` already initialized.')

  if (file_exists('.cache'))
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
  if (file_exists('.cache'))
    file_delete('.cache')
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
  saveRDS(camr, file=path(camr$origin, '.cache'))
}

#' Construct and check the existence of paths given the prefix directory.
#'
#' @author Michael Pascale
#'
#' @export
#' @md
camr_paths <- function () {
  if (is.null(camr[['prefix']]))
    stop('Prefix not initialized.')

  folders = c('Documents', 'Figures', 'Reports')
  expanded = c()

  for (folder in folders) {
    fullpath <- path(camr$prefix, folder)

    if(!dir_exists(fullpath) && askYesNo(str_glue('Directory "{fullpath}" does not exist. Create?')))
      dir_create(fullpath)

    expanded <- append(expanded, fullpath)
  }

  camr$paths <<- setNames(as.list(expanded), folders)
  camr_save()
}

#' Set a directory prefix for data files.
#'
#' @param prefix Optional. A path to the desired data directory. Defaults to the
#' working directory.
#'
#' @author Michael Pascale
#'
#' @export
#' @md
camr_prefix <- function (prefix=getwd()) {

  if (!is_absolute_path(prefix) && getwd() != camr$origin)
    warning('Relative prefix set outside of the camr origin.')

  prefix <- path_real(prefix)

  if (prefix == getwd() && prefix != camr$origin)
    warning('Prefix set implicitly outside of the camr origin.')


  camr$prefix <<- prefix
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
  camr[name]
}

#' Push a path to the directory stack.
#'
#' @param path Optional. The path to save. Defaults to the working directory.
#'
#' @author Michael Pascale
#'
#' @export
#' @md
camr_pushd <- function (path = getwd()) {
  path = path_real(path)
  if (dir_exists(path)) {
    camr$stack <<- c(path, camr$stack)
    camr_save()
  } else {
    warning('Path does not exist and so has not been added to the stack.')
  }
}

#' Pop the last path from the directory stack and switch into it.
#'
#' @author Michael Pascale
#'
#' @export
#' @md
camr_popd <- function () {
  if (length(camr$stack)) {
    setwd(camr$stack[1])
    camr$stack <<- camr$stack[-1]
    camr_save()
  } else {
    warning('The directory stack is empty.')
  }
}

#' Wrapper around [keyring::key_get()] with ability to pull keyring name from
#' the project `config.yml` and sensible defaults given the project structure.
#'
#' @param service The path to save. Defaults to the working directory.
#'
#' @param username Optional. The username of the account to lookup. If NULL
#' (default), the name of the current project will be used as defined in the
#' `config.yml`. If empty string, [keyring::key_get()] returns the first key
#' in the keyring matching the service.
#'
#' @param keyring Optional. The name of the keyring on which the key is stored.
#' If NULL (default), the keyrind specified in `config.yml` will be used.
#'
#' @author Michael Pascale
#'
#' @export
#' @md
camr_key <- function (service, username=NULL, keyring=NULL) {
  checkmate::assert_character(service,  pattern='\\w+')
  checkmate::assert_character(username, null.ok=TRUE)
  checkmate::assert_character(keyring,  pattern='\\w+', null.ok=TRUE)

  if (is.null(username)) {
    username <- config::get('project')
  }

  if (is.null(keyring)) {
    keyring <- config::get('keyring')
  }

  keyring::key_get(
    service = service,
    username = username,
    keyring = keyring
  )
}

#' Regular expression to extract datetimes from strings.
#'
#' Expects dates dash or forward-slash delimited and optionally followed by a
#' time component. Use with [stringr::str_extract()] to pull out datetimes from
#' character vectors.
#'
#' @author Michael Pascale
#'
#' @export
#' @md
camr_const_datetime_regex <- '\\d{1,4}[-\\/]\\d{1,2}[-\\/]\\d{1,4}([\\s,]*\\d{1,2}:\\d{2}(:\\d{2})?\\s*([aApP][mM])?)?'



