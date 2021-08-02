library(fs)
library(stringr)

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
    camr <<- list()
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
  saveRDS(camr, file='.cache')
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
#' @prefix Optional. A path to the desired data directory. Defaults to the
#' working directory.
#'
#' @author Michael Pascale
#'
#' @export
#' @md
camr_prefix <- function (prefix=getwd()) {
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
