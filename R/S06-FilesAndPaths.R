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
      warning('Uncommited changes are present. Output files will have a version ending in "m".')
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
