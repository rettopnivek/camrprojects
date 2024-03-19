# Experimental functions
# Written by...
#   Michael Pascale
# Maintained by...
#   Kevin Potter
# email:
#   kpotter5@mgh.harvard.edu
# Please email us directly if you
# have any questions or comments
# Last updated 2022-09-09

#### 1) camr_path ####
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
#' @param root Optional. Name of the base path to lookup.
#' @param path Optional. Path tail to concatenate at the end of the base path.
#' @param real Optional. Whether to check if the generated path exists.
#'   Defaults to `TRUE`.
#' @param real Optional. Whether to create a directory at the generated path.
#' Cannot be used with `real`. Defaults to `FALSE`.
#'
#' @author Michael Pascale
#'
#' @keywords internal
#'
#' @export
#' @md

camr_path <- function(
    prefix = c('input', 'output'),
    root = NULL,
    path = NULL,
    real = TRUE,
    create = FALSE ) {

  #.Deprecated('camr_build_path')

  prefix <- match.arg(prefix)

  # Check inputs
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
      stop( paste0(
        'Problem reading project config. More specifically, did not ',
        'find a default directory root.'
      ) )
  }

  base <- config::get(config_root_list)[[root]] %??%
    stop( paste0(
      'Problem reading project config. More specifically, did not ',
      'find the specified directory root.'
    ) )

  fullpath <- fs::path_join(c(base, path))

  if (!real) {
    if (create)
      fs::dir_create(fullpath)

    return(fullpath)
  }

  fs::path_real(fullpath)
}

#### 3) ... ####

camr_clinical_diagnoses <- function(
    diagnosis = "",
    type = "" ) {

  if ( diagnosis == 'CUD' ) {

    if ( type %in% c( 'Symptom counts', 'symptom counts',
                      'Symptoms', 'symptoms' ) ) {

      out$description <- paste0(
        'Symptom counts for the Cannabis Use Disorder Checklist'
      )

      out$units_of_measurement = "Symptom counts"

      out$values_and_labels = list(
        Values = c(0,2,4,6),
        Labels = c(
          "No disorder",
          "Mild disorder",
          "Moderate disorder",
          "Severe disorder"
        )
      )

      out$notes =
        "Reference: https://doi.org/10.1176/appi.books.9780890425596"

    }

  }

}

