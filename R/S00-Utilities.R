# Utilities
# Written by...
#   Michael Pascale
# Maintained by...
#   Michael Pascale
#   Kevin Potter
# Email:
#   mppascale@mgh.harvard.edu
#   kpotter5@mgh.harvard.edu
# Please email us directly if you
# have any questions or comments
# Last updated: 2022-09-08

# Table of contents
# 1) camr_key
# 2) camr_const_datetime_regex
# 3) `%??%`

#### 1) camr_key ####
#' Securely Fetch a Key with [keyring]
#'
#' Wrapper around [keyring::key_get()] with ability to pull keyring
#' name from the project `config.yml` and sensible defaults given the
#' project structure.
#'
#' @param service Name of the service with which to associate the key.
#' @param username Optional. The username of the account to lookup. If NULL
#' (default), the name of the current project will be used as defined in the
#' `config.yml`. If empty string, [keyring::key_get()] returns the first key
#' in the keyring matching the service.
#' @param keyring Optional. The name of the keyring on which the key is stored.
#' If NULL (default), the keyring specified in `config.yml` will be used.
#'
#' @author Michael Pascale
#'
#' @export
#' @md

camr_key <- function (
    service,
    username = NULL,
    keyring = NULL ) {

  checkmate::assert_character(service,  pattern = '^\\w+$')
  checkmate::assert_character(username, null.ok = TRUE)
  checkmate::assert_character(keyring,  pattern = '^\\w+$', null.ok = TRUE)

  username <- username %??% config::get('project') %??%
    stop( paste0(
      'Username unspecified and project name could not ',
      'be retrieved from config.yml.'
    ) )

  keyring <- keyring %??% config::get('keyring') %??%
    config::get('project') %??%
    stop( paste0(
      'Keyring unspecified and project name could not ',
      'be retrieved from config.yml.'
    ) )

  keyring::key_get(
    service = service,
    username = username,
    keyring = keyring
  )
}

#### 2) camr_const_datetime_regex ####
#' Regular Expression to Extract \code{datetimes} From Strings
#'
#' Expects dates dash or forward-slash delimited and optionally followed
#' by a time component. Use with [stringr::str_extract()] to pull out
#' \code{datetimes} from character vectors.
#'
#' @author Michael Pascale
#'
#' @export
#' @md

camr_const_datetime_regex <- paste0(
  '\\d{1,4}[-\\/]\\d{1,2}[-\\/]\\d{1,4}([\\s,]*',
  '\\d{1,2}:\\d{2}(:\\d{2})?\\s*([aApP][mM])?)?'
)

#### 3) `%??%` ####
#' Nullish Coalescing Operator
#'
#' Returns the RHS operand if the LHS operand is NULL or NA.
#'
#' @param lhs Any value.
#' @param rhs Any non-null and non-NA value.
#'
#' @return lhs or rhs
#'
#' @author Michael Pascale
#'
#' @examples
#' 1 %??% 2
#' NULL %??% 3
#' NA %??% NA %??% 4
#'
#' @export
#' @md

`%??%` <- function(
    lhs,
    rhs ) {

  if (is.null(lhs) || is.na(lhs))
    return(rhs)

  return( lhs )
}
