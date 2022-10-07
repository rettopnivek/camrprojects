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
# Last updated: 2022-10-07

# Table of contents
# 1) Operators
#   1.1) `%??%`
#   1.2) `%p%`
#   1.3) Addition/subtraction assignment operators
#     1.3.1) `%+=%`
#     1.3.2) `%-=%`
#   1.4) `%not_in%`
#   1.5) `%pm%`
#   1.6) `%em%`
# 2) Constants
#   2.1) camr_const_datetime_regex
# 3) Utility functions
#   3.1) camr_commit_number
#   3.2) Dates and times as strings
#     3.2.1) camr_ymd
#     3.2.2) camr_ymd_hm
#     3.2.3) camr_ymd_hms
#     3.2.4) camr_hms

#### 1) Operators ####

#### 1.1) `%??%` ####
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

#### 1.2) `%p%` ####
#' Operator to Concatenate Two Strings
#'
#' The operator \code{%p%} combines character vectors.
#'
#' @param x,y \strong{R} objects that can be converted
#'   to character vectors.
#'
#' @return A character vector of the concatenated values.
#'
#' @details The call \code{ x %p% y } is equivalent to the
#' call \code{ paste0(x,y)}; see \code{\link[base]{paste}}
#' for more details.
#'
#' @author Kevin Potter
#'
#' @examples
#' # Combine strings via operator
#' "Hello" %p% " " %p% "world"
#'
#' # Vectorized
#' x <- "I like "
#' y <- c("cats", "dogs", "fish")
#' x %p% y
#'
#' @export

`%p%` <- function(
    x,
    y ) {

  return( paste0( x, y ) )
}

#### 1.3) Addition/subtraction assignment operators ####
#' Addition/Subtraction Assignment Operators
#'
#' The operator \code{%+=%} adds the right operand
#' to a variable and assigns the resulting value to
#' the variable. The operator \code{%-=%} subtracts
#' the right operand from a variable and assigns the
#' resulting value to the variable.
#'
#' @param x,y Numeric vectors.
#'
#' @return A numeric vector.
#'
#' @details Care must be taken with order of operations.
#' Because \code{%+=%} and \code{%-=%} are functions,
#' they and their arguments will be evaluated first,
#' followed by subsequent operations. Hence a call like
#' \code{ x %+=% 1/2 } will not result in an increment of
#' 0.5 to x. Instead, x will be first be
#' incremented by 1. Then the call \code{ x/2 } will be
#' run with no assignment of values.
#'
#' @references
#' https://stackoverflow.com/questions/5738831
#'
#' @name Assignment
#'
#' @author Kevin Potter
#'
#' @examples
#' # Simple assignment
#' x <- 1
#' x %+=% 1
#' x
#' y <- 1
#' y %-=% 2
#' y
#'
#' # Order of operations can be tricky
#' x <- 1
#' y <- 1
#' invisible(x %+=% y / 2)
#' x
#' # Above is equivalent to (x %+=% y)/2
#'
#' # Therefore embed multiple operations in parentheses
#' x <- 1
#' y <- 1
#' x %+=% (y / 2)
#' x
#'
#' # Vectorized
#' x <- 1:3
#' x %+=% 3
#' x
#' x <- 3:1
#' x %-=% 2:0
#' x
NULL

#### 1.3.1) `%+=%` ####

#' @rdname Assignment
#' @export

`%+=%` <- function(
    x,
    y ) {
  eval.parent(substitute(x <- x + y))
}

#### 1.3.2) `%-=%` ####

#' @rdname Assignment
#' @export

`%-=%` <- function(
    x,
    y ) {
  eval.parent(substitute(x <- x - y))
}

#### 1.4) `%not_in%` ####
#' Binary Operator for Non-Matches
#'
#' The binary operator \code{%not_in%} returns a logical
#' vector indicating if there is no matches for its left
#' operand.
#'
#' @param x A vector of values to be matched.
#' @param y A vector of values to be matched against.
#'
#' @return A logical vector equal to \code{TRUE} for all elements
#' in \code{x} that did not match any elements in \code{y}.
#'
#' @author Kevin Potter
#'
#' @examples
#' x <- c( "Cat", "Dog", "Bat" )
#' y <- c( "Cat", "Dog" )
#'
#' x %not_in% y
#'
#' @export

`%not_in%` <- function(
    x,
    y ) {

  return( !x %in% y )

}

#### 1.5) `%pm%` ####
#' Binary Operator for Partial Matches
#'
#' The binary operator \code{%pm%} returns a logical
#' vector indicating if there is a partial match against its left
#' operand (see [base::grepl]).
#'
#' @param x A vector of values to be matched.
#' @param y The value to be partially matched against.
#'
#' @return A logical vector equal to \code{TRUE} for all elements
#' in \code{x} that contain \code{y} in some form.
#'
#' @details This operator is robust against \code{NA}, returning
#' \code{TRUE} if \code{y} equals \code{NA} and \code{FALSE}
#' otherwise.
#'
#' @author Kevin Potter
#'
#' @examples
#' x <- c( "Cat", "Dog", "Bat" )
#' x %pm% "at"
#'
#' # Can match NA
#' x[2] <- NA
#' x %pm% "at"
#' x %pm% NA
#'
#' @export

`%pm%` <- function(
    x,
    y ) {

  if ( is.na(y) ) {

    return( is.na(x) )

  } else {
    return( grepl( y, x, fixed = TRUE ) )
  }

}

#### 1.6) `%em%` ####
#' Binary Operator for Exact Matches
#'
#' The binary operator \code{%em%} returns a logical
#' vector indicating if there is a exact match against its left
#' operand.
#'
#' @param x A vector of values to be matched.
#' @param y The value to be partially matched against.
#'
#' @return A logical vector equal to \code{TRUE} for all elements
#' in \code{x} that exactly match \code{y}.
#'
#' @details This operator is robust against \code{NA}, returning
#' \code{TRUE} if \code{y} equals \code{NA} and \code{FALSE}
#' otherwise.
#'
#' @author Kevin Potter
#'
#' @examples
#' x <- c( "Cat", "Dog", "Bat" )
#' x %em% "Cat"
#'
#' # Can match NA
#' x[2] <- NA
#' x %em% "Cat"
#' x %em% NA
#'
#' @export

`%em%` <- function(
    x,
    y ) {

  if ( is.na(y) ) {

    return( is.na(x) )

  } else {
    return( x %in% y )
  }

}

#### 2) Constants ####

#### 2.1) camr_const_datetime_regex ####
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

#### 3) Utility functions ####

#### 3.1) camr_commit_number ####
#' Determine Last Commit Number for Current Branch
#'
#' Function to extract the 7-digit alphanumeric code
#' for the most recent commit associated with the
#' current branch.
#'
#' @returns A character string, the alphanumeric
#' code for the current commit.
#'
#' @export

camr_commit_number <- function() {

  return( substr( git2r::last_commit()$sha, 0, 7 ) )

}

#### 3.2) Dates and times as strings ####
#' Functions to Extract Dates and Times as Strings
#'
#' Functions that extract the current date and time (in UTC)
#' and convert them to formatted strings.
#'
#' @param sep A character string, the separator between
#'   the year, month, day, and/or hour, minutes, and seconds.
#' @param split A character string, the separator between
#'   the date and the time.
#'
#' @return A character string.
#'
#' @name Datetimes_as_strings
#'
#' @author Kevin Potter
#'
#' @examples
#' # YYYY_MM_DD
#' camr_ymd()
#' # YYYY-MM-DD
#' camr_ymd('-')
#' # YYYYMMDD
#' camr_ymd('')
#'
#' # YYYY_MM_DD-HH_MM
#' camr_ymd_hm()
#' # YYYYMMDD-HHMM
#' camr_ymd_hm('')
#' # YYYYMMDD_HHMM
#' camr_ymd_hm('', '_')
#'
#' # YYYY_MM_DD-HH_MM_SS
#' camr_ymd_hms()
#' # YYYYMMDD-HHMMSS
#' camr_ymd_hms('')
#' # YYYYMMDD_HHMMSS
#' camr_ymd_hm('', '_')
#'
#' # HH_MM_SS
#' camr_hms()
#' # HH:MM:SS
#' camr_hms(':')
NULL

#### 3.2.1) camr_ymd ####
#' @rdname Datetimes_as_strings
#' @export

camr_ymd <- function(
    sep = '_' ) {

  # Check inputs
  checkmate::assert_string( sep )

  out <- date_time <- format(
    as.POSIXct( Sys.time(), tz = "UTC" ),
    paste0( "%Y", sep, "%m", sep, "%d" )
  )

  return( out )
}

#### 3.2.2) camr_ymd_hm ####
#' @rdname Datetimes_as_strings
#' @export

camr_ymd_hm <- function(
    sep = '_', split = '-' ) {

  # Check inputs
  checkmate::assert_string( sep )
  checkmate::assert_string( split )

  out <- date_time <- format(
    as.POSIXct( Sys.time(), tz = "UTC" ),
    paste0(
      "%Y", sep, "%m", sep, "%d",
      split,
      "%H", sep, "%M"
    )
  )

  return( out )
}

#### 3.2.3) camr_ymd_hms ####
#' @rdname Datetimes_as_strings
#' @export

camr_ymd_hms <- function(
    sep = '_', split = '-' ) {

  # Check inputs
  checkmate::assert_string( sep )
  checkmate::assert_string( split )

  out <- date_time <- format(
    as.POSIXct( Sys.time(), tz = "UTC" ),
    paste0(
      "%Y", sep, "%m", sep, "%d",
      split,
      "%H", sep, "%M", sep, "%S"
    )
  )

  return( out )
}

#### 3.2.4) camr_hms ####
#' @rdname Datetimes_as_strings
#' @export

camr_hms <- function(
    sep = '_' ) {

  # Check inputs
  checkmate::assert_string( sep )

  out <- date_time <- format(
    as.POSIXct( Sys.time(), tz = "UTC" ),
    paste0( "%H", sep, "%M", sep, "%S" )
  )

  return( out )
}


