# Utilities
# Written by...
#   Michael Pascale
#   Kevin Potter
# Maintained by...
#   Kevin Potter
# Email:
#   kpotter5@mgh.harvard.edu
# Please email us directly if you
# have any questions or comments
# Last updated: 2023-11-15

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
#   1.7) `%rows%`
# 2) Constants
#   2.1) camr_const_datetime_regex
# 3) Utility functions
#   3.1) camr_commit_number
#   3.2) Dates and times as strings
#     3.2.1) camr_ymd
#     3.2.2) camr_ymd_hm
#     3.2.3) camr_ymd_hms
#     3.2.4) camr_hms
#   3.3) camr_pass
#   3.4) Naming convention utilities
#     3.4.1) camr_list_idx
#     3.4.2) camr_join_on_idx
#   3.5) camr_write_csv
#   3.6) camr_assert

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

#### 1.7) `%rows%` ####
#' Binary Operator for Subsetting Rows While Preserving Attributes
#'
#' The binary operator \code{%rows%} returns the subset of specified
#' rows for a data frame without removing column attributes.
#'
#' @param x A data frame.
#' @param y An integer vector, logical vector, or character vector
#'   specifying the rows in \code{x} to keep.
#'
#' @return A data frame, a subset of \code{x}.
#'
#' @author Kevin Potter
#'
#' @examples
#' dtf <- data.frame(
#'   X1 = 1:4,
#'   X2 = LETTERS[1:4],
#'   X3 = c( TRUE, TRUE, FALSE, FALSE )
#' )
#' attributes( dtf$X1 ) <- list( Example_attr = "Integer" )
#' attributes( dtf$X2 ) <- list( Example_attr = "Character" )
#' attributes( dtf$X3 ) <- list( Example_attr = "Logical" )
#'
#' # Each column has an attribute
#' str( dtf )
#'
#' # Normal indexing removes attributes
#' str( dtf[1:2,] )
#'
#' # Can use operator to avoid losing attributes
#' str( dtf %rows% 1:2 )
#'
#' @export

`%rows%` <- function(
    x,
    y ) {

  out <- x[y,]

  K <- ncol( x )

  for ( k in 1:K ) {
    attributes( out[[k]] ) <- attributes( x[[k]] )
  }

  return( out )
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

#### 3.3) camr_pass ####
#' Do Nothing
#'
#' This function provides functionality similar to Python's `pass` keyword. It
#' will ignore all input and return NULL.
#'
#' @param .lgl_pass_quetly Optional. If true a warning message is displayed.
#' @param .chr_pass_message Optional. Message to display if warnings enabled.
#'
#' @author Michael Pascale
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' tryCatch({
#'  # non-critical, ignorable error.
#' }, error=camr_pass)
#'
#' fn_example <- function(x, y) {
#'  camr_pass(.lgl_pass_quietly=FALSE)
#' }
#' }
camr_pass <- function(
    ...,
    .lgl_pass_quietly=TRUE,
    .chr_pass_message='(camr::camr_pass) not implemented.'
  ) {

  if (!.lgl_pass_quietly)
    warning(.chr_pass_message)

  NULL
}

#### 3.4) Naming Convention Utilities ####

##### 3.4.1) camr_list_idx #####
#' List CAM index columns of a dataframe.
#'
#' By convention, columns beginning with the prefix IDX are index columns.
#'
#' @param df_x A dataframe, with columns following the CAM naming convention.
#'
#' @return A character vector of the names in df_x prfixed by IDX.
#' @export

camr_list_idx <- function(df_x) {
  df_x |> colnames() |> keep(str_detect, '^IDX\\.')
}

##### 3.4.2) camr_join_on_idx #####
#' Safely left-join two dataframes on CAM index columns.
#'
#' `r lifecycle::badge("experimental")`
#'
#' By convention, columns beginning with the prefix IDX are index columns. This
#' function will perform a `left_join`, mapping the IDX columns of `df_y` onto
#' those matching in `df_x`. In the returned dataframe, the IDX columns will be
#' in the leftmost position.
#'
#' @param df_x A dataframe, with columns following the CAM naming convention.
#' @param df_y A dataframe, with columns following the CAM naming convention.
#'
#' @return left_join(df_x, df_y, by=camr_list_idx(df_y))
#' @keywords internal
#' @export
#'
#' @examples
#' \dontrun{
#' df_scans |>
#'   camr_join_on_idx(
#'     df_demographics)
#'   )
#' }
camr_join_on_idx <- function (df_x, df_y) {
  vchr_indicies <- camr_list_idx(df_y)

  if(!all(vchr_indicies %in% colnames(df_x))) {
    stop(
      paste0(
        'camr_join_on_idx expects that the ',
        'IDX columns of df_y will be present in df_x\n',
        '    df_y has indicies: ', paste(vchr_indicies, collapse=', ')
      )
    )
  }

  left_join(df_x, df_y, by=vchr_indicies) |> relocate(starts_with('IDX'))
}


##### 3.5) camr_write_csv #####
#' Write a CSV file with the standard options.
#'
#' `r lifecycle::badge("experimental")`
#'
#' The provided filename will be expanded with `camr_name_file`.
#'
#' @param df_x A dataframe to be written.
#' @param chr_filename A dataframe, with columns
#'   following the CAM naming convention.
#' @param ... Additional arguments to write.csv(),
#'
#' @keywords internal
#' @export
camr_write_csv <- function(
    df_x,
    chr_filename,
    row.names=FALSE,
    na='',
    lgl_literal=FALSE ) {

  assert_data_frame(df_x)
  assert_string(chr_filename, pattern='\\.csv$')
  assert_logical(lgl_literal, len=1, any.missing=FALSE)

  if (!lgl_literal)
    chr_filename <- camr_name_file(chr_filename)


  write.csv(
    df_x, chr_filename, row.names=row.names,
    na=na, eol='\r\n', fileEncoding='UTF-8'
  )
  message('Wrote ', deparse(substitute(df_x)), ' to ', chr_filename, '.')
}

##### 3.6) camr_assert #####
#' Pipe-friendly assertions
#'
#' `r lifecycle::badge("experimental")`
#'
#' The purpose of this function is to enable pipeable assertions on dataframe
#' columns without the need to break the pipe-chain or to use inline functions.
#'
#' For example...
#' `df_data |> (function (df) {if (!all(str_detect(df$x, '^a'))) stop('Elements of col x must start with "a".'); df})() |> ...`
#'
#' Can be written more readably...
#' `df_data |> camr_assert(all(str_detect(x, '^a')), 'Elements of col x must start with "a".') |> ...`
#'
#' If the assertion passes, the object being tested will be passed through unchanged.
#'
#' @param any_x An object, preferably a dataframe, to pass through.
#' @param lgl_expr A logical expression to evaluate within any_x.
#' @param chr_message A message to be displayed by stop() if lgl_expr is false.
#'
#' @return any_x
#'
#' @keywords internal
#' @export
#'
#' @examples
#' iris |>
#'   camr_assert(
#'     any(Species =='versicolor'),
#'     'Dataset contains no irises of species versicolor'
#'   ) |>
#'   summarize(
#'    m_length = mean(Petal.Length)
#'   )
camr_assert <- function (any_x, lgl_expr, chr_message) {

  if (!assert_logical(
    eval_tidy(enquo(lgl_expr), any_x),
    len=1,
    any.missing=FALSE
  )) stop(chr_message)

  any_x
}
