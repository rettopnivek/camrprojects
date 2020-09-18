#' Extract Unique Values from Data Frames or Lists
#'
#' A function that will search over a subset of rows in a data frame
#' (or a list structured like a data frame) and extract a unique
#' value after excluding missing or irrelevant data.
#'
#' @param x A data frame or a list of variables with matching lengths.
#' @param variable_names A vector of variables names to loop over when
#'   attempting to isolate the unique value.
#' @param entries A logical vector, \code{TRUE} for rows to search for
#'   the unique value and \code{FALSE} otherwise.
#' @param default The default output to return if no unique values can
#'   be found.
#' @param missing A vector of values to treat as missing and exclude
#'   when searching for the unique value.
#' @param reference An optional character string giving the variable
#'   name in \code{x} to use when printing warnings in the case of
#'   multiple values being found. Defaults to the first variable
#'   in \code{x}.
#' @param check_for_multiple Logical; if \code{TRUE} will check if
#'   more than one value was found in the subset of rows to consider
#'   and display a warning message with details if this occurs.
#'
#' @author Kevin Potter
#'
#' @return
#'
#' @examples
#' # Create example data frame
#' # Create three 'ID' levels
#' df <- data.frame( ID = rep( 1:3, each = 3 ) )
#' # Create an age variable, but assume entered
#' # over two different variables with several
#' # missing values
#' df$Age_session_1 <- NA; df$Age_session_1[c(1,4)] = c(25,20)
#' # Create a status variable, but assume multiple
#' # indicators for missing data
#' df$Status <- c( 'No entry', 'Good', 'Poor',
#'                 'No entry', 'N/A', 'Good',
#'                 'No entry', 'Poor', 'N/A' )
#'
#' # - Extract unique value for age for two different
#' #   variables per levels of 'ID', with user-defined
#' #   value for missing cases
#' # Loop over levels for 'ID'
#' for ( id in 1:3 ) {
#'   val <- extract_unique_value(
#'     x = df,
#'     variable_names = c( 'Age_session_1', 'Age_session_2' ),
#'     entries = df$ID == id,
#'     # Specify default value when no values found
#'     default = 'Missing'
#'   )
#'   # Display results
#'   cat( paste0( 'ID: ', id, '; Age = ', val, '\n' ) )
#' }
#'
#' # - Extract unique values for status given
#' #   multiple types of missing data indicators
#' # - Also display warning for case where there
#' #   were multiple non-missing values
#' # Traverse levels for 'ID' via the 'sapply' function
#' res <- sapply( 1:3, function( id ) {
#'   val <- extract_unique_value(
#'     x = df,
#'     variable_names = 'Status',
#'     entries = df$ID == id,
#'     # Deal with multiple indicators for missing data
#'     missing = c( 'No entry', 'N/A' ),
#'     # Specify reference for warnings
#'     reference = 'ID'
#'   )
#'   # Save results
#'   paste0( 'ID: ', id, '; Status = ', val, '\n' )
#' } )
#' # Display results
#' cat( res )
#'
#' @export

extract_unique_value <- function( x,
                                  variable_names,
                                  entries,
                                  default = "",
                                  missing = c( "" ),
                                  reference = NULL,
                                  check_for_multiple = TRUE ) {

  # Number of variables/columns to loop over
  K <- length( variable_names )

  # Initialize output
  output <- default
  current_value <- default

  # Specify reference variable if there are issues
  # (i.e., subject identifier)
  if ( is.null( reference ) ) {
    reference = names( x )[1]
  }

  # Loop over variables
  for ( k in 1:K ) {

    # Check for missing data over range of entries for subject
    no_missing <-
      entries &
      !is.na( x[[ variable_names[k] ]] ) &
      !x[[ variable_names[k] ]] %in% missing

    # If entry provided, set output to unique value
    if ( any( no_missing ) ) {

      current_value <- unique( x[[ variable_names[k] ]][ no_missing ] )

      # If specified, check if multiple values are found
      # and return a warning
      if ( check_for_multiple ) {

        # Print a warning if multiple values are detected
        if ( length( current_value ) > 1 ) {

          warning_message <- paste0(
            'Multiple values detected:\n',
            'Variable = ', variable_names[k], '\n',
            reference, ' = ', unique( x[[ reference ]][entries] )[1], '\n',
            'Values = ', paste( current_value, collapse = ', ' ), '\n'
          )

          warning( warning_message, call. = FALSE )

        }

      }
      # Take the first unique value
      current_value <- current_value[1]

      # Once a unique value is found, stop looping over
      # remaining variables
      break()
    }

  }
  # Update output
  output <- current_value

  # Return output
  return( output )
}

