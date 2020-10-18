#' Display Documention for Custom R Function
#'
#' Extracts and displays documention for a custom R function,
#' given a documentation format of
#'
#' @param f An R function saved in the current workspace.
#'
#' @details Documentation start with the phrase '  # Purpose:'.
#' Each subsequent line that starts with '  #' will be displayed.
#'
#' Standard CAM procedure for documenting functions is to have
#' lines following the headers 'Purpose:', 'Arguments:', and
#' 'Returns:'.
#'
#' @author Kevin Potter
#'
#' @examples
#' # Example custom function with standardized documentation
#' f = function( x ) {
#'   # Purpose:
#'   # Displays the values of 'x'
#'   # Arguments:
#'   # x - An R object
#'   # Returns:
#'   # Output of the 'print' call
#'
#'   # Internal notes
#'   print( x )
#' }
#' notes_for_custom_function( f )
#'
#' @export

notes_for_custom_function = function( f ) {

  f_details = capture.output( print( f ) )


  check_if_doc = any( grepl( "  # Purpose:", f_details, fixed = T ) )

  if ( check_if_doc ) {

    doc_start = which( grepl( "  # Purpose:", f_details, fixed = T ) )
    pound_sym = which( grepl( "  #", f_details, fixed = T ) )

    pound_sym =
      pound_sym[ c( 1, diff( pound_sym ) ) == 1 ]
    pound_sym = pound_sym[ pound_sym >= doc_start ]

    out = paste( f_details[ pound_sym ], collapse = '\n' )
    message( out )

  }

}


