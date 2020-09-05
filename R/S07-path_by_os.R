#' Navigate to Folder based on OS
#'
#' A function to navigate to different folders/paths
#' conditional on a user's operating system.
#'
#' @param paths A named vector consisting of...
#' \describe{
#'   \item{Windows}{The path for Windows-based operating systems}
#'   \item{Mac}{The path for Mac-based operating systems}
#'   \item{Linux}{The path for Linux-based operating systems}
#' }
#' @author Kevin Potter and William Schmitt
#'
#' @return A logical value, \code{TRUE} if the path
#' was successfully accessed.
#'
#' @examples
#' accessed_path <- path_by_os(
#'   c( Windows = 'C:/Users',
#'   Mac = '~/Users',
#'   Linux = '~' )
#' )
#' print( accessed_path ); print( getwd() )
#'
#' @export

path_by_os <- function( paths ) {

  # Determine OS
  OS <- sessionInfo()

  # Attempt to navigate to path based on
  # operating system

  if ( stringr::str_detect( OS$running, 'Windows' ) ) {
    # For Windows machines
    path_to_try <- paths[ 'Windows' ]
  }
  if ( stringr::str_detect( OS$running, 'macOS' ) ) {
    # For Macs
    path_to_try <- paths[ 'Mac' ]
  }
  if ( stringr::str_detect( OS$running, 'unix' ) ) {
    # For Macs
    path_to_try <- paths[ 'Linux' ]
  }

  # Attempt to access path
  accessed_path <- tryCatch(
    {
      setwd( path_to_try )
      TRUE
    },
    error = function(e) return( FALSE )
  )

  return( accessed_path )
}
