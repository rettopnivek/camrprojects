#' Navigate to Folder in Project Directory
#'
#' A convenience function that allows a user to quickly navigate 
#' to specified folders in the Project directory via an easily 
#' readable command. Requires a list called \code{folder_pathways}
#' with the paths and associated labels to exist in the global environment 
#' (see \code{\link{create_folder_pathways}}).
#' 
#' @param path The label associated with the desired folder to navigate to
#'   (e.g., 'Project', 'R', etc.).
#' 
#' @author Kevin Potter
#'
#' @export

go_to = function( path ) {
  
  if ( !exists( 'folder_pathways', envir = .GlobalEnv ) ) {
    stop( paste0(
      "The list 'folder_pathways' with the paths and associated ",
      "labels was not found in the global environment - please ",
      "create this list via the 'create_folder_pathways' function"
      ), call. = F )
  }
  
  # Extract stored paths
  nms = names( folder_pathways )
  nms = nms[ nms != 'Path_labels' ]
  # Extract labels/abbreviations for each path
  lbls = folder_pathways$Path_labels
  
  # Function to check if a string is 
  # present in vector of strings
  is_in = function( x, labels ) {
    return( x %in% labels )
  }
  
  location = ""
  
  # Loop over possible paths
  for ( i in 1:length( nms ) ) {
    
    if ( path %in% lbls[[ nms[i] ]] ) {
      # Save path for future navigation
      location = folder_pathways[[ nms[i] ]]
      
    }
    
  }
  
  if ( location == "" ) {
    stop( 'Label/abbreviation for folder path not available',
          call. = F )
  } else {
    setwd( location )
  }
  
}
