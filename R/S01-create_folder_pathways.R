#' Create or Update List with Paths for Project Directory Hierarchy
#'
#' Creates/update a list that contains paths for the folder hierarchy
#' for a standard CAM RStudio project. This list can then be accessed
#' by the \code{\link{go_to}} function for fast and readable folder
#' navigation.
#'
#' @param folder_pathways An optional existing list with paths and labels
#'   that can then be updated with the \code{add_path} argument. If
#'   \code{NULL} (the default), automatically generates the list AND
#'   the standard folder hierarchy for a CAM RStudio project, where...
#'   \describe{
#'     \item{Project}{The project directory}
#'     \item{+ R}{The folder for R scripts}
#'     \item{+ Data}{}
#'     \item{++ Data_processing}{Processing scripts to clean the raw data}
#'     \item{++ Raw_data_files}{Files, such as downloads from REDCap, to
#'       be cleaned and processed}
#'   }
#' @param add_path An optional named list to add a new folder path and
#'   associated labels to an existing \code{folder_pathways} object.
#'   Must contain three elements:
#'   \describe{
#'     \item{path}{The absolute path to the folder to add;}
#'     \item{title}{The internal label for the slot for the folder path;}
#'     \item{labels}{The set of labels to use in the \code{go_to} call.}
#'   }
#' @param redcap Logical; if \code{TRUE}, when creating the
#'   \code{folder_pathways} list, will check for a folder one level
#'   above the project directory for an installation of the R package
#'   'REDCapR' version 0.9.8, which is compatible with current CAM
#'   projects as of 09/04/2020.
#' @param dropbox Logical; if \code{TRUE}, when creating the
#'   \code{folder_pathways} list, will check for a text file
#'   in the 'Raw_data_files' folder that contains the path to a
#'   dropbox folder on the local machine. If the file does not
#'   exist, a warning will be displayed and a file template
#'   will be created. The user can then copy the path into the
#'   file.
#' @param api Logical; if \code{TRUE}when creating the
#'   \code{folder_pathways} list, will check for a text file
#'   in the 'Raw_data_files' folder that contains the API token
#'   for a specified REDCap project. If the file does not
#'   exist, a warning will be displayed and a file template
#'   will be created. The user can then copy their API token
#'   into the file.
#' @return A list with slots for folder pathway of interest (e.g.,
#'   the project directory, the R scripts folder, etc.), and an
#'   additional list with labels (e.g., \code{c('Project','project')})
#'   that can be used with the \code{\link{go_to}} function to navigate
#'   to the associated folder.
#'
#' @author Kevin Potter
#'
#' @examples
#' # For example purposes, create simple
#' # list with path and labels
#' folder_pathways <- list(
#'   Project = getwd(),
#'   Path_labels = list( Project = c( 'Project', 'project' ) )
#' )
#'
#' # Add a new path and labels to the existing list
#' lst <- list(
#'   path = paste0( getwd(), '/R' ),
#'   title = 'R',
#'   labels = c( 'R', 'r' )
#' )
#' folder_pathways = create_folder_pathways( folder_pathways, add_path = lst )
#'
#' @export

create_folder_pathways <- function( folder_pathways = NULL,
                                    add_path = NULL,
                                    redcap = TRUE,
                                    dropbox = TRUE,
                                    api = TRUE ) {

  # Initial creation of 'folder_pathways' list and
  # creation of directory structure for a standard
  # CAM project
  if ( is.null( folder_pathways ) ) {

    # Initialize list
    folder_pathways$Path_labels = list(
      Project = c( 'Project', 'project' )
    )

    # Check if current folder has a file with the '.Rproj'
    # extension
    # - if TRUE, in Project directory
    # - if FALSE, assume to be in the 'R' directory,
    #   one level below the Project directory
    if ( !any( grepl( '.Rproj', dir(), fixed = T ) ) ) {
      setwd( '..' )
      # Check if assumptions are true and throw an error
      # if '.Rproj' extension not found
      if ( !any( grepl( '.Rproj', dir(), fixed = T ) ) ) {
        stop(
          paste0(
            "Initial generation of the 'folder_pathways' list ",
            "must be done either in the Project directory or ",
            "in the 'R' sub-directory one level below the ",
            "Project directory"
          ), call. = F
        )
      }
    }

    ### Folder for project directory

    folder_pathways = list(
      Project <- getwd()
    )

    ### Folder for R scripts

    if ( !'R' %in% dir() ) {
      # If folder does not exist, create folder
      dir.create( 'R' )
    }
    setwd( 'R' )
    folder_pathways <- list(
      R_scripts <- getwd()
    )
    # Update path labels
    folder_pathways$Path_labels$R =
      c( 'R', 'r', 'R scripts', 'r scripts' )

    # Return to project directory
    setwd( folder_pathways$Project )

    ### Folder for data

    if ( !'Data' %in% dir() ) {
      # If folder does not exist, create folder
      dir.create( 'Data' )
    }
    setwd( 'Data' )
    folder_pathways$Data <- getwd()
    # Update path labels
    folder_pathways$Path_labels$Data =
      c( 'Data', 'data' )

    ### Folder for raw data files

    if ( !'Raw_data_files' %in% dir() ) {
      # If folder does not exist, create folder
      dir.create( 'Raw_data_files' )
    }
    setwd( 'Raw_data_files' )
    folder_pathways$Raw_data_files <- getwd()
    # Update path labels
    folder_pathways$Path_labels$Raw_data_files =
      c( 'Raw data', 'raw data', 'Raw', 'raw' )

    # If specified, check for file containing API token for
    # REDCap access
    if ( api ) {

      if ( !'API_token.txt' %in% dir() ) {
        warning(
          paste0(
            "The file 'API_token.txt' was not found - to download ",
            "data from REDCap, please get a project-specific token ",
            "and create a text file with the token in the 'Raw_data_files' ",
            "folder"
          ), call. = F
        )
      }

      fileConn <- file("API_token.txt")
      writeLines(c("COPY API TOKEN HERE"), fileConn)
      close(fileConn)

    }

    # If specified, check for file containing path to a
    # dropbox folder
    if ( dropbox ) {

      if ( !'Dropbox_folder_location.txt' %in% dir() ) {
        warning(
          paste0(
            "The file 'Dropbox_folder_location.txt' was not found - ",
            "to upload folders to a dropbox folder ",
            "please find the local path to the folder and add it ",
            "to the text file in the 'Raw_data_files' folder (e.g., ",
            "~/User/Dropbox_folder/)"
          ), call. = F
        )

        fileConn <- file("Dropbox_folder_location.txt")
        writeLines(c("COPY PATH TO DROPBOX FOLDER HERE"), fileConn)
        close(fileConn)

      } else {

        # Read in path for dropbox folder
        orig_dir <- getwd()
        setwd(
          scan( file = 'Dropbox_folder_location.txt',
                what = 'character', sep = '\n',
                quiet = T )
        )
        folder_pathways$Dropbox <- getwd()
        # Update path labels
        folder_pathways$Path_labels$Dropbox =
          c( 'Dropbox', 'dropbox' )
        setwd( orig_dir )

      }

    }

    ### Folder for data processing scripts

    setwd( folder_pathways$Data )
    if ( !'Data_processing' %in% dir() ) {
      # If folder does not exist, create folder
      dir.create( 'Data_processing' )
    }
    setwd( 'Data_processing' )
    folder_pathways$Data_processing <- getwd()
    # Update path labels
    folder_pathways$Path_labels$Data_processing =
      c( 'Processing', 'processing', 'Process', 'process',
         'Data processing', 'data processing',
         'Process data', 'process data',
         'Processing scripts', 'processing scripts' )

    ### Specific REDCapR package version

    # If specified, check if there is a folder for
    # the REDCapR package version 0.9.8
    if ( redcap ) {

      setwd( folder_pathways$Project )
      setwd( '..' )

      if ( !'REDCapR_previous_versions' %in% dir() ) {
        dir.create( 'REDCapR_previous_versions' )
        warning(
          paste0( 'Folder for REDCapR package version 0.9.8 ',
                  'created - make sure to install ',
                  'this version of the package (rather than ',
                  'most recent version)'
                ), call. = F
        )
      }
      setwd( 'REDCapR_previous_versions' )
      folder_pathways$REDCapR <- getwd()
      # Update path labels
      folder_pathways$Path_labels$REDCapR =
        c( 'REDCap', 'redcap', 'REDCapR', 'redcapr' )
      setwd( folder_pathways$R_scripts )

    }


  }

  # Update 'folder_pathways' list with new path and
  # associated labels
  if ( !is.null( add_path ) ) {

    # Check that necessary elements are included in
    # input
    if ( !all( names( add_path ) %in% c( 'path', 'title', 'labels' ) ) ) {
      stop( paste0(
        "To update 'folder_pathways' input with a new path and associated ",
        "labels, the 'add_path' list must have 3 named elements: ",
        "'path' with the folder path, 'title' with the internal list slot ",
        "name, and 'labels' with the labels for the path to use with the ",
        "'go_to' function"
      ), call. = F )
    }

    # Add path to list
    folder_pathways = c( folder_pathways, add_path$path )

    # Re-title new path
    nms = names( folder_pathways )
    nms[ nms == "" ] = add_path$title
    names( folder_pathways ) = nms

    # Add slot for path labels
    folder_pathways$Path_labels = c(
      folder_pathways$Path_labels, list( add_path$labels ) )

    # Re-title slot for path labels
    nms = names( folder_pathways$Path_labels )
    nms[ nms == "" ] = add_path$title
    names( folder_pathways$Path_labels ) = nms

  }

  return( folder_pathways )
}

