# General-purpose functions
# Written by...
#   Kevin Potter
#   William Schmitt
# email: kevin.w.potter@gmail.com
# Please email me directly if you
# have any questions or comments
# Last updated 2021-05-25

# Table of contents
# 1) load_package
# 2) Functions for file names
#   2.1) match_to_files
#   2.2) create_standardized_filename
# 3) extract_unique_value

#### 1) load_package ####
#' Installs and Loads an R Package
#'
#' Checks if a package is installed, and if not, sees if it
#' can be installed. Then, loads the package for easy use.
#'
#' @param package_name A character string with the package name.
#' @param from Where to download the package from; options include...
#' \enumerate{
#'   \item 'CRAN'
#'   \item 'Github'
#' }
#' @param repo An optional character string with the Github
#'   repository name (in the form 'username/repository').
#'   If \code{NULL}, assumes the username is 'rettopnivek'
#'   and that the repository is 'package_name'.
#' @param ... Additional parameters for the 'devtools'
#'   installation functions.
#'
#' @author Kevin Potter
#'
#' @examples
#' # Install/load package from CRAN
#' load_package( 'dplyr' )
#'
#' # Install/load package from author's github repository
#' load_package( 'utilityf', from = 'Github' )
#'
#' @export

load_package <- function( package_name,
                          from = 'CRAN',
                          repo = NULL,
                          ... ) {

  # List of packages that are installed
  installed_packages <- installed.packages()[,1]

  if ( !( package_name %in% installed_packages ) ) {

    # Install package via 'devtools' package

    # Check if 'devtools' is installed
    if ( !( 'devtools' %in% installed_packages ) ) {
      install.packages( 'devtools' )
    }

    # Installs package from CRAN repository
    if ( from == 'CRAN' ) {
      devtools::install_cran( package_name, ... )
    }

    # Installs package from Github repository
    if ( from == 'Github' ) {
      if ( is.null( repo ) ) {
        repo = paste0( "rettopnivek/", package_name )
      }
      devtools::install_github( repo, ... )
    }

  }

  # Load installed package for use
  library( package_name, character.only = T )

}

#### 2) Functions for file names ####

#### 2.1) match_to_files ####
#' Checks for Partial Matches Between a String and a Set of Files
#'
#' Checks if a file is present in the current working directory.
#' Can check either for regular files or files using MGH-CAM's
#' standardized file naming template:
#' TXX-Description-MM_DD_YYYY-vX.X.X.ext.
#'
#' @param string A file name or part of a file name to search for.
#' @param output Character string indicating the type of output to
#'   return, either...
#' \enumerate{
#'   \item 'Logical' or 'logical'
#'   \item 'Vector', 'vector', or 'vec'
#'   \item 'Index' or 'index'
#'   \item 'Name' or 'name'
#' }
#' @param std_name A logical indicating whether the file follows the
#'   standardized naming convention. If true, just matches the tag
#'   and description of the file.
#' @param ... Additional arguments passed to the
#'   \code{\link[base:list.files]{dir()}} function.
#'
#' @return Either...
#' \enumerate{
#'   \item A logical value, \code{TRUE} if the file is present;
#'   \item A logical vector for all files in the folder;
#'   \item The index position for the file if it exists;
#'   \item The file name.
#' }
#'
#' @author Kevin Potter
#'
#' @export

match_to_files <- function( string,
                            output = 'Logical',
                            std_name = FALSE,
                            ... ) {

  # All files and folders present
  # in working directory
  all_files <- dir( ... )

  # Determine if (standard) file name is present
  # in list of files/folders
  if (std_name) {
    fmatch <- regexpr('^\\w\\d{2}-[^-]*', string, perl = T)
    tag_and_desc <- regmatches(string, fmatch)
    check <- grepl(tag_and_desc, all_files, fixed = T)
  } else{
    check <- grepl( string, all_files, fixed = T )
  }

  # Output
  if ( output %in% c( 'Logical', 'logical' ) ) {
    return( any( check ) )
  }
  if ( output %in% c( 'Vector', 'vector', 'vec' ) ) {
    return( check )
  }
  if ( output %in% c( 'Index', 'index' ) ) {
    return( which( check ) )
  }
  if ( output %in% c( 'Name', 'name' ) ) {
    if ( any( check ) ) {
      return( all_files[ check ] )
    } else {
      return( NULL )
    }
  }

}

#### 2.2) create_standardized_filename ####
#' Create Standardized File Name
#'
#' Create a standardized file name of the
#' form: TXX-Description-MM_DD_YYYY.ext where 'T'
#' is a leading tag, 'XX' is a file number,
#' 'Description' is a human-readable
#' label, and 'ext' is a file extension.
#'
#' @param description A human-readable label, with
#'   words preferably separated by underscores.
#' @param extension A file extension, such as
#'   'RData', 'R', 'txt', 'pdf'.
#' @param tag A leading tag; If no value is provided,
#'   automatically set based on the file extension.
#'   Automatic assignments are...
#'   \itemize{
#'     \item 'S' for extension \code{R};
#'     \item 'D' for extensions \code{RData} and \code{csv};
#'     \item 'T' for extension \code{txt};
#'     \item 'W' for extension \code{docx};
#'     \item 'P' for extension \code{pptx};
#'     \item 'F' for extensions \code{pdf}, \code{jpg},
#'     \code{jpeg}, and \code{png}.
#'   }
#' @param number A file number. If no value is provided,
#'   automatically set based on number of files in current
#'   folder with matching tags.
#' @param file_date The date to include. If no value is
#'   provided, the current date is used.
#' @param additional Additional text to include following
#'   the date. If provided, is preceded by a '-'.
#' @param date_format The format to use for the current
#'   date, defaults to 'MM_DD_YYYY'.
#' @param exclude A vector of file names to exclude when
#'   automatically determining file numbers
#' @param remove Logical; if TRUE, attempts to locate
#'   previous versions of the outputted file name
#'   (i.e., same name but earlier dates) and remove
#'   them from the current folder.
#'
#' @return A character string.
#'
#' @author Kevin Potter
#'
#' @examples
#' # Different file types
#' make_file_name("Example", "RData")
#' make_file_name("Example", "pdf")
#' make_file_name("Example", "docx")
#'
#' # User-specified tags and numbers
#' make_file_name("Example", "RData", tag = "R", number = "02")
#' # Additional text
#' make_file_name("Example", "RData", additional = "v.1.0.0")
#' @export

create_standardized_filename <- function(description,
                                         extension,
                                         tag = NULL,
                                         number = NULL,
                                         file_date = NULL,
                                         additional = NULL,
                                         date_format = "%m_%d_%Y",
                                         exclude = "",
                                         remove = FALSE) {

  # Determine files in directory
  all_files <- dir()

  # If not specified, auto-generate file tag
  # based on extension
  if (is.null(tag)) {

    # Word document
    if (extension == "docx") {
      tag <- "W"
    }
    # PowerPoint
    if (extension == "pptx") {
      tag <- "P"
    }
    # Standard figure extensions
    if (extension %in% c("pdf", "jpg", "jpeg", "png")) {
      tag <- "F"
    }
    # Data files
    if (extension %in% c("RData", "csv")) {
      tag <- "D"
    }
    # R script file
    if (extension %in% c("R")) {
      tag <- "S"
    }
    # Text file
    if (extension %in% c("txt")) {
      tag <- "T"
    }
  }

  # If not specified, auto-generate file_date
  if (is.null(file_date)) {
    file_date <- format(
      Sys.Date(),
      date_format
    )
    file_date <- "-" %p% file_date
  } else {
    if (file_date != "") {
      file_date <- "-" %p% file_date
    }
  }

  # Check for matching tags and descriptions for
  # files present in folder
  if (length(all_files) > 0) {
    only_files_no_placeholder <-
      # Exclude folders
      grepl(".", all_files, fixed = T) &
      # Exclude user-specified files
      !all_files %in% exclude

    matching_tags <-
      substr(all_files, start = 1, stop = 1) == tag &
      only_files_no_placeholder

    matching_description <-
      grepl("-" %p% description %p% "-", all_files, fixed = T) &
      only_files_no_placeholder

    matching_extension <-
      grepl(extension %p% "$", all_files) &
      only_files_no_placeholder

    # Check for existing file
    found_match <-
      matching_description &
      matching_tags &
      matching_extension

    # If needed, increment file number
    if (is.null(number)) {
      if (any(found_match)) {
        number <- substr(
          all_files[found_match],
          start = 2, stop = 3
        )
      } else {
        number <- sum(matching_tags) + 1
      }

      # Make sure number is at least a double-digit and
      # convert to character string
      nc <- nchar(number)
      if (nc == 1) {
        number <- paste0("0", number)
      } else {
        number <- as.character(number)
      }
    }

    if (remove) {
      if (found_match) {
        old_file <- all_files[found_match]
        file.remove(old_file)
      }
    }
  } else {
    if (is.null(number)) {
      number <- 1

      # Make sure number is at least a double-digit and
      # convert to character string
      nc <- nchar(number)
      if (nc == 1) {
        number <- paste0("0", number)
      } else {
        number <- as.character(number)
      }
    }
  }

  if (!is.null(additional)) {
    additional <- paste0("-", additional)
  } else {
    additional <- ""
  }

  # Generate file name
  filename <- paste0(
    tag,
    number,
    "-",
    description,
    file_date,
    additional,
    ".",
    extension
  )

  return(filename)
}

#### 3) extract_unique_value ####
#' Extract Unique Values From Data Frames or Lists
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
#' @param allow_multiple Logical; if \code{TRUE} will allow multiple
#'   return values. By default, it does not override \code{check_for_multiple},
#'   so remember to change this if applicable.
#'
#' @author Kevin Potter, William Schmitt
#'
#' @return ...
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
                                  check_for_multiple = TRUE,
                                  allow_multiple = FALSE) {

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

      if (allow_multiple) {
        # Append to output
        output <- append(output, current_value)
      } else {

        # Take the first unique value
        output <- current_value[1]

        # Once a unique value is found, stop looping over
        # remaining variables
        break()
      }
    }

  }

  # Return output
  return( output )
}


