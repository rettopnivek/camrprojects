# Assorted templates
# Written by...
#   Kevin Potter
# Maintained by...
#   Kevin Potter
# email:
#   kpotter5@mgh.harvard.edu
# Please email us directly if you
# have any questions or comments
# Last updated 2023-11-15

# Table of contents
# 1) camr_document_function
# 2) camr_file_templates

#### 1) camr_document_function ####
#' Create a Template for a Function's Documentation
#'
#' Given an R script that contains a function whose
#' name and arguments follow a specific naming
#' convention, generates a template for the
#' function documentation (determining dependencies
#' on R packages, target outputs, and other project
#' functions).
#'
#' @param func_name A character string, the
#'   name of the function for which a template
#'   of the documentation is needed.
#' @param script_path A character string, the
#'   path to the R script containing the function
#'   (relative to the working directory).
#' @param tag_for_proj_fun A character string,
#'   the substring that precedes every project
#'   function. For example, if project functions
#'   follow the format `project.RXX.function_name`
#'   the input should be `project.R`.
#'
#' @returns A message displayed in the console
#' with the template for the function
#' documentation, which can be copied and
#' pasted as needed.
#'
#' @export

camr_document_function <- function(
    func_name,
    tag_for_proj_fun = NULL,
    script_path = NULL,
    folder_with_scripts = "R" ) {

  # setwd("E:/CAM/CAM_R_projects/CAM_data_processing/PEACH")
  # func_name <- 'peach.R02.processing_for_session_details'
  # script_path <- 'R/R02-process_session_details.R'
  # tag_for_proj_fun <- 'peach.R'

  # setwd(
  #   paste0(
  #     "E:/CAM/R_packages/Development_code/",
  #     "Development_code-camrprojects"
  #   )
  # )
  # func_name <- "project.R01.path_to_data"
  # script_path <- 'R/R01-first_set_of_functions.R'
  # tag_for_proj_fun <- 'project.R'

  #### 1.1) Setup ####

  # Automatically extract tag for project functions
  if ( is.null( tag_for_proj_fun ) ) {

    func_name_parts <-
      strsplit( func_name, split = ".", fixed = TRUE )[[1]]

    # If name is of form XX.YY.ZZ
    if ( length( func_name_parts ) == 3 ) {

      tag_for_proj_fun <- paste0(
          func_name_parts[1],
          ".",
          substr( func_name_parts[2], 1, 1 )
      )

      # Close 'If name is of form XX.YY.ZZ'
    }

    if ( is.null( tag_for_proj_fun ) ) {
      stop( "FORTHCOMING" )
    }

    # Close 'Automatically extract tag for project functions'
  }

  #### 1.1.1) remove_char ####

  remove_char <- function( chr_vec, chr_to_remove ) {

    out <- chr_vec
    J <- length( chr_to_remove )

    for ( j in 1:J ) {
      out <- gsub( chr_to_remove[j], '', out, fixed = TRUE )
    }

    return( out )
  }

  #### 1.1.2) Data types for arguments ####

  data_types <- c(

    int_ = "integer vector",

    num_ = "numeric vector",

    chr_ = "character vector",

    lgc_ = "logical vector",

    dtf_ = "data frame",

    lst_ = "list",

    mat_ = "matrix",

    fct_ = "factor",

    fun_ = "function",

    obj_ = "R object"

  )

  #### 1.1.3) List of generated targets ####

  # Check if script exists
  if ( '_targets.R' %in% dir() ) {

    # Read in '_targets.R' script
    targets_script <- scan(
      file = '_targets.R', what = 'character', sep = '\n'
    )

    # Identify generated targets

    index_for_generated_targets <-
      which( grepl( 'tar_target', targets_script, fixed = TRUE ) ) + 1

    generated_targets <- targets_script[
      index_for_generated_targets
    ]
    generated_targets <- remove_char(
      generated_targets, c( ",", " " )
    )
    not_generated_yet <-
      grepl( '#', generated_targets, fixed = TRUE )

    # Identify function used to generate target

    func_generating_targets <- targets_script[
      index_for_generated_targets + 1
    ]
    func_generating_targets <- remove_char(
      func_generating_targets, c( "(", " " )
    )

    # Remove targets not yet generated

    generated_targets <-
      generated_targets[ !not_generated_yet ]
    func_generating_targets <-
      func_generating_targets[ !not_generated_yet ]

    # Determine generating scripts

    script_generating_targets <- rep( "", length( generated_targets ) )

    for ( j in 1:length( generated_targets ) ) {

      external_func <- !grepl(
        tag_for_proj_fun, func_generating_targets[j], fixed = TRUE
      )

      if ( !external_func ) {
        script_generating_targets[j] <- strsplit(
          func_generating_targets[j], split = ".", fixed = TRUE
        )[[1]][2]
      }

    }

    # Close 'Check if script exists'
  } else {

    generated_targets <- ""

    # Close else for 'Check if script exists'
  }

  #### 1.1.4) List of R scripts ####

  if ( folder_with_scripts != "" ) {
    all_R_scripts <- dir( path = folder_with_scripts )
  } else {
    all_R_scripts <- dir()
  }

  R_scripts_tags <- substr( all_R_scripts, start = 1, stop = 3 )

  # Exhaustive search for script defining function
  if ( is.null( script_path ) ) {

    j <- 1
    func_defined <- FALSE
    while ( !any( func_defined ) & j <= length( all_R_scripts ) ) {

      if ( stringr::str_ends( all_R_scripts[j], ".R" ) ) {

        current_script <- scan(
          file = paste0( folder_with_scripts, "/", all_R_scripts[j] ),
          what = 'character', sep = '\n'
        )

        func_defined <- grepl(
          paste0( func_name, ' <- function(' ),
          current_script,
          fixed = TRUE
        )

      }

      j <- j + 1
    }


    if ( j > length( all_R_scripts ) ) {
      stop( "Cannot find function" )
    } else {
      script_path <- paste0(
        folder_with_scripts, "/", all_R_scripts[j-1]
      )
    }

  } else {
    current_script <- scan(
      file = script_path, what = 'character', sep = '\n'
    )
  }

  #### 1.2) Processing for current script and function ####

  #### 1.2.1) Read in current script ####

  N_lines <- length( current_script )

  # Lines in script for current function

  start_of_func <- which( grepl(
    paste0( func_name, ' <- function(' ),
    current_script,
    fixed = TRUE
  ) )

  end_of_func <- which(
    current_script == "}"
  )

  end_of_func <- end_of_func[
    min( which( end_of_func > start_of_func ) )
  ]

  #### 1.2.2) Determine packages used in script ####

  library_calls <- grepl(
    'library(', current_script, fixed = TRUE
  )

  # If any R packages were used
  if ( any( library_calls ) ) {

    # Convert to index
    index_for_library_calls <- which(
      library_calls
    )

    R_packages_used <- current_script[ index_for_library_calls ]
    R_packages_used <-
      remove_char( R_packages_used, c( "library", "(", ")", " ") )

    installed_packages <-
      installed.packages()

    R_package_versions <- installed_packages[
      which( installed_packages[,1] %in% R_packages_used ), 'Version'
    ]

    # Close 'If any R packages were used'
  } else {

    R_packages_used <- ""
    R_package_versions <- ""

    # Close else for 'If any R packages were used'
  }

  #### 1.2.3) Extract function arguments ####

  func_arg <-
    which( grepl( ') {', current_script, fixed = TRUE ) )

  index_for_func_arg <-
    ( start_of_func + 1 ):min( func_arg[ func_arg >= start_of_func ] )

  func_arg <- current_script[
    index_for_func_arg
  ]

  func_arg <-
    remove_char( func_arg, c( ",", ")", "{", " " ) )
  K <- length( func_arg )

  if ( all( func_arg == "" ) ) {
    K <- 0
    func_arg_types <- ""
  } else {

    default_values <- rep( "", K )

    for ( k in 1:K ) {
      if ( grepl( "=", func_arg[k], fixed = TRUE ) ) {
        arg_val <- strsplit( func_arg[k], split = "=", fixed = TRUE )[[1]]
        func_arg[k] <- arg_val[1]
        default_values[k] <- arg_val[2]
      }
    }

    func_arg_types <- substr( func_arg, start = 1, stop = 4 )

  }

  #### 1.2.4) Internal use of project functions ####

  lines_for_func <-
    start_of_func:end_of_func

  index_for_proj_fun_used <- which(
    grepl( tag_for_proj_fun, current_script, fixed = TRUE )
  )
  index_for_proj_fun_used <-  index_for_proj_fun_used[
    index_for_proj_fun_used %in% lines_for_func
  ]

  proj_fun_used <- rep( "", length( index_for_proj_fun_used ) )

  for ( k in seq_along( index_for_proj_fun_used ) ) {

    val <- strsplit( current_script[ index_for_proj_fun_used ][k],
                     split = " " )[[1]]
    proj_fun_used[k] <-
      val[ grepl( tag_for_proj_fun, val, fixed = TRUE ) ][1]

  }

  proj_fun_used <- remove_char( proj_fun_used, c( "(", ")" ) )
  proj_fun_used <- unique( proj_fun_used )
  proj_fun_used <- proj_fun_used[
    !grepl( func_name, proj_fun_used, fixed = TRUE )
  ]

  #### 1.2.5) Output ####

  return_statements <-
    grepl( 'return(', current_script, fixed = TRUE ) &
    1:length(current_script) %in% lines_for_func

  if ( any( return_statements ) ) {

    lines_for_return_statments <- current_script[ return_statements ]

    n_outputs <- sum( return_statements )

  } else {

    lines_for_return_statments <- ''
    n_outputs <- 0

  }

  #### 1.3) Create template for documentation ####

  #### 1.3.1) Title of function documentation ####

  current_script_tag <- strsplit(
    remove_char( script_path, "R/" ), split = "-", fixed = TRUE
  )[[1]][1]
  doc_title <- remove_char(
    func_name, c(
      tag_for_proj_fun, paste0( remove_char( current_script_tag, "R" ), "." )
    )
  )

  doc_title <- stringr::str_to_title(
    gsub( "_", " ", doc_title, fixed = TRUE )
  )

  #### 1.3.2) Description of function ####

  #### 1.3.3) Function arguments ####

  max_chr <- 60

  N_arg <- length( func_arg )

  if ( all( func_arg == "") ) {

    doc_arg <- ""

  } else {

    doc_arg <- paste0(
      "# @param '",
      func_arg,
      "' A ",
      data_types[ func_arg_types ]
    )
    extra <- rep( ".\n", N_arg )

    for ( j in 1:N_arg ) {

      if ( func_arg[j] %in% generated_targets ) {

        extra[j] <- paste0(
          " (see target output from the '",
          func_generating_targets[
            generated_targets %in% func_arg[j]
          ],
          "' function).\n"
        )

      }

    }

    doc_arg <- paste0( doc_arg, extra )

    for ( j in 1:N_arg ) {

      doc_arg[j] <-
        stringr::str_wrap( doc_arg[j], width = max_chr, exdent = 3 )
      if ( grepl( '\n', doc_arg[j], fixed = TRUE ) ) {
        doc_arg[j] <- gsub( '\n', '\n#', doc_arg[j], fixed = TRUE )
      }
      doc_arg[j] <- paste0( doc_arg[j], "\n" )

    }

  }

  #### 1.3.4) Prerequisites ####

  pre_req <- c()

  if ( !any( R_packages_used == "" ) ) {

    pre_req <- c(
      pre_req,
      paste0(
        "#   * The R package '",
        R_packages_used,
        "' (version ",
        R_package_versions, ")\n"
      )
    )

  }

  if ( !any( proj_fun_used == "" ) & !length( proj_fun_used ) == 0 ) {

    pre_req <- c(
      pre_req,
      paste0(
        "#   * The '",
        proj_fun_used,
        "' function\n"
      )
    )

  }

  pre_req_tag <- ""

  if ( length( pre_req ) > 0 ) {

    pre_req_tag <- paste0(
      '# \n',
      '# @details \n',
      '# Prerequisites: \n'
    )

  }

  #### 1.3.5) Objects returned ####

  doc_ret <- ''

  if ( n_outputs > 0 ) {


    if ( n_outputs == 1 ) {

      obj_ret <-
        remove_char( lines_for_return_statments, c( "return(", ")", " " ) )

      ret_type <-
        substr( obj_ret, start = 1, stop = 4 )

      if ( ret_type %in% names( data_types ) ) {

        doc_ret <- paste0( "# @returns A ", data_types[ret_type], ".\n" )

      }

    }

  }

  #### 1.3.6) Output ####

  output <- c(
    '\n',
    paste0( '# ', doc_title, '\n' ),
    '# \n',
    '# [Insert description]. \n',
    '# \n',
    doc_arg,
    pre_req_tag,
    pre_req,
    '# \n',
    doc_ret
  )

  message( paste( output, collapse = '' ) )

}

#### 2) camr_file_templates ####
#' Create File Template
#'
#' Function to create a template
#' for various type of files.
#'
#' @param chr_template A character string specifying
#'   the type of template to create, either...
#'   \itemize{
#'     \item renviron = A .renviron file.
#'     \item targets = A _targets.R file.
#'   }
#' @param chr_file_name An optional character
#'   string, the name for the new file.
#'
#' @returns As a side effect, a new file.
#'
#' @export

camr_file_templates <- function(
    chr_template,
    chr_file_name = '' ) {

  #### 2.1) Setup ####

  # Inputs for possible templates
  lst_templates <- list(

    renviron = c(
      ".renviron",
      "renviron",
      "env"
    ),

    targets = c(
      '_targets.R',
      '_targets',
      'targets',
      'create targets',
      'initialize targets'
    ),

    folders_for_analysis_project = c(
      "folders for analysis project",
      "analysis project folders",
      "analysis project directories",
      "folders for analyses",
      "directories for analyses"
    ),

    analysis_script = c(
      "analysis script",
      "analysis R script",
      "analysis r script"
    )

  )

  chr_proj_abbr <- Sys.getenv( "PROJECT_ABBR" )
  # Placeholder for project abbreviation
  if ( chr_proj_abbr == "" ) {

    chr_proj_abbr <- "PROJECT"

    # Close 'Placeholder for project abbreviation'
  }

  chr_user_name <- Sys.getenv( "USER_NAME" )
  # Placeholder for user name
  if ( chr_user_name == "" ) {

    chr_user_name <- "<Your name>"

    # Close 'Placeholder for user name'
  }

  chr_user_email <- Sys.getenv( "USER_EMAIL" )
  # Placeholder for email
  if ( chr_user_email == "" ) {

    chr_user_email <- "<Your email>"

    # Close 'Placeholder for email'
  }

  #### 2.2) Create template files ####

  #### 2.2.1) renviron ###

  # Create .renviron file
  if ( chr_template %in% lst_templates$renviron ) {

    chr_text <- c(
      paste0( "PROJECT_ABBR='", chr_proj_abbr, "'" ),
      "",
      paste0( "USER_NAME='", chr_user_name, "'" ),
      paste0( "USER_EMAIL='", chr_user_email, "'" ),
      "",
      "FOLDER_SOURCE='<Path to source folder>'",
      "FOLDER_OUTPUT='<Path to output folder>'",
      "",
      "API_REDCAP_URI='https://redcap.partners.org/redcap/api/'",
      "API_REDCAP_TOKEN='<Your token>'"
    )

    # Default file name
    if ( chr_file_name == '' ) {

      chr_file_name <- '.renviron'

      # Close 'Default file name'
    }

    write(
      paste0( paste( chr_text, collapse = '\n' ), '\n' ),
      file = chr_file_name
    )

    # Close 'Create .renviron file'
  }

  #### 2.2.2) targets ###

  # Create _targets.R file
  if ( chr_template %in% lst_templates$targets ) {

    chr_text <- c(

      "# Script to generate targets",
      paste0( "# Written by ", chr_user_name ),
      paste0( "# email: ", chr_user_email ),
      "# Please email me directly if you ",
      "# have any questions or comments",
      paste0( "# Last updated ", format( Sys.Date(), "%Y-%m-%d" ) ),
      "",
      "# Table of contents",
      "# 1) Initial setup",
      "#   1.1) Download source data",
      "#   1.2) R scripts and packages",
      "# 2) Generate targets",
      "",
      "### To generate targets ###",
      "# 1. Click on 'Session' and in pull-down menu for ",
      "#    'Set Working Directory' select 'To Project Directory'",
      "# 2. <Optional>",
      "#    - Clear workspace (click broom icon)",
      "#    - Restart R (Click on 'Session' and select 'Restart R')",
      "# 3. Type in console: targets::tar_make()",
      "",
      "#### 0) Template for function documentation ####",
      "# Title ",
      "# ",
      "# ... ",
      "# ",
      "# @param 'obj_x' An R object (see target output ",
      paste0(
        "#   from the '",
        chr_proj_abbr,
        ".RXX.example' function)."
      ),
      "# ",
      "# @details ",
      "# Prerequisites:",
      "#   * The R package '?' (version ?)",
      paste0(
        "#   * The '",
        chr_proj_abbr,
        ".RXX.example' function"
      ),
      "# ",
      "# @returns ...",
      "",
      "#### 1) Initial setup ####",
      "",
      "#### 1.1) Download source data ####",
      "",
      "if ( FALSE ) {",
      "  ",
      "  camrprojects::camr_copy_from_source()",
      "  ",
      "}",
      "",
      "#### 1.2) R scripts and packages ####",
      "",
      "# Load in package to manage generation of target outputs",
      "# install.packages( 'targets' )",
      "library(targets)",
      "",
      "# Source in all R scripts with functions",
      "camrprojects::camr_source_scripts( path = 'R' )",
      "",
      "# Load in packages",
      "tar_option_set(",
      "  packages = c(",
      "    # Data frame manipulation and '%>%' operator",
      "    #   To install:",
      "    #   install.packages( 'dplyr' )",
      "    'dplyr',",
      "    # R functions for CAM",
      "    #   To install:",
      "    #   devtools::install_github( 'rettopnivek/camrprojects')",
      "    'camrprojects'",
      "  )",
      ")",
      "",
      "#### 2) Generate targets ####",
      "",
      "list(",
      "  ",
      "  # - Example target",
      "  #   targets::tar_load( chr_example )",
      "  tar_target(",
      "    chr_example, ",
      "    paste( 'Hello', 'world' )",
      "  )",
      "",
      ")",
      ""
    )

    # Default file name
    if ( chr_file_name == '' ) {

      chr_file_name <- '_targets.R'

      # Close 'Default file name'
    }

    write(
      paste0( paste( chr_text, collapse = '\n' ), '\n' ),
      file = chr_file_name
    )

    # Close 'Create _targets.R file'
  }

  # Create folders for analysis project
  if ( chr_template %in% lst_templates$folders_for_analysis_project ) {

    mkdir( "Source" )

    chr_README <- paste0(
      "This directory contains local copies of source files. ",
      "Source files can be found on the Dropbox folder 'CAM Analyses'",
      " in 'PROJ-YYYY/PROJ-AZZZ-Name/Source'. ",
      "\n\n",
      "Patient data is not allowed on the MGB GitLab server. ",
      "The contents of this directory shall therefore be excluded from git.",
      "\n"
    )

    write(
      chr_README,
      file = "Source/README.txt"
    )

    mkdir( "Output" )

    chr_README <- paste0(
      "Output files should be uploaded to the Dropbox folder ",
      "'CAM Analyses in 'PROJ-YYYY/PROJ-AZZZ-Name/Output'.",
      "\n"
    )

    write(
      chr_README,
      file = "Output/README.txt"
    )

    mkdir( "R" )

  }

}
