# REDCap and API tools
# Written by...
#   Kevin Potter
#   William Schmitt
# email: kevin.w.potter@gmail.com
# Please email me directly if you
# have any questions or comments
# Last updated 2021-05-25

# Table of contents
# 1) Functions to read from REDCap
#   1.1) postFormJSONToDf
#   1.2) redcap_read
# 2) Functions for Naming Conventions
#   2.1) validate_var_name
#   2.2) rename_redcap_vars
# 3) pull_git_version
# 4) source_scripts

#### 1) Functions to read from REDCap ####

#### 1.1) postFormJSONToDf ####
#' Sends a POST Form via httr and Converts JSON Return to a Data Frame
#'
#' This function uses httr to submit a POST form to the given url and
#' converts the response (expected to be a JSON array) to a data.frame.
#' Returns a list containing the data frame and response from the server.
#'
#' @param url The URL where the POST request should be submitted.
#'
#' @param params A named list of parameters to submit with the POST
#' request.
#'
#' @return A list which contains the following elements:
#' * `data`: A data frame containing all records and all fields.
#' * `status`: The HTTP status code returned from the request. If an
#'   error was encountered after the request, returns -1.
#'
#' @author William Schmitt
#'
#' @md

postFormJSONToDf <- function(url, params) {

  # Declare constants
  SUCCESS_STATUS_CODE <- 200
  AFTER_REQUEST_ERROR <- -1

  # Submit request
  resp <- httr::POST(
    url = url,
    body = params,
    encode = 'form'
  )

  # Check response and return if it's not successful
  if (resp$status_code != SUCCESS_STATUS_CODE) {
    out <- list(data = data.frame(), status = resp$status_code)
    return(out)
  }

  tryCatch({

    # Convert response to JSON
    respJSON <- httr::content(resp, as = 'text')

    # Remove invalid chars (\r\n -- carriage return, or \t -- tabs)
    respJSON <- gsub('[\r\n]', '', respJSON)
    respJSON <- gsub('[\t]', '', respJSON)

    # Convert JSON to data frame
    respDF <- jsonlite::fromJSON(respJSON)

    # Prepare return argument
    out <- list(data = respDF, status = SUCCESS_STATUS_CODE)

  },
  error = function(e) list(data = data.frame(), status = AFTER_REQUEST_ERROR)
  )
}

#### 1.2) redcap_read ####
#' Read All Records From a REDCap Project
#'
#' This function seeks to mimic the [REDCapR::redcap_read()] function,
#' but with a more robust output that can handle bizarre characters
#' and calculated fields. This function handles all REST API calls
#' using httr. This function also uses calls to [furrr] and [future] for
#' downloading batches from REDCap. The [plan][future::plan()] is not specified.
#' Instead, the user should use the plan that works best for their
#' specific use.
#'
#' @param redcap_uri The URI (uniform resource identifier) of the
#'   REDCap project. Required.
#'
#' @param token The user-specific string that serves to authenticate
#'   the call to the REST API. Required.
#'
#' @param raw_or_label A string (either `'raw'` or `'label'`) which
#'   specifies whether the exported data should be the raw
#'   coded/numerical values or the human-readable labels.
#'   Default is label.
#'
#' @param batch_size An integer indicating the number of records to
#'   download at once.
#'
#' @return A list which contains the following elements:
#' * `data`: A data frame containing all records and all fields.
#' * `success`:A logical indicating the success of the download.
#' * `metaData`: A data frame containing the REDCap metadata for all fields.
#'
#' @author William Schmitt
#'
#' @export
#' @md

redcap_read = function( redcap_uri,
                        token,
                        raw_or_label = 'label',
                        batch_size = 100) {

  # Download REDCap Metadata (e.g. data dictionary) to determine
  # unique identifier field.
  metaDtfResp <- postFormJSONToDf(
    url = redcap_uri,
    params = list(
      token = token,
      content = 'metadata',
      format = 'json',
      returnFormat = 'json'
    )
  )
  metaDtf <- metaDtfResp$data

  # Extract unique identifier field
  uniId <- metaDtf[1, 1]

  # Notify user
  message(paste(
    'REDCap Metadata Download.',
    'Unique field identified:',
    uniId
  ))

  # Use unique id to get list of participants
  ptListResp <- postFormJSONToDf(
    url = redcap_uri,
    params = list(
      token = token,
      content = 'record',
      format = 'json',
      returnFormat = 'json',
      type = 'flat',
      fields = uniId
    )
  )
  ptList <- ptListResp$data

  # Get list of pts in batches
  ptList <- unlist(unique(ptList[, 1]))
  ptList <- split(ptList, ceiling(seq_along(ptList)/batch_size))

  # Notify user
  message(paste(
    purrr::reduce(purrr::map_int(ptList, length), sum),
    'records identified.',
    'Downloading',
    length(ptList),
    'batches of',
    batch_size,
    'records.'
  ))

  # Initialize parameter list for each download
  baseParams <- list(
    token = token,
    content = 'record',
    format = 'json',
    returnFormat = 'json',
    type = 'flat',
    rawOrLabel = raw_or_label
  )

  # Function to download a batch
  downloadBatch <- function(ids, baseParams, pb) {

    # Build list of record #s to filter data
    curIds <- as.list(ids)
    curIdNames <- paste0(
      'records[',
      0:(length(curIds) - 1),
      ']'
    )
    names(curIds) <- curIdNames

    # Create parameter vector
    paramList <- append(baseParams, curIds)
    datResp <- postFormJSONToDf(
      url = redcap_uri,
      params = paramList
    )

    # Update progress bar
    pb()

    # Return
    return(datResp$data)
  }

  # Download each batch with a progress bar indicator
  progressr::with_progress({
    # Initialize pb
    progressr::handlers('progress')
    pb <- progressr::progressor(length(ptList))
    pb(amount = 0)
    # Get data
    dat <- furrr::future_map_dfr(ptList, downloadBatch, baseParams, pb)
  })

  # Get numeric fields
  intFields <- dplyr::filter(
    metaDtf,
    stringr::str_detect(text_validation_type_or_show_slider_number, 'integer')
  )
  numFields <- dplyr::filter(
    metaDtf,
    stringr::str_detect(text_validation_type_or_show_slider_number, 'number')
  )
  calcFields <- dplyr::filter(
    metaDtf,
    field_type == 'calc'
  )

  # Typecast fields

  # Int validation --> integers
  dat <- dplyr::mutate(
    dat,
    dplyr::across(
      intFields$field_name,
      as.integer
    )
  )

  # Num validation --> numeric
  dat <- dplyr::mutate(
    dat,
    dplyr::across(
      numFields$field_name,
      as.numeric
    )
  )

  # Calculated fields --> numeric
  dat <- dplyr::mutate(
    dat,
    dplyr::across(
      calcFields$field_name,
      as.numeric
    )
  )
  # Package output
  message('Done.')
  out <- list(
    data = dat,
    success = 1,
    metaData = metaDtf
  )

  return(out)
}

#### 2) Functions for Naming Conventions ####

#### 2.1) validate_var_name ####
#' Validate That a String Conforms to Naming Standards
#'
#' Validate a string conforms to the Center for Addiction Medicine (CAM) R
#' data-frame column naming standard. The CAM name standard is
#' GGG.TTT.Optional.Var_name. This function maintains the ability to check if
#' `TTT` conforms to the standard for R types or for REDCap field types and is
#' vectorized over the input.
#'
#' The CAM Name Standard: GGG.TTT.Optional.Var_name.
#' \itemize{
#'   \item `GGG` is the variable's group, which roughly corresponds to data
#'     of a similar nature or type. Examples include: IDS for identification
#'    variables, SBJ for subject demographic variables, SSS for important
#'    study details, INV for inventory measures, etc.
#'   \item`TTT` is the variable's type, which corresponds to either a
#'     specific R type or a specific REDCap field type. Find permissible
#'     types below:
#'     \itemize{
#'       \item Possible R Types:
#'         \itemize{
#'           \item `INT`: An integer value.
#'           \item `DBL`: A double or non-integer numeric value.
#'           \item `CHR`: A character string.
#'           \item `DAT`: A date-time value. TODO: Decide on exact date-time
#'           object.
#'           \item `LGC`: A boolean value.
#'         }
#'       \item Possible REDCap Types:
#'         \itemize{
#'           \item `DAT`: Date-time fields.
#'           \item `INT`: Integer fields.
#'           \item `DBL`: Decimal/non-integer numeric fields.
#'           \item `CHR`: All other text or notes box fields.
#'           \item `MCQ`: Multiple choice drop-down lists and radio buttons.
#'           \item `CKB`: Checkbox fields.
#'           \item `YNQ`: Yes/No Questions.
#'           \item `FLE`: Signature and file-upload fields.
#'           \item `VAS`: Slider or visual analog scale (VAS) fields.
#'           \item `DSC`: Descriptive text fields.
#'         }
#'     }
#'   \item `Optional`: An optional, short, additional grouping identifier. This
#'   is most often indicated for inventories, to allow for additional grouping
#'   by the questionnaire/inventory itself. Multiple additional group identifier
#'   are allowed, so long as they maintain a period separator and contain only
#'   an alphanumeric sequence (i.e. no `_`). Case should be sensible; this
#'   section should either be all caps (e.g. abbreviations) or only the first
#'   letter should be capitalized.
#'   \item `Var_name`: A short, descriptive name for the field that would be
#'   appropriate for figure/table labeling. If the name consists of multiple
#'   words, the first word should be capitalized and subsequent words should be
#'   separated by `_` and appear in all lower-case. The one exception is
#'   if the entire word (e.g. an abbreviation) is capitalized.
#' }
#'
#' @param str A string vector of names to validate.
#' @param type A string of either "REDCap" or "R" to indicate how the type
#' section of the name should be validated.
#'
#' @author William Schmitt
#'
#' @return A boolean vector of equal length that specifies whether each element
#' passed validation.
#'
#' @examples
#'
#' # TODO
#'
#' @export

validate_var_name <- function(str, type = "R") {

  ## CONSTANTS

  # Define R and REDCap allowed types
  rTypes <- c("INT", "DBL", "CHR", "DAT", "LGC")
  rcTypes <- c(
    "DAT",
    "INT",
    "DBL",
    "CHR",
    "MCQ",
    "CKB",
    "YNQ",
    "FLE",
    "VAS",
    "DSC"
  )

  # Assert that str is a string vector
  if (typeof(str) != "character") {
    stop("validate_R_var_name: str not a character vector.")
  }

  # Assert that type is R or REDCap
  if (!type %in% c("R", "REDCap")) {
    stop("validate_R_var_name: type must be either R or REDCap.")
  }

  ## Split strings into name parts

  # Get number of name parts
  maxSplits <- max(stringr::str_count(str, "\\.") + 1, na.rm = T)
  if (maxSplits < 3) {
    return(rep(FALSE, length(str)))
  }

  # Split and coerce to data frame
  strSplit <- stringr::str_split_fixed(str, "\\.", maxSplits)
  strSplit <- as.data.frame(strSplit)

  ## Check 1st part: 3 letters, all capitals
  part1 <- strSplit[, 1]
  out1 <- stringr::str_detect(part1, "^[:upper:]{3}$")

  ## Check 2nd part: Must be in type library
  part2 <- strSplit[, 2]
  if (type == "R") {
    out2 <- part2 %in% rTypes
  } else if (type == "REDCap") {
    out2 <- part2 %in% rcTypes
  }

  ## Check last part: First word is title case and others are lowercase

  # Transpose strSplit so it's a list for each variable (split into parts)
  strSplitList <- purrr::pmap(strSplit, list)

  # Get max index of non "" element for each variable
  lastPartIdx <- suppressWarnings(purrr::map_dbl(
    strSplitList,
    ~ max(which(purrr::map_lgl(., ~ . != '')))
  ))

  # Use index to return last part, else return blank string
  lastPart <- purrr::map2_chr(
    lastPartIdx,
    1:length(lastPartIdx),
    ~ ifelse(.x > 2, strSplit[.y, .x], '')
  )

  # Break last part by _ separator
  lastPart <- stringr::str_split(lastPart, '_')

  # Does first word begin with capital and then is all lower or all upper?
  lastPartFirstCheck <- purrr::map_lgl(
    lastPart,
    ~ stringr::str_detect(
      .[1],
      '^[:upper:](([[:upper:]\\d]+$)|([[:lower:]\\d]+$))'
    )
  )

  # Remove first word
  lastPartOthers <- purrr::map(
    lastPart,
    ~ .[-1]
  )

  # Check all other words
  lastPartOtherCheck <- purrr::map_lgl(
    lastPartOthers,
    ~ all( stringr::str_detect(
      ., '(^[[:upper:]\\d]+$)|(^[[:lower:]\\d]+$)') | . == '')
  )
  outLast <- lastPartFirstCheck & lastPartOtherCheck

  ## Check mid parts: alphanumeric (with first one capitalized)

  # Extract list of midParts
  midParts <- purrr::map2(
    strSplitList,
    lastPartIdx,
    ~ ifelse(.y <= 3, '', .x[3:.y])
  )

  # Check that everything passes
  outMid <- purrr::map_lgl(
    midParts,
    ~ all( stringr::str_detect(
      ., '^[:upper:](([[:upper:]\\d]+$)|([[:lower:]\\d]+$))'))
  ) | purrr::map_lgl(midParts, ~ . == '')

  # Combine all checks and return
  return(out1 & out2 & outMid & outLast)
}

#### 2.2) rename_redcap_vars ####
#' Rename a REDCap Data Frame to Follow CAM Naming Standards
#'
#' Rename a data frame downloaded from REDCap using the field annotation located
#' in the metadata data frame downloaded with the data. See [valiate_var_name()]
#' for more information on the naming standard. Warns the user for any field
#' that does not contain a new name that conforms to the standard in the field
#' annotation.
#'
#' @param rcDtf A data frame containing the data downloaded from REDCap.
#' @param metaDtf A data frame containing the meta data (data dictionary)
#' downloaded from REDCap.
#'
#' @author William Schmitt
#'
#' @return A data frame where all columns have been renamed to conform to the
#' CAM standard for R names.
#'
#' @examples
#'
#' # TODO
#'
#' @export

rename_redcap_vars <- function(rcDtf, metaDtf) {

  ## Extract new names
  metaDtf <- dplyr::mutate(
    metaDtf,
    newName = stringr::str_extract(
      field_annotation,
      "(?<=VARNAME=)[[:alnum:]\\._]+(?=(\\r\\n)?)"
    )
  )

  # Validate new names
  metaDtf <- dplyr::mutate(
    metaDtf,
    validName = validate_var_name(newName, "REDCap")
  )

  # Warn about bad names
  badNames <- dplyr::filter(metaDtf, !validName)
  purrr::walk(
    badNames$field_name,
    ~ message(paste0("WARNING: Bad name for ", ., "."))
  )

  # Build data frame for old <> new mapping
  nameMap <- data.frame(
    current = names(rcDtf)
  )

  # Get new names ready to add
  newNames <- dplyr::transmute(
    metaDtf,
    field_name = field_name,
    field_type = field_type,
    field_choices = dplyr::if_else(
      field_type == "checkbox",
      select_choices_or_calculations,
      ''
    ),
    newName = dplyr::if_else(
      validName,
      newName,
      field_name
    )
  )

  # Turn choices for checkboxes into nested dtf
  newNames <- dplyr::mutate(
    newNames,
    field_choices = dplyr::if_else(
      field_type == "checkbox",
      stringr::str_extract_all(field_choices, '[:alnum:]*(?=,\\s)'),
      list("")
    )
  )

  # unnest to get one row per check option
  newNames <- tidyr::unnest(newNames, field_choices)

  # Rename newName to work for checkboxes
  newNames <- dplyr::mutate(
    newNames,
    dplyr::across(
      c(field_name, newName),
      ~ dplyr::if_else(
        field_type == "checkbox",
        paste0(., "___", field_choices),
        .
      )
    )
  )

  # Join with current names
  nameMap <- dplyr::left_join(
    nameMap,
    dplyr::select(newNames, newName, field_name),
    by = c("current" = "field_name")
  )

  # TEMPORARY: Fill in blanks
  nameMap <- dplyr::mutate(
    nameMap,
    newName = dplyr::if_else(
      is.na(newName),
      current,
      newName
    )
  )

  # Copy over new names and return
  dtf <- rcDtf
  names(dtf) <- nameMap$newName
  return(dtf)
}

#### 3) pull_git_version ####

#' Determine Git Version for Project
#'
#' A function to extract the git version for an R project
#' that has auto-generated change logs.
#'
#' @return A character string giving the version number, in the
#'   format of MAJOR.MINOR.PATCH.
#'
#' @author Willam Schmitt
#'
#' @export

pull_git_version <- function() {

  # Default output
  out <- '1.0.0'

  # Current working directory
  cur_dir <- getwd()

  # Attempt to determine version number
  if ( exists( 'folder_pathways', envir = .GlobalEnv ) ) {

    # Go to project directory
    go_to('Project')

    # Read in json
    if ( 'package.json' %in% dir() ) {

      pckg_json <- jsonlite::read_json( 'package.json' )
      out <- pckg_json$version

    } else {

      warning( "The 'package.json' file was not found" )

    }

  } else {
    warning( paste0(
      "The list 'folder_pathways' with the paths and associated ",
      "labels was not found in the global environment - please ",
      "create this list via the 'create_folder_pathways' function"
    ), call. = F )
  }

  # Return to current directory
  setwd( cur_dir )

  return( out )
}

#### 4) source_scripts ####

#' Source in Multiple Scripts in a Folder
#'
#' A convenience function that loops through
#' and sources files stored in a folder
#' located in the current working directory
#' (typically 'R').
#'
#' @param files_to_include A vector of either...
#'   \itemize{
#'     \item Numeric indices denoting which files
#'       to include;
#'     \item A character vector with the full file names
#'       for the files to include.
#'   }
#' @param path The folder name with the scripts to source.
#'
#' @author Kevin Potter
#'
#' @export

source_scripts = function( files_to_include = NULL,
                           path = 'R' ) {

  # Folders to load
  all_files <- dir(
    path = path
  )
  if ( !is.null( files_to_include ) ) {
    if ( is.numeric( files_to_include ) ) {
      files_to_source <- all_files[ files_to_include ]
    }
    if ( is.character( files_to_include ) ) {
      files_to_source <- all_files[ all_files %in% files_to_include ]
    }
  } else {
    files_to_source <- all_files
  }

  sapply( 1:length( files_to_source ), function(i) {
    source( paste0( path, "/", files_to_source[i] ) )
  } )

}
