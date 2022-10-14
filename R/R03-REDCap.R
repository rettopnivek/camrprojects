# Functions for REDCap
# Written by...
#   Michael Pascale
#   William Schmitt
# Maintained by...
#   Michael Pascale
#   Kevin Potter
# email:
#   mppascale@mgh.harvard.edu
#   kpotter5@mgh.harvard.edu
# Please email us directly if you
# have any questions or comments
# Last updated 2022-10-14

# Table of contents
# 1) Functions to read from REDCap
#   1.1) camr_redcap_read
#   1.2) camr_redcap_download
#     1.2.1) Download Project Information
#     1.2.2) Download Raw Data
#     1.2.3) Download Metadata
#     1.2.4) Download Form-Event Map
#     1.2.5) Generate README
# 2) Functions for Naming Conventions
#   2.1) validate_var_name
#   2.2) rename_redcap_vars
#   2.3) camr_ckdict
#     2.3.1) Validate Field Name
#     2.3.2) Validate VARNAME
#     2.3.3) Check VARNAME Format
#     2.3.4) Check Field Type Against VARNAME
#     2.3.5) Check Quality Control Fields
#     2.3.6) Construct New VARNAME

#### 1) Functions to read from REDCap ####

#### 1.1) camr_redcap_read ####
#' Read All Records From a REDCap Project
#'
#' This function seeks to mimic the `redcap_read()`
#' function for the (now defunct) `REDCapR` package,but
#' with a more robust output that can handle bizarre characters
#' and calculated fields. This function handles all REST API calls
#' using httr. This function also uses calls to [furrr] and [future]
#' for downloading batches from REDCap. The [plan][future::plan()]
#' is not specified. Instead, the user should use the plan that
#' works best for their specific use.
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

camr_redcap_read = function(
    redcap_uri,
    token,
    raw_or_label = 'label',
    batch_size = 100) {

  # Download REDCap Metadata (e.g. data dictionary) to determine
  # unique identifier field.
  metaDtfResp <- camr_post_form_JSON_to_dtf(
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
  ptListResp <- camr_post_form_JSON_to_dtf(
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

#### 1.2) camr_redcap_download ####
#' Download Data From a REDCap Project
#'
#' Function to download data from a REDCap
#' Project and create a list of outputs.
#'
#' @param chr_rc_uri A character string, the URL
#'   to use for the \code{httr::POST} function.
#' @param chr_rc_token A character string, the
#'   user's API token for the specified REDCap
#'   project.
#'
#' @return A list consisting of...
#' \itemize{
#'   \item{'description'} {String. Explains the project from
#'     which the data was downloaded}.
#'   \item{'database'}{List. Information regarding the REDCap
#'     project.}
#'   \item{ 'data' }{Dataframe. All REDCap data with one
#'     datapoint per row.}
#'   \item{ 'metadata' }{Dataframe. REDCap data dictionary.}
#'   \item{ 'form_event_map' }{Dataframe. Mappings between
#'     REDCap events and instruments administered at each.}
#' }
#'
#' @export

camr_redcap_download <- function(
    chr_rc_uri = "",
    chr_rc_token = "" ) {

  if ( chr_rc_uri == "" ) {

    chr_rc_uri <- Sys.getenv('API_REDCAP_URI')

  }

  if ( chr_rc_token == "" ) {

    chr_rc_token <- Sys.getenv('API_REDCAP_TOKEN')

  }

  checkmate::assert_string(chr_rc_uri, pattern = '^https://')
  checkmate::assert_string(
    chr_rc_token, n.chars = 32, pattern = '[0-9A-F]{32}'
  )

  #### 1.2.1) Download Project Information ####

  message('Downloading project information...')
  lst_formdata <- list(
    token = chr_rc_token,
    content = 'project',
    format = 'json'
  )

  rsp_response <- httr::POST(
    chr_rc_uri,
    body = lst_formdata, encode = "form"
  )
  lst_database <- httr::content(rsp_response)


  #### 1.2.2) Download Raw Data ####

  message('Downloading data...')

  lst_formdata <- list(
    token = chr_rc_token,
    content = 'record',
    action = 'export',
    format = 'csv',             # Download in CSV Format
    type = 'eav',               # Entity-Attribute-Value/Long Form
    csvDelimiter = 'tab',
    rawOrLabel = 'label',
    rawOrLabelHeaders = 'raw',
    exportCheckboxLabel = 'false',
    exportSurveyFields = 'false',
    exportDataAccessGroups = 'false'
  )

  dtm_init <- lubridate::now()

  rsp_response <- httr::POST(
    chr_rc_uri, body = lst_formdata, encode = "form"
  )

  # Read all columns as character types. We will use metadata
  # to infer correct types later on.
  # Trim whitespace and convert empty strings to NA.
  df_data_eav <- httr::content(
    rsp_response, as = 'parsed',
    type = 'text/tab-separated-values',
    encoding = 'UTF-8',
    trim_ws = TRUE,
    col_types = 'c', na = ''
  ) |> dplyr::filter(
    !is.na(value)
  )

  #### 1.2.3) Download Metadata ####

  message('Downloading metadata...')

  lst_formdata <- list(
    token = chr_rc_token,
    content = 'metadata',
    format = 'csv'
  )

  rsp_response <- httr::POST(
    chr_rc_uri, body = lst_formdata, encode = "form"
  )

  # UTF-8 encoding must be specified or httr::content
  # may segfault on "multibyte character".
  df_metadata <- httr::content(
    rsp_response, encoding = 'UTF-8', col_types='c'
  )


  #### 1.2.4) Download Form-Event Map ####

  message('Downloading form-event map...')

  lst_formdata <- list(
    token = chr_rc_token,
    content = 'formEventMapping',
    format = 'csv'
  )

  rsp_response <- httr::POST(
    chr_rc_uri, body = lst_formdata, encode = "form"
  )
  df_form_event_map <- httr::content(
    rsp_response,
    col_types =
      readr::cols(unique_event_name = 'c', form = 'c', arm_num = 'i')
  )


  #### 1.2.5) Generate README ####

  chr_description <- stringr::str_glue(
    paste0(
      'MGH Center for Addiction Medicine\n',
      'Title (IRB No. XXXXXXXXXXX)\n',
      '\n',
      'Unprocessed REDCap dataset.\n',
      '\n',
      'PID:        { lst_database$project_id }\n',
      'Timestamp:  { dtm_init |> with_tz(\'UTC\') ',
      '|> format_ISO8601(precision=\'ymdhms\', usetz=TRUE) }\n',
      'Username:   { Sys.getenv(\'USER\', Sys.getenv(\'USERNAME\')) }\n',
      'Contents:   { nrow(df_data_eav) } datapoints\n'
    ) )

  out <- list(
    description=chr_description,
    database=lst_database,
    data=df_data_eav |>
      dplyr::left_join(df_metadata |> dplyr::select(
        field_name, form_name), by='field_name'),
    metadata=df_metadata,
    form_event_map=df_form_event_map
  )

  return( out )
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
#' Rename a data frame downloaded from REDCap using the field annotation
#' located in the metadata data frame downloaded with the data. See
#' [valiate_var_name()] for more information on the naming standard.
#' Warns the user for any field that does not contain a new name that
#' conforms to the standard in the field annotation.
#'
#' @param rcDtf A data frame containing the data downloaded from REDCap.
#' @param metaDtf A data frame containing the meta data (data dictionary)
#' downloaded from REDCap.
#'
#' @author William Schmitt
#'
#' @return A data frame where all columns have been renamed to conform to
#' the CAM standard for R names.
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

#### 2.3) camr_ckdict ####
#' Verify REDCap data dictionary.
#'
#' @param dict REDCap metadata table as downloaded by [redcap_read()].
#'
#' @author Michael Pascale
#'
#' @export

camr_ckdict <- function (
    dict ) {

  dict[is.na(dict)] <- ''
  field_prefixes_list <- list()
  var_instrument_list <- list()
  var_name_list <- c()
  issue_list <- data.frame(
    severity = integer(),
    issue = character(),
    field = character(),
    variable = character()
  )

  add_issue <- function(
    issue_list,
    severity_level,
    field_name,
    field_type,
    description,
    variable = "" ) {

    new_issue <- list(
      severity = severity_level,
      field = field_name,
      type = field_type,
      issue = paste(
        description, collapse = ""
      ),
      variable = variable
    )

    out <- dplyr::bind_rows(
      issue_list,
      new_issue
    )

    return( out )
  }

  for (i in 1:nrow(dict)) {

    row <- dict[i,]
    field_type <- row$field_type
    field_name <- row$field_name
    field_instrument <- row$form_name
    field_choices <- row$select_choices_or_calculations
    field_annotation <- row$field_annotation

    #### 2.3.1) Validate Field Name ####

    if (stringr::str_length(field_name) > 26) {

      issue_list <- add_issue(
        issue_list,
        3, field_name, field_type,
        c( 'Field name is greater than 26 characters long.' )
      )

    }

    field_prefix <- stringr::str_match(field_name, '^[:alnum:]+')
    field_suffix <- stringr::str_match(field_name, '_[:alnum:]+$')

    if (is.na(field_prefix) || is.na(field_suffix)) {

      issue_list <- add_issue(
        issue_list,
        2, field_name, field_type,
        c( 'Field name is not of standard format.' )
      )

    }

    #### 2.3.2) Validate VARNAME ####

    if (field_type != 'descriptive') {
      var_name <-
        stringr::str_match(field_annotation, '(?<=VARNAME=)[\\w\\.]+')

      if (is.na(var_name)) {

        issue_list <- add_issue(
          issue_list,
          2, field_name, field_type,
          c( 'Field has no VARNAME defined in its field annotations.' )
        )

      }

      var_decomposed <-
        stringr::str_match(
          var_name, '(\\w+?)\\.(\\w+?)\\.(?:(\\w+?)\\.)?(.*)'
        )
      var_group <- var_decomposed[2]
      var_type <- var_decomposed[3]
      var_instrument <- var_decomposed[4]
      var_tail <- var_decomposed[5]

      #### 2.3.3) Check VARNAME Format ####

      if(any(is.na(c(var_group, var_type, var_tail)))) {

        issue_list <- add_issue(
          issue_list,
          2, field_name, field_type,
          c( 'Field has an impropertly formatted ',
             'VARNAME in its field annotations.'),
          var_name
        )

        next
      }

      if(!is.element(var_group, c('IDS', 'SBJ', 'INV', 'QCC'))) {

        issue_list <- add_issue(
          issue_list,
          2, field_name, field_type,
          c( 'Field has an invalid group tag ',
             'in its VARNAME annotation.'),
          var_name
        )

      }

      content <- c('DAT', 'INT', 'DBL', 'CHR', 'CLC',
                   'MCQ', 'CKB', 'YNQ', 'FLE', 'VAS', 'DSC')

      if(!is.element(var_type, content)) {

        issue_list <- add_issue(
          issue_list,
          2, field_name, field_type,
          c( 'Field has an invalid type tag in ',
             'its VARNAME annotation.'),
          var_name
        )

      }

      if (!is.na(var_instrument)) {
        if (is.null(unlist(field_prefixes_list[field_prefix]))) {
          field_prefixes_list[field_prefix] = var_instrument
        } else {
          if (field_prefixes_list[field_prefix] != var_instrument) {

            issue_list <- add_issue(
              issue_list,
              2, field_name, field_type,
              c( 'Field has an instrument tag in its VARNAME ',
                 'annotation which is inconsistent with other ',
                 'field of the same prefix.'),
              var_name
            )

          }
        }

        if (is.null(unlist(var_instrument_list[var_instrument]))) {
          var_instrument_list[var_instrument] = field_prefix
        } else {
          if (var_instrument_list[var_instrument] != field_prefix){

            issue_list <- add_issue(
              issue_list,
              2, field_name, field_type,
              c( 'Instrument tag is utilized with multiple ',
                 'different field prefixes.'),
              var_name
            )

          }
        }
      }

      var_name_list <- c(var_name_list, var_name)

      #### 2.3.4) Check Field Type Against VARNAME ####

      if (field_type == 'text') {
        validation <- row$text_validation_type_or_show_slider_number

        if ( !is.na(validation) &&
             any(
               (validation == '' && var_type != 'CHR'),
               (validation == 'integer' && var_type != 'INT'),
               (validation == 'number' &&
                !is.element(var_type, c('INT', 'DBL'))),
               (stringr::str_detect(validation, 'number\\ddp') &&
                var_type != 'DBL'),
               (stringr::str_detect(validation, '^(date|time)') &&
                var_type != 'DAT'),
               (is.element(validation, c('alpha_only', 'email',
                                         'phone', 'zipcode', 'ssn',
                                         'mrn_10d')) && var_type != 'CHR')
             )
        ) {

          issue_list <- add_issue(
            issue_list,
            2, field_name, field_type,
            c( 'Field validation does not correspond ',
               'with type tag in VARNAME annotation.'),
            var_name
          )

        }


      } else if (field_type == 'radio') {

        pairs <- stringr::str_split(field_choices, '\\|')
        pairs <- sapply(pairs, stringr::str_trim)
        pairs <- sapply(pairs, stringr::str_match, '^\\d+,\\s*\\d+$')
        integer_choices <- all(!is.na(pairs))

        if (var_type == 'INT') {

          if (!integer_choices)
            issue_list <- add_issue(
              issue_list,
              2, field_name, field_type,
              c( 'Field lacks integer choices despite ',
                 'type tag in its VARNAME annotation.'),
              var_name
            )

        } else if (var_type != 'MCQ') {
          issue_list <- add_issue(
            issue_list,
            2, field_name, field_type,
            c( 'Field validation does not correspond ',
               'with type tag in VARNAME annotation. ',
               'Recommend MCQ.'),
            var_name
          )
        } else if (integer_choices) {
          issue_list <- add_issue(
            issue_list,
            3, field_name, field_type,
            c( 'Field has only integer choices despite ',
               'type tag in its VARNAME annotation.'),
            var_name
          )
        }

      } else if (field_type == 'checkbox') {

        if (var_type != 'CKB')
          issue_list <- add_issue(
            issue_list,
            2, field_name, field_type,
            c( 'Field validation does not correspond with ',
               'type tag in VARNAME annotation. Recommend CKB.'),
            var_name
          )

      } else if (field_type == 'yesno') {

        if (var_type != 'YNQ')
          issue_list <- add_issue(
            issue_list,
            2, field_name, field_type,
            c( 'Field validation does not correspond with type tag ',
               'in VARNAME annotation. Recommend YNQ.'),
            var_name
          )

      } else if (field_type == 'dropdown') {

        if (var_type != 'MCQ')
          issue_list <- add_issue(
            issue_list,
            2, field_name, field_type,
            c( 'Field validation does not correspond with type tag',
               ' in VARNAME annotation. Recommend MCQ.'),
            var_name
          )

      } else if (field_type == 'calc') {

        if (var_type != 'CLC')
          issue_list <- add_issue(
            issue_list,
            2, field_name, field_type,
            c( 'Field validation does not correspond with type tag ',
               'in VARNAME annotation. Recommend CLC.'),
            var_name
          )

      } else if (field_type == 'file') {

        if (var_type != 'FLE')
          issue_list <- add_issue(
            issue_list,
            2, field_name, field_type,
            c( 'Field validation does not correspond with type tag ',
               'in VARNAME annotation. Recommend FLE.'),
            var_name
          )

      } else if (field_type == 'notes') {

        if (var_type != 'CHR')
          issue_list <- add_issue(
            issue_list,
            2, field_name, field_type,
            c( 'Field validation does not correspond with type tag ',
               'in VARNAME annotation. Recommend CHR.'),
            var_name
          )

      } else {
        issue_list <- add_issue(
          issue_list,
          1, field_name, field_type,
          c( 'Field validation invalid.'),
          var_name
        )
      }

      #### 2.3.5) Check Quality Control Fields ####

      if (!is.na(field_suffix)) {
        if (field_suffix == 'compby') {

          if (var_group != 'QCC' || var_type != 'CHR'
              || var_tail != 'compby')
            issue_list <- add_issue(
              issue_list,
              1, field_name, field_type,
              c( 'Field is for quality control but VARNAME annotation ',
                 'is not of form QCC.CHR.XXXX.compby.'),
              var_name
            )

        } else if (field_suffix == 'ptinitials') {

          if (var_group != 'QCC' || var_type != 'CHR' ||
              var_tail != 'ptinitials')
            issue_list <- add_issue(
              issue_list,
              1, field_name, field_type,
              c( 'Field is for quality control but VARNAME annotation ',
                 'is not of form QCC.CHR.XXXX.ptinitials.'),
              var_name
            )

        } else if (field_suffix == 'date') {

          if (var_group != 'QCC' || var_type != 'DAT' || var_tail != 'date')
            issue_list <- add_issue(
              issue_list,
              1, field_name, field_type,
              c( 'Field is for quality control but VARNAME annotation is ',
                 'not of form QCC.CHR.XXXX.date.'),
              var_name
            )

        } else if (field_suffix == 'ptinitials_date') {

          if (var_group != 'QCC' || var_type != 'DAT' ||
              var_tail != 'ptinitials_date')
            issue_list <- add_issue(
              issue_list,
              1, field_name, field_type,
              c( 'Field is for quality control but VARNAME annotation is ',
                 'not of form QCC.CHR.XXXX.ptinitials_date.'),
              var_name
            )

        }
      }

      #### 2.3.6) Construct New VARNAME ####

      new_var_name <- stringr::str_c(
        na.omit(c(var_group, var_type,
                  var_instrument, var_tail)), collapse='.')
      new_annotation <- stringr::str_replace(
        field_annotation, '(?<=VARNAME=)[\\w\\.]+', new_var_name)
    }
  }

  for (dup in  dict$field_name[which(duplicated(dict$field_name))])
    issue_list <- add_issue(
      issue_list,
      1, dup, "",
      c( 'Field name not unique.')
    )

  for (dup in var_name_list[which(duplicated(var_name_list))])
    issue_list <- add_issue(
      issue_list,
      1, "", "",
      c( 'VARNAME not unique.'),
      dup
    )

  calculation_references <- unlist(stringr::str_match_all(
    dict$select_choices_or_calculations, '(?<=\\[)\\w+(?=\\])'))
  for (ref in calculation_references[
    which(!(calculation_references %in% dict$field_name))
  ])
    issue_list <- add_issue(
      issue_list,
      1, ref, "",
      c( 'Field does not exist but is referenced in a calculation.')
    )

  branching_references <- unlist(stringr::str_match_all(
    dict$branching_logic, '(?<=\\[)\\w+(?=\\])'))
  for (ref in branching_references[
    which(!(branching_references %in% dict$field_name))])
    issue_list <- add_issue(
      issue_list,
      1, ref, "",
      c( 'Field does not exist but is referenced in branching logic.')
    )

  return( issue_list )
}

