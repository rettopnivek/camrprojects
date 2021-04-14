#' Read all records from a REDCap project
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

