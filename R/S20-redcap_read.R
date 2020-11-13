#' Read all records from a REDCap project
#'
#' This function seeks to mimic the [REDCapR::redcap_read()] function,
#' but with a more robust output that can handle bizarre characters
#' and calculated fields. Instead of relying on base R HTTP protocols,
#' this function handles all REST API calls using RCurl.
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
  metaDatJSON <- RCurl::postForm(
    uri = redcap_uri,
    token = token,
    content = 'metadata',
    format = 'json',
    returnFormat = 'json'
  )

  # Remove invalid chars (\r\n -- carriage return, or \t -- tabs)
  metaDatJSON <- gsub('[\r\n]', '', metaDatJSON)
  metaDatJSON <- gsub('[\t]', '', metaDatJSON)

  # Convert to data frame
  metaDat <- jsonlite::fromJSON(metaDatJSON)

  # Extract unique identifier field
  uniId <- metaDat[1, 1]

  # Notify user
  message(paste(
    'REDCap Metadata Download.',
    'Unique field identified:',
    uniId
  ))

  # Use unique id to get list of participants
  ptListJSON <- RCurl::postForm(
    uri = redcap_uri,
    token = token,
    content = 'record',
    format = 'json',
    returnFormat = 'json',
    type = 'flat',
    fields = uniId
  )
  ptList <- jsonlite::fromJSON(ptListJSON)

  # Determine number of batches to download
  ptList <- unique(ptList[, 1])
  numBatches <- ceiling(length(ptList)/batch_size)

  # Notify user
  message(paste(
    length(ptList),
    'records identified.',
    'Downloading',
    numBatches,
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

  # Download each batch
  dat <- data.frame()
  for (i in 1:numBatches) {
    message(paste(
      '  Downloading Batch',
      i,
      'of',
      numBatches
    ))

    # Calculate range
    minId <- (i - 1) * batch_size + 1
    maxId <- min(i*batch_size, length(ptList))

    # Build list of record #s to filter data
    curIds <- as.list(ptList[minId:maxId])
    curIdNames <- paste0(
      'records[',
      0:(length(curIds) - 1),
      ']'
    )
    names(curIds) <- curIdNames

    # Create parameter vector
    paramList <- append(baseParams, curIds)
    datJSON <- RCurl::postForm(
      uri = redcap_uri,
      .params = paramList
    )

    # Remove invalid chars (\r\n -- carriage return, or \t -- tabs)
    datJSON <- gsub('[\r\n]', '', datJSON)
    datJSON <- gsub('[\t]', '', datJSON)
    curDat <- jsonlite::fromJSON(datJSON)
    dat <- rbind(dat, curDat)
  }

  # Package output
  message('Done.')
  out <- list(data = dat, success = 1)

  return(out)
}

