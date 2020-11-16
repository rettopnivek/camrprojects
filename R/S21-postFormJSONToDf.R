#' Sends a POST Form via httr and converts JSON return to a data.frame
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
