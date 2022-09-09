# Input and output
# Written by...
#   William Schmitt
# Maintained by...
#   Michael Pascale
#   Kevin Potter
# Email:
#   mppascale@mgh.harvard.edu
#   kpotter5@mgh.harvard.edu
# Please email us directly if you
# have any questions or comments
# Last updated: 2022-09-08

# Table of contents
# 1) camr_post_form_JSON_to_df

#### 1) camr_post_form_JSON_to_df ####
#' Sends a POST Form via httr and Converts JSON Return to a Data Frame
#'
#' This function uses httr to submit a POST form to the given url and
#' converts the response (expected to be a JSON array) to a data.frame.
#' Returns a list containing the data frame and response from the server.
#'
#' @param url The URL where the POST request should be submitted.
#' @param params A named list of parameters to submit with the POST
#' request.
#'
#' @author William Schmitt
#'
#' @return A list which contains the following elements:
#' * `data`: A data frame containing all records and all fields.
#' * `status`: The HTTP status code returned from the request. If an
#'   error was encountered after the request, returns -1.
#'
#' @export
#' @md

camr_post_form_JSON_to_df <- function(
    url,
    params ) {

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
