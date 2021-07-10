# ...
# Written by...
#   Eylul Akman
#   Kevin Potter
# email: kevin.w.potter@gmail.com
# Please email me directly if you
# have any questions or comments
# Last updated 2021-05-26

# Table of contents
# 1) process_comm

#### 1) process_comm ####
#' Process data from the COMM questionnaire
#'
#' This function scores the COMM questionnaire by summing the individual item
#' ratings and providing the subsequent indication. The function takes in the
#' processed data frame of REDCap data, subsets just its COMM variables, scores
#' the COMM (creating a sum and indication column), then adds these columns
#' back to the larger processing data frame.
#'
#' @param rc_dtf The unprocessed, direct from REDCap data frame. Required.
#'
#' @param dtf The processed data frame that you can assume has the
#' necessary session details. Required.
#'
#' @return `dtf` with new columns that have meta-data (i.e. attributes)
#' filled out.
#'
#' @author Eylul Akman
#'
#' @export
#' @md

# adds sum & indication columns for COMM score to data
process_comm = function( comm_dtf ) {

  # sums all item ratings
  comm_dtf <- dplyr::transmute(
    comm_dtf,
    INV.INT.COMM.Sum = rowSums(dplyr::across(
      dplyr::starts_with('INV.INT.COMM.Item')))
  )

  # indicates positive or negative score based on sum
  comm_dtf$INV.CHR.COMM.Indication <- ifelse(
    comm_dtf$INV.INT.COMM.Sum >= 9,
    '+', '-'
  )

  return(comm_dtf)
}
