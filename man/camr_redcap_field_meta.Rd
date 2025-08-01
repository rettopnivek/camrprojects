% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/R03-REDCap.R
\name{camr_redcap_field_meta}
\alias{camr_redcap_field_meta}
\title{Get REDCap project field metadata as a cleaned dataframe}
\usage{
camr_redcap_field_meta(api_token_path)
}
\arguments{
\item{api_token_path}{path to text file containing REDCap project API token}
}
\value{
a dataframe containing metadata on the fields from the REDCap project
}
\description{
Pull project field metadata via the REDCap API and clean it up to be useful.
The output contains field names, field types, answer choices, branching logic,
etc
}
\author{
Zach Himmelsbach
}
