% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/R14-Yaml_codebook.R
\name{camr_map_targets_to_forms}
\alias{camr_map_targets_to_forms}
\title{Map targets objects to the REDCap forms they are processed from}
\usage{
camr_map_targets_to_forms(
  api_token_path,
  src_dir = "src",
  exclude_targets = c("rds_download", "chr_path_redcap_data", "chr_path_tlfb_data",
    "lst_redcap_data", "df_tlfb_raw", "df_id_table", "df_redcap_raw")
)
}
\arguments{
\item{api_token_path}{file path to txt file containing redcap project token}

\item{src_dir}{Directory to search for target-generating functions. Defaults
to "src" (in accordance with the standard processing pipeline)}

\item{exclude_targets}{character vector of targets object to skip for mapping.
See default for typically excluded objects (these are file targets or won't
map 1-to-1)}
}
\value{
a named character vector with mappings from targets objects to
REDCap forms. The names store the target object names and the values in the
vector are the REDCap forms.
}
\description{
Get mapping from targets to REDCap forms. This utility finds the function
used to generate a target and then - within that function - identifies the
REDCap form associated with that targets object
}
\author{
Zach Himmelsbach
}
