#' Create a codebook for a target dataframe using codebook yaml
#'
#' @description
#' Matches variables in target dataframe with variable info stored in a directory
#' of yaml files. Outputs a dataframe with the names of all the variables and
#' codebook info (what instrument it's from, the meaning of its values, etc).
#'
#' @author Zach Himmelsbach
#'
#' @export
#' @importFrom yaml read_yaml
#'
#' @param df Target dataframe. Output will contain a row for every variable
#'           in this dataframe.
#' @param yaml_dir Directory containing yaml codebook files for your project
#'
#' @returns A dataframe containing the codebook.
camr_gen_codebook <- function(df,
                         yaml_dir) {

  ## Search yamls for varname ----
  yaml_map <- sapply(colnames(df),
                     function(var) search_yaml_for_var(var, yaml_dir))

  ## Get info required for codebook ----
  yaml_info <- purrr::map2(colnames(df),
                           yaml_map,
                           extract_yaml) |> setNames(colnames(df)) |>
    (\(list_of_rows) do.call(rbind, list_of_rows))() |> as.data.frame()

  ## Clean up output ----
  codebook <- clean_codebook_df(yaml_info)

  return(codebook)
}

#' Check codebook for consistency with data
#'
#' @description
#' Check that dataframe data matches expectations established by codebook.
#' Intended for use during initial generation of codebooks and also after
#' later updates of codebook. Prints warnings if matching expectations are not met.
#'
#' @export
#'
#' @param data_df A dataframe of data that should match the codebook
#' @param codebook_df A dataframe containing the codebook
#'
#' @returns (Invisibly) Logical vector. Indicates whether each variable in
#' `data_df` contains unexpected values (based on the codebook).
camr_check_codebook <- function(data_df, codebook_df) {
  # Check that all variables are represented in codebook
  missing_vars <- setdiff(colnames(data_df), unique(codebook_df$Variable))
  if (length(missing_vars) > 0) {
    warning(sprintf("These variables were not found in the codebook: %s",
                    paste(missing_vars, collapse = ", ")))
  }

  # Check that essential entries are non-missing
  missing_values <- codebook_df$Variable[is.na(codebook_df$Values)]
  if (length(missing_values) > 0) {
    stop(sprintf("The following variables do not have value or data type info in the codebook: %s",
                 paste(missing_values, collapse = ", ")))
  }

  # Check that unique values match codebook values
  check_values <- function(varname) {
    expected_values <- codebook_df[codebook_df$Variable == varname, ] |>
      dplyr::pull(Values)
    if (expected_values %in% c("numeric", "character")) {
      if (class(data_df[varname]) != expected_values) {
        warning(sprintf("%s is not the declared data type.", varname))
        return(FALSE)
      }
      else return(TRUE)
    }
    else {
      if (!all(data_df[varname] %in% c(expected_values, NA))) {
        warning(sprintf("Undeclared values found in %s", varname))
        warning(sprintf("The following values are in the data but not the codebook: %s",
                        paste(setdiff(unique(data_df[varname]), expected_values), collapse = ", ")))
        return(FALSE)
      }
      else return(TRUE)
    }
  }
  value_match <- sapply(colnames(data_df), check_values)

  return(invisible(value_match))
}

#' Find the yaml file that contains target variable
#'
#' @description Search a yaml codebook file for a target variable
#'
#' @importFrom yaml read_yaml
#'
#' @param varname String. Name of target variable.
#' @param yaml_dir String. Directory containing yaml codebooks to search
#' @param priority_yamls Character vector. Ordered vector of yaml files to
#' prefer in cases where the variable is found in multiple files. The first
#' element of this vector has highest priority. Defaults to
#' `c(df_sl_subject_ids.yaml, df_vl_visits.yaml)`
#'
#' @returns String. The name of the yaml file containing info on `varname`.
search_yaml_for_var <- function(varname,
                                yaml_dir,
                                priority_yamls = c(
                                  "df_sl_subject_ids.yaml",
                                  "df_vl_visits.yaml"
                                )) {
  # get all the yamls
  yamls <- list.files(path = yaml_dir,
                      full.names = FALSE)
  # get yamls with target variable
  ymls_with_var <- yamls[sapply(fs::path(yaml_dir, yamls),
                                function(yml) varname %in% names(yaml::read_yaml(yml)$variables))]

  if (length(ymls_with_var) == 1) return(fs::path(yaml_dir, ymls_with_var))
  else if (any(priority_yamls %in% ymls_with_var)) {
    return(fs::path(yaml_dir, priority_yamls[priority_yamls %in% ymls_with_var][1]))
  }
  else if (length(ymls_with_var) == 0) {
    warning(sprintf("%s not found in any yaml", varname))
    return(NULL)
  }
  else {
    warning(sprintf("%s found in multiple files: %s",
                    varname, paste(ymls_with_var, collapse = ", ")))
    warning("Returning first file found")
    return(fs::path(yaml_dir, ymls_with_var[1]))
  }

}

#' Extracted needed info from yaml
#'
#' @description
#' Grabs variable and form info, for a target variable, from a yaml codebook file
#'
#' @importFrom yaml read_yaml
#'
#' @param varname String. Name of target variable.
#' @param yaml_file String. Path to yaml file containing info on `varname`
#'
#' @returns List. List of info on target variable from yaml codebook file.
extract_yaml <- function(varname, yaml_file) {
  form_info <- yaml::read_yaml(yaml_file)[c("REDCap_form", "codebook_title")]
  var_info <- yaml::read_yaml(yaml_file)$variables[[varname]]
  return(c(form_info, var_info))
}

#' Clean codebook dataframe
#'
#' @description
#' Takes in a codebook dataframe and processes it to a standard, clean form
#' with fixed set of columns.
#'
#' @param df A dataframe built from yaml codebooks. Expects the row names
#' to be the names of variables.
#'
#' @returns A dataframe with a row for each variable.
clean_codebook_df <- function(df) {
  ## Move varnames from row names into a column ----
  df$Variable <- row.names(df)
  row.names(df) <- NULL

  ## Combine description and item wording ----
  df$description[df$description == "..." | is.na(df$description)] <- ""
  df$item_wording[df$item_wording == "..." | is.na(df$item_wording)] <- ""

  df$`Variable Info` <- paste(df$description, "\n", df$item_wording)

  ## Rename columns ----
  df <- df |> dplyr::rename(Form = codebook_title,
                            `Data Type` = data_type,
                            Values = unique_values)

  ## Combine data type and value info
  df <- df |> dplyr::mutate(Values = ifelse(Values == "..." | is.na(Values),
                                            `Data Type`,
                                            Values))

  ## Keep only needed columns ----
  df <- df |> dplyr::select(Variable, Form,
                            `Variable Info`, Values,
                            REDCap_form, redcap_source_vars)

  return(df)

}
