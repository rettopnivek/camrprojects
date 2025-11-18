#' Process REDCap variables into standard CAM variables
#'
#' @description
#' Takes in a dataframe where column names are REDCap variable names. Processes
#' data according to field metadata (e.g. Dropdown and Radio variables are
#' converted to factors). Only variables that follow the REDCap variable convention
#' (i.e. they contain "_") will be processed. Other variables (e.g. IDS.CHR.Subject),
#' will be left alone. Checklist variables are split into multiple indicator variables.
#'
#' @param df A dataframe. The column names must follow the CAM REDCap varname
#' convention (`form_name`_`fieldname`).
#' @param prefix A string. The first part of the new variable names. Usually
#' this will be "INV" (the default), but sometimes you will want to use "SBJ".
#' @param api_token_path. A string. The path to your REDCap project API token.
#' You can add it to your project .Renviron file as "API_TOKEN_FILE" and the
#' default will pick it up automatically.
#' @param custom_form_name A string. A custom form name to be used in place
#' of the automatically derived form name
#' (useful when e.g. combining multiple forms, like constructing
#' visit info from the start of visit AND end of visit forms). If `NULL`, the form
#' name is automatically derived from the REDCap variable name.
#'
#' @return A dataframe with variables processed and renamed according to CAM
#' standards.
#'
#' @export
#' @importFrom dplyr mutate select rename left_join all_of row_number
#' @importFrom tidyr separate_rows pivot_wider
#' @importFrom rlang .data
#'
#'
camr_process_redcap_vars <- function(df,
                                     prefix = "INV",
                                     api_token_path = Sys.getenv("API_TOKEN_FILE"),
                                     custom_form_name = NULL) {
  ## Load metadata ----
  redcap_var_metadata <- camrprojects::camr_redcap_field_meta(api_token_path)

  ## Make sure metadata has the expected columns ----
  stopifnot(all(c("field_name", "field_type") %in% names(redcap_var_metadata)))

  ## Select variables to process ----
  # REDCap vars include "_" by convention
  # This will skip over variables that are already processed (e.g. IDS.CHR.Subject)
  vars_to_process <- colnames(df)[grepl("_", colnames(df))]

  ## Process variables ----
  for (v in vars_to_process) {
    print(v)
    df <- process_redcap_var(df = df,
                             var_name = v,
                             redcap_var_metadata = redcap_var_metadata,
                             prefix = prefix,
                             custom_form_name = custom_form_name)
  }

  return(df)
}

#' Function to process individual variables in a script
#'
#' @description
#' This function applies the same processing as `camr_process_redcap_vars`,
#' but allows it to be called on a single variable, and just returns the data
#' (so it can be used e.g. inside calls to `dplyr::mutate`).
#' It also skips the use of automated naming.
#' Note: this does NOT work for checklist variables.
#'
#' @param redcap_varname A string. The name of the redcap variable.
#' @param api_token_path. A string. Path to a .txt file containing the redcap
#'                        project API token.
#' @returns A vector of the variable processed according to `camr_process_redcap_vars`.
#'
#' @export
#'
camr_process_redcap_var <- function(x,
                                    api_token_path = Sys.getenv("API_TOKEN_PATH")) {

  # Capture the column name as a string
  var_name <- rlang::as_name(rlang::ensym(x))

  # Create a temporary one-column data frame
  df_tmp <- data.frame(tmp = unlist(x), stringsAsFactors = FALSE)
  names(df_tmp) <- var_name

  # Load metadata
  meta <- camr_redcap_field_meta(api_token_path)

  # Call your full processing function
  out <- process_redcap_var(
    df = df_tmp,
    var_name = var_name,
    redcap_var_metadata = meta,
    prefix = "ABC",
    custom_form_name = NULL
  )

  # Return processed vector (mutate needs a vector)
  out[[1]]
}


#' Helper function to process individual variables
#'
#' @description
#' Take in a dataframe and process a single variable to CAM standards.
#'
#' @param df A dataframe.
#' @param var_name A string. The name of the variable to be processed.
#' @param redcap_var_metadata A dataframe. The output of
#' `camrprojects::camr_redcap_field_meta(api_token_path)`. Contains info about
#' variable types and text field validation. Used to determine how a variable
#' should be processed (e.g. dropdowns are converted to factors)
#' @param prefix A string. The first part of the output variable name. Usually
#' this will be "INV" but sometimes other choices will be used (e.g. "SBJ" or "IDX")
#' @param custom_form_name A string. A custom form name to be used in place
#' of the automatically derived form name
#' (useful when e.g. combining multiple forms, like constructing
#' visit info from the start of visit AND end of visit forms). If `NULL`, the form
#' name is automatically derived from the REDCap variable name.
#'
#' @returns A dataframe with `var_name` processed and renamed according to CAM standards.
#'
#' @importFrom dplyr mutate select rename left_join all_of row_number
#' @importFrom tidyr separate_rows pivot_wider
#' @importFrom rlang .data
#'
process_redcap_var <- function(df,
                               var_name,
                               redcap_var_metadata,
                               prefix,
                               custom_form_name) {

  ## Get relevant metadata for this variable ----
  meta_row <- redcap_var_metadata[redcap_var_metadata$field_name == var_name, ]
  var_type <- meta_row[["field_type"]]

  ## Process variable (depending on REDCap field type) ----
  # Yes/no vars are converted to logical
  if (var_type == "yesno") {
    df <- df |> dplyr::mutate(!!var_name := as.logical(as.numeric(.data[[var_name]])))
    data_type <- "LGL"
  }
  # Calculated fields are numeric
  else if (var_type == "calc") {
    df <- df |> dplyr::mutate(!!var_name := as.numeric(.data[[var_name]]))
    data_type <- "NUM"
  }
  # Descriptive fields are removed
  else if (var_type %in% c("descriptive", "file")) {
    df <- df |> dplyr::select(-dplyr::all_of(var_name))
    return(df)
  }
  # Radio and dropdowns are factors
  else if (var_type %in% c("radio", "dropdown")) {
    answer_choices <- meta_row[["answer_choices"]] |> unlist()
    df <- df |> dplyr::mutate(!!var_name := factor(.data[[var_name]],
                                                   levels = names(answer_choices),
                                                   labels = answer_choices))
    data_type = "FCT"
  }
  # Sliders are numeric
  else if (var_type == "slider") {
    df <- df |> dplyr::mutate(!!var_name := as.numeric(.data[[var_name]]))
    data_type <- "NUM"
  }
  # Text inputs depend on validation info
  else if (var_type %in% c("text", "notes")) {
    # Get validation info
    validation <- meta_row[["text_validation_type_or_show_slider_number"]]

    if (validation == "integer") {
      df <- df |> dplyr::mutate(!!var_name := as.integer(.data[[var_name]]))
      data_type <- "INT"
    }
    else if (validation %in% c("number", "number_2dp")) {
      df <- df |> dplyr::mutate(!!var_name := as.numeric(.data[[var_name]]))
      data_type <- "DBL"
    }
    else if (validation == "date_mdy") {
      df <- df |> dplyr::mutate(!!var_name := lubridate::ymd(.data[[var_name]]))
      data_type <- "DAT"
    }
    else if (validation == "datetime_mdy") {
      df <- df |> dplyr::mutate(!!var_name := lubridate::ymd_hm(.data[[var_name]]))
      data_type <- "DTM"
    }
    else if (validation == "time") {
      df <- df |> dplyr::mutate(!!var_name := lubridate::hm(.data[[var_name]]))
      data_type <- "TIME"
    }
    else {
      df <- df |> dplyr::mutate(!!var_name := as.character(.data[[var_name]]))
      data_type <- "CHR"
    }
  }
  # Checklists are broken into separate variables
  else if (var_type == "checkbox") {
    answer_choices <- meta_row[["answer_choices"]]
    df <- make_indicators(df, var_name, answer_choices, prefix, custom_form_name)
    data_type <- NULL
  }

  ## Rename variable (except checkboxes) ----
  # Checkbox renaming is handled within `make_indicators`
  form_name <- stringr::str_extract(var_name, "^[^_]+")
  if (!is.null(custom_form_name)) {
    form_name <- custom_form_name
  }
  field_name <- sub(paste0(form_name, "_"), "", var_name) |>
    stringr::str_replace_all("_", " ") |>
    stringr::str_to_title() |>
    stringr::str_replace_all(" ", "")
  new_var_name <- paste(prefix, data_type, toupper(form_name), field_name, sep=".")

  if (!is.null(data_type)) {
    df <- df |> dplyr::rename(!!new_var_name := var_name)
  }

  return(df)
}

#' Helper function to process and rename checkbox vars
#'
#' @description
#' Take in a df and process a single checklist REDCap variable. The variable is
#' split into a set of indicator (logical) columns, one for each checkbox.
#'
#' @param df A dataframe.
#' @param var_name A string. The name of the variable in `df` to be processed.
#' @param answer_choices A list. The possible checkboxes.
#' @param prefix A string. The first part of the output variable name. Usually
#' this will be "INV" but sometimes other choices will be used (e.g. "SBJ" or "IDX")
#' @param custom_form_name A string. A custom form name to be used in place
#' of the automatically derived form name
#' (useful when e.g. combining multiple forms, like constructing
#' visit info from the start of visit AND end of visit forms). If `NULL`, the form
#' name is automatically derived from the REDCap variable name.
#' @param data_type A string. The data type to be used in renaming. Defaults to "LGL".
#' @param sep A string. The character that separates the checkbox choices in the
#' raw REDCap data. Defaults to "\\|" as the data come in e.g. "1|3|5|6"
#'
#' @importFrom dplyr mutate select rename left_join all_of row_number
#' @importFrom tidyr separate_rows pivot_wider
#' @importFrom rlang .data
#'
make_indicators <- function(df,
                            var_name,
                            answer_choices,
                            prefix,
                            custom_form_name,
                            data_type = "LGL",
                            sep = "\\|") {

  # Extract form name (e.g., "pscn")
  form_name <- stringr::str_extract(var_name, "^[^_]+") |>
    stringr::str_to_upper()

  if (!is.null(custom_form_name)) {
    form_name <- custom_form_name
  }

  # Extract core variable name (e.g., "substance_used")
  core_var <- stringr::str_remove(var_name, "^[^_]+_")

  # Extract all choice names
  all_choices <- names(unlist(answer_choices))

  # Step 1: Create the wide indicator matrix
  indicators <- df |>
    dplyr::mutate(row_id = dplyr::row_number()) |>
    dplyr::select(row_id, dplyr::all_of(var_name)) |>
    tidyr::separate_rows(dplyr::all_of(var_name), sep = sep) |>
    dplyr::mutate(value = TRUE) |>
    tidyr::pivot_wider(
      names_from = dplyr::all_of(var_name),
      values_from = value,
      values_fill = FALSE
    )

  # Step 2: Ensure all possible choice columns exist
  missing_cols <- setdiff(all_choices, names(indicators))
  if (length(missing_cols) > 0) {
    indicators[missing_cols] <- FALSE
  }

  # Step 3: Reorder columns
  indicators <- indicators |>
    dplyr::select(row_id, dplyr::all_of(all_choices))

  # Step 4: Rename columns properly
  base_name <- core_var |>
    stringr::str_to_title() |>
    stringr::str_replace_all("_", "") |>
    stringr::str_replace_all(" ", "")

  new_names <- paste0(prefix, ".",
                      data_type, ".",
                      form_name, ".",
                      base_name, ".", all_choices)
  colnames(indicators)[-1] <- new_names

  # Step 5: Join back to original df
  out <- df |>
    dplyr::mutate(row_id = dplyr::row_number()) |>
    dplyr::left_join(indicators, by = "row_id") |>
    dplyr::select(-row_id, -dplyr::all_of(var_name))

  return(out)
}

#' A function for guaranteeing all REDCap fields appear even if
#' no participant has required them yet
#'
#' @description
#' This function ensures that all REDCap project variables are represented as
#' columns in `df`. Missing columns are added to the returned dataframe.
#'
#' @param df A dataframe.
#' @param target_form A character vector. The REDCap form names you want
#' to guarantee variables from. E.g. `target_form = "demographics"` ensures
#' all fields on the Demographics form are on the dataframe.
#' @param api_token_file A string. Path to a .txt file with the REDCap project
#' API token
#' @param meta_data_df A dataframe (optional). A dataframe containing variable
#' metadata for the REDCap project (produced by [camr_redcap_field_meta]). If
#' provided, `api_token_file` is not used to pull metadata and this is used
#' instead. This can help to avoid repeatedly pulling the project metadata.
#'
#' @returns A dataframe with all REDCap project variables represented. Variables
#' that were missing will be all NA.
#'
#' @importFrom dplyr select filter pull
#' @export
#'
camr_guarantee_fields <- function(df,
                                  target_form,
                                  api_token_file=Sys.getenv("API_TOKEN_FILE"),
                                  meta_data_df=NULL) {
  if (!is.null(meta_data_df)) {
    redcap_metadata <- meta_data_df
  }
  else redcap_metadata <- camrprojects::camr_redcap_field_meta(api_token_file)

  complete_fields <-  redcap_metadata |>
    dplyr::select(field_name, form_name) |>
    dplyr::filter(form_name == target_form) |> dplyr::pull(field_name)

  for (col in complete_fields) {
    if (!col %in% colnames(df)) df[[col]] <- NA
  }

  return(df)
}



