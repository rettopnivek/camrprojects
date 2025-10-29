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
                                     api_token_path = Sys.getenv("API_TOKEN_FILE")) {
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
    df <- process_redcap_var(df, v, redcap_var_metadata, prefix)
  }

  return(df)
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
                               prefix) {

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
  else if (var_type == "descriptive") {
    df <- df |> dplyr::select(-dplyr::all_of(var_name))
  }
  # Radio and dropdowns are factors
  else if (var_type %in% c("radio", "dropdown")) {
    answer_choices <- meta_row[["answer_choices"]]
    df <- df |> dplyr::mutate(!!var_name := factor(.data[[var_name]],
                                                   levels = names(answer_choices),
                                                   labels = unlist(answer_choices)))
    data_type = "FCT"
  }
  # Sliders are numeric
  else if (var_type == "slider") {
    df <- df |> dplyr::mutate(!!var_name := as.numeric(.data[[var_name]]))
    data_type <- "NUM"
  }
  # Text inputs depend on validation info
  else if (var_type == "text") {
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
      df <- df |> dplyr::mutate(!!var_name := lubridate::mdy(.data[[var_name]]))
      data_type <- "DAT"
    }
    else if (validation == "datetime_mdy") {
      df <- df |> dplyr::mutate(!!var_name := lubridate::ymd_hm(.data[[var_name]]))
      data_type <- "DTM"
    }
    else if (validation == "time") {
      df <- df |> dplyr::mutate(!!var_name := lubridate::hms(.data[[var_name]]))
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
    df <- make_indicators(df, var_name, answer_choices, prefix)
    data_type <- NULL
  }

  ## Rename variable (except checkboxes) ----
  # Checkbox renaming is handled within `make_indicators`
  form_name <- stringr::str_extract(var_name, "^[^_]+")
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
                            data_type = "LGL",
                            sep = "\\|") {

  # Extract form name
  form_name <- stringr::str_extract(var_name, "^[^_]+")

  # Extract field names from choice codes
  all_choices <- names(unlist(answer_choices))

  # Step 1: Create the wide indicator matrix
  indicators <- df |>
    mutate(row_id = row_number()) |>
    select(row_id, all_of(var_name)) |>
    tidyr::separate_rows(all_of(var_name), sep = sep) |>
    mutate(value = TRUE) |>
    tidyr::pivot_wider(
      names_from = all_of(var_name),
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
    select(row_id, all_of(all_choices))

  # Step 4: Rename columns using the same convention
  new_names <- sapply(all_choices, function(choice) {
    field_name <- stringr::str_to_title(choice) |>
      stringr::str_replace_all("_", "") |>
      stringr::str_replace_all(" ", "")
    paste(prefix, data_type, toupper(form_name), field_name, sep = ".")
  }, USE.NAMES = FALSE)

  colnames(indicators)[-1] <- new_names  # skip row_id

  # Step 5: Join back to original df
  df |>
    mutate(row_id = row_number()) |>
    left_join(indicators, by = "row_id") |>
    select(-row_id, -all_of(var_name))  # remove original checkbox column
}



