#' Rename a REDCap Data Frame to Follow CAM Naming Standards
#'
#' Rename a data frame downloaded from REDCap using the field annotation located
#' in the metadata data frame downloaded with the data. See [valiate_var_name()]
#' for more information on the naming standard. Warns the user for any field
#' that does not contain a new name that conforms to the standard in the field
#' annotation.
#'
#' @param rcDtf A data frame containing the data downloaded from REDCap.
#' @param metaDtf A data frame containing the meta data (data dictionary)
#' downloaded from REDCap.
#'
#' @author William Schmitt
#'
#' @return A data frame where all columns have been renamed to conform to the
#' CAM standard for R names.
#'
#' @examples
#'
#' # TODO
#'
#' @export

rename_redcap_vars <- function(rcDtf, metaDtf) {

  ## Extract new names
  metaDtf <- dplyr::mutate(
    metaDtf,
    newName = stringr::str_extract(
      field_annotation,
      "(?<=VARNAME=)[[:alnum:]\\._]+(?=(\\r\\n)?)"
    )
  )

  # Validate new names
  metaDtf <- dplyr::mutate(
    metaDtf,
    validName = validate_var_name(newName, "REDCap")
  )

  # Warn about bad names
  badNames <- dplyr::filter(metaDtf, !validName)
  purrr::walk(
    badNames$field_name,
    ~ message(paste0("WARNING: Bad name for ", ., "."))
  )

  # Build data frame for old <> new mapping
  nameMap <- data.frame(
    current = names(rcDtf)
  )

  # Get new names ready to add
  newNames <- dplyr::transmute(
    metaDtf,
    field_name = field_name,
    field_type = field_type,
    field_choices = dplyr::if_else(
      field_type == "checkbox",
      select_choices_or_calculations,
      ''
    ),
    newName = dplyr::if_else(
      validName,
      newName,
      field_name
    )
  )

  # Turn choices for checkboxes into nested dtf
  newNames <- dplyr::mutate(
    newNames,
    field_choices = if_else(
      field_type == "checkbox",
      stringr::str_extract_all(field_choices, '[:alnum:]*(?=,\\s)'),
      list("")
    )
  )

  # unnest to get one row per check option
  newNames <- tidyr::unnest(newNames, field_choices)

  # Rename newName to work for checkboxes
  newNames <- dplyr::mutate(
    newNames,
    dplyr::across(
      c(field_name, newName),
      ~ if_else(
          field_type == "checkbox",
          paste0(., "___", field_choices),
          .
      )
    )
  )

  # Join with current names
  nameMap <- left_join(
    nameMap,
    dplyr::select(newNames, newName, field_name),
    by = c("current" = "field_name")
  )

  # TEMPORARY: Fill in blanks
  nameMap <- dplyr::mutate(
    nameMap,
    newName = if_else(
      is.na(newName),
      current,
      newName
    )
  )

  # Copy over new names and return
  dtf <- rcDtf
  names(dtf) <- nameMap$newName
  return(dtf)
}
