#' Generate data quality rules based on REDCap metadata
#'
#' @description
#' Write data quality rules based on REDCap Metadata. Values should not be
#' missing when they are 1) Required AND 2) reached by branching logic.
#' Produces a csv that can be uploaded to the data quality module in REDCap.f
#'
#' @param path_to_api_token path to a text file containing project API token
#' @param output_path path to a file where csv should be saved; will *not*
#' overwrite an existing file. Defaults to "data_quality_rules.csv"
#' @param excluded_vars Character vector of REDCap vars that we do not want to
#' check. By default, we exclude the MyCap variables.
#'
#' @importFrom stringr str_trim
#' @importFrom dplyr   filter
#' @export
#'
#' @author Zach Himmelsbach
#'
#' @returns side-effect: saves csv at `output_path`. Invisibly returns the
#' data quality rules csv for inspection
#'
camr_gen_data_quality_rules <- function(path_to_api_token,
                                   output_path = "data_quality_rules.csv",
                                   excluded_vars = c("record_joindate",
                                                     "record_joindate_utc",
                                                     "record_timezone",
                                                     "record_code")) {
  # Check inputs ----
  if (file.exists(output_path)) stop("Cannot overwrite an existing file")
  if (!file.exists(path_to_api_token)) stop("API token file not found")

  # Load project metadata ----
  meta <- camr_redcap_field_meta(path_to_api_token)

  # We auto generate rules only for required fields
  meta <- meta |> dplyr::filter(required_field)

  # Exclude excluded_vars fields
  meta <- meta |> dplyr::filter(!(field_name %in% excluded_vars))

  # Check if project is longitudinal
  is_longitudinal <- camr_redcap_project_info(path_to_api_token)$is_longitudinal == 1

  # get record_status event for longitudinal projects
  record_status_event <- ""
  if (is_longitudinal) {
    event_map <- camr_instrument_event_map(path_to_api_token)
    record_status_event <- event_map$unique_event_name[event_map$form=="record_status"]
    # Add braces so redcap parses as variable
    record_status_event <- paste0("[", record_status_event, "]")
  }

  # Build rules ----
  # Helper function to write rule for individual items
  write_rule <- function(meta_row) {
    if (meta_row$branching_logic == "") return(sprintf("[%s] = ''", meta_row$field_name))
    else if (meta_row$field_type == "checkbox") {
      all_missing <- paste(sprintf("[%s(%s)] = '0'", meta_row$field_name,
                                                    names(meta_row$answer_choices)),
                           collapse = " and ")
      return(sprintf("((%s) and (%s))", meta_row$branching_logic,
                                       all_missing))
    }
    else {
      return(sprintf("((%s) and [%s] = '')", meta_row$branching_logic,
                                      meta_row$field_name))
    }
  }
  # Helper function to combine rules for one instrument
  combine_rules <- function(name_of_form,
                         pre_rules = paste0(record_status_event,
                                            "[record_status] > '0' and (\n  "),
                         post_rules = "\n)") {
    df <- meta |> dplyr::filter(form_name == name_of_form)
    rules <- apply(df, MARGIN = 1, FUN = write_rule)
    rules <- paste(rules, collapse = " or \n  ")
    rules <- paste0(pre_rules, rules, post_rules)

    return(rules)
  }

  rules_vec <- sapply(unique(meta$form_name), combine_rules) |> setNames(unique(meta$form_name))
  rules_df <- data.frame(rule_name = names(rules_vec),
                         rule_logic = rules_vec,
                         real_time_execution = rep("n", length(rules_vec)))

  write.csv(rules_df, output_path, row.names = FALSE, eol="\n")
  print(sprintf("Data quality rules written to %s", output_path))
  warning("Additional rules may need to be manually written, e.g. consult flagging rules")
  warning("If the project includes very long instruments (e.g. the MINI),
          inspect in RStudio (Excel will not display correctly due to cell limit)")
  return(invisible(rules_df))
}
