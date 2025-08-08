#' Create a table1 style table for weekly updates and
#' data inspection
#'
#' @description
#' This function creates a descriptive table from the specified columns
#' in a given dataframe. It relies on CAM variable naming conventions
#' to identify numeric and categorical variables. For numeric variables,
#' it creates an inline histogram. Optionally, you can save the table as
#' docx or html (note that the histograms won't render in docx)
#'
#'
#' @param df A dataframe with columns that follow CAM-standard naming
#'           conventions
#' @param var_label_list A list with names that correspond to the
#' variables of `df` that you want to summarize. The values are the
#' labels that will appear in the table. E.g. `list(SBJ.INT.Age = "Age")`
#' @param hist_bar_color Optional. A character or hex code for the color you'd
#' like the inline histograms to be. Defaults to "blue".
#' @param output Optional. A character vector with paths as elements. Paths
#' can be relative to your project. If provided, the table will be saved.
#' Supports .html and .docx paths.
#'
#' @returns A gt_tbl object. Side effect: if `output` was provided, the table
#' will be saved as .html or .docx at the designated path(s)
#'
#' @import dplyr gt gtExtras
#' @export
#'
#' @author Zach Himmelsbach
#'
#' @examples
#' df <- targets::tar_read(df_demographics)
#' demos_to_plot <- list("SBJ.INT.CompleteAge" = "Age",
#'                       "SBJ.FCT.Gender" = "Gender",
#'                       "SBJ.FCT.Ethnicity" = "Ethnicity")
#' make_table1(df, demos_to_plot,
#' output = c("outputs/tables/demo_table.html",
#'            "outputs/tables/demo_table.docx"))
#'
#'
camr_make_table1 <- function(df,
                        var_label_list,
                        hist_bar_color = "blue",
                        output = NULL) {

  # Helper function to switch from var names to labels
  get_label <- function(varname) {
    if (is.na(varname)) return("")
    return(var_label_list[[varname]])
  }
  get_label <- Vectorize(get_label, vectorize.args = "varname")

  numeric_vars <- names(var_label_list)[grepl("\\.(INT)|(DBL)\\.", names(var_label_list))]
  factor_vars <- setdiff(names(var_label_list), numeric_vars)

  # --- summaries of numeric variables ----
  if (length(numeric_vars) > 0) {
    numeric_tbl <- df |>
      dplyr::select(all_of(numeric_vars)) |>
      tidyr::pivot_longer(everything(), names_to = "Variable") |>
      dplyr::group_by(Variable) |>
      dplyr::summarize(
        `Mean (SD)` = sprintf("%.1f (%.1f)", mean(value), sd(value)),
        dist = list(value), # To create histogram using gt_plt_dist()
        min_val = min(value),
        max_val = max(value),
        variable_grp = NA_character_ # The numerics don't need group labels
      ) |>
      dplyr::mutate(Variable = get_label(Variable),
                    axis_vals = sprintf(
                      "<div style='font-size:8px;
                            display:flex;
                            justify-content:space-between;
                            width:100%%'>
                <span>%s</span><span>%s</span>
               </div>",
                      formatC(min_val, 1, format = "f"),
                      formatC(max_val, 1, format = "f")
                    )
      ) |>
      dplyr::select(-c(min_val, max_val))
  }
  else if (length(numeric_vars) == 0) {
    numeric_tbl <- data.frame()
  }

  # --- Summaries of categorical variables ----
  if (length(factor_vars) > 0) {
    categorical_tbl <- df |>
      dplyr::select(all_of(factor_vars)) |>
      tidyr::pivot_longer(everything(), names_to = "variable_grp", values_to = "Variable") |>
      dplyr::group_by(variable_grp, Variable) |>
      dplyr::summarize(n = n()) |> ungroup() |> group_by(variable_grp) |>
      dplyr::mutate(pct = n/sum(n) * 100) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        `Mean (SD)` = sprintf("%d (%.0f%%)", n, pct),
        dist = list(NA),
        axis_vals = NA_character_
      ) |>
      dplyr::ungroup()
  }
  else if (length(factor_vars) == 0) {
    categorical_tbl <- data.frame()
  }

  # --- Bind variables together and make inline historgram (with gt) ----
  table_data <- dplyr::bind_rows(numeric_tbl, categorical_tbl) |>
    dplyr::mutate(variable_grp = get_label(variable_grp)) |>
    dplyr::select(variable_grp, Variable, `Mean (SD)`, dist, axis_vals)
  if (length(numeric_vars) == 0) {
    table_data <- table_data |> select(-c(dist, axis_vals))
  }

  table1 <- table_data |>
    gt::gt(rowname_col = "Variable",
           groupname_col = "variable_grp")

  if (length(numeric_vars) > 0) {
    # Histogram for numeric rows
    table1 <- table1 |> gtExtras::gt_plt_dist(column = dist,
                                              type = "histogram",
                                              fill_color = hist_bar_color,
                                              line_color = hist_bar_color,
                                              bw = 1,
                                              same_limit = FALSE) |>

      gtExtras::gt_merge_stack(col1 = dist, col2=axis_vals) |>
      # Formatting
      gt::cols_label(dist = html("Distribution<br><span style='font-size:10px'>(w/ min and max)</span>"))
  }

  table1 <- table1 |> gt::tab_stubhead("Variable") |>
    gt::cols_label(`Mean (SD)` = "Summary") |>
    gt::tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_row_groups()
    )

  if (!is.null(output)) {
    for (path in output) gt::gtsave(table1, path)
  }
  return(table1)
}
