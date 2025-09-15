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
#' like the inline histograms (and horizontal bars for factor variables) to be.
#' Defaults to "blue".
#' @param group_var  Optional String. The name of a factor variable by which to group
#' the summaries (e.g. SBJ.FCT.RandGroup to separate results by Treatment group)
#' @param output Optional Character. A character vector with paths as elements. Paths
#' can be relative to your project. If provided, the table will be saved.
#' Supports .html and .docx paths.
#' @param hide_distributions If TRUE, hide the inline distribution visualizations.
#'                           Defaults to `FALSE`.
#' @param add_NA_level Logical. If TRUE, includes NA as a reported level for
#' factor variables.
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
#' df <- data.frame(SBJ.FCT.Race = sample(c("White", "Black", "Crimean"), 100, replace=TRUE, prob = c(.5, .4, .1)),
#'                  SBJ.FCT.Sex = factor(sample(c("Male", "Female"), 100, replace = TRUE),
#'                                       levels = c("Male", "Female")),
#'                  SBJ.FCT.Ethnicity = factor(sample(c("Hispanic or Latino", "Not Hispanic or Latino"), 100, replace = TRUE),
#'                                             levels = c("Hispanic or Latino", "Not Hispanic or Latino")),
#'                  SBJ.INT.Age = sample(13:50, 100, replace = TRUE),
#'                  SBJ.FCT.Treatment = factor(sample(1:2, 100, replace = TRUE), levels = 1:2, labels = c("Group A", "Group B")))
#' # Factors will appear ordered by their levels
#' df$SBJ.FCT.Race <- df$SBJ.FCT.Race |> factor(levels = c("White", "Black", "Crimean"))
#' demos_to_plot <- list("SBJ.INT.Age" = "Age",
#'                       "SBJ.FCT.Race" = "Race",
#'                       "SBJ.FCT.Sex" = "Sex",
#'                       "SBJ.FCT.Ethnicity" = "Ethnicity")
#' camr_make_table1(df, demos_to_plot, group_var = "SBJ.FCT.Treatment")#, Can save out tables using arg below
#'             #output = c("outputs/tables/demo_table.html",
#'                        #"outputs/tables/demo_table.docx"))
#'
#'
camr_make_table1 <- function(df,
                        var_label_list,
                        hist_bar_color = "blue",
                        group_var = NULL,
                        output = NULL,
                        hide_distributions = FALSE,
                        add_NA_level = TRUE) {

  # Check output file extensions
  if (!is.null(output)) {
    if (!all(fs::path_ext(output) %in% c("pdf", "html", "png", "rtf"))) {
      stop("Only pdf, html, png, and rtf outputs are supported. Check the file extension on your `output`.")
    }
  }

  # Helper for saving out files
  save_gt_table <- function(gt_tbl_obj, output) {
    for (path in output) {
      tryCatch(gt::gtsave(gt_tbl_obj, path),
               error = function(e) stop("You must have Chrome installed to save table as .pdf or .png"))
    }
  }

  numeric_vars <- names(var_label_list)[grepl("\\.(INT)|(DBL)\\.", names(var_label_list))]
  factor_vars <- setdiff(names(var_label_list), numeric_vars)

  if (add_NA_level == TRUE) {
    df <- df |>
      mutate(across(all_of(factor_vars), ~ forcats::fct_na_value_to_level(.x, level = "NA")))
  }

  # Handle group_var cases recursively
  if (!is.null(group_var)) {
    group_dfs <- split(df, df[[group_var]])
    group_gts <- lapply(group_dfs, function(d) camr_make_table1(d, var_label_list, hist_bar_color, add_NA_level = FALSE))
    combined_gt <- cbind_gt_groups(gts = group_gts,
                                   group_var = group_var,
                                   hist_bar_color = hist_bar_color) |>
      gt::tab_style(
        style = cell_text(align = "left"),
        locations = cells_body(columns = Variable))

    # Optionally hide distribution columns
    if (hide_distributions) {
      combined_gt <- combined_gt |> gt::cols_hide(matches("__dist$"))
    }

    if (!is.null(output)) {
      save_gt_table(combined_gt, output = output)
    }

    return(combined_gt)
  }

  # Helper function to switch from var names to labels
  get_label <- function(varname) {
    if (is.na(varname)) return("")
    return(var_label_list[[varname]])
  }
  get_label <- Vectorize(get_label, vectorize.args = "varname")

  # --- summaries of numeric variables ----
  if (length(numeric_vars) > 0) {
    numeric_tbl <- df |>
      dplyr::select(all_of(numeric_vars)) |>
      tidyr::pivot_longer(everything(), names_to = "Variable") |>
      dplyr::group_by(Variable) |>
      dplyr::summarize(
        `Mean (SD)` = sprintf("%.1f (%.1f); NA = %i", mean(value, na.rm = TRUE), sd(value, na.rm = TRUE), sum(is.na(value))),
        dist = list(value), # To create histogram using gt_plt_dist()
        min_val = min(value, na.rm = TRUE),
        max_val = max(value, na.rm = TRUE),
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
      dplyr::group_by(variable_grp) |>
      # Keep factors in order of their levels
      dplyr::mutate(
        Variable = factor(
          Variable,
          levels = {
            col_data <- df[[unique(variable_grp)]]
            # Convert character vars to factors
            if (is.factor(col_data)) levels(col_data) else unique(col_data)
          }
        )
      ) |>
      dplyr::group_by(variable_grp, Variable) |>
      dplyr::summarize(n = n()) |> ungroup() |>
      # Get zeros for unused levels (instead of them being NA)
      group_by(variable_grp) |>
      tidyr::complete(
        Variable,
        fill = list(n = 0)
      ) |> dplyr::filter((Variable %in% levels(df[[unique(variable_grp)]]))) |>
      group_by(variable_grp) |>
      dplyr::mutate(pct = n/sum(n) * 100) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        `Mean (SD)` = sprintf("%d (%.0f%%)", n, pct),
        dist = list(NA_real_),
        axis_vals = sprintf('<div style="width: %spx; height: 20px; background-color: %s;"></div>', pct, hist_bar_color)
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
  # if (length(numeric_vars) == 0) {
  #   table_data <- table_data |> select(-c(dist, axis_vals))
  # }

  table1 <- table_data |>
    gt::gt(rowname_col = "Variable",
           groupname_col = "variable_grp")

  if (TRUE) {
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

  if (hide_distributions) {
    table1 <- table1 |> gt::cols_hide(dist)
  }

  if (!is.null(output)) {
    save_gt_table(table1, output = output)
  }

  return(table1)
}

#' Column bind two gt tables
#'
#' @description
#' A helper function to column bind two gt tables. Used when running camr_make_table1
#' with groups.
#'
#' @param gts A named list of gt tables. The names should be the levels of the factor
#'            they were grouped by.
#' @param group_var A string. The name of the grouping factor variable.
#' @param fct_group_col A string. The name of the column in the gt tables
#'                      that groups factor levels. (e.g. for the factor levels
#'                      "Male" and "Female" this variable holds the value "Sex")
#' @param id_col A string. The column the two tables will be merged on. Usually
#'               "Variable"
#' @param keep_order Logical. Indicator for preserving the order of vars in the
#'                   inputted tables. Defaults to TRUE.
#' @param hist_bar_color The color you want the histograms for numeric variables
#'                       to appear.
#'
#' @returns A gt_tbl object that combines the tables in `gts`.
#'
#' @importFrom purrr imap reduce set_names
cbind_gt_groups <- function(gts,
                            group_var,
                            fct_group_col = "variable_grp",
                            id_col = "Variable",
                            keep_order = TRUE,
                            hist_bar_color = "blue") {
  if (is.null(names(gts)) || any(names(gts) == "")) {
    stop("Please pass a *named* list of gt_tbls; the names are used for spanner labels")
  }

  # Pull underlying data from each gt_tbl
  cleaned <- purrr::imap(gts,
                         function(gt_obj, label) {
                           dat <- gt_obj[["_data"]] |> dplyr::as_tibble()
                           if (!id_col %in% names(dat)) {
                             stop(sprintf("'%s' not found in a table's _data.", id_col))
                           }

                           present <- setdiff(names(dat), c(id_col, group_var, fct_group_col))
                           out <- dat |> dplyr::select(all_of(c(id_col, fct_group_col)), all_of(present))

                           # Keep order of the first table
                           if (keep_order && identical(label, names(gts)[1])) {
                             out <- out |> dplyr::mutate(.order_ = row_number())
                           }

                           # Rename group columns
                           rename_map <- purrr::set_names(
                             present,
                             paste0(label, "__", present)
                           )
                           out |> dplyr::rename(!!!rename_map)
                         })
  combined <- purrr::reduce(cleaned, function(df1, df2) {base::merge(df1, df2, all = TRUE)})

  # order factors
  if (keep_order && ".order_" %in% names(combined)) {
    combined <- dplyr::arrange(combined, .order_)
    combined$.order_ <- NULL
  }

  gt_tbl <- gt::gt(combined, groupname_col = fct_group_col) |>
    gt::cols_hide(columns = all_of(fct_group_col))

  # Add a spanner for each group and fix subcolumn labels
  for (label in names(gts)) {
    cols_for_label <- names(combined)[grepl(paste0(label, "__"), names(combined))]
    if (length(cols_for_label) == 0L) next

    # under this spanner, show sub-headers without the "{label}__" prefix
    display_labels <- sub(paste0("^", label, "__"), "", cols_for_label)

    gt_tbl <- gt_tbl |>
      gt::tab_spanner(
        label = label,
        columns = all_of(cols_for_label)
      ) |>
      gt::cols_label(!!!set_names(as.list(display_labels), cols_for_label))
  }

  gt_tbl <- gt_tbl |> gt::cols_label(!!id_col := id_col)

  for (lab in names(gts)) {
    dist_col <- paste0(lab, "__dist")
    axis_col <- paste0(lab, "__axis_vals")
    if (dist_col %in% names(combined)) {
      gt_tbl <- gt_tbl |>
        gtExtras::gt_plt_dist(
          column = dist_col,
          type = "histogram",
          fill_color = hist_bar_color,
          line_color = hist_bar_color,
          bw = 1,
          same_limit = FALSE
        ) |>
        gtExtras::gt_merge_stack(col1 = dist_col, col2=axis_col) |>
        # Formatting
        gt::cols_label(!!dist_col := html("Distribution<br><span style='font-size:10px'>(w/ min and max)</span>"))
    }
  }

  # Format Factor Variable Group Names
  gt_tbl <- gt_tbl |> gt::tab_stubhead("Variable") |>
    gt::tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_row_groups()
    ) |>
    gt::tab_style(
      style = cell_text(align = "left"),
      locations = cells_body(columns = Variable))

  return(gt_tbl)

}
