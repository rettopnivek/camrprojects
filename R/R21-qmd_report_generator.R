#' Generate a Quarto Report from a Named List
#' Developed for producing weekly reports
#'
#' This function generates a Quarto (`.qmd`) report and optionally renders it to HTML,
#' using a named list of objects as input. The list is saved as an `.RDS` file in
#' the specified output directory and loaded within the Quarto document for rendering.
#'
#' It supports recursive nesting of sections, rendering each level with appropriate heading depth.
#'
#' @param report_list A named list containing report sections and their contents.
#'        Each element can be a nested list or individual objects.
#' @param report_title A character string for the report title.
#' @param output_directory The directory where the `.qmd`, `.html`, and `.rds` files will be saved.
#'        The report title will be used as the base filename.
#' @param render_html Logical, whether to render the Quarto report to HTML after generating the `.qmd` file. Default is `TRUE`.
#'
#' @return Invisibly returns a list with paths to the `.qmd`, `.html`, and `.rds` files.
#' @export
#'
#' @importFrom quarto quarto_render
#'
#' @examples
#' report_data <- list(
#'   Summary = list(
#'     Intro = "This is an intro.",
#'     Results = "<b>Bold result here</b>"
#'   ),
#'   Details = "Some detailed explanation.",
#'   Deep = list(
#'     Sub = list(
#'       Item1 = "Nested item 1",
#'       Item2 = list(
#'         "Deep text",
#'         "<i>Deep HTML</i>"
#'       )
#'     )
#'   )
#' )
#' camr_gen_quarto_report(
#'   report_data,
#'   report_title = "Example Report",
#'   output_directory = "reports/",
#'   render_html = FALSE
#' )
camr_gen_quarto_report <- function(report_list,
                                   report_title = "Weekly Report",
                                   output_directory = "report_output",
                                   render_html = TRUE) {
  if (!is.list(report_list) || is.null(names(report_list))) {
    stop("`report_list` must be a named list.")
  }

  # Create output directory if it doesn't exist
  if (!dir.exists(output_directory)) {
    dir.create(output_directory, recursive = TRUE)
  }

  # Clean the report title to generate a base filename
  base_filename <- tolower(gsub(" +", "_", gsub("[^a-zA-Z0-9 ]", "", report_title)))

  # Build full file paths
  qmd_file <- file.path(output_directory, paste0(base_filename, ".qmd"))
  html_file <- file.path(output_directory, paste0(base_filename, ".html"))
  rds_file <- file.path(output_directory, paste0(base_filename, ".rds"))

  # Save the RDS file
  saveRDS(report_list, file = rds_file)

  # Quarto YAML and setup
  lines <- c(
    "---",
    sprintf("title: \"%s (%s)\"", report_title, Sys.Date()),
    "format: html",
    "toc: true",
    "execute:",
    "  echo: false",
    "  warning: false",
    "  message: false",
    "  error: false",
    "---",
    "",
    "```{r}",
    "library(htmltools)",
    sprintf("report_list <- readRDS(\"%s\")", normalizePath(rds_file, winslash = "/")),
    "```",
    ""
  )

  # --- Recursive helper function ---
  render_section <- function(obj, key_path, depth = 1) {
    section_lines <- character()
    key <- tail(key_path, 1)
    heading <- paste0(strrep("#", depth), " ", key)
    section_lines <- c(section_lines, heading, "")

    if (is.list(obj)) {
      names_vec <- names(obj)
      for (i in seq_along(obj)) {
        item <- obj[[i]]
        item_name <- if (!is.null(names_vec) && names_vec[i] != "") names_vec[i] else paste0("Item ", i)
        section_lines <- c(section_lines, render_section(item, c(key_path, item_name), depth + 1))
      }
    } else {
      # Determine object access path
      accessor <- paste0("report_list", paste0(sprintf("[['%s']]", key_path), collapse = ""))
      is_html_string <- is.character(obj) && length(obj) == 1

      if (is_html_string) {
        chunk <- c(
          "```{r, results='asis'}",
          paste0("htmltools::HTML(", accessor, ")"),
          "```",
          ""
        )
      } else {
        chunk <- c(
          "```{r}",
          accessor,
          "```",
          ""
        )
      }
      section_lines <- c(section_lines, chunk)
    }

    section_lines
  }

  # Render top-level sections
  for (section_name in names(report_list)) {
    section <- report_list[[section_name]]
    lines <- c(lines, render_section(section, key_path = section_name, depth = 1))
  }

  # Write the .qmd file
  writeLines(lines, qmd_file)

  # Optionally render to HTML
  if (render_html) {
    quarto::quarto_render(
      input = qmd_file,
      output_file = basename(html_file)
    )
  }

  invisible(list(
    qmd = qmd_file,
    html = if (render_html) html_file else NULL,
    rds = rds_file
  ))
}
