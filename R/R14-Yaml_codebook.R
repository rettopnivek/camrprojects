#' Create a set of yaml codebooks for variables in targets defined
#' by _targets.R
#'
#' @description
#' Copy codebook for variables in the standard pipeline and generate stub entries
#' for new variables. Creates a directory in your project called "codebook" with
#' one .yaml file per target in _targets.R. Some information is automatically
#' inferred for new variables (data type, unique values, etc)
#'
#' @param codebook_dir Character. Directory to store the codebook files. This
#' must *not* currently exist. Default "codebook".
#' @param max_unique Integer. If an integer or character variable has less than
#' `max_unique` distinct values, those values are listed in the codebook. Default 20.
#' @param exclude_targets a character vector with names of targets to exclude.
#' Excludes core targets by default.
#'
#' @returns side effect: `codebook_dir` directory is populated with yaml codebooks
#' for each targets object. Invisibly returns list of variables for which
#' standard pipeline entries were *not* found
#'
#' @importFrom targets tar_manifest tar_read
#' @importFrom yaml write_yaml
#' @importFrom fs dir_create path file_exists
#' @importFrom stringr str_match
#' @export
#'
camr_yaml_codebook <- function(codebook_dir    = "codebook",
                               max_unique      = 20,
                               exclude_targets = c('rds_download',
                                                   'chr_path_redcap_data',
                                                   'chr_path_tlfb_data',
                                                   'lst_redcap_data',
                                                   'df_tlfb_raw',
                                                   'df_id_table',
                                                   'df_redcap_raw')) {

  # Input Checks ----
  if (dir.exists(codebook_dir)) {
    stop("`codebook_dir` already exists - choose a new folder or delete first")
  }

  # Setup ----
  dir <- fs::path(codebook_dir)
  fs::dir_create(dir)

  # helper maps ----
  # varname convention abbreviations
  class_abbrev <- list(
    CHR = "character",
    FCT = "factor",
    INT = "integer",
    LGL = "logical",
    DAT = c("POSIXct", "Date"),
    DTM = "POSIXct",
    DBL = "numeric"
  )

  # Types for which we'll list unique values
  values_list <- c("CHR", "FCT", "INT", "LGL")

  # Types we assume contain identifiable info
  identifiable_types <- c("CHR", "DAT", "DTM")

  # Load target dataframes ----
  tgts <- setdiff(targets::tar_manifest()$name, exclude_targets)

  # Vector of dataframes we've written files for
  written <- character(0)

  # Iterate over targets ----
  for (tgt in tgts) {
    obj <- try(targets::tar_read_raw(tgt), silent = TRUE)
    if (inherits(obj, "try-error") || !inherits(obj, "data.frame")) next

    ## Create stub ----
    stub <- list(
      dataframe = tgt,
      generated_on = format(Sys.Date()),
      n_rows = nrow(obj),
      n_cols = ncol(obj),
      variables = list()
    )

    for (v in colnames(obj)) {
      x <- obj[[v]]

      # Get type declared by variable name
      m <- stringr::str_match(v, "^[A-Za-z]*\\.([A-Za-z]*)\\.")
      declared <- toupper(ifelse(is.na(m[, 2]), "UNK", m[, 2]))

      # Check against actual data type
      expected_class <- class_abbrev[[declared]]
      actual_class <- class(x)[1]

      # Throw error if
      # The abbreviation is not standard; the stated class doesn't match actual
      # Allow mismatch if all the values are missing (because type can't be inferred)
      if (!is.null(expected_class) && !(actual_class %in% expected_class) && !all(is.na(x))) {
        warning(sprintf(
          "Type mismatch: variable '%s' is declared %s but is actually %s (df: %s)",
          v, declared, actual_class, tgt),
          call. = FALSE
        )
      }

      # Check if we should record unique values
      uniq_map <- NULL
      if (declared %in% values_list) {
        uniq <- unique(na.omit(x))
        if (length(uniq) <= max_unique) {
          if (declared == "FCT") { # Check this
            uniq_map <- setNames(as.list(levels(x)),
                                 seq_along(levels(x)))
          } else if (declared == "INT") {
            uniq_map <- setNames(as.list(rep("...",length(uniq))),
                                 uniq)
          } else if (declared == "LGL") {
            uniq_map <- setNames(as.list(rep("...", length(uniq))),
                                 as.character(uniq))
          } else if (declared == "CHR") {
            uniq_map <- as.list(sort(uniq))
          }
        }
      }

      ## Assemble entry ----
      stub$variables[[v]] <- list(
        description = "...",
        item_wording = "...",
        redcap_source_vars = "...",
        data_type = declared,
        unique_values = uniq_map,
        unit_of_measurement = "...",
        missing_values = "NA",
        redcap_visits = "...",
        deidentified = !(declared %in% identifiable_types)
      )
    }

    ## Write yaml ----
    out_file <- fs::path(dir, paste0(tgt, ".yml"))
    yaml::write_yaml(stub, out_file, indent = 2)
    written <- c(written, out_file)
  }

  message("Wrote ", length(written), " YAML stub(s) to ", dir)
  invisible(written)
}

#' Scrape functions that generate targets to map variable names to the
#' REDCap variables they were created from
#'
#' @description
#' Scrape the functions that generate targets and map final variable names
#' to their REDCap ancestor variables. Produces a yaml file that lists the
#' target dataframes with their mappings (from final vars to REDCap vars). The
#' scraper looks for transmute calls. If a variable is constructed from more than
#' one REDCap ancester, the mapping is listed as "composite".
#'
#' @param output_dir file path to directory where the yaml should be saved
#' @param src_dir directory to search for functions that generate variables.
#' Default is "src/".
#' @param exclude_targets a character vector with names of targets to exclude.
#' Excludes core targets by default.
#' @returns side effect: a yaml file is created in `output_dir`. Invisibly returns
#' the list object containing the mapping
#'
#' @importFrom stringr str_extract
#' @export
#'
camr_map_redcap_vars <- function(output_dir,
                                 src_dir="src",
                                 exclude_targets = c('rds_download',
                                                     'chr_path_redcap_data',
                                                     'chr_path_tlfb_data',
                                                     'lst_redcap_data',
                                                     'df_tlfb_raw',
                                                     'df_id_table',
                                                     'df_redcap_raw')) {
  # Input checks ----
  stopifnot(dir.exists(src_dir))
  if (!dir.exists(output_dir)) dir.create(output_dir)

  # Get the targets and functions from the manifest ----
  man <- targets::tar_manifest(fields = c("name", "command"))
  man <- man[!(man$name %in% exclude_targets),]

  # Helper to pull the functions
  extract_fun <- function(cmd) {
    m <- stringr::str_extract(cmd,
                              "^([A-Za-z][A-Za-z0-9_.]*)\\(",
                              group = 1)
  }
  # Extract the function names
  man$fun <- vapply(man$command, extract_fun, character(1))

  # Map functions to the scripts that create them ----
  r_files <- list.files(src_dir, pattern = "\\.R$",
                        recursive = TRUE, full.names = TRUE)

  fun_table <- purrr::map_dfr(r_files, function(f) {
    lines <- readLines(f, warn = FALSE)
    defs <- stringr::str_match_all(
      lines,
      "\\s*([A-Za-z][A-Za-z0-9_.]*)\\s*<-\\s*function"
    )
    df <- try(df <- do.call(rbind.data.frame, defs))
    if ("try-error" %in% class(df) || nrow(df) == 0) df <- data.frame(script = f, V2 = NA)
    else df$script <- f
    df <- df[,c("V2", "script")]
    colnames(df) <- c("fun", "script")
    return(df)
  })
  fun_table <- fun_table[!is.na(fun_table$fun),]
  if (nrow(fun_table) == 0) {
    stop("No functions found in src_dir: ", src_dir)
  }
  fun_map <- split(fun_table$script, fun_table$fun)

  # Function to pull variable pairs from transmute calls ----
  parse_transmute_pairs <- function(txt) {
    bodies <- stringr::str_match_all(txt,
                                     stringr::regex("transmute\\s*\\((.*?)\n\\s*\\)\\s*\\|>\\s*\n", dotall = TRUE))[[1]][,2]
                                     # Note: this regex is fragile but works well enough for our purposes
                                     # The issue is
    purrr::map(bodies, function(body) {
      # Split on comma (that aren't inside quotes or parentheses)
      pieces <- strsplit(body,
                         "[^\"'],",
                         perl = TRUE)[[1]]
      purrr::map(pieces, function(piece) {
        parts <- strsplit(piece, "=", fixed = TRUE)[[1]]
        if (length(parts) != 2) return(NULL)
        lhs <- stringr::str_trim(parts[1]) |>
          stringr::str_extract("(#.*?\n)?\\s*([A-Za-z][A-Za-z0-9_.]*)", group = 2)
        rhs <- stringr::str_trim(parts[2])

        tokens <- stringr::str_extract_all(rhs, "[A-Za-z][A-Za-z0-9_.]*")[[1]]
        tokens <- setdiff(tokens, c("c", "as.integer", "as.numeric",
                                    "as.character", "as.logical"))
        src <- if (length(tokens) == 1) tokens else "composite"
        list(lhs = lhs, src = src)
      }) |> purrr::compact()
    }) |> purrr::flatten()
  }

  # Go through each target, locate its function's file, and build mapping ----
  mapping <- list()
  for (i in seq_len(nrow(man))) {
    target_name <- man$name[i]
    function_name <- man$fun[i]
    if (is.na(function_name) || !(function_name %in% names(fun_map))) next

    helper_file <- fun_map[[function_name]][1] # There shouldn't be duplicates but...
    txt <- paste(readLines(helper_file, warn = FALSE), collapse = "\n")

    pairs <- parse_transmute_pairs(txt)
    if (length(pairs) == 0) next
    mapping[[target_name]] <- purrr::reduce(pairs, function(acc, pr) {
      acc[[pr$lhs]] <- pr$src
      acc
    }, .init = list())
  }

  if (length(mapping) == 0) {
    warning("No transmute() mappings found")
    return(invisible(NULL))
  }

  # Write yaml ----
  out_file <- file.path(
    output_dir,
    sprintf("redcap_var_map_%s.yaml", format(Sys.time(), "%Y%m%dT%H%M%S"))
  )
  yaml::write_yaml(mapping, out_file)
  message("Variable map written to ", out_file)
  invisible(out_file)
}

#' Get REDCap project field metadata as a dataframe
#'
#' @description
#' Pull project field metadata via the REDCap API and clean it up to be useful.
#' The output contains field names, field types, answer choices, branching logic,
#' etc
#'
#' @param api_token_path path to text file containing REDCap project API token
#' @returns a dataframe containing metadata on the fields from the REDCap project
#'
#' @importFrom httr    POST content
#' @importFrom stringr str_trim
#' @importFrom purrr   map set_names transpose
#' @export
#'
camr_redcap_field_meta <- function(api_token_path) {
  # Read token
  token <- readLines(api_token_path, warn = FALSE)[1] |> stringr::str_trim()

  # Request metadata
  url <- "https://redcap.partners.org/redcap/api/"
  formData <- list("token"=token,
                   content='metadata',
                   format='json',
                   returnFormat='json'
  ) # json is easiest; the csv doesn't parse correctly
  response <- httr::POST(url, body = formData, encode = "form")


  meta_df <- jsonlite::fromJSON(httr::content(response,
                                              "text",
                                              encoding = "UTF-8"),
                                simplifyDataFrame = TRUE)

  # Convert field labels that contain html to text
  rows_w_html <- grepl("<.*>", meta_df$field_label)
  meta_df$field_label[rows_w_html] <- sapply(meta_df$field_label[rows_w_html],
                                             function(s) {rvest::read_html(s) |>
                                               rvest::html_elements("p") |>
                                               rvest::html_text2() |>
                                               paste(collapse = "; ")})

  # Separate calculations and answer choices
  # keep original field for debugging
  meta_df$original_choices <- meta_df$select_choices_or_calculations
  meta_df$calculation <- ""
  meta_df$calculation[meta_df$field_type == "calc"] <- meta_df$select_choices_or_calculations[meta_df$field_type == "calc"]
  meta_df$select_choices_or_calculations[meta_df$field_type == "calc"] <- ""
  meta_df <- meta_df |> dplyr::rename(answer_choices = select_choices_or_calculations)

  # Convert answer choices to a list with numbers as names and labels as values
  parse_choices <- function(x) {
    stringr::str_split(x, "\\s*\\|\\s*")[[1]] |>
      purrr::map(~ stringr::str_match(.x, "^(\\d+), (.*)$")[,2:3]) |>
      purrr::transpose() |>
      (function(kv) purrr::set_names(kv[[2]], kv[[1]]))() |> as.list()
  }

  meta_df$answer_choices <- lapply(meta_df$answer_choices, parse_choices)

  # Fill in answer choices for yes/no fields
  meta_df$answer_choices[meta_df$field_type == "yesno"] <- lapply(1:length(meta_df$answer_choices[meta_df$field_type == "yesno"]),
                                                                  \(x) list("1" = "Yes", "0" = "No"))

  # Convert "identifier" and "required_field" fields to logical
  meta_df$identifier <- meta_df$identifier == "y"
  meta_df$required_field <- meta_df$required_field == "y"

  return(meta_df)
}

#' Use variable mappings from camr_map_redcap_vars to fill in codebook info
#'
#' @description
#' Load redcap variable info using REDCap API and fill in codebook info for
#' related targets' vars.
#'
#' @param api_token_path path to text file containing REDCap project API token
#' @param codebook_dir path to directory containing codebook yaml files
#' @param mapping_file path to yaml file with map from targets to REDCap variables
#' @param output_dir new directory where the resulting codebook yaml will be saved
#' (this will *not* overwrite the existing codebook)
#' @returns side effect: writes yaml codebook to `output_path`. The path is also
#' invisibly returned.
#'
#' @importFrom fs        dir_exists path_abs path_ext_remove
#' @importFrom yaml      read_yaml
#' @export
#'
camr_fill_codebook_from_redcap <- function(api_token_path,
                                           codebook_dir,
                                           mapping_file,
                                           output_dir) {
  # Input checks ----
  if (fs::dir_exists(output_dir)) stop("You cannot use this method to overwrite and existing codebook")
  if (!fs::dir_exists(codebook_dir)) stop("Codebook directory not found")
  if (!file.exists(mapping_file)) stop("Variable mapping file not found")
  if (!file.exists(api_token_path)) stop("API Token text file not found")

  # Load inputs ----
  var_mapping <- yaml::read_yaml(mapping_file)
  codebook <- lapply(fs::path_abs(paste0(codebook_dir, '/', list.files(codebook_dir))), yaml::read_yaml)
  names(codebook) <- fs::path_ext_remove(list.files(codebook_dir))
  fs::dir_create(output_dir)

  # Get field's metadata from REDCap
  meta <- camr_redcap_field_meta(api_token_path)

  # Helper function to pull item metadata ----
  pull_item_info <- function(redcap_var_name) {
    meta[meta$field_name == redcap_var_name,]
  }

  item_info <- lapply(var_mapping,
                      function(df) lapply(df, \(d) lapply(d,
                                                          \(v) try(pull_item_info(v)))))

  # Fill out codebook with info from item metadata ----
  # Helper function to fill out codebook for individual variables
  fill_codebook_variable <- function(processed_dataframe,
                                     processed_varname,
                                     codebook) {
    # find variable info in redcap metadata
    var_info <- item_info[[processed_dataframe]][[processed_varname]]

    # replace codebook entries
    var_entry <- codebook[[processed_dataframe]][["variables"]][[processed_varname]]
    var_entry["unique_values"] <- ifelse(length(var_info[[1]][["answer_choices"]]) > 0,
                                         var_info[[1]][["answer_choices"]],
                                         var_entry["unique_values"])
    var_entry[["item_wording"]] <- var_info[[1]][["field_label"]]
    var_entry[["redcap_source_vars"]] <- var_mapping[[processed_dataframe]][[processed_varname]]

    return(var_entry)
  }

  # Loop over dataframes (and variables within those dataframes),
  # and fill out entries
  for (df_name in names(codebook)) {
    for (var_name in names(codebook[[df_name]][["variables"]])) {
      if (is.null(var_mapping[[df_name]][[var_name]])) next
      codebook[[df_name]][["variables"]][[var_name]] <- fill_codebook_variable(df_name,
                                                                               var_name,
                                                                               codebook)
    }
    # Write out yaml codebook for the dataframe (to output_dir)
    yaml::write_yaml(codebook[[df_name]], paste0(output_dir, '/', df_name, '.yaml'))
  }

  return(output_dir)

}

#' Extract pairs of new and source variables from every dplyr::transmute call in a script
#'
#' @param file Path to an R script that defines target function
#' @param redcap_vars a character vector of the variable names in the redcap project
#' @param keep Optional character vector of variable names to keep
#'
#' @returns A list of lists, each with the processed variable (as key) and
#' source variable (as value). The value defaults to "composite"
parse_transmute_pairs <- function(file, redcap_vars, keep = NULL) {

  res <- list() # list to collect pairs
  expr <- parse(file, keep.source = FALSE)

  walk_ast <- function(node) {
    if (is.call(node)) {

      fun_sym <- node[[1]]
      fun_chr <- paste(deparse(fun_sym), collapse = "")

      # check for transmute or dplyr::transmute
      if (grepl("(^|::)transmute$", fun_chr)) {
        print(paste0("found transmute in ", file))
        args <- as.list(node)[-1]
        arg_names <- names(args) %||% rep("", length(args))

        for (i in seq_along(args)) {
          lhs <- arg_names[i]
          print(lhs)
          if (lhs == "") next

          if (!is.null(keep) && !(lhs %in% keep)) next

          rhs <- args[[i]]
          found_redcap_vars <- sapply(redcap_vars,
                                      function(x) stringr::str_match(as.character(rhs), paste0("(?:[^A-Za-z0-9_.]|^)(", x, ")", "(?:[^A-Za-z0-9_.]|$)"))[,2])
          tokens <- found_redcap_vars[which(!is.na(found_redcap_vars))]
          if (length(tokens) == 0) print(as.character(rhs))
          src <- if (length(tokens) == 1) tokens else if (length(tokens > 1)) paste0("composite: ",
                                                             paste(tokens, collapse = ", ")) else NA
          res[[length(res) + 1]] <<- list(lhs = lhs, src = src)
        }
      }

      # Check every part of the call
      lapply(as.list(node), walk_ast)
    }
  }

  lapply(expr, walk_ast)
  return(res)
}

#' Scrape functions that generate targets to map variable names to the
#' REDCap variables they were created from
#'
#' @description
#' Scrape the functions that generate targets and map final variable names
#' to their REDCap ancestor variables. Produces a yaml file that lists the
#' target dataframes with their mappings (from final vars to REDCap vars). The
#' scraper looks for transmute calls. If a variable is constructed from more than
#' one REDCap ancester, the mapping is listed as "composite".
#'
#' @param output_dir file path to directory where the yaml should be saved
#' @param api_token_path file path to txt file containing redcap project token
#' @param exclude_redcap_vars character vector of redcap variables the parser
#' should *not* search for. Default is "co" because it captures tons of stuff.
#' @param src_dir directory to search for functions that generate variables.
#' Default is "src/".
#' @param exclude_targets a character vector with names of targets to exclude.
#' Excludes core targets by default.
#' @returns side effect: a yaml file is created in `output_dir`. Invisibly returns
#' the list object containing the mapping
#'
#' @importFrom stringr str_extract
#' @export
#'
camr_map_redcap_vars <- function(output_dir,
                                 api_token_path,
                                 exclude_redcap_vars = "co",
                                 src_dir="src",
                                 exclude_targets = c('rds_download',
                                                     'chr_path_redcap_data',
                                                     'chr_path_tlfb_data',
                                                     'lst_redcap_data',
                                                     'df_tlfb_raw',
                                                     'df_id_table',
                                                     'df_redcap_raw')) {
  # Input checks ----
  stopifnot(dir.exists(src_dir))
  if (!dir.exists(output_dir)) dir.create(output_dir)

  # Get project varnames ----
  meta <- camr_redcap_field_meta(api_token_path)
  redcap_vars <- meta$field_name
  redcap_vars <- redcap_vars[!(redcap_vars %in% exclude_redcap_vars)]

  # Manifest ----
  man <- targets::tar_manifest(fields = c("name", "command"))
  man <- man[!(man$name %in% exclude_targets),]

  # extract function names from commands
  man$fun <- vapply(man$command,
                    \(cmd) stringr::str_extract(cmd, "^([A-Za-z][A-Za-z0-9_.]*)\\(", 1),
                    character(1))

  # which script defines which function
  r_files <- list.files(src_dir, pattern = "\\.R$", recursive = TRUE,
                        full.names = TRUE)
  fun_table <- purrr::map_dfr(r_files,
                              function(f) {
                                defs <- stringr::str_match_all(
                                  readLines(f, warn = FALSE),
                                  "\\s*([A-Za-z][A-Za-z0-9_.]*)\\s*<-\\s*function")
                                df <- try(do.call(rbind.data.frame, defs))
                                if (inherits(df, "try-error") || nrow(df) == 0) {
                                  return(data.frame(fun = character(), script = character()))
                                }
                                df$script <- f
                                df <- df[,c("V2", "script")]
                                colnames(df) <- c("fun", "script")
                                return(df)
                              })
  if (nrow(fun_table) == 0) {
    stop("No functions found in src_dir: ", src_dir)
  }
  fun_map <- split(fun_table$script, fun_table$fun)

  # Create mapping ----
  mapping <- list()

  for (i in seq_len(nrow(man))) {
    target_name <- man$name[i]
    function_name <- man$fun[i]

    if (is.na(function_name) || !(function_name %in% names(fun_map))) next

    helper_file <- fun_map[[function_name]][1]

    # Pull variable names from target df
    colnames_target <- tryCatch(
      names(targets::tar_read(target_name)),
      error = function(e) character())

    pairs <- parse_transmute_pairs(helper_file,
                                   redcap_vars = redcap_vars)

    if (length(pairs) == 0) next

    mapping[[target_name]] <- purrr::reduce(
      pairs,
      function(acc, pr) {
        acc[[pr$lhs]] <- pr$src
        return(acc)
      },
      .init = list())
  }

  if (length(mapping) == 0) {
    warning("no transmute mappings found")
    return(invisible(NULL))
  }

  # Write YAML ----
  out_file <- file.path(output_dir,
                       sprintf("redcap_var_map_%s.yaml", format(Sys.time(), "%Y%m%dT%H%M%S")))
  yaml::write_yaml(mapping, out_file)
  message("Variable map written to ", out_file)
  invisible(out_file)
}
