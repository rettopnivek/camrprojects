#' Initialize a new pipeline based on a REDCap project and the standard pipeline
#'
#' @description Clone the standard pipeline, pull REDCap project metadata, and
#' draft an R project directory with _targets.R file stubs to match instruments
#' in the REDCap project. For instruments with processing functions in the
#' standard pipeline, the corresponding processing functions are copied and
#' renamed with the prefixes based on the provided project nickname. Instruments
#' without standard processing functions will have placeholder functions and targets.
#'
#' @param token_file Path to a text file that contains **only** the API token
#' for the target REDCap project.
#' @param project_name A descriptive project title (usually matching the REDCap
#' project name, but not necessarily)
#' @param nickname Short prefix to apply to all project-specific functions
#' @param dest_dir Directory in which the new project should be created. Must
#' *not* exist (it will be created by this function)
#' @param std_repo_url HTTPS or SSH URL of the standard pipeline repository
#' @param std_repo_branch Branch to clone from std_repo_url (default "main")
#' @param redcap_url API endpoint. Defaults to Sys.getenv("REDCAP_API_URL") and
#' aborts if still empty
#'
#' @return side effect: `dest_dir` is populated with a provisional targets pipeline.
#' `dest_dir` is also invisibly returned as a string.
#'
#' @export
#'
#' @examples
#' init_pipeline(
#'   token_file      = "~/Documents/api_tokens/groundbreakingstudy.txt",
#'   project_name.   = "Groundbreaking Substance Use Study"
#'   nickname        = "gbsus"
#'   dest_dir.       = "~/Documents/code_repos/gbsus_2025"
#'   std_repo_url    = "git@gitlab.ourOrg.org:our_team/standard-data-pipeline.git"
#'   redcap_url.     = "https://redcap.ourOrg.org/redcap/api/"
#' )
init_pipeline <- function(token_file,
                                project_name,
                                nickname,
                                dest_dir,
                                std_repo_url,
                                std_repo_branch = 'main',
                                redcap_url      = Sys.getenv("REDCAP_API_URL")) {
  # Input Checks ----
  stopifnot(file.exists(token_file))
  if (nzchar(redcap_url) == FALSE) {
    stop("`redcap_url` is empty - supply it explicitly or set REDCAP_API_URL")
  }
  if (dir.exists(dest_dir)) {
    stop("`dest_dir` already exists - choose a new folder or delete first")
  }
  if (!nzchar(nickname) || grepl("[^A-Za-z0-9_]", nickname)) {
    stop("`nickname` must be a non-empty string with only letters, numbers, and underscores")
  }

  # Helper Functions ----
  # Print progress messages
  msg <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")),
                           sprintf(...), '\n')
  # convert instrument names to acceptable R object names (for function naming)
  slug <- function(x) gsub("[^A-Za-z0-9_]", "_", tolower(x))

  # map from instrument type to src subfolder
  folder_map <- c(sl = 'subject', vl = 'visit', ml = 'measurement')

  # 1: git clone ----
  msg("Cloning Standard Pipeline from %s (branch = %s) ...", std_repo_url, std_repo_branch)
  parent_dir <- fs::path_dir(dest_dir)
  fs::dir_create(parent_dir)
  gert::git_clone(std_repo_url, dest_dir, branch = std_repo_branch)

  # 2: connect to REDCap & pull metadata ----
  msg("Connecting to REDCap ...")
  token <- trimws(readLines(token_file, warn = FALSE))
  rc <- redcapAPI::redcapConnection(url = redcap_url, token = token)

  msg("Downloading project metadata")
  md <- redcapAPI::exportMetaData(rc)
  mapping <- redcapAPI::exportMappings(rc)
  # Identify any repeating instruments
  repeating <- tryCatch(redcapAPI::exportRepeatingInstrumentsEvents(rc),
                        error = function(e) NULL)
  rep_forms <- if (!is.null(repeating)) repeating$instrument else character()

  instruments <- unique(md$form_name)
  #print(instruments)

  # Categorize as subject- visit- or measurement- level
  classify_form <- function(form) {
    is_repeating <- form %in% rep_forms
    n_events <- sum(mapping$form == form)
    if (is_repeating) "ml"
    else if (n_events > 1) "vl"
    else "sl"
  }

  form_types <- setNames(vapply(instruments, classify_form, character(1)),
                         instruments)
  #print(form_types)

  # 3: index std_ functions ----
  msg("Scanning standard pipeline for existing instrument functions ...")
  src_files <- fs::dir_ls(fs::path(dest_dir, "src"), recurse = TRUE, glob = "*.R")
  form_lookup <- purrr::map_chr(src_files, function(path) {
    lines <- readLines(path, warn = FALSE)
    m <- stringr::str_match(lines, "REDCap.Form\\s*==\\s*['\"]([A-Za-z0-9_]+)['\"]")
    found <- m[,2][!is.na(m[,2])]
    if (length(found)) found[1] else NA_character_
  })
  names(src_files) <- form_lookup
  has_std <- !is.na(names(src_files))
  std_map <- src_files[has_std]
  #print(names(std_map))

  # 4: rewrite /src ----
  msg("Rewriting /src ...")
  purrr::iwalk(form_types, function(ftype, form) {
    subdir <- folder_map[[ftype]]
    dest_sub <- fs::path(dest_dir, "src", subdir)
    fs::dir_create(dest_sub)

    if (!is.na(std_map[form])) {
      ## When there's an existing standard function, we copy and (if necessary) ----
      ## move it to the appropriate directory (subject, visit, or measure)
      print(paste('Standard pipeline function available for', form))
      orig_path <- std_map[[form]]
      new_fname <- sprintf("%s_%s.R", nickname, slug(form))
      new_path <- fs::path(dest_sub, new_fname)

      code <- readLines(orig_path, warn = FALSE)
      # The next line is dangerous and should be updated to look only for function names
      code <- gsub("std_", paste0(nickname, "_"), code, fixed = TRUE)
        # plus, there might be multiple functions in a file
      # Add comment with name of standard pipeline function and date copied
      code <- c(sprintf("# Generated from %s in standard pipeline on %s", basename(orig_path), Sys.Date()),
                code)
      writeLines(code, new_path)
      fs::file_delete(orig_path)
    } else {
      ## Create stub (for instruments without an existing standard pipeline function) ----
      print(paste('Standard pipeline function NOT available for', form))
      fun_name <- sprintf("%s_%s_%s", nickname, ftype, slug(form))
      stub <- c(
        sprintf("%s <- function(df_redcap_raw) {", fun_name),
        sprintf("  df_%s <- df_redcap_raw |>", slug(form)),
        sprintf("    dplyr::filter(VST.CHR.REDCap.Form == '%s')", form),
        "  # TODO: add cleaning steps for this instrument",
        sprintf("  return(df_%s)", slug(form)),
        "}"
      )
      writeLines(stub, fs::path(dest_sub, sprintf("%s.R", fun_name)))
    }
  })

  # 5: rewrite _targets.R ----
  msg("Writing _targets.R ...")
  tf <- fs::path(dest_dir, "_targets.R")
  # Set of targets that we ALWAYS want
  core_targets <- c(
    "rds_download",
    "chr_path_redcap_data",
    "chr_path_tlfb_data",
    "lst_redcap_data",
    "df_tlfb_raw",
    "df_id_table",
    "df_redcap_raw",
    "df_vl_tlfb",
    "df_ml_tlfb"
  )
  txt <- readLines(tf, warn = FALSE)
  # Find first line of targets list
  open <- which(grepl("^\\s*list\\s*\\(", txt))[1]
  # Last line of targets list
  close <- tail(which(grepl("^\\s*\\)\\s*$", txt)), n=1)

  # Split file into Head (pre-targets), Targets, and Tail (anything after targets)
  head_txt <- txt[1:open]
  tail_txt <- txt[(close + 1):length(txt)]

  # locate each target inside list
  blocks <- list()
  start <- NULL
  for (i in seq(open+1, close-1)) {
    if (grepl("^\\s*tar_target\\s*\\(", txt[i])) start <- i
    if (!is.null(start) && grepl("^\\),\\s*$", txt[i])) {
      blocks <- append(blocks, list(start:i))
      start <- NULL
    }
  }

  # Keep only core target blocks
  kept_body <- character()
  for (idx in blocks) {
    block <- txt[idx]
    var_line <- sub(",.*$", "", trimws(block[which(nzchar(trimws(block)))[2]]))
    if (var_line %in% core_targets) kept_body <- c(kept_body, block)
  }

  # Make sure last kept block ends with a comma (so we can append)
  if (length(kept_body)) {
    last <- length(kept_body)
    if (!grepl("\\),\\s*$", kept_body[last])) {
      kept_body[last] <- sub("\\)\\s*$", "),", kept_body[last])
    }
  }

  # Create blocks for non-core instruments
  inst_forms <- names(form_types)
  new_blocks <- purrr::imap_chr(inst_forms, function(form, idx) {
    lvl <- form_types[[form]]
    var <- sprintf("df_%s_%s", lvl, form)
    fun <- sprintf("%s_%s_%s", nickname, lvl, form)
    comma <- if (idx < length(inst_forms)) "," else ""

    glue::glue(
      "\ttar_targets(\n",
      "    {var},\n",
      "    {fun}(df_redcap_raw),\n",
      "\t){comma}\n",
      .open = "{", .close = "}", .trim = FALSE
    )
  })

  # Combine new targets with head and tail of file
  body_txt <- c(kept_body, new_blocks, ")")
  body_txt <- gsub("std_", paste0(nickname, "_"), body_txt, fixed = TRUE)

  writeLines(c(head_txt, body_txt, tail_txt))

  # 6: Close
  msg("Project %s initialised at %s", project_name, dest_dir)
  usethis::create_project(dest_dir)
  invisible(dest_dir) # Return the new project folder string
}
