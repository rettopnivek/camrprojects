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
#' init_recap_project(
#'   token_file      = "~/Documents/api_tokens/groundbreakingstudy.txt",
#'   project_name.   = "Groundbreaking Substance Use Study"
#'   nickname        = "gbsus"
#'   dest_dir.       = "~/Documents/code_repos/gbsus_2025"
#'   std_repo_url    = "git@gitlab.ourOrg.org:our_team/standard-data-pipeline.git"
#'   redcap_url.     = "https://redcap.ourOrg.org/redcap/api/"
#' )
init_redcap_project <- function(token_file,
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
  msg("Cloning Standard Pipeline from %s (branch = %s)", std_repo_url, std_repo_branch)
  parent_dir <- fs::path_dir(dest_dir)
  fs::dir_create(parent_dir)
  git2r::clone(std_repo_url, dest_dir, branch = std_repo_branch)

  # 2: connect to REDCap & pull metadata ----
  msg("Connecting to REDCap")
  token <- trimws(readLines(token_file, warn = FALSE))
  rc <- redcapAPI::redcapConnection(redcap_uri = redcap_url, token = token)

  msg("Downloading project metadata")
  md <- redcapAPI::exportMetaData(rc)
  mapping <- redcapAPI::exportFormEventMapping(rc)
  # Identify any repeating instruments
  repeating <- tryCatch(redcapAPI::exportRepeatingInstrumentsEvents(rc),
                        error = function(e) NULL)
  rep_forms <- if (!is.null(repeating)) repeating$instrument else character()

  instruments <- unique(md$form_name)

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

  # 3: index std_ functions ----
  msg("Scanning standard pipeline for existing instrument functions")
  src_files <- fs::dir_ls(fs::path(dest_dir, "src"), recurse = TRUE, glob = "*.R")
  form_lookup <- purrr::map_chr(src_files, function(path) {
    lines <- readLines(path, warn = FALSE)
    m <- stringr::str_match(lines, "REDCap.Form\s*==\s*['\"]([A-Za-z0-9_]+)['\"]")
    found <- m[,2][!is.na(m[,2])]
    if (length(found)) found[1] else NA_character_
  })
  names(src_files) <- form_lookup
  has_std <- !is.na(names(src_files))
  std_map <- src_files[has_std]

  # 4: rewrite /src ----
  msg("Rewriting \src")
  purrr::iwalk(form_types, function(ftype, form) {
    subdir <- folder_map[[ftype]]
    dest_sub <- fs::path(dest_dir, "src", subdir)
    fs::dir_create(dest_sub)

    if (!is.na(std_map[[form]])) {
      ## When there's an existing standard function, we copy and (if necessary) ----
      ## move it to the appropriate directory (subject, visit, or measure)
      orig_path <- std_map[[form]]
      new_fname <- sprintf("%s_%s.R", nickname, slug(form))
      new_path <- fs::path(dest_sub, new_fname)

      code <- readLines(orig_path, warn = FALSE)
      # The next line is dangerous and should be updated to look only for function names
      code <- gsub("std_", paste0(nickname, "_"), code, fixed = TRUE)
        # plus, there might be multiple functions in a file
      # Add comment with name of standard pipeline function and date copied
      code <- c(sprintf("# Generated from %s on %s", basename(orig_path), Sys.Date()),
                code)
      writeLines(code, new_path)
      fs::file_delete(orig_path)
    } else {
      ## Create stub (for instruments without an existing standard pipeline function) ----
      fun_name <- sprintf("%s_%s_%s", nickname, nickname, ftype, slug(form))
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
  txt <- readLines(tf, warn = FALSE)
  open <- which(grepl("^\\s*list\\s*\\(", txt))[1]
  close <- tail(which(grepl("^\\s*\\)\\s*$")))

  # locate each target inside list
  blocks <- list()
  start <- NULL
  for (i in seq(open+1, close-1)) {
    if (grepl("^\\s*tar_target\\s*\\(", txt[i])) start <- i
    if (!is.null(start) && grepl("\\), \\s*$")) {
      blocks <- append(blocks, list(start:i))
      start <- NULL
    }
  }

  ## helper to remove or edit blocks ----
  kill_idx <- integer()
  for (idx in blocks) {
    block <- txt[idx]
    # Extract target var (second line, before first comma)
    var_line <- stringr::str_trim(block[2])
    tgt_var <- sub(",.*$", "", var_line)

    # extract std_ function
    fun_line <- block[stringr::str_detect(block, "std_")][1]
    fun_match <- stringr::str_match(fun_line, "std_(sl|vl|ml)_([A-Za-z0-9_]+)")
    if (is.na(fun_match[1])) next

    old_lvl <- fun_match[2]
    form <- fun_match[3]

    if (!(form %in% names(form_types))) {
      # remove this target entry
      kill_idx <- c(kill_idx, idx)
      next
    }

    # For targets we keep, changes nickname (and possibly level)
    new_lvl <- form_types[[form]]
    repl_fun <- paste0(nickname, "_", new_lvl, "_", form)
    repl_var <- paste0("df_", new_lvl, "_", form)

    # target output line update
    block[2] <- sub(tgt_var, repl_var, block[2], fixed = TRUE)
    # function name update
    block <- gsub(fun_match[1], repl_fun, block, fixed = TRUE)
    # change prefixes to project nickname
    block <- gsub("std_", paste0(nickname, "_"), block, fixed = TRUE)

    txt[idx] <- block
  }

  # drop removed target blocks
  if (length(kill_idx)) txt <- txt[-unlist(kill_idx)]

  # global replacement of std_ prefix (for core targets that were not checked)
  block <- gsub("std_", paste0(nickname, "_"), txt, fixed = TRUE)
  writeLines(txt, tf)

  # 6: Close
  msg("Project %s initialised at %s", project_name, dest_dir)

  invisible(dest_dir) # Return the new project folder string
}
