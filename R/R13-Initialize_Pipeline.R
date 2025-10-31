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
#' @param excluded_forms vector of form/instrument names that should *not* have
#' a target or processing function. Default vector includes administrative forms
#'
#' @return side effect: `dest_dir` is populated with a provisional targets pipeline.
#' `dest_dir` is also invisibly returned as a string.
#'
#' @importFrom redcapAPI redcapConnection exportMetaData exportMappings exportRepeatingInstrumentsEvents
#' @export
#'
#' @author Zach Himmelsbach
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
camr_init_pipeline <- function(token_file,
                               project_name,
                               nickname,
                               dest_dir,
                               std_repo_url,
                               std_repo_branch = 'main',
                               redcap_url      = Sys.getenv("REDCAP_API_URL"),
                               excluded_forms = c('informed_consent',
                                                  'contact_information',
                                                  'intake_summary',
                                                  'adverse_event_review',
                                                  'concomitant_medication_review',
                                                  'data_checking',
                                                  'clinician_consult',
                                                  'remuneration')) {
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
  ## Debug print
  rep_forms <- if (!is.null(repeating)) unique(repeating$form_name) else character()

  instruments <- unique(md$form_name)
  # Remove timeline followback (which we'll always keep by default)
  instruments <- setdiff(instruments, "timeline_followback")
  # Remove excluded instruments (e.g. admin things we don't need to process)
  instruments <- setdiff(instruments, excluded_forms)
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

  # 3: index std_ functions ----
  msg("Scanning standard pipeline for existing instrument functions ...")
  # Scripts/functions (in src/process) that we want to keep no matter what
  scripts_to_keep <- c('src/process/measurement/ME04-TLFB.R',
                       'src/process/visit/VI22-TLFB.R')
  src_files <- fs::dir_ls(fs::path(dest_dir, "src/process"), recurse = TRUE, glob = "*.R")
  # Don't look in files with functions for core targets (e.g. TLFB)
  src_files <- src_files[!stringr::str_detect(src_files, paste(scripts_to_keep, collapse = "|"))]
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
      print(orig_path)
      # Extract the folder name from the original path
      # e.g., src/process/measurement/ME02-AdverseEvents.R → measurement
      orig_subdir <- fs::path_file(fs::path_dir(orig_path))

      # Map it to your new src folder, e.g., src/measurement/
      dest_sub <- fs::path(dest_dir, "src", orig_subdir)
      fs::dir_create(dest_sub)

      # New path: keep original filename
      new_path <- fs::path(dest_sub, fs::path_file(orig_path))
      print(new_path)

      # Read the code and prepend header
      code <- readLines(orig_path, warn = FALSE)
      code <- c(sprintf("# Generated from %s in standard pipeline on %s", basename(orig_path), Sys.Date()),
                code)
      writeLines(code, new_path)

      # Edit map to standard functions
      std_map[[form]] <<- new_path
      print(std_map[[form]])

    } else {
      ## Create stub (for instruments without an existing standard pipeline function) ----
      print(paste('Standard pipeline function NOT available for', form))
      fun_name <- sprintf("%s_%s_%s", nickname, ftype, slug(form))
      stub <- c(
        sprintf("%s <- function(df_redcap_raw) {", fun_name),
        sprintf("  df_%s <- df_redcap_raw |>", slug(form)),
        sprintf("    dplyr::filter(VST.CHR.REDCap.Form == '%s') |>", form),
        sprintf("    pivot_wider(
        names_from=field_name,
        values_from=value,
        values_fn=camr_util_collapse
    ) |>"),
        sprintf("    camr_guarantee_fields(\"%s\") |>", form),
        sprintf("    camr_process_redcap_vars() |>"),
        "  # TODO: add processing steps for this instrument",
        sprintf('    mutate(
    IDX.CHR.Subject = IDS.CHR.Subject,
    IDX.CHR.Visit = VST.CHR.Visit
    ) |>
      select(IDX.CHR.Subject,
             IDX.CHR.Visit,
             starts_with("INV."))'),
        sprintf("  return(df_%s)", slug(form)),
        "}"
      )
      writeLines(stub, fs::path(dest_sub, sprintf("%s.R", fun_name)))
    }
  })

  msg("Pruning unused standard pipeline functions ...")
  all_std_files  <- fs::dir_ls(fs::path(dest_dir, "src/process"), recurse = TRUE, glob = "*.R")

  # # delete files that aren't part of the current project and aren't part of scripts_to_keep
  # to_delete <- setdiff(all_std_files, scripts_to_keep)
  # if (length(to_delete)) {
  #   msg("Deleting %d unused standard pipeline files", length(to_delete))
  #   fs::file_delete(to_delete)
  # }

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
  # There may be nothing in tail
  if ((close + 1) > length(txt)) tail_txt <- ""

  # locate each target inside list
  blocks <- list()
  start <- NULL
  for (i in seq(open+1, close-1)) {
    if (grepl("^\\s*tar_(target|file)\\s*\\(", txt[i])) start <- i
    if (!is.null(start) && grepl("^\\s{0,2}\\),\\s*$", txt[i])) {
      blocks <- append(blocks, list(start:i))
      start <- NULL
    }
  }

  #print("Printing Detected Targets:")
  #sapply(blocks, function(idx) print(txt[idx]))
  # Keep only core target blocks
  kept_body <-purrr::keep(blocks, function(idx) {
    block <- txt[idx]

    # Find target name and check against core targets
    tar_line <- block[2] # This will break if comments are added...
    in_core <- stringr::str_detect(tar_line, core_targets) |> any()
    return(in_core)
  })

  kept_body <- sapply(kept_body, function(idx) paste(txt[idx], collapse = '\n')) |>
    unlist()

  # Make sure last kept block ends with a comma (so we can append)
  if (length(kept_body)) {
    last <- length(kept_body)
    if (!grepl("\\),\\s*$", kept_body[last])) {
      kept_body[last] <- sub("\\)\\s*$", "),", kept_body[last])
    }
  }

  kept_body <- paste(kept_body, collapse = '\n')

  # Create blocks for non-core instruments
  inst_forms <- unique(names(form_types))
  new_blocks <- purrr::imap_chr(inst_forms, function(form, idx) {
    lvl <- form_types[[form]]
    var <- sprintf("df_%s_%s", lvl, form)

    # Use original function name if this form had a standard processing script
    if (!is.na(std_map[form])) {
      # Try to extract the base function name from the original file
      orig_code <- readLines(std_map[[form]], warn = FALSE)
      fun_candidates <- stringr::str_match(orig_code, "^\\s*([A-Za-z0-9_]+)\\s*<-\\s*function")
      fun_candidates <- fun_candidates[,2][!is.na(fun_candidates[,2])]
      fun <- if (length(fun_candidates)) fun_candidates[1] else sprintf("%s_%s_%s", nickname, lvl, form)
    } else {
      fun <- sprintf("%s_%s_%s", nickname, lvl, form)
    }

    glue::glue(
      "  tar_target(\n",
      "    {var},\n",
      "    {fun}(df_redcap_raw),\n",
      "  ),\n",
      .open = "{", .close = "}", .trim = FALSE
    )
  })

  ## Organize blocks into subject, visit, and measure targets
  subject_blocks <- c("###### Subject Level Data ######",
                      new_blocks[stringr::str_detect(new_blocks, '_sl_')])
  visit_blocks <- c("###### Visit Level Data ######",
                    new_blocks[stringr::str_detect(new_blocks, '_vl_')])
  measure_blocks <- c("####### Measurement Level Data ######",
                      new_blocks[stringr::str_detect(new_blocks, '_ml_')])

  new_blocks <- c(subject_blocks, visit_blocks, measure_blocks)
  new_blocks[length(new_blocks)] <- sub('\\),\n$', '\\)\n',
                                        new_blocks[length(new_blocks)])

  # Combine new targets with head and tail of file
  body_txt <- c("##### Core Targets #####", kept_body, new_blocks, ")")
  #body_txt <- gsub("std_", paste0(nickname, "_"), body_txt, fixed = TRUE)

  writeLines(c(head_txt, body_txt, tail_txt), tf)

  # 6: Final cleanup (keep TLFB scripts, no reorganization) ----
  msg("Relocating essential scripts out of src/process ...")

  for (script in scripts_to_keep) {
    old_path <- fs::path(dest_dir, script)  # full path including src/process/measurement/...
    print(old_path)
    if (fs::file_exists(old_path)) {
      # Strip "src/process/" from path to get subfolder + filename
      rel_path <- fs::path_rel(old_path, start = fs::path(dest_dir, "src/process"))
      subfolder <- fs::path_dir(rel_path)       # e.g., measurement
      new_dir <- fs::path(dest_dir, "src", subfolder)
      fs::dir_create(new_dir)

      new_path <- fs::path(new_dir, fs::path_file(old_path))
      fs::file_copy(old_path, new_path, overwrite = TRUE)
      msg("Moved %s → %s", old_path, new_path)
    } else {
      warning("Expected script not found: ", old_path)
    }
  }

  # Delete the old src/process folder
  process_dir <- fs::path(dest_dir, "src", "process")
  if (fs::dir_exists(process_dir)) {
    msg("Deleting old src/process directory ...")
    fs::dir_delete(process_dir)
  }

  # 7: Close ----
  msg("Project %s initialised at %s", project_name, dest_dir)
  usethis::create_project(dest_dir)
  # Remove unnecessary "/R" directory that create_project adds
  fs::dir_delete(fs::path(dest_dir,'R'))
  invisible(dest_dir) # Return the new project folder string
}

