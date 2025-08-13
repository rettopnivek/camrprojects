# Functions to compare sample data with Census (PUMS) and
# NSDUH samples

#' Download PUMS from census data
#'
#' @description
#' Function that downloads PUMS data
#'
#' @param census_api_key_path A string with the path to a txt file
#' with an API key for census.gov. Not required if data are cached.
#' @param cache_dir A string with the directory to cache the pums data
#'                  Defaults to "~/.cache/pums". Creates the directory
#'                  if it does not exist.
#' @param additional_vars A character vector with PUMS variable names
#'                        to download
#' @param year An integer. The year of PUMS data to download. Note that
#'             the processing code is based on 2023. (A warning will print
#'             for non-2023 years). Defaults to 2023.
#' @param survey A string. The survey you want to download. Defaults to
#'               "asc1".
#' @returns Side effect: downloads PUMS data to `cache_path`.
#'          Invisibly returns a dataframe with the PUMS microdata
#'
#' @importFrom tidycensus census_api_key get_pums
#' @importFrom fs dir_create
#'
download_census <- function(census_api_key_path,
                            cache_dir = "~/.cache/pums",
                            additional_vars = NULL,
                            year = 2023,
                            survey = "acs1") {
  # API setup ----
  # load census API token
  api_token <- readLines(census_api_key_path)[1] |>
    stringr::str_trim()

  # Put api key in .Renvirons
  tidycensus::census_api_key(api_token)

  vars_to_pull <- c("AGEP", "RAC1P", "SEX",
                    "PWGTP", "HISP",
                    additional_vars)

  # Get Age, Race, and Sex distributions (from PUMS data) ----
  age_sex <- tidycensus::get_pums(
    state = "all",
    year = year,
    survey = survey,
    variables = vars_to_pull
  )

  # Cache data
  fs::dir_create(cache_dir)
  meta <- list(variables = vars_to_pull,
               year = year,
               survey = survey)
  output <- list(meta = meta, data = age_sex)
  saveRDS(output, fs::path(cache_dir,paste0("pums_",year,"_",Sys.time(), ".RDS")))
  return(invisible(output))
}

#' Process PUMS Census Data (for comparison with CAM study samples)
#'
#' @description
#' Process raw PUMS data to prepare for comparison with CAM study data. Expects
#' list output from `download_census`. By default the processing retains
#' repondents of all ages from all 50 states (plus DC and Puerto Rico.)
#'
#' @param pums_list A list. The list output by `download_census`, where the
#'                  first element is metadata and the second element is a
#'                  dataframe.
#' @param age_lower_limit Optional. An integer. The minimum age (inclusive) for your
#'                        population of interest.
#' @param age_upper_limit Optional An integer. The maximum age (inclusive) for your
#'                        population of interest.
#' @param states_to_drop Optional. A character vector. The abbreviations of the states
#'                       NOT in your population (e.g. c("PR", "DC")).
#' @param states_to_keep Optional. A character vector. The abbreviations of the
#'                       states (e.g. c("NY", "MA")) that delimit your population.
#' @returns A list with a metadata element and the dataframe containing
#'          the processed PUMS data
#'
process_pums <- function(pums_list_obj,
                         age_lower_limit = NULL,
                         age_upper_limit = NULL,
                         states_to_drop = NULL,
                         states_to_keep = NULL) {

  if (pums_list_obj$meta$year != 2023) {
    warning(sprintf("Recoding based on 2023 Codebook. Check for changes in
            %s codebook.", year))
  }

  # Can specify states to drop or states to keep, but not both
  stopifnot(is.null(states_to_drop) | is.null(states_to_keep))

  # Recode variables
  state_map <- list("01" = "AL",
                    "02" = "AK",
                    "04" = "AZ",
                    "05" = "AR",
                    "06" = "CA",
                    "08" = "CO",
                    "09" = "CT",
                    "10" = "DE",
                    "11" = "DC",
                    "12" = "FL",
                    "13" = "GA",
                    "15" = "HI",
                    "16" = "ID",
                    "17" = "IL",
                    "18" = "IN",
                    "19" = "IA",
                    "20" = "KS",
                    "21" = "KY",
                    "22" = "LA",
                    "23" = "ME",
                    "24" = "MD",
                    "25" = "MA",
                    "26" = "MI",
                    "27" = "MN",
                    "28" = "MS",
                    "29" = "MO",
                    "30" = "MT",
                    "31" = "NE",
                    "32" = "NV",
                    "33" = "NH",
                    "34" = "NJ",
                    "35" = "NM",
                    "36" = "NY",
                    "37" = "NC",
                    "38" = "ND",
                    "39" = "OH",
                    "40" = "OK",
                    "41" = "OR",
                    "42" = "PA",
                    "44" = "RI",
                    "45" = "SC",
                    "46" = "SD",
                    "47" = "TN",
                    "48" = "TX",
                    "49" = "UT",
                    "50" = "VT",
                    "51" = "VA",
                    "53" = "WA",
                    "54" = "WV",
                    "55" = "WI",
                    "56" = "WY",
                    "57" = "PR")
  age_sex <- pums_list_obj$data |>
    dplyr::mutate(Sex = dplyr::case_when(SEX == 2 ~ "Female",
                                         SEX == 1 ~ "Male"),
                  Age = AGEP |> as.numeric(),
                  Hispanic = dplyr::case_when(HISP != "01" ~ "Hispanic or Latino",
                                              HISP == "01" ~ "Not Hispanic or Latino"),
                  Race = factor(RAC1P, levels = 1:9,
                                labels = c("White",
                                           "Black",
                                           "American Indian",
                                           "Alaska Native",
                                           "Native Tribe Specified",
                                           "Asian",
                                           "Hawaiian/Pac Islander",
                                           "Other",
                                           "More than one race")),
                                      State = unlist(state_map[STATE])) |>
    select(PWGTP, Sex, Age, Hispanic, Race, State)

  # We never distinguish between American Indian, Alaska Native, and "Native Tribe Specified"
  # So we'll combine those categories
  age_sex <- age_sex |>
    mutate(Race = forcats::fct_collapse(Race,
                                        "White" = "White",
                                        "Black" = "Black",
                                        "Asian" = "Asian",
                                        "Hawaiian/Pacific Islander" = "Hawaiian/Pac Islander",
                                        "American Indian/Alaska Native" = c("American Indian",
                                                                            "Alaska Native",
                                                                            "Native Tribe Specified"),
                                        "More than one race" = "More than one race"))

  if (!is.null(age_lower_limit)) {
    age_sex <- age_sex |> dplyr::filter(Age >= age_lower_limit)
    pums_list_obj$meta$lower_age <- age_lower_limit
  }
  if (!is.null(age_upper_limit)) {
    age_sex <- age_sex |> dplyr::filter(Age <= age_upper_limit)
    pums_list_obj$meta$upper_age <- age_upper_limit
  }
  if (!is.null(states_to_drop)) {
    age_sex <- age_sex |> dplyr::filter(!(State %in% states_to_drop))
    pums_list_obj$meta$states_excluded <- states_to_drop
  }
  else if (!is.null(states_to_keep)) {
    age_sex <- age_sex |> dplyr::filter(State %in% states_to_keep)
    pums_list_obj$meta$states_included <- states_to_keep
  }
  else pums_list_obj$meta$states_included <- "All States, DC, and Puerto Rico"

  return(list(meta = pums_list_obj$meta,
              data = age_sex))
}

#' Collapse census data by variable using survey weights
#'
#' @description
#' Collapse PUMS microdata into representative summary by grouping variable
#'
#' @param pums_list_obj A processed PUMS dataframe. The output of `process_pums`.
#' @param group_variable Variable by which to group summary (e.g. Race or Sex)
#' @param weight_variable Varible with population weights. Defaults to
#'                        PWGTP
#' @returns A list. Metadata (as a list), plus a dataframe with proportions of
#'          population in each category.
#'
collapse_pums <- function(pums_data,
                          group_variable,
                          weight_variable = PWGTP) {
  pums_data <- pums_data |> dplyr::group_by({{group_variable}}) |>
    dplyr::summarize(weight = sum({{weight_variable}}))  |>
    dplyr::ungroup() |>
    dplyr::mutate(`Census Proportion` = weight/sum(weight))

  return(pums_data)
}

#' Function to get summary dataframe of PUMS data
#'
#' @description
#' Loop over the grouping variables in processed PUMS data and produce a
#' list of summary dataframes
#'
get_pums_summaries <- function(pums_data) {
  summary_vars <- list(race_var = "Race",
                       hisp_var = "Hispanic",
                       sex_var = "Sex",
                       age_var = "Age")
  summary_dfs <- lapply(summary_vars, \(v) collapse_pums(pums_data,
                                          !!sym(v),
                                          PWGTP))
  return(summary_dfs)
}

#' Process sample data to make Census comparison table
#'
#' @description
#' `pums_prep_sample_data` processes sample data (usually from a pipeline-processed
#'                         demographics target dataframe) to prepare it for
#'                         making a comparison table with PUMS data.
#'
#' @param demographics_target_df A dataframe. The pipeline-processed demographics
#'        for your sample
#' @param race_var The race variable in your target dataframe.
#'                 Defaults to `SBJ.FCT.Race`
#' @param sex_var The sex (at birth) variable in your target dataframe.
#'                Defaults to `SBJ.FCT.Sex`
#' @param hispanic_var The ethnicity (Hispanic-indicating) variable in your
#'                     target dataframe. Defaults to `SBJ.FCT.Ethnicity`
#' @param race_map A list mapping the PUMS race values to your sample. The default
#'                 contains the mapping for the standard pipeline. The names
#'                 of the default list should be maintained (they reflect the
#'                 options on the 2023 census).
#'
pums_prep_sample_data <- function(demographics_target_df,
                                  race_var = "SBJ.FCT.Race",
                                  sex_var = "SBJ.FCT.Sex",
                                  hispanic_var = "SBJ.FCT.Ethnicity",
                                  age_var = "SBJ.INT.AgeAtEnrollment",
                                  race_map = list("White" = "White",
                                                  "Black" = "Black",
                                                  "Asian" = "Asian",
                                                  "Hawaiian/Pacific Islander" = "Hawaiian/Pacific Islander",
                                                  "American Indian/Alaska Native" = "American Indian/Alaska Native",
                                                  "Other" = "Middle Eastern/North African",
                                                  "More than one race" = "More than one race")) {
  sample_df <- demographics_target_df

  # Drop races not in mapping and print warning
  dropped_races <- base::setdiff(unique(demographics_target_df[[race_var]]),
                                 unlist(race_map))
  if (length(dropped_races)) {
    warning(sprintf("The following race values are not mapped and will be dropped: %s",
                    paste(dropped_races, collapse = "; ")))
    sample_df <- demographics_target_df |> filter(! .data[[race_var]] %in% dropped_races)
  }

  # Drop sexes not in Census and print warning
  census_sexes <- c("Male", "Female")
  dropped_sexes <- base::setdiff(unique(demographics_target_df[[sex_var]]),
                                 census_sexes)
  if (length(dropped_sexes)) {
    warning(sprintf("The following sex values are not in the Census and will be dropped: %s",
                    paste(dropped_sexes, collapse = "; ")))
    sample_df <- sample_df |> filter(! .data[[sex_var]] %in% dropped_sexes)
  }

  # Drop ethnicity values not in Census and print warning
  census_ethnicity <- c("Hispanic or Latino", "Not Hispanic or Latino")
  dropped_ethnicity <- base::setdiff(unique(demographics_target_df[[hispanic_var]]),
                                     census_ethnicity)
  if (length(dropped_ethnicity)) {
    warning(sprintf("The following ethnicity values are not in the Census: %s",
                                                 paste(dropped_ethnicity, collapse = "; ")))
    warning(sprintf("The Census options include: %s",
                    paste(census_ethnicity, collapse = ";")))
    sample_df <- sample_df |> filter(! .data[[hispanic_var]] %in% dropped_ethnicity)
  }


  # Create vars that match the PUMS
  sample_df <- sample_df |>
    mutate(Race = dplyr::case_when(.data[[race_var]] %in% race_map[["White"]] ~ "White",
                                   .data[[race_var]] %in% race_map[["Black"]] ~ "Black",
                                   .data[[race_var]] %in% race_map[["Asian"]] ~ "Asian",
                                   .data[[race_var]] %in% race_map[["Hawaiian/Pacific Islander"]] ~ "Hawaiian/Pacific Islander",
                                   .data[[race_var]] %in% race_map[["American Indian/Alaska Native"]] ~ "American Indian/Alaska Native",
                                   .data[[race_var]] %in% race_map[["Other"]] ~ "Other",
                                   .data[[race_var]] %in% race_map[["More than one race"]] ~ "More than one race"),
           Sex = dplyr::case_when(.data[[sex_var]] == "Female" ~ "Female",
                                  .data[[sex_var]] == "Male" ~ "Male"),
           Hispanic = dplyr::case_when(.data[[hispanic_var]] == "Hispanic or Latino" ~ "Hispanic or Latino",
                                       .data[[hispanic_var]] == "Not Hispanic or Latino" ~ "Not Hispanic or Latino"),
           Age = .data[[age_var]])

  # Aggregate and bind summaries
  summary_vars <- list(race_var = "Race",
                       hisp_var = "Hispanic",
                       sex_var = "Sex",
                       age_var = "Age")
  summary_dfs <- lapply(summary_vars,
         \(v) sample_df |> group_by(!!sym(v)) |>
           summarize(`Sample Proportion` = n()/nrow(sample_df),
                     `Sample Count` = n()) |> ungroup())

  return(summary_dfs)
}

#' Make table comparing PUMS-derived population with sample demographics
#'
#' @description
#' Creates a table comparing the sex, ethnicity, and age distributions of
#' a sample with a population derived from the Census's PUMS data.
#'
make_pums_comparison_table <- function(sample_summary_dfs,
                                       pums_summary_dfs,
                                       output_path) {

  ## Merge together summary dataframes (from sample and PUMS) ----
  comp_vars <- names(pums_summary_dfs)
  print(sapply(sample_summary_dfs, \(df) colnames(df)[1]))
  comp_dfs <- lapply(comp_vars,
                     \(v) base::merge(sample_summary_dfs[[v]],
                                      pums_summary_dfs[[v]],
                                      by = colnames(pums_summary_dfs[[v]])[1],
                                      all = TRUE) |>
                       select(!!sym(colnames(pums_summary_dfs[[v]])[1]),
                              `Sample Count`, `Sample Proportion`, `Census Proportion`) |>
                       mutate(across(where(is.numeric), ~ round(.x, 2)),
                              across(where(is.numeric), ~ ifelse(is.na(.x), 0, .x)))) |>
    setNames(sapply(pums_summary_dfs, \(df) colnames(df)[1]))

  ## Make html tables from list of summary dataframes ----
  n_cols <- ncol(comp_dfs$Race)
  col_width <- round(100/n_cols, 1)

  make_table_html <- function(df, caption = NULL) {
    table <- knitr::kable(df, format = "html", table.attr = "class='table table-striped'") |>
      kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                    full_width = FALSE,
                    position = "center")
    for (i in seq_len(n_cols)) {
      table <- kableExtra::column_spec(table, i, width = paste0(col_width, "%"))
    }

    paste0("<div style='display: inline-block; margin-right; 20px;'>", as.character(table), "</div>")
  }

  tables <- sapply(comp_dfs, make_table_html)

  full_html <- paste0("<html><head><meta charset='UTF-8'></head><body>",
                      "<link rel='stylesheet' href='https://cdn.jsdelivr.net/npm/bootstrap@3.4.1/dist/css/bootstrap.min.css'>",
                      paste(tables, collapse = "<br><br>"),
                      "</body></html>")
  writeLines(full_html, output_path)
  return(invisible(full_html))

}

#' Process PUMS and Sample Data. Produce a table comparing the sample to the Census.
#'
#'
#' @export
#'
camr_census_compare <- function(demographics_df,
                                output_path,
                                census_age_max = NULL,
                                census_age_min = NULL,
                                census_states_to_keep = NULL,
                                census_states_to_drop = NULL,
                                sample_data_args = NULL,
                                pums_cache = "~/.cache/pums",
                                census_api_key_path = NULL,
                                force_pums_download = FALSE) {

  # Download PUMS if forced or Cache is empty
  if (force_pums_download | (!dir.exists(pums_cache))) {
    download_census(census_api_key_path, cache_dir = pums_cache)
  }

  if (length(list.files(pums_cache)) > 1) {
    stop("Your PUMS cache contains more than one file.")
  }

  # Load and prep PUMS data
  pums <- readRDS(fs::path(pums_cache, list.files(pums_cache)[1]))
  pums <- process_pums(pums,
                       age_lower_limit = census_age_min,
                       age_upper_limit = census_age_max,
                       states_to_drop = census_states_to_drop,
                       states_to_keep = census_states_to_keep)
  pums <- list(meta = pums$meta,
               data = get_pums_summaries(pums_data = pums$data))

  # Prep sample data
  if (is.null(sample_data_args)) {
    sample_dfs <- pums_prep_sample_data(demographics_df)
  }
  else sample_dfs <- rlang::exec(pums_prep_sample_data,
                                 !!!sample_data_args,
                                 demographics_target_df = demographics_df)

  make_pums_comparison_table(sample_dfs, pums$data,
                             output_path = output_path)
}

#' Download and cache NSDUH data
#'
#' @description
#' Download and cache data from NSDUH
#'
get_nsduh <- function() {

}
