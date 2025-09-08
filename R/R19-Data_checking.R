# Functions to visualize data checking progress

#' Download logging data from REDCap
#'
#' @description
#' Function that downloads logging data from a REDCap project
#'
#' @param chr_rc_uri A character string, the URL to use for the httr::POST function.
#'
#' @param chr_rc_token A character string, the user's API token for the specified REDCap project.
#'
#' @returns A dataframe containing unprocessed REDCap logging information.
#'
download_logging <- function (
    chr_rc_uri = "",
    chr_rc_token = ""
) {

  if ( chr_rc_uri == "" ) {

    chr_rc_uri <- Sys.getenv('API_REDCAP_URI')

  }

  if ( chr_rc_token == "" ) {

    chr_rc_token <- Sys.getenv('API_REDCAP_TOKEN')

  }

  checkmate::assert_string(chr_rc_uri, pattern = '^https://')
  checkmate::assert_string(
    chr_rc_token, n.chars = 32, pattern = '[0-9A-F]{32}'
  )

  lst_formdata <- list(
    token = chr_rc_token,
    logtype='manage',
    content = 'log',
    format = 'csv'
  )

  rsp_response <- httr::POST(
    chr_rc_uri,
    body = lst_formdata, encode = "form"
  )

  df_logging_raw <- httr::content(rsp_response)

}

#' Process logging data from REDCap
#'
#' @description
#' Function that processes raw logging data from REDCap. Expects dataframe output from `download_logging`.
#'
#' @param df_sl_subject_ids A dataframe from the data processing pipeline containing record and subject identifiers.
#'
#' @param df_vl_visits A dataframe from the data processing pipeline containing visit dates.
#'
#' @param df_logging_raw A dataframe containing raw logging data from REDCap.
#'
#' @param record_col A column name specifying the record IDs in df_sl_subject_ids.
#'
#' @param visit_date_col A column name specifying the visit dates in df_vl_visits.
#'
#' @returns A dataframe containing processed REDCap logging information.
#'
process_logging <- function (df_sl_subject_ids, df_vl_visits, df_logging_raw, record_col = IDS.CHR.Record, visit_date_col = SSS.DAT.Visit) {

  vct_ids <- df_sl_subject_ids |>
    dplyr::filter(!stringr::str_detect(IDX.CHR.Subject, 'TEST')) |>
    dplyr::pull({{record_col}})

  dat_project_start <- df_vl_visits |>
    dplyr::filter(!stringr::str_detect(IDX.CHR.Subject, 'TEST')) |>
    dplyr::arrange({{visit_date_col}}) |>
    dplyr::filter(row_number() == 1) |>
    dplyr::pull({{visit_date_col}})

  df_logging <- df_logging_raw |>
    dplyr::filter(stringr::str_detect(details, 'Execute data quality|data query')) |>
    dplyr::transmute(
      date = lubridate::date(timestamp),
      type = dplyr::case_when(
        stringr::str_detect(details, 'Execute data quality') ~ 'rule',
        stringr::str_detect(details, 'data query')  ~ 'query'
      ),
      action = dplyr::case_when(
        stringr::str_detect(details, 'Execute data quality') ~ 'execute',
        stringr::str_detect(details, 'Open data query') ~ 'open',
        stringr::str_detect(details, 'Assign data query') ~ 'assign',
        stringr::str_detect(details, 'Respond to data query') ~ 'respond',
        stringr::str_detect(details, 'Close data query') ~ 'close'
      ),
      details,
      record = stringr::str_extract(details, '(?<=Record: )\\d+'),
      visit = stringr::str_extract(details, '(?<=Event: )(.*?)(?=\\:)'),
      trigger_type = dplyr::case_when(
        stringr::str_detect(details, 'Field: ') ~ 'field',
        stringr::str_detect(details, 'Data Quality Rule: #') ~ 'rule'
      ),
      trigger_field = stringr::str_extract(details, '(?<=Field: )(.*?)(?=(\\)|\\,))'),
      trigger_rule = stringr::str_extract(details, '(?<=Data Quality Rule: )(.*?)(?=(\\)|\\,))'),
      trigger = ifelse(type == 'query', paste0('record ', record, ', ', visit, ', ', dplyr::coalesce(trigger_field, trigger_rule)), NA),
      username
    ) |>
    dplyr::filter(action != 'assign' & date >= dat_project_start & (is.na(record) | record %in% vct_ids))

  return(df_logging)

}

#' Generate data quality rule date table
#'
#' @description
#' Function that pulls the last 3 dates the data quality rules were run. Expects dataframe output from `process_logging`.
#'
#' @param df_logging A dataframe containing processed REDCap logging information.
#'
#' @param users_exclude An optional vector of REDCap usernames listing who to exclude from data quality rule execution.
#'
#' @param out An optional file path to save out an HTML file containing the table.
#'
#' @returns A gt table printing the last 3 dates the data quality rules were executed.
#'
dqr_date_table <- function (df_logging, users_exclude = NULL, out = NULL) {

  if (is.null(users_exclude)) {

    df_logging |>
      dplyr::filter(type == 'rule') |>
      dplyr::group_by(date) |>
      dplyr::summarise(
        rules_run = sum(action == 'execute')
      ) |>
      # only count when at least 10 rules were run
      # less may be testing, not clicking all custom
      dplyr::filter(rules_run >= 10) |>
      dplyr::arrange(desc(date)) -> df_rules_run

  } else {

    df_logging |>
      dplyr::filter(type == 'rule' & !username %in% users_exclude) |>
      dplyr::group_by(date) |>
      dplyr::summarise(
        rules_run = sum(action == 'execute')
      ) |>
      # only count when at least 10 rules were run
      # less may be testing, not clicking all custom
      dplyr::filter(rules_run >= 10) |>
      dplyr::arrange(desc(date)) -> df_rules_run

  }

  df_rules_run[1:3, 1] |>
    dplyr::rename(Date = date) |>
    gt::gt() -> gt_dqr_dates

  if (!is.null(out)) {

    gt::gtsave(gt_dqr_dates, filename = file.path(out, paste0('dqr_dates_', Sys.Date(), '.html')))

  }

  return(gt_dqr_dates)

}

#' Generate histogram of data query response times.
#'
#' @description
#' Function that plots response times to data queries used in the REDCap data checking workflow. Expects dataframe output from `process_logging`.
#'
#' @param df_logging A dataframe containing processed REDCap logging information.
#'
#' @param out An optional file path to save out an SVG file containing the plot.
#'
#' @returns A ggplot object plotting the response times to data queries in REDCap.
#'
query_hist <- function (df_logging, out = NULL) {

  df_logging |>
    dplyr::filter(type == 'query') |>
    dplyr::select(date, trigger, action) |>
    dplyr::arrange(date) |>
    tidyr::pivot_wider(
      names_from = action,
      values_from = date,
      values_fn = ~paste0(.x, collapse = ',')
    ) |>
    tidyr::separate(open, into = c('open_1', 'open_2'), sep = ',') |>
    tidyr::separate(respond, into = c('respond_1', 'respond_2'), sep = ',') |>
    tidyr::separate(close, into = c('close_1', 'close_2'), sep = ',') |>
    suppressWarnings() |>
    tidyr::pivot_longer(
      cols = -trigger,
      names_to = 'name',
      values_to = 'value'
    ) |>
    dplyr::filter(!is.na(value)) |>
    dplyr::mutate(
      trigger = paste0(trigger, ', flag ', str_extract(name, '\\d')),
      name = stringr::str_remove(name, '_\\d')
    ) |>
    tidyr::pivot_wider(
      names_from = name,
      values_from = value
    ) |>
    dplyr::mutate(
      status = dplyr::case_when(
        !is.na(close) ~ 'Closed',
        TRUE ~ 'Open'
      ) |> factor(levels = c('Open', 'Closed')),
      close = dplyr::case_when(
        !is.na(close) ~ lubridate::date(close),
        TRUE ~ Sys.Date()
      ),
      time_to_close = difftime(close, open, units = 'days') |> as.integer()
    ) -> df_query_close

  ggplot2::ggplot(df_query_close, ggplot2::aes(time_to_close, fill = status)) +
    ggplot2::geom_histogram() +
    ggplot2::labs(title = 'Data Query Resolution Latency', subtitle = 'Time Since Opening', x = 'Days', y = 'Count', fill = 'Query Status') +
    ggplot2::theme_classic() -> p_query_hist

  if (!is.null(out)) {

    ggplot2::ggsave(filename = file.path(out, paste0('query_plot_', Sys.Date(), '.svg')), plot = p_query_hist, width = 6, height = 4, units = "in")

  }

  return(p_query_hist)

}

#' Generate histogram of time between visits and initial manual data checks
#'
#' @description
#' Function that plots the time between visits and initial manual data checks.
#'
#' @param df_redcap_raw A dataframe containing raw REDCap study data outputted by the data processing pipeline.
#'
#' @param out An optional file path to save out an SVG file containing the plot.
#'
#' @returns A ggplot object plotting the time between visits and manual data checks in REDCap.
#'
data_check_hist <- function (df_redcap_raw, out = NULL) {

  df_visits <- df_redcap_raw |>
    dplyr::filter((VST.CHR.REDCap.Form == 'start_of_visit_form' & !stringr::str_detect(IDS.CHR.Subject, 'TEST'))) |>
    tidyr::pivot_wider(
      names_from = field_name,
      values_from = value
    ) |>
    dplyr::transmute(
      IDX.CHR.Subject = IDS.CHR.Subject,
      VST.CHR.Visit,
      SSS.LGL.Visit.Attd = sov_ptshow |> as.integer() |> as.logical(),
      SSS.DAT.Visit = sov_date |> date()
    ) |>
    dplyr::filter(!is.na(SSS.LGL.Visit.Attd))

  df_redcap_raw |>
    dplyr::filter((VST.CHR.REDCap.Form == 'data_checking' | field_name == 'data_checking_complete') & !stringr::str_detect(IDS.CHR.Subject, 'TEST') & VST.CHR.Visit != 'Study Level') |>
    dplyr::select(-VST.CHR.REDCap.Form) |>
    tidyr::pivot_wider(
      names_from = field_name,
      values_from = value
    ) |>
    dplyr::transmute(
      IDX.CHR.Subject = IDS.CHR.Subject,
      VST.CHR.Visit,
      QCC.DAT.Check.Completed = check_datetime |> lubridate::date(),
      data_checking_complete,
      dplyr::across(dplyr::matches('check_(?!compby|datetime)', perl = TRUE), as.integer)
    ) |>
    dplyr::right_join(df_visits, by = dplyr::join_by(IDX.CHR.Subject, VST.CHR.Visit)) |>
    dplyr::mutate(
      QCC.FCT.Check.Status = data_checking_complete |> tidyr::replace_na('Missing') |> dplyr::recode('0' = 'Incomplete', '1' = 'Unverified', '2' = 'Complete') |> factor(levels = c('Missing', 'Incomplete', 'Unverified', 'Complete')),
      QCC.DAT.Check.Completed = dplyr::case_when(
        !is.na(QCC.DAT.Check.Completed) ~ lubridate::date(QCC.DAT.Check.Completed),
        TRUE ~ Sys.Date()
      ),
      time_to_check = difftime(QCC.DAT.Check.Completed, SSS.DAT.Visit, units = 'days') |> as.integer()
    ) -> df_check

  ggplot2::ggplot(df_check, ggplot2::aes(time_to_check, fill = QCC.FCT.Check.Status)) +
    ggplot2::geom_histogram() +
    ggplot2::labs(title = 'Manual Data Checking Latency', subtitle = 'Time Between Visit and Data Check', x = 'Days', y = 'Count', fill = 'Status') +
    ggplot2::theme_classic() -> p_manual_hist

  if (!is.null(out)) {

    ggplot2::ggsave(filename = file.path(out, paste0('manual_plot_', Sys.Date(), '.svg')), plot = p_manual_hist, width = 6, height = 4, units = "in")

  }

  return(p_manual_hist)

}

#' Process REDCap logging data. Produce plots and tables summarizing data checking procedures.
#'
#' @description
#' Function that downloads and processes REDCap logging data and joins with data processing pipeline standard outputs to produce figures reporting progress on a study's data checking procedures. Figures include a table presenting the last 3 dates that data quality rules, a histogram depicting the amount of time since a data query was opened, and a histogram depicting the amount of time between a study visit and manual data checking for that visit. By default, the function will output a list of objects containing these figures, and you can set a file path `out` to save out these figures as well.
#'
#' @param df_sl_subject_ids A dataframe from the data processing pipeline containing record and subject identifiers.
#'
#' @param df_vl_visits A dataframe from the data processing pipeline containing visit dates.
#'
#' @param df_redcap_raw A dataframe containing raw REDCap study data outputted by the data processing pipeline.
#'
#'@param chr_rc_uri A character string, the URL to use for the httr::POST function. Pulls from the standard `.renviron` file by default.
#'
#' @param chr_rc_token A character string, the user's API token for the specified REDCap project. Pulls from the standard `.renviron` file by default.
#'
#' @param record_col A column name specifying the record IDs in df_sl_subject_ids.
#'
#' @param visit_date_col A column name specifying the visit dates in df_vl_visits.
#'
#' @param users_exclude An optional vector of REDCap usernames listing who to exclude from data quality rule execution.
#'
#' @param out An optional file path to save out an HTML file and two SVG files containing the table and plots.
#'
#' @returns A list of objects containing two ggplot objects visualizing data checking times and one gt table presenting the last three dates that data quality rules were executed.
#'
#' @export
#' @author Bryn Evohr
#'
#' @examples
#' # at minimum, the following three dataframes from a data processing pipeline in the standard format are required
#' lst_data_checking_figs <- camr_data_checking_report(df_sl_subject_ids, df_vl_visits, df_redcap_raw)
#'
#' # additional arguments can be specified to exclude users' actions (i.e., programmers testing data quality rules) from reports or to output files in a specified location
#' lst_data_checking_figs <- camr_data_checking_report(df_sl_subject_ids, df_vl_visits, df_redcap_raw, users_exclude = c('be931', 'll278', 'zh121'), out = file.path(getwd(), 'data/output'))
#'
#' # each figure can be extracted from the list to include in a progress report
#' lst_data_checking_figs$gt_dqr_dates
#' lst_data_checking_figs$p_query_hist
#' lst_data_checking_figs$p_manual_hist
#'
camr_data_checking_report <- function (df_sl_subject_ids, df_vl_visits, df_redcap_raw, chr_rc_uri = "", chr_rc_token = "", record_col = IDS.CHR.Record, visit_date_col = SSS.DAT.Visit, users_exclude = NULL, out = NULL) {

  df_logging_raw <- download_logging(chr_rc_uri, chr_rc_token)

  df_logging <- process_logging(df_sl_subject_ids, df_vl_visits, df_logging_raw)

  gt_dqr_dates <- dqr_date_table(df_logging, users_exclude, out)

  p_query_hist <- query_hist(df_logging, out)

  p_manual_hist <- data_check_hist(df_redcap_raw, out)

  lst_figs <- list()

  lst_figs[['gt_dqr_dates']] <- gt_dqr_dates
  lst_figs[['p_query_hist']] <- p_query_hist
  lst_figs[['p_manual_hist']] <- p_manual_hist

  return(lst_figs)

}
