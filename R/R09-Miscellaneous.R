# Miscellaneous functions
# Written by...
#   Kevin Potter
#   Bryn Evohr
# Maintained by...
#   Bryn Evohr
# Email:
#   bevohr@mgh.harvard.edu
# Please email us directly if you
# have any questions or comments
# Last updated: 2024-10-07

# Table of contents
# 1) camr_read_tlfb_v2
# 2) camr_meddra_join

#### 1) camr_read_tlfb ####
#' Read in TLFB V1/V2/V3 JSON Data
#'
#' @param chr_path_to_json_dir Directory path containing JSON files.
#' @param recursive Whether to search subfolders of the directory for data files.
#'
#' @import lubridate
#' @importFrom purrr map
#' @importFrom tibble tibble_row
#' @importFrom jsonlite read_json
#'
#' @return A dataframe.
#' @export

camr_read_tlfb <- function (chr_path_to_json_dir, recursive=FALSE) {

  dir(chr_path_to_json_dir, pattern='\\.json$', full.names = TRUE, recursive=recursive) |>
    map(\(chr_path){

      lst_data <- read_json(chr_path)

      # Empty data files will have a filename but NA for all other columns.
      if (length(lst_data) < 1)
        return(tibble::tibble_row(cal_filename=basename(chr_path), cal_path=chr_path))

      # Check JSON structure to determine format version.
      if (is.null(lst_data$start)) {

        if (length(names(lst_data)) < 1) {

          # Read V0 JSON.
          # Some information was recorded in REDCap or in the JSON filename. This was not a robust method.
          from_name <- str_match(basename(chr_path), '^(?<id>\\w+)-(?<record>\\d+)-(?<timepoint>\\w+)-(?<date>\\d{4}-\\d{2}-\\d{2}).json')
          tibble_row(
            cal_app_version = 0L,
            cal_filename=basename(chr_path),
            cal_subject=from_name[1, 'id'],
            cal_pid=NA,
            cal_record=from_name[1, 'record'],
            cal_timepoint=from_name[1, 'timepoint'],

            cal_start=NA,
            cal_end=NA,
            cal_days=NA,

            cal_staff=NA,
            cal_datetime=ymd(from_name[1, 'date']),

            # Let cal_events be a nested dataframe containing the substance use
            # and key date calendar events.
            cal_events=lst_data |>
              map(\(evt) data.frame(
                event_title     = evt$title      %||% NA,
                event_type      = evt$type       %||% NA,
                event_start     = ymd(str_sub(evt$start %||% NA, 1, 10)),
                event_category  = evt$category   %||% NA,
                event_substance = evt$substance  %||% NA,
                event_occasions = evt$occasions  %||% NA,
                event_amount    = evt$amount     %||% NA,
                event_units     = evt$units      %||% NA,
                event_unitsOther= evt$unitsOther %||% NA
              )) |>
              bind_rows() |>
              list(),

            cal_path = chr_path
          )
        } else {
          if (is.null(lst_data$from)) {
            warning('Skipping file where unknown format detected: ', chr_path, .immediate=TRUE)
            return(NULL)
          }

          from_name <- str_match(basename(chr_path), '^(?<id>\\w+)-(?<record>\\d+)-(?<timepoint>\\w+)-(?<date>\\d{4}-\\d{2}-\\d{2}).json')
          tibble_row(
            cal_app_version = 1L,
            cal_filename=basename(chr_path),
            cal_subject=lst_data$id %||% from_name[1, 'id'],
            cal_pid=NA,
            cal_record=lst_data$record %||% from_name[1, 'record'],
            cal_timepoint=from_name[1, 'timepoint'],

            cal_start=ymd(str_sub(lst_data$from  %||% NA, 1, 10)),
            cal_end=ymd(str_sub(lst_data$to %||% NA, 1, 10)),
            cal_days=interval(cal_start, cal_end) |> time_length('days'),

            cal_staff=NA,
            cal_datetime=ymd(from_name[1, 'date']),

            # Let cal_events be a nested dataframe containing the substance use
            # and key date calendar events.
            cal_events=lst_data$events |>
              map(\(evt) data.frame(
                event_title     = evt$title      %||% NA,
                event_type      = evt$extendedProps$type       %||% NA,
                event_start     = ymd(str_sub(evt$start, 1, 10)) %||% NA,
                event_category  = evt$extendedProps$category   %||% NA,
                event_substance = evt$extendedProps$substance  %||% NA,
                event_occasions = evt$extendedProps$occasions  %||% NA,
                event_amount    = evt$extendedProps$amount     %||% NA,
                event_units     = evt$extendedProps$units      %||% NA,
                event_notes     = evt$extendedProps$notes %||% NA
              )) |>
              bind_rows() |>
              list(),

            cal_path = chr_path
          )
        }

      } else if (!is.null(lst_data$appversion)) {
        # Read V3 JSON.
        # Version 3 was in use beginning October 2024.
        tibble_row(
          cal_app_version = 3L,
          cal_filename=basename(chr_path),
          cal_subject=lst_data$subject,
          cal_pid=lst_data$pid,
          cal_record=lst_data$record,
          cal_timepoint=lst_data$event,

          cal_start=ymd(lst_data$start),
          cal_end=ymd(lst_data$end),
          cal_days=interval(cal_start, cal_end) |> time_length('days'),

          cal_staff=lst_data$staff,
          cal_datetime=as_datetime(lst_data$datetime %||% NA),

          # Let cal_events be a nested dataframe containing the substance use
          # and key date calendar events.
          cal_events=lst_data$events |>
            map(\(evt) data.frame(
              event_title     = evt$`_title`      %||% NA,
              event_type      = evt$`_type`       %||% NA,
              event_start     = ymd(str_sub(evt$`_date` %||% NA, 1, 10)),
              event_category  = evt$`_category`   %||% NA,
              event_substance = evt$`_substance`  %||% NA,
              event_method    = evt$`_method`     %||% NA,
              event_methodOther= evt$`_methodOther` %||% NA,
              event_methodType= evt$`_methodType` %||% NA,
              event_methodTypeOther= evt$`_methodTypeOther` %||% NA,
              event_occasions = evt$`_times`  %||% NA,
              event_amount    = as.character(evt$`_amount`     %||% NA),
              event_units     = evt$`_units`      %||% NA,
              event_unitsOther= evt$`_unitsOther` %||% NA
            )) |>
            bind_rows() |>
            list(),

          cal_path = chr_path
        )

      } else {
        # Read V2 JSON.
        # Version 2 was in use beginning in May 2021. More information is
        # recorded directly in the file.
        tibble_row(
          cal_app_version = 2L,
          cal_filename=basename(chr_path),
          cal_subject=lst_data$subject,
          cal_pid=lst_data$pid,
          cal_record=lst_data$record,
          cal_timepoint=lst_data$event,

          cal_start=ymd(lst_data$start),
          cal_end=ymd(lst_data$end),
          cal_days=interval(cal_start, cal_end) |> time_length('days'),

          cal_staff=lst_data$staff,
          cal_datetime=as_datetime(lst_data$datetime %||% NA),

          # Let cal_events be a nested dataframe containing the substance use
          # and key date calendar events.
          cal_events=lst_data$events |>
            bind_rows() |>
            (\(x) {if (length(names(x)) > 0) names(x) <- paste0('event_', names(x)); x})() |>
            list(),

          cal_path = chr_path
        )
      }

    }, .progress=list(format='Reading TLFB V1/V2/V3 JSON Data {cli::pb_bar} {cli::pb_current}/{cli::pb_total} {cli::pb_eta_str}')) |>
    bind_rows()
}

#' @rdname camr_read_tlfb
#' @export
camr_read_tlfb_v2 <- function(...) {
  camr_read_tlfb(...)
}
#### 2) camr_meddra_join ####
#' Join MedDRA Terms
#'
#' Function to add MedDRA terms and levels
#' based on the corresponding MedDRA code
#' in a dataframe.
#'
#' @param df The dataframe.
#' @param code_col The column name with MedDRA codes.
#' @param lst_meddra The MedDRA database list.
#'
#' @author Bryn Evohr
#'
#' @returns Dataframe joined with MedDRA terms and levels.
#'
#' @examples
#' data(lst_meddra)
#'
#' camr_meddra_join(df_medhx, INV.INT.MedHx.MedDRA.Code,
#'   lst_meddra) |>
#'   mutate(
#'     INV.CHR.MedHx.MedDRA.Term = code_name,
#'     INV.CHR.MedHx.MedDRA.HLT.Term = hlt_name,
#'     INV.CHR.MedHx.MedDRA.HLGT.Term = hlgt_name,
#'     INV.CHR.MedHx.MedDRA.SOC.Term = soc_name,
#'     INV.FCT.MedHx.MedDRA.Level = level,
#'     .after = INV.INT.MedHx.MedDRA.Code
#'   ) |>
#'   select(-c(level, code_name, hlt_name,
#'     hlgt_name, soc_name)) -> df_medhx_meddra
#'
#' @export

camr_meddra_join <- function (df, code_col, lst_meddra) {

  lst_meddra$codes -> df_meddra_codes
  lst_meddra$terms -> df_meddra_terms

  eqCode <- rlang::enquo(code_col)

  df |>
    mutate(
      code = !!eqCode
    ) |>
    left_join(df_meddra_codes, by = 'code') -> df_meddra_join

  df_meddra_join |>
    filter(level == 'llt') |>
    left_join(df_meddra_terms |> filter(primary_soc_fg != 'N') |> select(
      llt_code, llt_name, pt_code, pt_name, hlt_code, hlt_name, hlgt_code, hlgt_name, soc_code, soc_name
    ) |> distinct(), by = c('code' = 'llt_code')) |>
    mutate(
      code_name = llt_name,
      llt_code = code,
      llt_name,
      pt_code,
      pt_name,
      hlt_code,
      hlt_name,
      hlgt_code,
      hlgt_name,
      soc_code,
      soc_name,
      .after = level
    )-> df_llt

  df_meddra_join |>
    filter(level == 'pt') |>
    left_join(df_meddra_terms |> filter(primary_soc_fg != 'N') |> select(
      pt_code, pt_name, hlt_code, hlt_name, hlgt_code, hlgt_name, soc_code, soc_name
    ) |> distinct(), by = c('code' = 'pt_code')) |>
    mutate(
      code_name = pt_name,
      llt_code = NA,
      llt_name = NA,
      pt_code = code,
      pt_name,
      hlt_code,
      hlt_name,
      hlgt_code,
      hlgt_name,
      soc_code,
      soc_name,
      .after = level
    ) -> df_pt

  df_meddra_join |>
    filter(level == 'hlt') |>
    left_join(df_meddra_terms |> filter(primary_soc_fg != 'N') |> select(
      hlt_code, hlt_name, hlgt_code, hlgt_name, soc_code, soc_name
    ) |> distinct(), by = c('code' = 'hlt_code')) |>
    mutate(
      code_name = hlt_name,
      llt_code = NA,
      llt_name = NA,
      pt_code = NA,
      pt_name = NA,
      hlt_code = code,
      hlt_name,
      hlgt_code,
      hlgt_name,
      soc_code,
      soc_name,
      .after = level
    )  -> df_hlt

  df_meddra_join |>
    filter(level == 'hlgt') |>
    left_join(df_meddra_terms |> filter(primary_soc_fg != 'N') |> select(
      hlgt_code, hlgt_name, soc_code, soc_name
    ) |> distinct(), by = c('code' = 'hlgt_code')) |>
    mutate(
      code_name = hlgt_name,
      llt_code = NA,
      llt_name = NA,
      pt_code = NA,
      pt_name = NA,
      hlt_code = NA,
      hlt_name = NA,
      hlgt_code = code,
      hlgt_name,
      soc_code,
      soc_name,
      .after = level
    )  -> df_hlgt

  df_meddra_join |>
    filter(level == 'soc') |>
    left_join(df_meddra_terms |> filter(primary_soc_fg != 'N') |> select(
      soc_code, soc_name
    ) |> distinct(), by = c('code' = 'soc_code')) |>
    mutate(
      code_name = soc_name,
      llt_code = NA,
      llt_name = NA,
      pt_code = NA,
      pt_name = NA,
      hlt_code = NA,
      hlt_name = NA,
      hlgt_code = NA,
      hlgt_name = NA,
      soc_code = code,
      soc_name,
      .after = level
    )  -> df_soc

  df_meddra_join |>
    filter(is.na(level)) |>
    mutate(
      level = NA,
      code_name = NA,
      hlt_name = NA,
      hlgt_name = NA,
      soc_name = NA
    ) |>
    select(-code) -> df_no_code

  rbind(df_llt, df_pt, df_hlt, df_hlgt, df_soc) |>
    select(-c(code, llt_code, llt_name, pt_code, pt_name, hlt_code, hlgt_code, soc_code)) |>
    rbind(df_no_code) -> df_all_codes

}
