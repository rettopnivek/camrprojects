# Miscellaneous functions
# Written by...
#   Kevin Potter
# Maintained by...
#   Kevin Potter
# Email:
#   kpotter5@mgh.harvard.edu
# Please email us directly if you
# have any questions or comments
# Last updated: 2023-07-24

#### 1) example_CAM_data_set ####
#' A Fake Example CAM Data Set
#'
#' A data set formatted like typical CAM data. Provides
#' fake data for a hypothetical study with 8 screened individuals
#' and six enrolled participants who each completed two visits
#' in which they were dosed with a placebo or active drug and
#' completed the Brief Pain Inventory (BPI) severity subscale
#' prior to and after being dosed.
#'
#' @docType data
#'
#' @usage data(example_CAM_data_set)
#'
#' @format A data frame with 32 rows and 18 variables:
#' \describe{
#'   \item{IDS.INT.Screening}{Identifier for screened individuals}
#'   \item{SSS.DAT.Date_of_visit}{The date of the study session}
#'   \item{IDS.CHR.Participant}{Identifier for the enrolled individual}
#'   \item{SSS.CHR.Session}{Label for the study session}
#'   \item{SSS.CHR.Event}{Label for time points within a study session}
#'   \item{IDX.INT.Person}{Index for individuals}
#'   \item{IDX.INT.Session}{Index for study sessions}
#'   \item{IDX.INT.Event}{Index for time points within a study session}
#'   \item{SSS.CHR.Condition}{Whether dosed with active drug or placebo}
#'   \item{SBJ.INT.Age}{Age of individual in years}
#'   \item{SBJ.INT.Age_at_visit_1}{Alternative measure of age}
#'   \item{SBJ.INT.Biological_sex}{Biological sex of participant where
#'     1 = male and 2 = female}
#'   \item{SBJ.CHR.Race}{Racial identify of individual}
#'   \item{SBJ.CHR.Ethnicity}{Ethnicity of individual}
#'   \item{INV.INT.HADS_anxiety}{Scores on the anxiety subscale of the
#'     Hospital Anxiety and Depression Scale}
#'   \item{INV.INT.HADS_depression}{Scores on the depression subscale of the
#'     Hospital Anxiety and Depression Scale}
#'   \item{INV.INT.AIS}{Scores on the Athens Insomnia Scale}
#'   \item{INV.DBL.BPI_severity}{Scores on the severity subscale of the
#'     Brief Pain Inventory}
#' }
#'
#' @keywords datasets
#'
#' @examples
#'
#' data(example_CAM_data_set)
#'
"example_CAM_data_set"

#### 2) Read in TLFB Data ####
#' Read in TLFB V1/V2 JSON Data
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
camr_read_tlfb_v2 <- function (chr_path_to_json_dir, recursive=FALSE) {
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

    }, .progress=list(format='Reading TLFB V1/V2 JSON Data {cli::pb_bar} {cli::pb_current}/{cli::pb_total} {cli::pb_eta_str}')) |>
    bind_rows()
}
