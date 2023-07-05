# Miscellaneous functions
# Written by...
#   Kevin Potter
# Maintained by...
#   Michael Pascale
#   Kevin Potter
# Email:
#   mppascale@mgh.harvard.edu
#   kpotter5@mgh.harvard.edu
# Please email us directly if you
# have any questions or comments
# Last updated: 2022-10-07

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