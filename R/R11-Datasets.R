# Preloaded datasets
# Written by...
#   Kevin Potter
#   Bryn Evohr
# Maintained by...
#   Bryn Evohr
# Email:
#   bevohr@mgh.harvard.edu
# Please email us directly if you
# have any questions or comments
# Last updated: 2024-03-12

# Table of contents
# 1) example_CAM_data_set
# 2) lst_meddra
# 3) lst_asri_scoring
# 4) lst_promis29_scoring
# 5) lst_wais_scoring
# 6) lst_wtar_scoring

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

#### 2) lst_meddra ####
#' MedDRA Database
#'
#' A list containing three dataframes of the MedDRA database.
#' Allows individuals to take downloaded MedDRA codes from
#' REDCap and pull corresponding terms and MedDRA categories.
#'
#' @docType data
#'
#' @usage data(lst_meddra)
#'
#' @format A list with 3 dataframes:
#' \describe{
#'   \item{codes}{All MedDRA codes and the corresponding level}
#'   \item{codes$levels}{MedDRA level}
#'   \item{codes$code}{MedDRA code}
#'   \item{terms}{All level MedDRA terms and codes}
#'   \item{terms$llt_code}{MedDRA code to identify the Lowest Level Term}
#'   \item{terms$pt_code}{MedDRA code to identify the Preferred Term}
#'   \item{terms$hlt_code}{MedDRA code to identify the High Level Term}
#'   \item{terms$hlgt_code}{MedDRA code to identify the High Level Group Term}
#'   \item{terms$soc_code}{MedDRA code to identify the System Organ Class}
#'   \item{terms$llt_name}{Full name of the Lowest Level Term}
#'   \item{terms$pt_name}{Full name of the Preferred Term}
#'   \item{terms$hlt_name}{Full name of the High Level Term}
#'   \item{terms$hlgt_name}{Full name of the High Level Group Term}
#'   \item{terms$soc_name}{Full name of the System Organ Class}
#'   \item{terms$soc_abbrev}{System Organ Class abbreviation}
#'   \item{terms$pt_soc_code}{The primary System Organ Class to which the Preferred Term is linked}
#'   \item{terms$primary_soc_fg}{Flag to indicate primary System Organ Class}
#'   \item{codebook}{Data codebook for MedDRA terms}
#'   \item{codebook$varname}{Variable name for terms dataframe}
#'   \item{codebook$datatype}{Variable data type for terms dataframe}
#'   \item{codebook$description}{Variable description for terms dataframe}
#' }
#'
#' @keywords datasets
#'
#' @examples
#'
#' data(lst_meddra)
#'
"lst_meddra"

#### 3) lst_asri_scoring ####
#' ASRI Scoring Tables
#'
#' A list containing three dataframes with scoring tables
#' for the ASRI (Conners ADHD Self-Report Index).
#'
#' @docType data
#'
#' @usage data(lst_asri_scoring)
#'
#' @format A list with 3 dataframes:
#' \describe{
#'   \item{probability}{Raw scores and associated probability}
#'   \item{raw}{Raw score calculated for each sex, age, question, and response}
#'   \item{tscore}{T-score corresponding to each sex, age, and raw score}
#' }
#'
#' @keywords datasets
#'
#' @examples
#'
#' data(lst_asri_scoring)
#'
"lst_asri_scoring"

#### 4) lst_promis29_scoring ####
#' PROMIS-29 Scoring Tables
#'
#' A list containing seven dataframes with scoring tables
#' for the PROMIS-29 subscores.
#'
#' @docType data
#'
#' @usage data(lst_promis29_scoring)
#'
#' @format A list with 7 dataframes:
#' \describe{
#'   \item{ability_to_participate}{Raw scores, t-scores and SEs for the Ability
#'   to Participate in Social Roles and Activities domain}
#'   \item{anxiety}{Raw scores, t-scores and SEs for the Anxiety domain}
#'   \item{depression}{Raw scores, t-scores and SEs for the Depression domain}
#'   \item{fatigue}{Raw scores, t-scores and SEs for the Fatigue domain}
#'   \item{pain_interference}{Raw scores, t-scores and SEs for the Pain
#'   Interference domain}
#'   \item{physical_function}{Raw scores, t-scores and SEs for the Physical
#'   Function domain}
#'   \item{sleep_disturbance}{Raw scores, t-scores and SEs for the Sleep
#'   Disturbance domain}
#' }
#'
#' @keywords datasets
#'
#' @examples
#'
#' data(lst_promis29_scoring)
#'
"lst_promis29_scoring"

#### 5) lst_wais_scoring ####
#' WAIS-IV Digit Span Scoring Tables
#'
#' A list containing four dataframes with scoring tables
#' for the WAIS-IV Digit Span.
#'
#' @docType data
#'
#' @usage data(lst_wais_scoring)
#'
#' @format A list with 4 dataframes:
#' \describe{
#'   \item{dsb}{Age and raw and scaled scores for the Digit Span Backward}
#'   \item{dsf}{Age and raw and scaled scores for the Digit Span Forward}
#'   \item{dss}{Age and raw and scaled scores for the Digit Span Sequencing}
#'   \item{total}{Age and raw and scaled scores for the Digit Span Total}
#' }
#'
#' @keywords datasets
#'
#' @examples
#'
#' data(lst_wais_scoring)
#'
"lst_wais_scoring"

#### 6) lst_wtar_scoring ####
#' WTAR Scoring Tables
#'
#' A list containing three dataframes with scoring tables
#' for the WTAR (Weschler Test of Adult Reading).
#'
#' @docType data
#'
#' @usage data(lst_wtar_scoring)
#'
#' @format A list with 3 dataframes:
#' \describe{
#'   \item{fsiq}{Standard score and corresponding VIQ, PIQ, and FSIQ}
#'   \item{percentile}{Standard score and corresponding percentile}
#'   \item{standard}{Age, raw score, and corresponding standard score}
#' }
#'
#' @keywords datasets
#'
#' @examples
#'
#' data(lst_wtar_scoring)
#'
"lst_wtar_scoring"
