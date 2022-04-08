# Dictionary meta-data functions
# Written by...
#   Kevin Potter
# Maintained by...
#   Michael Pascale
#   Kevin Potter
# email: mppascale@mgh.harvard.edu
#        kpotter5@mgh.harvard.edu
# Please email us directly if you
# have any questions or comments
# Last updated 2022-02-04

# Table of contents
# 1) Scale and subscale functions
#   1.1) known_scales
#     1.1.1) HADS
#     1.1.2) CUDIT-R
#     1.1.3) AUDIT
#     1.1.4) AIS
#     1.1.5) BPI
#     1.1.6) PCS
#     1.1.7) PSS
#     1.1.8) SF-12
#     1.1.9) MCQ-SF
#     1.1.10) CHRT
#     1.1.11) CGI
#     1.1.12) MASQ
#     1.1.13) UPPS-P
#     1.1.14) MMM
#     1.1.15) CES-D
#     1.1.16) CWS
#     1.1.17) PDI
#     1.1.18) WHODAS
#     1.1.19) ASRS
#     1.1.20) BIS-11
#     1.1.21) BRIEF
#     1.1.22) DOSPERT
#     1.1.23) KSADS
#     1.1.24) PERS
#     1.1.25) CUD-CHECK
#     1.1.26) PCL
#     1.1.27) WTAR
#     1.1.28) AUQ
#     1.1.29) APSS
#     1.1.30) BPAQ
#     1.1.31) DMQ
#     1.1.32) ERS
#     1.1.33) GLTEQ
#     1.1.34) MCQ
#     1.1.35) MEEQ
#   1.2) scale_format
# 2) column_abbreviations
# 3) Internal functions
#   1.2) create_summary_for_x
# 4) Functions for class 'dictionary_meta_data'
#   4.1) new_dictionary_meta_data
#   4.2) is.dictionary_meta_data
#   4.3) validate_dictionary_meta_data
#   4.4) add_dictionary_meta_data
#   4.5) meta
#   4.6) print.dictionary_meta_data
#   4.7) summary.dictionary_meta_data
#   4.8) subset.dictionary_meta_data
#   4.9) update_meta_data
#   4.10) data_frame_from_dictionary_meta_data

### TO DO ###
# - Finish function documentation
# - Add more scales to 'known_scales'
# - Add more checks to 'validate_dictionary_meta_data'
# - Add examples to functions
# - Create sample data set

#### 1) Scale and subscale functions ####

#### 1.1) known_scales ####
#' Pre-defined Output for Known Scales and Subscales
#'
#' A convenience function that, given an abbreviation
#' for a scale, inventory, or questionnaire, returns
#' a list of information useful for data dictionary
#' purposes.
#'
#' @param abbreviation The abbreviation for a scale, used to
#'   look up the associated pre-defined output.
#' @param subscale The name of a subscale of the scale
#'   given above, used to look up the relevant ancillary
#'   output.
#'
#' @author  Kevin Potter
#'
#' @return A list consisting of...
#' \itemize{
#'   \item \code{Description} A brief description of the
#'     scale/subscale and its purpose
#'   \item \code{Scale} A list with the scale name, abbreviation,
#'     number of items, lowest to highest possible score (if
#'     applicable), clinical cut-offs (if applicable), and
#'     APA-styled references for the scale development/interpretation;
#'   \item \code{Subscale} A list with the subscale name,
#'     number of items, lowest to highest possible score,
#'     clinical cut-offs (if applicable).
#'   \item \code{Units} The units of measurement.
#' }
#'
#' @examples
#' # List all possible inputs (i.e., known
#' # scales, inventories, and questionnaires)
#' known_scales()
#'
#' # Example input
#' known_scales( 'HADS' )
#' known_scales( 'HADS', subscale = 'Anxiety' )
#'
#' @export

known_scales <- function( abbreviation = NULL,
                          subscale = '' ) {

  if ( is.null( abbreviation ) & subscale == '' ) {

    inputs <- c(
      'Accepted inputs:\n\n',

      'Adolescent Psychotic Symptom Screener\n',
      '  abbreviation = "APSS"\n\n',

      'Adult ADHD Self-Report Scale\n',
      '  abbreviation = "ASRS"\n\n',

      'Alcohol Urge Questionnaire\n',
      '  abbreviation = "AUQ"\n',
      '    subscale = "Urge"\n\n',

      'Alcohol Use Disorders Identification Test\n',
      '  abbreviation = "AUDIT"\n\n',

      'Athens Insomnia Scale\n',
      '  abbreviation = "AIS"\n\n',

      'Barratt Impulsiveness Scale\n',
      '  abbreviation = "BIS-11"\n',
      '    subscale = "Attentional"\n',
      '    subscale = "Motor"\n',
      '    subscale = "Planning"\n\n',

      'Behavior Rating Inventory of Executive Function\n',
      '  abbreviation = "BRIEF"\n\n',

      'Brief Pain Inventory (Short form)\n',
      '  abbreviation = "BPI"\n',
      '    subscale = "Severity"\n',
      '    subscale = "Interference"\n\n',

      'Buss-Perry Aggression Questionnaire\n',
      '  abbreviation = "BPAQ"\n',
      '    subscale = "Physical Aggression"\n',
      '    subscale = "Verbal Aggression"\n',
      '    subscale = "Anger"\n',
      '    subscale = "Hostility"\n\n',

      'Cannabis Use Disorder Identification Test - Revised\n',
      '  abbreviation = "CUDIT"\n\n',

      'Cannabis Withdrawal Scale\n',
      '  abbreviation = "CWS"\n',
      '    subscale = "Intensity"\n',
      '    subscale = "Negative impact"\n\n',

      'Clinical Global Impression scale\n',
      '  abbreviation = "CGI"\n',
      '    subscale = "Anxiety"\n',
      '    subscale = "Depression"\n\n',

      'Concise Health Risk Tracking scale\n',
      '  abbreviation = "CHRT"\n',
      '    subscale = "Despair"\n', # 3
      '    subscale = "Helplessness"\n', # 2
      '    subscale = "Lack of social support"\n', # 2
      '    subscale = "Pessimism"\n', # 2
      '    subscale = "Suicidal thoughts"\n', # 3
      '    subscale = "Propensity"\n\n',

      'Domain-Specific Risk-Taking Scale\n',
      '  abbreviation = "DOSPERT"\n',
      '    subscale = "Ethical"\n',
      '    subscale = "Financial"\n',
      '    subscale = "Health/Safety"\n',
      '    subscale = "Social"\n',
      '    subscale = "Recreational"\n',
      '    subscale = "Ethical Risk Perception"\n',
      '    subscale = "Financial Risk Perception"\n',
      '    subscale = "Health/Safety Risk Perception"\n',
      '    subscale = "Social Risk Perception"\n',
      '    subscale = "Recreational Risk Perception"\n',
      '    subscale = "Ethical Risk Benefit"\n',
      '    subscale = "Financial Risk Benefit"\n',
      '    subscale = "Health/Safety Risk Benefit"\n',
      '    subscale = "Social Risk Benefit"\n',
      '    subscale = "Recreational Risk Benefit"\n\n',

      'Drinking Motives Questionnaire-Revised\n',
      '  abbreviation = "DMQ"\n',
      '    subscale = "Social"\n',
      '    subscale = "Coping"\n',
      '    subscale = "Enhancement"\n',
      '    subscale = "Conformity"\n\n',

      'DSM-5 Cannabis Use Disorder Checklist\n',
      '  abbreviation = "CUD-CHECK"\n\n',

      'Emotion Reactivity Scale\n',
      '  abbreviation = "ERS"\n',
      '    subscale = "Sensitivity"\n',
      '    subscale = "Intensity/arousal"\n',
      '    subscale = "Persistence"\n',

      'Godin Leisure-Time Exercise Questionnaire',
      '  abbrevation = "GLTEQ"\n\n',

      'Hospital Anxiety Depression Scale\n',
      '  abbrevation = "HADS"\n',
      '    subscale = "Anxiety"\n',
      '    subscale = "Depression"\n\n',

      'Kiddie Schedule for Affective Disorders and Schizophrenia\n',
      '  abbreviation = "KSADS"\n\n',

      'Marijuana Craving Questionnaire (Short form)\n',
      '  abbrevation = "MCQ-SF"\n',
      '    subscale = "Compulsitivity"\n',
      '    subscale = "Emotionality"\n',
      '    subscale = "Expectancy"\n',
      '    subscale = "Purposefulness"\n\n',

      'Marijuana Effect Expectancy Questionnaire\n',
      '  abbrevation = "MEEQ"\n',
      '    subscale = "Cognitive and Behavioral Impairment"\n',
      '    subscale = "Relaxation and Tension Reduction"\n',
      '    subscale = "Social and Sexual Facilitation"\n',
      '    subscale = "Perceptual and Cognitive Enhancement"\n',
      '    subscale = "Global Negative Effects"\n',
      '    subscale = "Craving and Physical Effects"\n\n',

      'Marijuana Motives Measure\n',
      '  abbrevation = "MMM"\n',
      '    subscale = "Coping"\n',
      '    subscale = "Conformity"\n',
      '    subscale = "Social"\n',
      '    subscale = "Enhancement"\n',
      '    subscale = "Expansion"\n\n',

      'Monetary Choice Questionnaire\n',
      '  abbrevation = "MCQ"\n\n',

      'Mood and Anxiety Symptom Questionnaire\n',
      '  abbreviation = "MASQ"\n',
      '    subscale = "General distress anxious symptoms"\n',
      '    subscale = "Anxious arousal"\n',
      '    subscale = "General distress depressive symptoms"\n',
      '    subscale = "Anhedonic depression"\n\n',

      'Pain Catastrophizing Scale\n',
      '  abbreviation = "PCS"\n\n',

      'Perceived Stress Scale\n',
      '  abbreviation = "PSS"\n\n',

      'Peters et al. Delusions Inventory\n',
      '  abbreviation = "PDI"\n',
      '    subscale = "Distress"\n',
      '    subscale = "Preoccupation"\n',
      '    subscale = "Conviction"\n',
      '    subscale = "PDI yes/no"\n',
      '    subscale = "PDI total"\n\n',

      'Provider Expectations for Recovery Scale\n',
      '  abbreviation = "PERS"\n\n',

      'PTSD Checklist\n',
      '  abbreviation = "PCL"\n\n',

      'Short Form Health Survey\n',
      '  abbreviation = "SF-12"\n',
      '    subscale = "Physical"\n',
      '    subscale = "Mental"\n\n',

      'The Center for Epidemiologic Studies Depression Scale\n',
      '  abbreviation = "CES-D"\n\n',

      'Urgency, Premeditation (lack of), Perseverance \n',
      '(lack of), Sensation Seeking, Positive Urgency, \n',
      'Impulsive Behavior Scale\n',
      '  abbreviation = "UPPS-P"\n',
      '    subscale = "Negative urgency"\n',
      '    subscale = "Lack of premeditation"\n',
      '    subscale = "Lack of perseverance"\n',
      '    subscale = "Sensation seeking"\n',
      '    subscale = "Positive urgency"\n\n',

      'Wechsler Test of Adult Reading\n',
      '  abbreviation = "WTAR"\n\n',

      'World Health Organization Disability Assement Scale 2.0\n',
      '  abbrevation = "WHODAS 2.0"\n',
      '    subscale = "IRT-based scores"\n',
      '    subscale = "Percentile rank"\n\n'

    )
    message( inputs )

    # End function call
    return( invisible( NULL ) )
  }

  out <- list(
    Description = NULL,
    Scale = NULL,
    Subscale = NULL,
    Units = NULL
  )

  #### 1.1.1) HADS ####
  if ( abbreviation %in% c( 'HADS' ) ) {

    ### Overall
    out$Scale <- list(
      name = 'Hospital Anxiety Depression Scale',
      n_items = 14,
      range = c( NA, NA ),
      abbreviation = 'HADS',
      cut_off = NA,
      reference = paste0(
        'Zigmond, A. S., & Snaith, R. P. (1983). The hospital anxiety ',
        'and depression scale. Acta Psychiatrica Scandinavica, ',
        '67 (6), 361-370. ',
        'https://doi.org/10.1111/j.1600-0447.1983.tb09716.x'
      ),
      interpretation = paste0(
        ""
      )
    )

    ### Subscales
    if ( subscale != '' ) {

      ### Anxiety
      if ( subscale %in% c( 'Anxiety', 'anxiety' ) ) {

        out$Description <- paste0(
          'Scores for the HADS anxiety subscale - measure ',
          'of the degree of anxiety in a patient'
        )

        out$Units <- "Summed score"

        out$Subscale <- list(
          name = 'Anxiety',
          n_items = 7,
          range = c( 0, 21 ),
          cut_off = c( Borderline = 8, Abnormal = 11 ),
          interpretation = paste0(
            "Higher scores indicate greater anxiety"
          )
        )

        # Close 'Anxiety'
      }

      ### Depression
      if ( subscale == 'Depression' ) {

        out$Description <- paste0(
          'Scores for the HADS anxiety subscale - measure ',
          'of the degree of depression experienced by a patient'
        )

        out$Units <- "Summed score"

        out$Subscale <- list(
          name = 'Depression',
          n_items = 7,
          range = c( 0, 21 ),
          cut_off = c( Borderline = 8, Abnormal = 11 ),
          interpretation = paste0(
            "Higher scores indicate greater depression"
          )
        )

        # Close 'Depression'
      }

      # Close 'Subscales'
    }

    # Close 'HADS'
  }

  #### 1.1.2) CUDIT-R ####
  if ( abbreviation %in% c( 'CUDIT', 'CUDIT-R' ) ) {

    out$Description <- paste0(
      'Scores for the CUDIT-R - measure ',
      'of the degree of problematic cannabis use behavior ',
      'for a subject'
    )

    out$Units <- "Summed score"

    out$Scale <- list(
      name = 'Cannabis Use Disorder Identification Test - Revised',
      n_items = 8,
      range = c( Min = 0, Max = 32 ),
      abbreviation = 'CUDIT-R',
      cut_off = c( Hazardous = 8, Possible_disorder = 12 ),
      reference = paste0(
        'Adamson, S. J., Kay-Lambkin, F. J., Baker, A. L., ',
        'Lewin, T. J., Thornton, L., Kelly, B. J., & Sellman, J. D. ',
        '(2010) An improved brief measure of cannabis misuse: ',
        'The cannabis use disorders identification test - revised ',
        '(CUDIT-R). Drug and Alcohol Dependence, 110, 137-143. ',
        'https://doi.org/10.1016/j.drugalcdep.2010.02.017'
      ),
      interpretation = paste0(
        "Higher scores indicate more problematic cannabis use"
      )
    )

    # Close 'CUDIT-R'
  }

  #### 1.1.3) AUDIT ####
  if ( abbreviation == 'AUDIT' ) {

    out$Description <- paste0(
      'Scores for the AUDIT - measure ',
      'of the degree of problematic alcohol use behavior ',
      'for a subject'
    )

    out$Units <- "Summed score"

    out$Scale <- list(
      name = 'Alcohol Use Disorders Identification Test',
      n_items = 10,
      range = c( Min = 0, Max = 40 ),
      abbreviation = 'AUDIT',
      cut_off = c( Hazardous = 8, Harmful = 16, High_risk = 20 ),
      reference = paste0(
        'Allen, J. P., Reinert, D. F., Volk, R. J. ',
        '(2001). The alcohol use disorders identification test: ',
        'An aid to recognition of alcohol problems in primary ',
        'care patients. Preventive Medicine, 33 (5), 428-433. ',
        'https://doi.org/10.1006/pmed.2001.0910'
      ),
      interpretation = paste0(
        "Higher scores indicate more problematic alcohol use ",
        "behavior"
      )
    )

    # Close 'AUDIT'
  }

  #### 1.1.4) AIS ####
  if ( abbreviation == 'AIS' ) {

    out$Description <- paste0(
      'Scores for the AIS - measure of the degree of insomnia ',
      'experienced by a subject'
    )

    out$Units <- "Summed score"

    out$Scale <- list(
      name = 'Athens Insomnia Scale',
      n_items = 8,
      range = c( Min = 0, Max = 24 ),
      abbreviation = 'AIS',
      cut_off = c( 6 ),
      reference = paste0(
        'Soldatos, C. R., Dikeos, D.G., & Paparrigopoulos, T.J. ',
        '(2000). Athens insomnia scale: Validation of an ',
        'instrument based on ICD-10 criteria. Journal of ',
        'Psychosomatic Research, 48 (6), 555–560. ',
        'https://doi.org/10.1016/s0022-3999(00)00095-7'
      ),
      interpretation = paste0(
        "Higher scores indicate a greater degree of insomnia"
      )
    )

    # Close 'AIS'
  }

  #### 1.1.5) BPI ####
  if ( abbreviation %in% c( 'BPI', 'BPI-SF' ) ) {

    ### Overall
    out$Scale <- list(
      name = 'Brief Pain Inventory (Short form)',
      n_items = 11,
      range = c( NA, NA ),
      abbreviation = 'BPI-SF',
      cut_off = NA,
      reference = paste0(
        'Cleeland, C. S., & Ryan, K. M. (1994). Pain assessment: ',
        'Global use of the Brief Pain Inventory. Annals of the ',
        'Academy of Medicine, Singapore, 23(2), 129-138. ',
        'https://pubmed.ncbi.nlm.nih.gov/8080219/'
      ),
      interpretation = ""
    )

    ### Overall
    if ( subscale != '' ) {

      ### Severity
      if ( subscale == 'Severity' ) {

        out$Description <- paste0(
          'Scores for the BPI severity subscale - measure of the ',
          'degree of pain severity experienced by a subject ',
          'within the last 24 hours'
        )

        out$Units <- "Average over items"

        out$Subscale <- list(
          name = 'Pain severity',
          n_items = 4,
          range = c( Min = 0, Max = 10 ),
          cut_off = NA,
          interpretation = paste0(
            "Higher scores indicate greater pain severity ",
            "in the last 24 hours"
          )
        )

        # Close 'Severity'
      }

      ### Interference
      if ( subscale == 'Interference' ) {

        out$Description <- paste0(
          'Scores for the BPI interference subscale - measure of ',
          'the extent pain for a participant interfered with daily ',
          'functioning'
        )

        out$Units <- "Average over items"

        out$Subscale <- list(
          name = 'Pain interference',
          n_items = 7,
          range = c( Min = 0, Max = 10 ),
          cut_off = NA,
          interpretation = paste0(
            "Higher scores indicate greater pain interference ",
            "in daily function"
          )
        )

        # Close 'Interference'
      }

      # Close 'Subscales'
    }

    # Close 'BPI'
  }

  #### 1.1.6) PCS ####

  ### Summed score
  if ( abbreviation == 'PCS' ) {

    out$Description <- paste0(
      "Scores for the PCS - measure of the degree to which ",
      "a subject engages in pain catastrophizing"
    )

    out$Units <- "Summed score"

    out$Scale <- list(
      name = 'Pain Catastrophizing Scale',
      n_items = 13,
      range = c( Min = 0, Max = 52 ),
      abbreviation = 'PCS',
      cut_off = NA,
      reference = paste0(
        'Sullivan, M. J. L., Bishop, S. R., & Pivik, J. (1995). ',
        'The Pain Catastrophizing Scale: Development and ',
        'validation. Psychological Assessment, 7(4), 524–532. ',
        'https://doi.org/10.1037/1040-3590.7.4.524'
      ),
      interpretation = paste0(
        "Higher scores indicate that a subject engages in ",
        "a greater degree of catastrophizing about pain"
      )
    )

    # Close 'PCS'
  }

  ### Percentages
  if ( abbreviation %in% 'PCS (%)' ) {

    out$Description <- paste0(
      "Percentages for the PCS - measure of the degree to ",
      "which a subject engages in pain catastrophizing"
    )

    out$Units <- "Percentages"

    out$Scale <- list(
      name = 'Pain Catastrophizing Scale',
      n_items = 13,
      range = c( Min = 0, Max = 100 ),
      abbreviation = 'PCS (%)',
      cut_off = NA,
      reference = paste0(
        'Sullivan, M. J. L., Bishop, S. R., & Pivik, J. (1995). ',
        'The Pain Catastrophizing Scale: Development and ',
        'validation. Psychological Assessment, 7(4), 524–532. ',
        'https://doi.org/10.1037/1040-3590.7.4.524'
      ),
      interpretation = paste0(
        "Higher scores indicate that a subject engages in ",
        "a greater degree of catastrophizing about pain"
      )
    )

    # Close 'PCS (%)'
  }

  #### 1.1.7) PSS ####
  if ( abbreviation == 'PSS' ) {

    out$Description <- paste0(
      "Percentages for the PSS - measure of the degree to ",
      "which subjects view situations in their life ",
      "as stressful"
    )

    out$Units <- "Summed score"

    out$Scale <- list(
      name = 'Perceived Stress Scale',
      n_items = 10,
      range = c( Min = 0, Max = 40 ),
      abbreviation = 'PSS',
      cut_off = c( Moderate = 14, High = 27),
      reference = c(
        paste0(
          'Cohen, S., Kamarch, T., & Mermelstein, R. (1983). ',
          'A global measure of perceived stress. Journal of ',
          'Health and Social Behavior, 24, 385-396. ',
          'https://doi.org/10.2307/2136404'
        ),
        paste0(
          'Cohen, S., & Williamson, G. (1988). Perceived ',
          'stress in a probability sample of the United States. ',
          'In S. Spacapan, & S. Oskamp (Eds.), The social ',
          'psychology of health: Claremont symposium on ',
          'applied social psychology. Sage Publications, Inc.'
        )
      ),
      interpretation = paste0(
        "Higher scores indicate a greater degree of perceived stress"
      )
    )

    # Close 'PSS'
  }

  #### 1.1.8) SF-12 ####
  if ( abbreviation == 'SF-12' ) {

    ### Overall
    out$Scale <- list(
      name = 'Short-Form 12-item Health Survey',
      n_items = 12,
      range = c( NA, NA ),
      abbreviation = 'SF-12',
      cut_off = NA,
      reference = c(
        paste0(
          "Ware Jr, J. E., Kosinski, M., & Keller, S. D. (1996). ",
          "A 12-Item Short-Form Health Survey: construction of scales ",
          "and preliminary tests of reliability and validity. Medical ",
          "care, 220-233. ",
          "https://doi.org/10.1097/00005650-199603000-00003"
        )
      )
    )

    ### Subscales
    if ( subscale != '' ) {

      ### Mental
      if ( subscale == 'Mental' ) {

        out$Description <- paste0(
          "Normed scores for the SF-12 mental subscale - ",
          "measure of the degree of general mental health ",
          "of the respondent"
        )

        out$Units <- paste0(
          "Normed score with a population mean of 50 and ",
          "standard deviation of 10"
        )

        out$Subscale <- list(
          name = 'General mental health',
          n_items = 6,
          range = c( Min = 0, Max = 100 ),
          cut_off = NA,
          interpretation = paste0(
            "Higher scores indicate indicate better degree of ",
            "mental health"
          )
        )

        # Close 'Mental'
      }

      ### Physical
      if ( subscale == 'Physical' ) {

        out$Description <- paste0(
          "Normed scores for the SF-12 physical subscale - ",
          "measure of the degree of general physical health of ",
          "the respondent"
        )

        out$Units <- paste0(
          "Normed score with a population mean of 50 and ",
          "standard deviation of 10"
        )

        out$Subscale <- list(
          name = 'General physical health',
          n_items = 6,
          range = c( Min = 0, Max = 100 ),
          cut_off = NA,
          interpretation = paste0(
            "Higher scores indicate better degree of ",
            "physical health"
          )
        )

        # Close 'Physical'
      }

      # Close 'Subscales'
    }

    # Close 'SF-12'
  }

  #### 1.1.9) MCQ-SF ####
  if ( abbreviation == 'MCQ-SF' ) {

    ### Overall
    out$Scale <- list(
      name = 'Marijuana Craving Questionnaire (Short form)',
      n_items = 12,
      range = c( NA, NA ),
      abbreviation = 'MCQ-SF',
      cut_off = NA,
      reference = c(
        paste0(
          "Heishman, S. J., Singleton, E. G., & Liguori, A. (2001). ",
          "Marijuana Craving Questionnaire: Development and initial ",
          "validation of a self‐report instrument. Addiction, 96(7), ",
          "1023-1034. https://doi.org/10.1046/j.1360-0443.2001.967102312.x"
        ),
        paste0(
          "Heishman, S. J., Evans, R. J., Singleton, E. G., Levin, K. H., ",
          "Copersino, M. L., & Gorelick, D. A. (2009). Reliability and ",
          "validity of a short form of the Marijuana Craving Questionnaire. ",
          "Drug and alcohol dependence, 102(1-3), 35-40. ",
          "https://doi.org/10.1016/j.drugalcdep.2008.12.010"
        )
      ),
      interpretation = ""
    )

    #### Subscales
    if ( subscale != '' ) {

      ### Compulsivity
      if (subscale == 'Compulsivity') {

        out$Description <- paste0(
          'Scores for the MCQ compulsivity subscale - measure ',
          'of the inability to control marijuana use'
        )

        out$Units <- "Summed score"

        out$Subscale <- list(
          name = 'Compulsivity',
          n_items = 3,
          range = c( Min = 3, Max = 21 ),
          cut_off = NA,
          interpretation = paste0(
            "Higher scores indicate greater difficulty in ",
            "controlling marijuana use"
          )
        )

        # Close 'Compulsivity'
      }

      ### Emotionality
      if (subscale == 'Emotionality') {


        out$Description <- paste0(
          'Scores for the MCQ emotionality subscale - measure ',
          'of the degree participants use marijuana for relief ',
          'from withdrawal or negative mood'
        )


        out$Units <- "Summed score"

        out$Subscale <- list(
          name = 'Emotionality',
          n_items = 3,
          range = c( Min = 3, Max = 21 ),
          cut_off = NA,
          interpretation = paste0(
            "Higher scores indicate a greater willingness to ",
            "use marijuana to control withdrawal and negative mood"
          )
        )

        # Close 'Emotionality'
      }

      ### Expectancy
      if (subscale == 'Expectancy') {


        out$Description <- paste0(
          'Scores for the MCQ expectancy subscale - measure ',
          'of the degree of anticipation of positive outcomes ',
          'from using marijuana'
        )


        out$Units <- "Summed score"

        out$Subscale <- list(
          name = 'Expectancy',
          n_items = 3,
          range = c( Min = 3, Max = 21 ),
          cut_off = NA,
          interpretation = paste0(
            "Higher scores indicate higher anticipation of ",
            "postive outcomes from using marijuana"
          )
        )

        # Close 'Expectancy'
      }

      ### Purposefulness
      if (subscale == 'Purposefulness') {


        out$Description <- paste0(
          'Scores for the MCQ purposefulness subscale - measure ',
          'of the degree of intention and planning to use ',
          'marijuana for positive outcomes'
        )

        out$Units <- "Summed score"

        out$Subscale <- list(
          name = 'Purposefulness',
          n_items = 3,
          range = c( Min = 3, Max = 21 ),
          cut_off = NA,
          interpretation = paste0(
            "Higher scores indicate a greater degree of planning ",
            "and intent to use marijuana for postive outcomes"
          )
        )

        # Close 'Purposefulness'
      }

      # Close 'Subscales'
    }

    # Close 'MCQ-SF'
  }

  #### 1.1.10) CHRT ####
  if ( abbreviation == 'CHRT' ) {

    out$Description <- paste0(
      "Measure of risk based on suicidal ideation and intent"
    )

    out$Units <- "Summed score"

    out$Scale <- list(
      name = 'Concise Health Risk Tracking scale',
      n_items = 12,
      range = c( Min = 12, Max = 60 ),
      abbreviation = 'CHRT',
      cut_off = c( NA ),
      reference = c(
        paste0(
          "Trivedi, M. H., Wisniewski, S. R., Morris, D. W., Fava, M., ",
          "Gollan, J. K., Warden, D., Nierenberg, A. A., Gaynes, B. N., ",
          "Husain, M. M., Luther, J. F., Zisook, S., &  Rush, A. J. ",
          "(2011). Concise Health Risk Tracking scale: A brief ",
          "self-report and clinician rating of suicidal risk. The ",
          "Journal of Clinical Psychiatry, 72(6), 757-764. ",
          "https://doi.org/10.4088/JCP.11m06837"
        )
      ),
      interpretation = paste0(
        "Higher scores indicate greater risk of suicide"
      )
    )

    #### Subscales
    if ( subscale != '' ) {

      ### Despair
      if (subscale == 'Despair') {


        out$Description <- paste0(
          'Scores for the CHRT despair items - measure of ',
          'degree of despair a person feels'
        )

        out$Units <- "Summed score"

        out$Subscale <- list(
          name = 'Despair',
          n_items = 3,
          range = c( Min = 0, Max = 12 ),
          cut_off = NA,
          interpretation = paste0(
            "Higher scores indicate greater ",
            "feelings of despair"
          )
        )

        # Close 'Despair'
      }

      ### Helplessness
      if (subscale == 'Helplessness') {


        out$Description <- paste0(
          'Scores for the CHRT helplessness items - measure of ',
          'how helpless a person feels'
        )

        out$Units <- "Summed score"

        out$Subscale <- list(
          name = 'Helplessness',
          n_items = 2,
          range = c( Min = 0, Max = 4 ),
          cut_off = NA,
          interpretation = paste0(
            "Higher scores indicate greater ",
            "feelings of helplessness"
          )
        )

        # Close 'Helplessness'
      }

      ###  Lack of social support
      if (subscale == 'Lack of social support') {

        out$Description <- paste0(
          'Scores for the CHRT lack of social support items',
          ' - measure of ',
          'the degree a person feels a lack of social support'
        )

        out$Units <- "Summed score"

        out$Subscale <- list(
          name = 'Lack of social support',
          n_items = 2,
          range = c( Min = 0, Max = 4 ),
          cut_off = NA,
          interpretation = paste0(
            "Higher scores indicate greater ",
            "sense of a lack of social support"
          )
        )

        # Close 'Lack of social support'
      }

      ###  Pessimism
      if (subscale == 'Pessimism') {

        out$Description <- paste0(
          'Scores for the CHRT pessimism items',
          ' - measure of ',
          'how pessimistic a person feels'
        )

        out$Units <- "Summed score"

        out$Subscale <- list(
          name = 'Pessimism',
          n_items = 2,
          range = c( Min = 0, Max = 8 ),
          cut_off = NA,
          interpretation = paste0(
            "Higher scores indicate greater ",
            "feelings of pessimism"
          )
        )

        # Close 'Pessimism'
      }

      ###  Suicidal thoughts
      if (subscale == 'Suicidal thoughts') {

        out$Description <- paste0(
          'Scores for the CHRT suicidal thought items',
          ' - measure of ',
          'how often someone has suicidal thoughts'
        )

        out$Units <- "Summed score"

        out$Subscale <- list(
          name = 'Suicidal thoughts',
          n_items = 3,
          range = c( Min = 0, Max = 12 ),
          cut_off = NA,
          interpretation = paste0(
            "Higher scores indicate more frequent ",
            "occurences of suicidal thoughts"
          )
        )

        # Close 'Suicidal thoughts'
      }

      ###  Propensity
      if (subscale == 'Propensity') {


        out$Description <- paste0(
          'Scores for the CHRT propensity items',
          ' - measure of ',
          'the propensity a person might have for suicide'
        )

        out$Units <- "Summed score"

        out$Subscale <- list(
          name = 'Propensity',
          n_items = 9,
          range = c( Min = 0, Max = 36 ),
          cut_off = NA,
          interpretation = paste0(
            "Higher scores indicate a greater ",
            "propensity towards suicide"
          )
        )

        # Close 'Propensity'
      }

      # Close 'Subscales'
    }

    # Close 'CHRT'
  }

  #### 1.1.11) CGI ####
  if ( abbreviation == 'CGI' ) {

    ### Overall
    out$Scale <- list(
      name = 'Clinical Global Impression scale',
      n_items = 2,
      range = c( NA, NA ),
      abbreviation = 'CGI',
      cut_off = NA,
      reference = c(
        paste0(
          "Guy, W. (Ed.) (1976). ECDEU assessment manual for ",
          "psychopharmacology. US Department of Heath, ",
          "Education, and Welfare; Public Health Service ",
          "Alcohol, Drug Abuse, and Mental Health Administration."
        )
      )
    )

    ### Subscales
    if ( subscale != '' ) {

      ### Severity
      if ( subscale == 'Severity' ) {


        out$Description <- paste0(
          "Rating of illness severity at current time ",
          "based on total clinical experience with particular ",
          "population"
        )


        out$Units <- paste0(
          "Likert scale from 1 = not at all ill to ",
          "7 = among the most extremely ill patients"
        )

        out$Subscale <- list(
          name = 'Severity of illness',
          n_items = 1,
          range = c( Min = 1, Max = 7 ),
          cut_off = NA,
          interpretation = paste0(
            "Higher scores indicate indicate greater severity ",
            "of illness"
          )
        )

        # Close 'Severity'
      }

      ### Improvement
      if ( subscale == 'Improvement' ) {



        out$Description <- paste0(
          "Measure of total improvement irrespective of ",
          "drug treatment compared to previous study ",
          "visit for participant"
        )


        out$Units <- paste0(
          "Likert scale from 1 = very much improved to ",
          "7 = very much worse"
        )

        out$Subscale <- list(
          name = 'Global improvement',
          n_items = 1,
          range = c( Min = 1, Max = 7 ),
          cut_off = NA,
          interpretation = paste0(
            "Higher scores indicate worsening of condition"
          )
        )

        # Close 'Improvement'
      }

      # Close 'Subscales'
    }

    # Close 'CGI'
  }

  #### 1.1.12) MASQ ####
  if ( abbreviation == 'MASQ' ) {

    ### Overall
    out$Scale <- list(
      name = 'Mood and Anxiety Symptom Questionnaires',
      n_items = 62,
      range = c( NA, NA ),
      abbreviation = 'MASQ',
      cut_off = NA,
      reference = c(
        paste0(
          "Watson, D., Weber, K., Assenheimer, J. S., ",
          "Clark, L. A., Strauss, M. E., & McCormick, R. A. ",
          "(1995). Testing a tripartite model: I. Evaluating ",
          "the convergent and discriminant validity of anxiety ",
          "and depression symptom scales. ",
          "Journal of Abnormal Psychology, 104 (1), 4-14. ",
          "https://doi.org/10.1037/0021-843X.104.1.3"
        ),
        paste0(
          "Watson, D., & Clark, L. A. (1991). The mood and anxiety ",
          "symptom questionnaire. Unpublished manuscript."
        )
      )
    )

    ### Subscales
    if ( subscale != '' ) {

      ### General distress anxious symptoms
      if ( subscale %in% c(
        'GDA',
        'General distress anxious symptoms',
        'General Distress Anxious Symptoms'
      ) ) {


        out$Description <- paste0(
          "Scores for the MASQ anxious general distress ",
          "anxious symptoms subscale - measure of anxious ",
          "mood"
        )

        out$Units <- "Summed score"

        out$Subscale <- list(
          name = 'General distress anxious symptoms',
          n_items = 11,
          range = c( Min = 11, Max = 55 ),
          cut_off = NA,
          interpretation = paste0(
            "Higher scores indicate a greater degree ",
            "of general distress and anxiety"
          )
        )

        # Close 'Mood and Anxiety Symptom Questionnaires'
      }

      ### Anxious arousal
      if ( subscale %in% c(
        'AA',
        'Anxious arousal',
        'Anxious Arousal'
      ) ) {

        out$Description <- paste0(
          "Scores for the MASQ anxious arousal subscale - ",
          "measure of symptoms of somatic tension and hyperarousal ",
          "specific to anxiety"
        )

        out$Units <- "Summed score"

        out$Subscale <- list(
          name = 'Anxious arousal',
          n_items = 17,
          range = c( Min = 17, Max = 85 ),
          cut_off = NA,
          interpretation = paste0(
            ""
          )
        )

        # Close 'Anxious arousal'
      }

      ### General distress depressive symptoms
      if ( subscale %in% c(
        'GDD',
        'General distress depressive symptoms',
        'General Distress Depressive Symptoms'
      ) ) {


        out$Description <- paste0(
          "Scores for the MASQ anxious general distress ",
          "depressive symptoms subscale - measure of depressed ",
          "mood"
        )

        out$Units <- "Summed score"

        out$Subscale <- list(
          name = 'General distress depressive symptoms',
          n_items = 12,
          range = c( Min = 12, Max = 60 ),
          cut_off = NA,
          interpretation = paste0(
            "Higher scores indicate a greater degree ",
            "of general distress and depression"
          )
        )

        # Close 'General distress depressive symptoms'
      }

      ### Anhedonic depression
      if ( subscale %in% c(
        'AD',
        'Anhedonic depression',
        'Anhedonic Depression'
      ) ) {

        out$Description <- paste0(
          "Scores for the MASQ anhedonic depression ",
          "subscale - measure of anhedonia and low ",
          "positive affect"
        )

        out$Units <- "Summed score"

        out$Subscale <- list(
          name = 'Anhedonic depression',
          n_items = 22,
          range = c( Min = 22, Max = 110 ),
          cut_off = NA,
          interpretation = paste0(
            "Higher scores indicate greater degree of anhedonia ",
            "and depression"
          )
        )

        # Close 'Anhedonic depression'
      }

      # Close 'Subscales'
    }

    # 1 Not at All/Slightly
    # 2	A Little
    # 3	Moderately
    # 4	Quite a Bit
    # 5	Extremely

    # Close 'MASQ'
  }

  #### 1.1.13) UPPS-P ####
  if ( abbreviation == 'UPPS-P' ) {

    ### Overall
    out$Scale <- list(
      name = '',
      n_items = 59,
      range = c( NA, NA ),
      abbreviation = 'UPPS-P',
      cut_off = NA,
      reference = c(
        paste0(
          "Whiteside, S. P., & Lynam, D. R. (2001). ",
          "The five factor model and impulsivity: Using a ",
          "structural model of personality to understand ",
          "impulsivity. Personality and Individual Differences, ",
          "30 (4), 669-689. ",
          "https://doi.org/10.1016/S0191-8869(00)00064-7"
        ),
        paste0(
          "Cyders, M. A., Smith, G. T., Spillane, N. S., Fischer, ",
          "S., Annus, A. M., & Peterson, C. (2007). Integration of ",
          "impulsivity and positive mood to predict risky behavior: ",
          "Development and validation of a measure of positive ",
          "urgency. Psychological Assessment, 19 (1), 107-118. ",
          "https://doi.org/10.1037/1040-3590.19.1.107"
        )
      )
    )

    ### Subscales
    if ( subscale != '' ) {

      ### Negative urgency
      if ( subscale %in% c(
        'Negative urgency',
        '(Negative) Urgency',
        'Urgency'
      ) ) {


        out$Description <- paste0(
          "Scores for the UPPS-P negative urgency ",
          "subscale - measure of the tendency for ",
          "a person to act rashly under extreme ",
          "negative emotions"
        )

        out$Units <- "Summed score"

        out$Subscale <- list(
          name = 'Negative urgency',
          n_items = 12,
          range = c( Min = 12, Max = 48 ),
          cut_off = NA,
          interpretation = paste0(
            "Higher scores indicate a greater tendency ",
            "towards rashness during negative moods"
          )
        )

        # Close 'Negative urgency'
      }

      ### Lack of premeditation
      if ( subscale %in% c(
        'Lack of premeditation',
        '(Lack of) Premeditation',
        'Premeditation'
      ) ) {


        out$Description <- paste0(
          "Scores for the UPPS-P lack of premeditation ",
          "subscale - measure of the tendency for a ",
          "person to act without thinking"
        )

        out$Units <- "Summed score"

        out$Subscale <- list(
          name = 'Lack of premeditation',
          n_items = 11,
          range = c( Min = 11, Max = 44 ),
          cut_off = NA,
          interpretation = paste0(
            "Higher scores indicate a greater ",
            "tendency to act without thinking beforehand"
          )
        )

        # Close 'Lack of premeditation'
      }

      ### Lack of perseverance
      if ( subscale %in% c(
        'Lack of perseverance',
        '(Lack of) Perseverance',
        'Perseverance'
      ) ) {


        out$Description <- paste0(
          "Scores for the UPPS-P lack of perseverance ",
          "subscale - a measure of the inability to ",
          "remain focused on a task"
        )

        out$Units <- "Summed score"

        out$Subscale <- list(
          name = 'Lack of premeditation',
          n_items = 10,
          range = c( Min = 10, Max = 40 ),
          cut_off = NA,
          interpretation = paste0(
            "Higher scores indicate a greater ",
            "inability to remain focused on a task"
          )
        )

        # Close 'Lack of perseverance'
      }

      ### Sensation seeking
      if ( subscale %in% c(
        'Sensation seeking',
        'Sensation Seeking'
      ) ) {

        out$Description <- paste0(
          "Scores for the UPPS-P sensation seeking ",
          "subscale - measure of the tendency for a ",
          "person to seek out novel and thrilling experiences"
        )

        out$Units <- "Summed score"

        out$Subscale <- list(
          name = 'Sensation seeking',
          n_items = 12,
          range = c( Min = 12, Max = 48 ),
          cut_off = NA,
          interpretation = paste0(
            "Higher scores indicate a greater ",
            "willingness to seek out novel and ",
            "thrilling experiences"
          )
        )

        # Close 'Sensation seeking'
      }

      ### Positive urgency
      if ( subscale %in% c(
        'Positive urgency',
        'Positive Urgency'
      ) ) {


        out$Description <- paste0(
          "Scores for the UPPS-P positive urgency ",
          "subscale - measure of the tendency for ",
          "a person to act rashly under extreme ",
          "positive emotions"
        )

        out$Units <- "Summed score"

        out$Subscale <- list(
          name = 'Sensation seeking',
          n_items = 14,
          range = c( Min = 14, Max = 56 ),
          cut_off = NA,
          interpretation = paste0(
            "Higher scores indicate a greater ",
            "tendency to act rashly during ",
            "positive moods"
          )
        )

        # Close 'Positive urgency'
      }

      # Close 'Subscales'
    }

    # Raw scale
    # 1	= Agree Strongly
    # 2	= Agree Somewhat
    # 3	= Disagree Somewhat
    # 4	= Disagree Strongly

    # Flipped to indicate greater
    # endorsement of impulsivity
    # 1	= Disagree Strongly
    # 2	= Disagree Somewhat
    # 3	= Agree Somewhat
    # 4	= Agree Strongly

    # Close 'UPPS-P'
  }

  #### 1.1.14) MMM ####
  if ( abbreviation == 'MMM' ) {

    ### Overall
    out$Scale <- list(
      name = 'Marijuana Motives Measure',
      n_items = 25,
      range = c( NA, NA ),
      abbreviation = 'MMM',
      cut_off = NA,
      reference = c(
        paste0(
          "Simons, J., Correia, C. J., Carey, K. B., & Borsari, ",
          "B. E. (1998). Validating a five-factor marijuana motives ",
          "measure: Relations with use, problems, and alcohol motives. ",
          "Journal of Counseling Psychology, 45 (3), 265–273. ",
          "https://doi.org/10.1037/0022-0167.45.3.265"
        )
      )
    )

    ### Subscales
    if ( subscale != '' ) {

      ### Coping
      if ( subscale %in% c(
        'Coping'
      ) ) {

        out$Description <- paste0(
          "Scores for the MMM coping ",
          "subscale - measure of degree ",
          "to which someone uses cannabis ",
          "to help cope with negative feelings ",
          "and situations"
        )

        out$Units <- "Mean score"

        out$Subscale <- list(
          name = 'Coping',
          n_items = 5,
          range = c( Min = 1, Max = 5 ),
          cut_off = NA,
          interpretation = paste0(
            "Higher scores indicate a greater willingness ",
            "to use cannabis to cope with negative ",
            "feelings and situations"
          )
        )

        # Close 'Coping'
      }

      ### Conformity
      if ( subscale %in% c(
        'Conformity'
      ) ) {

        out$Description <- paste0(
          "Scores for the MMM conformity ",
          "subscale - measure of degree ",
          "to which someone uses cannabis ",
          "to conform with expectations of peers"
        )

        out$Units <- "Mean score"

        out$Subscale <- list(
          name = 'Conformity',
          n_items = 5,
          range = c( Min = 1, Max = 5 ),
          cut_off = NA,
          interpretation = paste0(
            "Higher scores indicate a greater willingness ",
            "to use cannabis in order to conform to the ",
            "expectations of others"
          )
        )

        # Close 'Conformity'
      }

      ### Social
      if ( subscale %in% c(
        'Social'
      ) ) {

        out$Description <- paste0(
          "Scores for the MMM social ",
          "subscale - measure of degree ",
          "to which someone uses cannabis ",
          "to better enjoy social situations"
        )

        out$Units <- "Mean score"

        out$Subscale <- list(
          name = 'Social',
          n_items = 5,
          range = c( Min = 1, Max = 5 ),
          cut_off = NA,
          interpretation = paste0(
            "Higher scores indicate a greater willingness ",
            "to use cannabis to better enjoy social situations"
          )
        )

        # Close 'Social'
      }

      ### Enhancement
      if ( subscale %in% c(
        'Enhancement'
      ) ) {

        out$Description <- paste0(
          "Scores for the MMM enhancement ",
          "subscale - measure of degree ",
          "to which someone uses cannabis ",
          "for pleasurable feelings"
        )

        out$Units <- "Mean score"

        out$Subscale <- list(
          name = 'Enhancement',
          n_items = 5,
          range = c( Min = 1, Max = 5 ),
          cut_off = NA,
          interpretation = paste0(
            "Higher scores indicate a greater willingness ",
            "to use cannabis for pleasurable feelings"
          )
        )

        # Close 'Enhancement'
      }

      ### Expansion
      if ( subscale %in% c(
        'Expansion'
      ) ) {

        out$Description <- paste0(
          "Scores for the MMM expansion ",
          "subscale - measure of degree ",
          "to which someone uses cannabis ",
          "to be more open to new experiences"
        )

        out$Units <- "Mean score"

        out$Subscale <- list(
          name = 'Expansion',
          n_items = 5,
          range = c( Min = 1, Max = 5 ),
          cut_off = NA,
          interpretation = paste0(
            "Higher scores indicate a greater willingness ",
            'to use cannabis for new experiences'
          )
        )

        # Close 'Expansion'
      }

      # Close 'Subscales'
    }

    # 1 = Almost never/never
    # 2	= Some of the time
    # 3	= Half of the time
    # 4	= Most of the time
    # 5	= Almost always/always

    # Close 'MMM'
  }

  #### 1.1.15) CES-D ####
  if ( abbreviation == 'CES-D' ) {

    out$Description <- paste0(
      'Scores for the CES-D - measure ',
      'of depressive symptomatology'
    )

    out$Units <- "Summed score"

    out$Scale <- list(
      name = 'The Center for Epidemiologic Studies Depression Scale',
      n_items = 20,
      range = c( 0, 60 ),
      abbreviation = 'CES-D',
      cut_off = 15,
      reference = c(
        paste0(
          "Radloff, L. S. (1977) The CES-D scale: A self-report ",
          "depression scale for research in the general ",
          "population. Applied Psychological Measurement, 1 (3) 385-401. ",
          "https://doi.org/10.1177/014662167700100306"
        )
      ),
      interpretation = paste0(
        "Higher scores indicate a greater degree of depression"
      )
    )

    # 0 = Rarely or none of the time (less than 1 day )
    # 1 = Some or a little of the time (1-2 days)
    # 2 = Occasionally or a moderate amount of time (3-4 days)
    # 3 = Most or all of the time (5-7 days)

    # Close 'CES-D'
  }

  #### 1.1.16) CWS ####
  if ( abbreviation %in% c( 'CWS' ) ) {

    ### Overall
    out$Scale <- list(
      name = 'Cannabis Withdrawal Scale',
      n_items = 38,
      range = c( NA, NA ),
      abbreviation = 'CWS',
      cut_off = NA,
      reference = paste0(
        'Allsop, D. J., Norberg, M. M., Copeland, J., Fu, S., & ',
        'Budney, A. J. (2011). The Cannabis Withdrawal Scale development: ',
        'Patterns and predictors of cannabis withdrawal and distress. ',
        'Drug Alcohol Dependence, 119, 123-129. ',
        'https://doi.org/10.1016/j.drugalcdep.2011.06.003'
      )
    )

    ### Subscales
    if ( subscale != '' ) {

      ### Intensity
      if ( subscale %in% c( 'Intensity' ) ) {

        out$Description <- paste0(
          'Scores for the CWS intensity subscale - measure of ',
          'intensity of cannabis withdrawal symptoms'
        )

        out$Units <- "Summed score"

        out$Subscale <- list(
          name = 'Intensity',
          n_items = 19,
          range = c( 0, 190 ),
          cut_off = NA,
          interpretation = paste0(
            "Higher scores indicate more intense ",
            "cannabis withdrawal symptoms"
          )
        )

        # Close 'Intensity'
      }

      ### Negative impact
      if ( subscale == 'Negative impact' ) {


        out$Description <- paste0(
          'Scores for the CWS negative impact subscale - ',
          'measure of distress or impairment in functioning ',
          'due to withdrawal'
        )

        out$Units <- "Summed score"

        out$Subscale <- list(
          name = 'Negative impact',
          n_items = 19,
          range = c( 0, 190 ),
          cut_off = NA,
          interpretation = paste0(
            "Higher scores indicate a greater degree of ",
            "negative impact due to cannabis withdrawal"
          )
        )

        # Close 'Negative impact'
      }

      # Close 'Subscales'
    }

    # 0	0 (Not at All)
    # 1	1
    # 2	2
    # 3	3
    # 4	4
    # 5	5 (Moderately)
    # 6	6
    # 7	7
    # 8	8
    # 9	9
    # 10	10 (Strongly Agree)

    # Close 'CWS'
  }

  #### 1.1.17) PDI ####
  if ( abbreviation %in% c( 'PDI' ) ) {

    ### Overall
    out$Scale <- list(
      name = 'Peters et al. Delusions Inventory',
      n_items = 63,
      range = c( 0, 336 ),
      abbreviation = 'PDI',
      cut_off = NA,
      reference = c(
        paste0(
          'Peters, E. R., Joseph, S. A., & Garety, P. A. ',
          '(1999). Measurement of delusional ideation in the ',
          'normal population: introducing the PDI ',
          '(Peters et al. Delusions Inventory). Schizophrenia ',
          'Bulletin, 25(3), 553-576. ',
          'https://doi.org/10.1093/oxfordjournals.schbul.a033401'
        ),
        paste0(
          'Peters, E., Joseph, S., Day, S., Garety, P. (2004). ',
          'Measuring delusional ideation: The 21-Item Peters et al. ',
          'Delusions Inventory (PDI). Schizophrenia Bulletin, 30 ',
          '(4), 1005-1022. ',
          'https://doi.org/10.1093/oxfordjournals.schbul.a007116'
        )
      ),
      interpretation = paste0(
        "Higher scores indicate greater proneness to delusional ",
        "ideation"
      )
    )

    ### Subscales
    if ( subscale != '' ) {

      ### Distress
      if ( subscale %in% c( 'Distress' ) ) {

        out$Description <- paste0(
          'Scores for the PDI distress subscale - measure of',
          'how upsetting are the delusional ideations'
        )

        out$Units <- "Summed score"

        out$Subscale <- list(
          name = 'Distress',
          n_items = 21,
          range = c( 0, 105 ),
          cut_off = c( NA ),
          interpretation = paste0(
            "Higher scores indicate greater distress"
          )
        )

        # Close 'Distress'
      }

      ### Preoccupation
      if ( subscale %in% c( 'Preoccupation' ) ) {

        out$Description <- paste0(
          'Scores for the PDI preoccupation subscale - measure of',
          'how preoccupied an individual is with the delusional ',
          'ideations'
        )

        out$Units <- "Summed score"

        out$Subscale <- list(
          name = 'Preoccupation',
          n_items = 21,
          range = c( 0, 105 ),
          cut_off = c( NA ),
          interpretation = paste0(
            "Higher scores indicate greater preoccu"
          )
        )

        # Close 'Preoccupation'
      }

      ### Conviction
      if ( subscale %in% c( 'Conviction' ) ) {

        out$Description <- paste0(
          'Scores for the PDI conviction subscale - measure of',
          'how much an individual believes in the delusional ',
          'ideations'
        )

        out$Units <- "Summed score"

        out$Subscale <- list(
          name = 'Conviction',
          n_items = 21,
          range = c( 0, 105 ),
          cut_off = c( NA ),
          interpretation = paste0(
            "Higher scores indicate greater belief in delusions"
          )
        )

        # Close 'Conviction'
      }

      ### PDI yes/no
      if ( subscale %in% c( 'PDI yes/no' ) ) {

        out$Description <- paste0(
          'Scores for the PDI yes/no subscale - measure of',
          'number of delusional ideations a person experiences'
        )

        out$Units <- "Summed score"

        out$Subscale <- list(
          name = 'PDI yes/no',
          n_items = 21,
          range = c( 0, 21 ),
          cut_off = c( NA ),
          interpretation = paste0(
            "Higher scores indicate a greater degree of delusional ",
            "ideation"
          )
        )

        # Close 'PDI yes/no'
      }

      ### PDI total
      if ( subscale %in% c( 'PDI total' ) ) {

        out$Description <- paste0(
          'Scores for the PDI - measure of',
          'general delusional ideation summing over all subscales'
        )

        out$Units <- "Summed score"

        out$Subscale <- list(
          name = 'PDI total',
          n_items = 63,
          range = c( 0, 336 ),
          cut_off = c( NA ),
          interpretation = paste0(
            "Higher scores indicate a greater degree of delusional ",
            "ideation"
          )
        )

        # Close 'PDI yes/no'
      }

      # Close 'Subscales'
    }

    # Close 'PDI'
  }

  #### 1.1.18) WHODAS 2.0 ####
  if ( abbreviation %in% c( 'WHODAS', 'WHODAS 2.0' ) ) {

    ### Overall
    out$Scale <- list(
      name = 'World Health Organization Disability Assement Scale 2.0',
      n_items = 36,
      range = c( NA, NA ),
      abbreviation = 'WHODAS 2.0',
      cut_off = NA,
      reference = c(
        paste0(
          'Ustun, T. B., & World Health Organization. (2010). ',
          'Measuring health and disability: Manual for WHO ',
          'Disability Assessment Schedule WHODAS 2.0. Geneva: ',
          'World Health Organization.'
        )
      ),
      interpretation = paste0(
        'Higher scores indicate greater difficulties ',
        'with activities of daily living'
      )
    )

    ### Subscales
    if ( subscale != '' ) {

      ### IRT-based scores
      if ( subscale %in% c( 'IRT-based scores',
                            'Item response theory',
                            'IRT',
                            'Item response theory scores',
                            'IRT scores' ) ) {

        out$Description <- paste0(
          'Item respone theory based scores for the ',
          'WHODAS 2.0 - measure of degree of disability ',
          'which negatively impacts daily life'
        )

        out$Units <- "Scaled score"

        out$Subscale <- list(
          name = 'IRT-based scores',
          n_items = 36,
          range = c( 0, 100 ),
          cut_off = NA,
          interpretation = paste0(
            "Higher scores indicate a greater degree of disability"
          )
        )

        # Close 'IRT-based scores'
      }

      ### Percentile rank
      if ( subscale %in% c( 'Percentile rank',
                            'percentile rank' ) ) {

        out$Description <- paste0(
          'Percentile rank based on norms for the ',
          'WHODAS 2.0 - measure of ',
          'percentage of individuals who report a lesser ',
          'degree of disability'
        )

        out$Units <- "Percentile rank"

        out$Subscale <- list(
          name = 'Percentile rank',
          n_items = 36,
          range = c( 0, 100 ),
          cut_off = NA,
          interpretation = paste0(
            "Higher scores indicate a greater percentage of ",
            "individuals with a lesser degree of disability"
          )
        )

        # Close 'Percentile rank'
      }

      # Close 'Subscales'
    }

    # Close 'WHODAS 2.0'
  }

  #### 1.1.19) ASRS ####
  if ( abbreviation == 'ASRS' ) {

    out$Description <- paste0(
      'Scores for the ASRS - measure of symptoms ',
      'suggesting ADHD'
    )

    out$Units <- 'Summed score'

    out$Scale <- list(
      name = 'Adult ADHD Self-Report Scale',
      n_items = 18,
      range = c( Min = 0, Max = 72 ),
      abbreviation = 'ASRS',
      cut_off = NA,
      reference = paste0(
        'Kessler, R. C., Adler, L., Ames, M., Demler, O., Faraone, ',
        'S., Hiripi, E. V. A., ... & Walters, E. E. (2005). The World ',
        'Health Organization Adult ADHD Self-Report Scale (ASRS): a ',
        'short screening scale for use in the general population. ',
        'Psychological medicine, 35(2), 245-256. ',
        'https://doi.org/10.1017/S0033291704002892'
      ),
      interpretation = paste0(
        'Higher scores indicate more severe ADHD-like symptoms'
      )
    )

    # Close 'ASRS'
  }

  #### 1.1.20) BIS-11 ####
  if ( abbreviation %in% c( 'BIS', 'BIS-11' ) ) {

    ### Overall
    out$Scale <- list(
      name = 'Barratt Impulsiveness Scale',
      n_items = 30,
      range = c( 30, 120 ),
      abbreviation = 'BIS-11',
      cut_off = c( High = 72 ),
      reference = paste0(
        'Patton, J. H., Stanford, M. S., & Barratt, E. S. (1995). ',
        'Factor structure of the Barratt impulsiveness scale. ',
        'Journal of clinical psychology, 51(6), 768-774. ',
        'https://doi.org/10.1002/1097-4679(199511)51:6<',
        '768::AID-JCLP2270510607>3.0.CO;2-1'
      ),
      interpretation = paste0(
        'Higher scores indicate greater impulsivity'
      )
    )

    ### Subscales
    if ( subscale != '' ) {

      ### Attentional
      if ( subscale %in% c( 'Attentional', 'attentional' ) ) {

        out$Description <- paste0(
          'Scores for the BIS-11 Attentional Facet - ',
          'measure of attention span'
        )

        out$Units <- 'Summed score'

        out$Subscale <- list(
          name = 'Attentional',
          n_items = 8,
          range = c( 8, 32 ),
          cut_off = NA,
          interpretation = paste0(
            'Higher scores indicate poorer attention span and ',
            'poorer cognitive stability'
          )
        )

        # Close 'Attentional'
      }

      ### Motor
      if ( subscale %in% c( 'Motor', 'motor' ) ) {

        out$Description <- paste0(
          'Scores for the BIS-11 Motor Facet - ',
          'measure of control over motor impulses'
        )

        out$Units <- 'Summed score'

        out$Subscale <- list(
          name = 'Motor',
          n_items = 11,
          range = c( 11, 44 ),
          cut_off = NA,
          interpretation = paste0(
            'Higher scores indicate poorer control over ',
            'motor actions'
          )
        )

        # Close 'Motor'
      }

      ### Planning
      if ( subscale %in% c( 'Planning', 'planning' ) ) {

        out$Description <- paste0(
          'Scores for the BIS-11 Planning Facet - ',
          'measure of control in planning for the future'
        )

        out$Units <- 'Summed score'

        out$Subscale <- list(
          name = 'Planning',
          n_items = 11,
          range = c( 11, 44 ),
          cut_off = NA,
          interpretation = paste0(
            'Higher scores indicate poorer self-control ',
            'in planning for the future'
          )
        )

        # Close 'Planning'
      }

      # Close 'Subscales'
    }

    # Close 'BIS-11'
  }

  #### 1.1.21) BRIEF ####
  if ( abbreviation %in% c( 'BRIEF', 'BRIEF-A' ) ) {

    out$Description <- paste0(
      'Scores for the BRIEF - measure of executive ',
      'function impairment'
    )

    out$Units <- 'Summed score'

    out$Scale <- list(
      name = 'Behavior Rating Inventory of Executive Function',
      n_items = 75,
      range = c( Min = 75, Max = 525 ),
      abbreviation = 'BRIEF',
      cut_off = NA,
      reference = paste0(
        'Baron, I. S. (2000). Behavior Rating Inventory of ',
        'Executive Function. Child Neuropsychology, 6(3), 235–238. ',
        'https://doi.org/10.1076/chin.6.3.235.3152'
      ),
      interpretation = paste0(
        'Higher scores indicate poorer executive function'
      )
    )

    # Close 'BRIEF'
  }

  #### 1.1.22) DOSPERT ####
  if ( abbreviation == 'DOSPERT' ) {

    ### Overall
    out$Scale <- list(
      name = 'Domain-Specific Risk-Taking Scale',
      n_items = 90,
      range = c( 90, 630 ),
      abbreviation = 'DOSPERT',
      cut_off = NA,
      reference = paste0(
        'Weber, E. U., Blais, A. R., & Betz, N. E. (2002). ',
        'A domain‐specific risk‐attitude scale: Measuring ',
        'risk perceptions and risk behaviors. Journal of ',
        'behavioral decision making, 15(4), 263-290. ',
        'https://doi.org/10.1002/bdm.414'
      ),
      interpretation = paste0(
        'Higher scores indicate greater propensity ',
        'for risk-taking'
      )
    )

    ### Subscales
    if ( subscale != '' ) {

      ### Ethical
      if ( subscale %in% c( 'Ethical', 'ethical' ) ) {

        out$Description <- paste0(
          'Scores for the DOSPERT (E) - measure of ',
          'risk-taking in ethical situations'
        )

        out$Units <- 'Summed score'

        out$Subscale <- list(
          name = 'Ethical',
          n_items = 6,
          range = c( 6, 42 ),
          cut_off = NA,
          interpretation = paste0(
            'Higher scores indicate greater propensity ',
            'to engage in ethically risky activities or behaviors'
          )
        )

        # Close 'Ethical'
      }

      ### Financial
      if ( subscale %in% c( 'Financial', 'financial' ) ) {

        out$Description <- paste0(
          'Scores for the DOSPERT (F) - measure of ',
          'risk-taking in financial situations'
        )

        out$Units <- 'Summed score'

        out$Subscale <- list(
          name = 'Financial',
          n_items = 6,
          range = c( 6, 42 ),
          cut_off = NA,
          interpretation = paste0(
            'Higher scores indicate greater propensity ',
            'to engage in financially risky activities or behaviors'
          )
        )

        # Close 'Financial'
      }

      ### Health/Safety
      if ( subscale %in% c( 'Health/Safety', 'health/safety',
                            'Health/safety', 'Health', 'Safety',
                            'health', 'safety ' ) ) {

        out$Description <- paste0(
          'Scores for the DOSPERT (H/S) - measure of ',
          'risk-taking in health/safety-related situations'
        )

        out$Units <- 'Summed score'

        out$Subscale <- list(
          name = 'Health/Safety',
          n_items = 6,
          range = c( 6, 42 ),
          cut_off = NA,
          interpretation = paste0(
            'Higher scores indicate greater propensity ',
            'to engage in activities or behaviors that pose ',
            'a risk to personal health and safety'
          )
        )

        # Close 'Health/Safety'
      }

      ### Social
      if ( subscale %in% c( 'Social', 'social' ) ) {

        out$Description <- paste0(
          'Scores for the DOSPERT (S) - measure of ',
          'risk-taking in social situations'
        )

        out$Units <- 'Summed score'

        out$Subscale <- list(
          name = 'Social',
          n_items = 6,
          range = c( 6, 42 ),
          cut_off = NA,
          interpretation = paste0(
            'Higher scores indicate greater propensity ',
            'to engage in socially risky activities or behaviors'
          )
        )

        # Close 'Social'
      }

      ### Recreational
      if ( subscale %in% c( 'Recreational', 'recreational' ) ) {

        out$Description <- paste0(
          'Scores for the DOSPERT (R) - measure of ',
          'risk-taking in recreational situations'
        )

        out$Units <- 'Summed score'

        out$Subscale <- list(
          name = 'Recreational',
          n_items = 6,
          range = c( 6, 42 ),
          cut_off = NA,
          interpretation = paste0(
            'Higher scores indicate greater propensity ',
            'to engage in risky recreational activities'
          )
        )

        # Close 'Recreational'
      }

      ### Ethical Risk Perception
      if ( subscale %in% c( 'Ethical Risk Perception', 'Ethical risk perception' ) ) {

        out$Description <- paste0(
          'Scores for the DOSPERT (EP) - measure of ',
          'risk perception in ethical situations'
        )

        out$Units <- 'Summed score'

        out$Subscale <- list(
          name = 'Ethical Risk Perception',
          n_items = 6,
          range = c( 6, 42 ),
          cut_off = NA,
          interpretation = paste0(
            'Higher scores indicate greater perception of risk ',
            'in ethical situations'
          )
        )

        # Close 'Ethical Risk Perception'
      }

      ### Financial Risk Perception
      if ( subscale %in% c( 'Financial Risk Perception', 'Financial risk perception' ) ) {

        out$Description <- paste0(
          'Scores for the DOSPERT (FP) - measure of ',
          'risk perception in financial situations'
        )

        out$Units <- 'Summed score'

        out$Subscale <- list(
          name = 'Financial Risk Perception',
          n_items = 6,
          range = c( 6, 42 ),
          cut_off = NA,
          interpretation = paste0(
            'Higher scores indicate greater perception of risk ',
            'in financial situations'
          )
        )

        # Close 'Financial Risk Perception'
      }

      ### Health/Safety Risk Perception
      if ( subscale %in% c( 'Health/Safety Risk Perception',
                            'Health/safety risk perception' ) ) {

        out$Description <- paste0(
          'Scores for the DOSPERT (H/SP) - measure of ',
          'risk perception in health/safety-related situations'
        )

        out$Units <- 'Summed score'

        out$Subscale <- list(
          name = 'Health/Safety Risk Perception',
          n_items = 6,
          range = c( 6, 42 ),
          cut_off = NA,
          interpretation = paste0(
            'Higher scores indicate greater perception of risk ',
            'in health/safety-related situations'
          )
        )

        # Close 'Health/Safety Risk Perception'
      }

      ### Social Risk Perception
      if ( subscale %in% c( 'Social Risk Perception',
                            'Social risk perception' ) ) {

        out$Description <- paste0(
          'Scores for the DOSPERT (SP) - measure of ',
          'risk perception in social situations'
        )

        out$Units <- 'Summed score'

        out$Subscale <- list(
          name = 'Social Risk Perception',
          n_items = 6,
          range = c( 6, 42 ),
          cut_off = NA,
          interpretation = paste0(
            'Higher scores indicate greater perception ',
            'of risk in social situations'
          )
        )

        # Close 'Social Risk Perception'
      }

      ### Recreational Risk Perception
      if ( subscale %in% c( 'Recreational Risk Perception',
                            'Recreational risk perception' ) ) {

        out$Description <- paste0(
          'Scores for the DOSPERT (RP) - measure of ',
          'risk perception in recreational situations'
        )

        out$Units <- 'Summed score'

        out$Subscale <- list(
          name = 'Recreational Risk Perception',
          n_items = 6,
          range = c( 6, 42 ),
          cut_off = NA,
          interpretation = paste0(
            'Higher scores indicate greater perception ',
            'of risk in recreational situations'
          )
        )

        # Close 'Recreational Risk Perception'
      }

      ### Ethical Risk Benefit
      if ( subscale %in% c( 'Ethical Risk Benefit',
                            'Ethical risk benefit' ) ) {

        out$Description <- paste0(
          'Scores for the DOSPERT (EB) - measure of expected ',
          'benefit of risk-taking in ethical situations'
        )

        out$Units <- 'Summed score'

        out$Subscale <- list(
          name = 'Ethical Risk Benefit',
          n_items = 6,
          range = c( 6, 42 ),
          cut_off = NA,
          interpretation = paste0(
            'Higher scores indicate greater expected ',
            'benefits from risk-taking in ethical situations'
          )
        )

        # Close 'Ethical Risk Benefit'
      }

      ### Financial Risk Benefit
      if ( subscale %in% c( 'Financial Risk Benefit',
                            'Financial risk benefit' ) ) {

        out$Description <- paste0(
          'Scores for the DOSPERT (FB) - measure of expected ',
          'benefit of risk-taking in financial situations'
        )

        out$Units <- 'Summed score'

        out$Subscale <- list(
          name = 'Financial Risk Benefit',
          n_items = 6,
          range = c( 6, 42 ),
          cut_off = NA,
          interpretation = paste0(
            'Higher scores indicate greater expected ',
            'benefits from risk-taking in financial situations'
          )
        )

        # Close 'Financial Risk Benefit'
      }

      ### Health/Safety Risk Benefit
      if ( subscale %in% c( 'Heatlh/Safety Risk Benefit',
                            'Health/safety risk benefit' ) ) {

        out$Description <- paste0(
          'Scores for the DOSPERT (H/SB) - measure of expected ',
          'benefit of risk-taking in health/safety-related ',
          'situations'
        )

        out$Units <- 'Summed score'

        out$Subscale <- list(
          name = 'Health/Safety Risk Benefit',
          n_items = 6,
          range = c( 6, 42 ),
          cut_off = NA,
          interpretation = paste0(
            'Higher scores indicate greater expected ',
            'benefits from risk-taking in health/safety-',
            'related situations'
          )
        )

        # Close 'Health/Safety Risk Benefit'
      }

      ### Social Risk Benefit
      if ( subscale %in% c( 'Social Risk Benefit',
                            'Social risk benefit' ) ) {

        out$Description <- paste0(
          'Scores for the DOSPERT (SB) - measure of expected ',
          'benefit of risk-taking in social situations'
        )

        out$Units <- 'Summed score'

        out$Subscale <- list(
          name = 'Social Risk Benefit',
          n_items = 6,
          range = c( 6, 42 ),
          cut_off = NA,
          interpretation = paste0(
            'Higher scores indicate greater expected ',
            'benefits from risk-taking in social situations'
          )
        )

        # Close 'Social Risk Benefit'
      }

      ### Recreational Risk Benefit
      if ( subscale %in% c( 'Recreational Risk Benefit',
                            'Recreational risk benefit' ) ) {

        out$Description <- paste0(
          'Scores for the DOSPERT (RB) - measure of expected ',
          'benefit of risk-taking in recreational situations'
        )

        out$Units <- 'Summed score'

        out$Subscale <- list(
          name = 'Recreational Risk Benefit',
          n_items = 6,
          range = c( 6, 42 ),
          cut_off = NA,
          interpretation = paste0(
            'Higher scores indicate greater expected ',
            'benefits from risk-taking in recreational situations'
          )
        )

        # Close 'Recreational Risk Benefit'
      }

      # Close 'Subscales'
    }

    # Close 'DOSPERT'
  }

  #### 1.1.23) KSADS ####
  if ( abbreviation == 'KSADS' ) {

    out$Description <- paste0(
      'Scores for the KSADS - measure of ',
      'family history of addiction'
    )

    out$Units <- 'Summed score'

    out$Scale <- list(
      name = 'Kiddie Schedule for Affective Disorders and Schizophrenia',
      n_items = 7,
      range = c( Min = 0, Max = 21 ),
      abbreviation = 'KSADS',
      cut_off = NA,
      reference = paste0(
        'Chambers, W. J., Puig-Antich, J., Hirsch, M., Paez, P., ',
        'Ambrosini, P. J., Tabrizi, M. A., & Davies, M. (1985). The ',
        'assessment of affective disorders in children and ',
        'adolescents by semistructured interview: test-retest ',
        'reliability of the Schedule for Affective Disorders ',
        'and Schizophrenia for School-Age Children, Present ',
        'Episode Version. Archives of general psychiatry, ',
        '42(7), 696-702. ',
        'https://doi.org/10.1001/archpsyc.1985.01790300064008'
      ),
      interpretation = paste0(
        'Higher scores indicate a more extensive family history ',
        'of substance abuse'
      )
    )

    # Close 'KSADS'
  }

  #### 1.1.24) PERS ####
  if ( abbreviation == 'PERS' ) {

    out$Description <- paste0(
      'Scores for the PERS - measure of ',
      'clinician expectations for patient recovery'
    )

    out$Units <- 'Summed score'

    out$Scale <- list(
      name = 'Provider Expectations for Recovery Scale',
      n_items = 12,
      range = c( Min = 12, Max = 60 ),
      abbreviation = 'KSADS',
      cut_off = NA,
      reference = paste0(
       'Salyers, M. P., Brennan, M., & Kean, J. (2013). Provider ',
       'Expectations for Recovery Scale: Refining a measure of ',
       'provider attitudes. Psychiatric Rehabilitation Journal, 36(3), 153. ',
       'https://doi.org/10.1037/prj0000010'
      ),
      interpretation = paste0(
        'Higher scores indicate greater clinican optimism about patient recovery'
      )
    )

    # Close 'PERS'
  }

  #### 1.1.25) CUD-CHECK ####
  if ( abbreviation == 'CUD-CHECK' ) {

    out$Description <- paste0(
      'Scores for the CUD Checklist - measure of ',
      'the likelihood of a Cannabis Use Disorder'
    )

    out$Units <- 'Summed score'

    out$Scale <- list(
      name = 'DSM-5 Cannabis Use Disorder Checklist',
      n_items = 12,
      range = c( Min = 0, Max = 12 ),
      abbreviation = 'CUD-CHECK',
      cut_off = c( `Possible disorder` = 2 ),
      reference = paste0(
        'American Psychiatric Association. (2013). Diagnostic and ',
        'statistical manual of mental disorders (DSM-5®). American ',
        'Psychiatric Pub. ',
        'https://doi.org/10.1176/appi.books.9780890425596'
      ),
      interpretation = paste0(
        'Higher scores indicate greater likelihood of presence of ',
        'Cannabis Use Disorder'
      )
    )

    # Close 'CUD-CHECK'
  }

  #### 1.1.26) PCL ####
  if ( abbreviation == 'PCL' ) {

    out$Description <- paste0(
      'Scores for the PCL - measure of ',
      'post-traumatic stress-related symptoms'
    )

    out$Units <- 'Summed score'

    out$Scale <- list(
      name = 'PTSD Checklist',
      n_items = 20,
      range = c( Min = 0, Max = 80 ),
      abbreviation = 'PCL',
      cut_off = c( `Possible disorder` = 32 ),
      reference = paste0(
        'Weathers, F. W., Litz, B. T., Herman, D. S., Huska, J. A., ',
        '& Keane, T. M. (1993, October). The PTSD Checklist (PCL): ',
        'Reliability, validity, and diagnostic utility. In annual ',
        'convention of the international society for traumatic stress ',
        'studies, San Antonio, TX (Vol. 462).'
      ),
      interpretation = paste0(
        'Higher scores indicate presence of more post-traumatic ',
        'stress symptoms'
      )
    )

    # Close 'PCL'
  }

  #### 1.1.27) WTAR ####
  if ( abbreviation == 'WTAR' ) {

    out$Description <- paste0(
      'Scores for the WTAR - measure of intellectual function'
    )

    out$Units <- 'Summed score'

    out$Scale <- list(
      name = 'Wechsler Test of Adult Reading',
      n_items = 50,
      range = c( Min = 0, Max = 50 ),
      abbreviation = 'WTAR',
      cut_off = NA,
      reference = paste0(
        'Holdnack, H.A. (2001). Wechsler Test of Adult Reading: ',
        'WTAR. San Antonio. The Psychological Corporation.'
      ),
      interpretation = paste0(
        'Higher scores indicate greater intellectual function'
      )
    )

    # Close 'WTAR'
  }

  #### 1.1.28) AUQ ####
  if ( abbreviation %in% c( 'AUQ', 'URGE' ) ) {

    ### Overall
    out$Scale <- list(
      name = 'Alcohol Urge Questionnaire',
      n_items = 8,
      range = c( 8, 56 ),
      abbreviation = 'AUQ',
      cut_off = NA,
      reference = paste0(
        'Bohn, M. J., Krahn, D. D., & Staehler, B. A. (1995). ',
        'Development and initial validation of a measure of ',
        'drinking urges in abstinent alcoholics. Alcoholism: ',
        'clinical and experimental research, 19(3), 600-606. ',
        'https://doi.org/10.1111/j.1530-0277.1995.tb01554.x'
      ),
      interpretation = paste0(
        'Higher scores indicate greater craving of alcohol'
      )
    )

    ### Subscales
    if ( subscale != '' ) {

      ### Urge
      if ( subscale %in% c( 'URGE', 'Urge', 'urge' ) ) {

        out$Description <- paste0(
          'Scores for the AUQ Urge subscale - measure of the urge to consume alcohol'
        )

        out$Units <- 'Averaged score'

        out$Subscale <- list(
          name = 'Alcohol Consumption Urge',
          n_items = 3,
          range = c( 0, 4 ),
          cut_off = NA,
          interpretation = paste0(
            'Higher scores indicate greater urge to consume alcohol'
          )
        )

        # Close 'Urge'
      }

      # Close 'Subscales'
    }

    # Close 'AUQ'
  }

  #### 1.1.29) APSS ####
  if ( abbreviation == 'APSS' ) {

    out$Description <- paste0(
      'Scores for the APSS - measure of the degree of risk for psychotic-like experiences for a subject'
    )

    out$Units <- 'Summed score'

    out$Scale <- list(
      name = 'Adolescent Psychotic Symptom Screener',
      n_items = 7,
      range = c( Min = 0, Max = 7 ),
      abbreviation = 'APSS',
      cut_off = c( `at risk` >= 32 ),
      reference = paste0(
        'Kelleher, I., Harley, M., Murtagh, A., & Cannon, M. (2011).',
        ' Are screening instruments valid for psychotic-like ',
        'experiences? A validation study of screening questions ',
        'for psychotic-like experiences using in-depth clinical ',
        'interview. Schizophrenia bulletin, 37(2), 362–369. ',
        'https://doi.org/10.1093/schbul/sbp057'
      ),
      interpretation = paste0(
        'Higher scores indicate greater risk for psychotic-like experiences'
      )
    )

    # Close 'APSS'
  }

  #### 1.1.30) BPAQ ####
  if ( abbreviation %in% c( 'BPAQ' ) ) {

    ### Overall
    out$Scale <- list(
      name = 'Buss-Perry Aggression Questionnaire',
      n_items = 29,
      range = c( 29, 145 ),
      abbreviation = 'BPAQ',
      cut_off = NA,
      reference = paste0(
        'Hamilton, et al. (2011) The PhenX Toolkit: ',
        'Get the Most From Your Measures. American Journal of ',
        'Epidemiology, 174(3), 253-60.'
      )

      )

    ### Subscales
    if ( subscale != '' ) {

      ### Physical Aggression
      if ( subscale %in% c( 'Physical Aggression', 'BPAQ-PA' ) ) {

        out$Description <- paste0(
          'Scores for the BPAQ-PA – measure of the level of physical aggression of a subject'
        )

        out$Units <- 'Averaged score'

        out$Subscale <- list(
          name = 'Physical Aggression',
          n_items = 9,
          range = c( 1, 5 ),
          cut_off = NA,
          interpretation = paste0(
            'Higher scores indicate higher physically aggressive behavior'
          )
        )

        # Close 'Physical Aggression'
      }

      ### Verbal Aggression
      if ( subscale %in% c( 'Verbal Aggression', 'BPAQ-VA' ) ) {

        out$Description <- paste0(
          'Scores for the BPAQ-VA – measure of the level of verbal aggression of a subject'
        )

        out$Units <- 'Averaged score'

        out$Subscale <- list(
          name = 'Verbal Aggression',
          n_items = 5,
          range = c( 1, 5 ),
          cut_off = NA,
          interpretation = paste0(
            'Higher scores indicate higher verbally aggressive behavior'
          )
        )

        # Close 'Verbal Aggression'
      }

      ### Anger
      if ( subscale %in% c( 'Anger', 'BPAQ-A' ) ) {

        out$Description <- paste0(
          'Scores for the BPAQ-A – measure of the level of anger of a subject'
        )

        out$Units <- 'Averaged score'

        out$Subscale <- list(
          name = 'Anger',
          n_items = 7,
          range = c( 1, 5 ),
          cut_off = NA,
          interpretation = paste0(
            'Higher scores indicate higher anger'
          )
        )

        # Close 'Anger'
      }
      ### Hostility
      if ( subscale %in% c( 'Hostility', 'BPAQ-H' ) ) {

        out$Description <- paste0(
          'Scores for the BPAQ-H – measure of the level of hostility of a subject'
        )

        out$Units <- 'Averaged score'

        out$Subscale <- list(
          name = 'Hostility',
          n_items = 8,
          range = c( 1, 5 ),
          cut_off = NA,
          interpretation = paste0(
            'Higher scores indicate higher hostility'
          )
        )

        # Close 'Hostility'
      }
      # Close 'Subscales'
    }

    # Close 'BPAQ'
  }

  #### 1.1.31) DMQ ####
  if ( abbreviation %in% c( 'DMQ' ) ) {

    ### Overall
    out$Scale <- list(
      name = 'The Drinking Motives Questionnaire-Revised',
      n_items = 20,
      range = c( 4, 20 ),
      abbreviation = 'DMQ',
      cut_off = NA,
      reference = paste0(
        'Cooper, M. L. (1994). Motivations for Alcohol Use Among ',
        'Adolescents: Development and Validation of a Four-Factor ',
        'Model. Psychological Assessment, 6 (2), 117-128.'
      )

    )

    ### Subscales
    if ( subscale != '' ) {

      ### Physical Aggression
      if ( subscale %in% c( 'Social', 'SOC' ) ) {

        out$Description <- paste0(
          'Scores for the DMQ Social (SOC) subscale - measure of a patient’s social motives for alcohol use '
        )

        out$Units <- 'Averaged score'

        out$Subscale <- list(
          name = 'Social',
          n_items = 5,
          range = c( 1, 5 ),
          cut_off = NA,
          interpretation = paste0(
            'Higher scores indicate greater social motives for alcohol use'
          )
        )

        # Close 'Social'
      }

      ### Coping
      if ( subscale %in% c( 'Coping', 'COP' ) ) {

        out$Description <- paste0(
          'Scores for the DMQ Coping (COP) subscale – measure of a patient’s coping motives for alcohol use '
        )

        out$Units <- 'Averaged score'

        out$Subscale <- list(
          name = 'Coping',
          n_items = 5,
          range = c( 1, 5 ),
          cut_off = NA,
          interpretation = paste0(
            'Higher scores indicate greater coping motives for alcohol use'
          )
        )

        # Close 'Coping'
      }

      ### Enhancement
      if ( subscale %in% c( 'Enhancement', 'ENH' ) ) {

        out$Description <- paste0(
          'Scores for the DMQ Enhancement (ENH) subscale – measure of a patient’s enhancement motives for alcohol use '
        )

        out$Units <- 'Averaged score'

        out$Subscale <- list(
          name = 'Enhancement',
          n_items = 5,
          range = c( 1, 5 ),
          cut_off = NA,
          interpretation = paste0(
            'Higher scores indicate greater enhancement motives for alcohol use'
          )
        )

        # Close 'Enhancement'
      }
      ### Conformity
      if ( subscale %in% c( 'Conformity', 'CON' ) ) {

        out$Description <- paste0(
          'Scores for the DMQ Conformity (CON) subscale – measure of a patient’s conformity motives for alcohol use '
        )

        out$Units <- 'Averaged score'

        out$Subscale <- list(
          name = 'Conformity',
          n_items = 5,
          range = c( 1, 5 ),
          cut_off = NA,
          interpretation = paste0(
            'Higher scores indicate greater conformity motives for alcohol use'
          )
        )

        # Close 'Conformity'
      }
      # Close 'Subscales'
    }

    # Close 'DMQ'
  }

  #### 1.1.32) ERS ####
  if ( abbreviation %in% c( 'ERS' ) ) {

    ### Overall
    out$Scale <- list(
      name = 'The Emotion Reactivity Scale',
      n_items = 21,
      range = c( 0, 84 ),
      abbreviation = 'ERS',
      cut_off = NA,
      reference = paste0(
        'Nock, M. K., Wedig, M. M., Holmberg, E. B., & Hooley, J. M.',
        ' (2008). The Emotion Reactivity Scale: Development, ',
        'Evaluation, and Relation to Self-Injurious Thoughts and ',
        'Behaviors. Behavior Therapy, 39(2), 107–116. ',
        'https://doi.org/10.1016/j.beth.2007.05.005'
      )

    )

    ### Subscales
    if ( subscale != '' ) {

      ### Sensitivity
      if ( subscale %in% c( 'Sensitivity' ) ) {

        out$Description <- paste0(
          'Scores for the ERS Sensitivity subscale – measure of a patient’s emotional sensitivity '
        )

        out$Units <- 'Summed score'

        out$Subscale <- list(
          name = 'Sensitivity',
          n_items = 10,
          range = c( 0, 40 ),
          cut_off = NA,
          interpretation = paste0(
            'Higher scores indicate higher emotional sensitivity.'
          )
        )

        # Close 'Sensitivity'
      }

      ### Intensity/Arousal
      if ( subscale %in% c( 'Intensity', 'Arousal', 'Intensity/arousal' ) ) {

        out$Description <- paste0(
          'Scores for the ERS Intensity/arousal Subscale - measure of a patient’s emotion reaction intensity '
        )

        out$Units <- 'Summed score'

        out$Subscale <- list(
          name = 'Intensity/arousal',
          n_items = 7,
          range = c( 0, 28 ),
          cut_off = NA,
          interpretation = paste0(
            'Higher scores indicate higher intensity of emotions'
          )
        )

        # Close 'Intensity/arousal'
      }

      ### Persistence
      if ( subscale %in% c( 'Persistence' ) ) {

        out$Description <- paste0(
          'Scores for the ERS persistence subscale – measure of a patient’s persistence (duration) of emotional reactions'
        )

        out$Units <- 'Summed score'

        out$Subscale <- list(
          name = 'Persistence',
          n_items = 4,
          range = c( 0, 16 ),
          cut_off = NA,
          interpretation = paste0(
            'Higher scores indicate longer persistence (duration) of emotional reactions'
          )
        )

        # Close 'Persistence'
      }
      # Close 'Subscales'
    }

    # Close 'ERS'
  }
  #### 1.1.33) GLTEQ ####
  if ( abbreviation == 'GLTEQ' ) {

    out$Description <- paste0(
      'Scores for the GLTEQ – measure of the level of physical activity of a subject'
    )

    out$Units <- 'Summed score'

    out$Scale <- list(
      name = 'Godin Leisure-Time Exercise Questionnaire',
      n_items = 3,
      range = c( Min = 0, Max = NA ),
      abbreviation = 'GLTEQ',
      cut_off = c( `Active` >= 32, `Moderately Active` = 14-23, `Insufficiently Active/Sedentary` < 14 ),
      reference = paste0(
        'Godin Leisure-Time Exercise Questionnaire. (1997). ',
        'Medicine & Science in Sports & Exercise, 29(6), 36–38.'
      ),
      interpretation = paste0(
        'Higher scores indicate more physically active lifestyle'
      )
    )

    # Close 'GLTEQ'
  }

  #### 1.1.33) MCQ ####
if ( abbreviation == 'MCQ' ) {

    out$Description <- paste0(
      'Scores for the MCQ – measure of whether the participant prefers smaller immediate rewards over delayed larger rewards'
    )

    out$Units <- 'Rate of Discounting (k)'

    out$Scale <- list(
      name = 'Monetary Choice Questionnaire',
      n_items = 27,
      range = c( Min = 0.0, Max = 0.5 ),
      abbreviation = 'MCQ',
      cut_off = NA,
      reference = NA,
      interpretation = paste0(
        'Higher k values (higher discounting rates) indicate higher level of impulsivity (preference for immediate rewards)'
      )
    )

    # Close 'MCQ'
}
  #### 1.1.34) MEEQ ####
  if ( abbreviation %in% c( 'MEEQ' ) ) {

    ### Overall
    out$Scale <- list(
      name = 'Marijuana Effect Expectancy Questionnaire',
      n_items = 48,
      range = c( 48, 240 ),
      abbreviation = 'MEEQ',
      cut_off = NA,
      reference = NA

    )

    ### Subscales
    if ( subscale != '' ) {

      ### Cognitive and Behavioral Impairment
      if ( subscale %in% c( 'Cognitive and Behavioral Impairment' ) ) {

        out$Description <- paste0(
          'Scores for the MEEQ Cognitive and Behavioral Impairment subscale - measure of the degree of expectancy of cognitive and behavioral impairment resulting from marijuana use'
        )

        out$Units <- 'Summed score'

        out$Subscale <- list(
          name = 'Cognitive and Behavioral Impairment',
          n_items = 10,
          range = c( 10, 50 ),
          cut_off = NA,
          interpretation = paste0(
            'Higher scores indicate greater expectancy of cognitive and behavioral impairment due to marijuana use '
          )
        )

        # Close 'Cognitive and Behavioral Impairment'
      }

      ### Relaxation and Tension Reduction
      if ( subscale %in% c( 'Relaxation and Tension Reduction' ) ) {

        out$Description <- paste0(
          'Scores for the MEEQ Relaxation and Tension Reduction subscale - measure of the degree of expectancy of relaxation and tension reduction resulting from marijuana use '
        )

        out$Units <- 'Summed score'

        out$Subscale <- list(
          name = 'Relaxation and Tension Reduction',
          n_items = 8,
          range = c( 8, 40 ),
          cut_off = NA,
          interpretation = paste0(
            'Higher scores indicate greater expectancy of relaxation and tension reduction due to marijuana use'
          )
        )

        # Close 'Relaxation and Tension Reduction'
      }

      ### Social and Sexual Facilitation
      if ( subscale %in% c( 'Social and Sexual Facilitation' ) ) {

        out$Description <- paste0(
          'Scores for the MEEQ Social and Sexual Facilitation subscale - measure of the degree of expectancy of social and sexual facilitation resulting from marijuana use '
        )

        out$Units <- 'Summed score'

        out$Subscale <- list(
          name = 'Social and Sexual Facilitation',
          n_items = 9,
          range = c( 9, 45 ),
          cut_off = NA,
          interpretation = paste0(
            'Higher scores indicate greater expectancy of social and sexual facilitation due to marijuana use'
          )
        )

        # Close 'Social and Sexual Facilitation'
      }
      ### Perceptual and Cognitive Enhancement
      if ( subscale %in% c( 'Perceptual and Cognitive Enhancement' ) ) {

        out$Description <- paste0(
          'Scores for the MEEQ Perceptual and Cognitive Enhancement subscale - measure of the degree of expectancy of perceptual and cognitive enhancement resulting from marijuana use'
        )

        out$Units <- 'Summed score'

        out$Subscale <- list(
          name = 'Perceptual and Cognitive Enhancement',
          n_items = 8,
          range = c( 8, 40 ),
          cut_off = NA,
          interpretation = paste0(
            'Higher scores indicate greater expectancy of perceptual and cognitive enhancement due to marijuana use '
          )
        )

        # Close 'Perceptual and Cognitive Enhancement'
      }

      ### Global Negative Effects
      if ( subscale %in% c( 'Global Negative Effects' ) ) {

        out$Description <- paste0(
          'Scores for the MEEQ Global Negative Effects subscale - measure of the degree of expectancy of negative effects resulting from marijuana use '
        )

        out$Units <- 'Summed score'

        out$Subscale <- list(
          name = 'Global Negative Effects',
          n_items = 9,
          range = c( 9, 45 ),
          cut_off = NA,
          interpretation = paste0(
            'Higher scores indicate greater expectancy of negative effects due to marijuana use  '
          )
        )

        # Close 'Global Negative Effects'
      }

      ### Craving and Physical Effects
      if ( subscale %in% c( 'Craving and Physical Effects' ) ) {

        out$Description <- paste0(
          'Scores for the MEEQ Craving and Physical Effects subscale - measure of the degree of expectancy of craving and physical effects resulting from marijuana use '
        )

        out$Units <- 'Summed score'

        out$Subscale <- list(
          name = 'Craving and Physical Effects',
          n_items = 6,
          range = c( 6, 30 ),
          cut_off = NA,
          interpretation = paste0(
            'Higher scores indicate greater expectancy of craving and physical effects due to marijuana use '
          )
        )

        # Close 'Craving and Physical Effects'
      }
      # Close 'Subscales'
    }

    # Close 'MEEQ'
  }

  if ( is.null( out$Scale ) ) {
    stop( 'Scale or subscale not found' )
  } else {

    if ( is.null( out$Subscale ) ) {
      out$Subscale <- ''
    }

    return( out )
  }
}



#### 1.2) scale_format ####
#' Standardized Reporting Format for Scales/Inventories/Questionnaires
#'
#' A function to help structure data dictionary information for
#' scales/inventories/questionnaires into a standardized
#' format.
#'
#' @param name A character string with the full name of
#'   the scale (e.g., 'Hospital Anxiety Depression Scale'
#'   or the 'Cannabis Use Disorder Identification Test').
#' @param n_items An integer with the number of
#'   questions/items in the scale (or subscale).
#' @param range A vector with the lower and upper limits
#'   for the possible scores the measure can have.
#' @param abbreviation A character string with the abbreviation
#'   for the scale (e.g., 'HADS' or 'CUDIT').
#' @param cut_off An optional numeric vector giving any cut-offs
#'   used in the scale (e.g., the CUDIT uses a clinical cut-off
#'   of scores at \code{8} or higher to indicate problematic
#'   cannabis use).
#' @param reference A character vector with APA-style
#'   references that describe the measure and relevant
#'   details on its scoring.
#' @param interpretation A brief note on how to interpret
#'   measure scores (e.g., describe what higher scores
#'   indicate, such as how higher scores on the CUDIT
#'   indicate more problematic cannabis use).
#' @param subscale Logical; if \code{TRUE}, instead outputs
#'   the reporting format for subscales (consisting only
#'   of the subscale name its number of items).
#'
#' @author Kevin Potter
#'
#' @return A list.
#'
#' @examples
#' scale_format(
#'   name = 'Cannabis Use Disorder Identification Test (Revised)',
#'   n_items = 8,
#'   range = c( 0, 32 ),
#'   abbreviation = 'CUDIT-R',
#'   interpretation = 'Higher scores indicate more problematic cannabis use'
#' )
#'
#' @export

scale_format <- function(name,
                         n_items,
                         range,
                         abbreviation = '',
                         cut_off = NA,
                         reference = '',
                         interpretation = '',
                         description = '',
                         units_of_x = '',
                         subscale = FALSE ) {

  if ( length( range ) == 1 ) {
    range = rep( range, 2 )
  }

  if ( subscale ) {

    out <- list(
      name = name,
      n_items = n_items,
      range = c( Min = range[1], Max = range[2] ),
      cut_off = cut_off,
      interpretation = interpretation
    )

  } else {
    # Initialize output
    out <- list(
      name = name,
      n_items = n_items,
      range = c( Min = range[1], Max = range[2] ),
      abbreviation = abbreviation,
      cut_off = cut_off,
      reference = reference,
      interpretation = interpretation
    )
  }

  return( out )
}

#### 2) column_abbreviations ####
#' Abbreviations and Labels for Column Names
#'
#' Function to match labels describing
#' variable categories and data types to
#' abbreviations contained within a column
#' name.
#'
#' @param column_name A standardized variable name
#'   (i.e., 'GGG.TTT.Var_name' or
#'   'GGG.TTT.Optional.Var_name').
#' @param type The type of abbreviation whose
#'   label is to be extracted.
#' @param custom An optional matrix with two
#'   columns, 'Abbr' and 'Label', for
#'   custom abbreviations and labels,
#'   respectively.
#' @param n The number of rows to create when
#'   generating a template for the matrix to
#'   pass to the \code{custom} argument.
#' @param separator The punctuation used
#'   to separate abbreviations, typically
#'   a period.
#'
#' @author  Kevin Potter
#'
#' @return A character string. If only \code{n} is
#'   provided, a matrix with \code{n} rows with
#'   columns 'Abbr' and 'Label' to use as a
#'   template for custom abbreviations/labels.
#'
#' @examples
#' # List pre-defined abbreviations and labels
#' column_abbreviations( type = 'category' )
#' column_abbreviations( type = 'data type' )
#'
#' # Match abbreviation for variable categories
#' column_abbreviations(
#'   'IDS.CHR.Subject', type = 'category'
#' )
#'
#' # Match abbreviation for data type
#' column_abbreviations(
#'   'IDS.CHR.Subject', type = 'data type'
#' )
#'
#' # Create template for custom abbreviations
#' # and labels
#' M <- column_abbreviations( n = 3 )
#' M[,1] <- c( 'BL', 'Y1', 'Y2' )
#' M[,2] <- c( 'Baseline', 'Year 1', 'Year 2' )
#' column_abbreviations( 'SMK.INT.BL.Quit_status', custom = M )
#' column_abbreviations( 'SMK.INT.Y2.Quit_status', custom = M )
#'
#' @export

column_abbreviations <- function( column_name = '',
                                  type = NULL,
                                  custom = NULL,
                                  n = NA,
                                  separator = "." ) {

  # Pre-defined abbreviations and
  # labels for variable categories
  abbr_labels.var_cat <- rbind(
    c( 'IDS', 'Identifiers' ),
    c( 'SSS', 'Session details' ),
    c( 'SBJ', 'Subject details' ),
    c( 'STD', 'Student details' ),
    c( 'PNT', 'Patient details' ),
    c( 'PRT', 'Participant details' ),
    c( 'DMG', 'Demographic information' ),
    c( 'URN', 'Urine test data' ),
    c( 'INV', 'Inventories and questionnaires and scales' ),
    c( 'QTN', 'Inventories and questionnaires and scales' ),
    c( 'SCL', 'Inventories and questionnaires and scales' ),
    c( 'TLF', 'Timeline follow-back data' ),
    c( 'CTB', 'CANTAB battery data' ),
    c( 'DRY', 'Phone app diary data' ),
    c( 'MNI', 'MINI diagnoses results' ),
    c( 'CNM', 'Concomitant medication details' ),
    c( 'MRI', 'MRI scan details' ),
    c( 'SCN', 'Neural imaging scan details' ),
    c( 'RMT', 'Remote survey results' ),
    c( 'DTQ', 'Data quality' ),
    c( 'INC', 'Indices for inclusion' )
  )
  colnames( abbr_labels.var_cat ) <- c(
    'Abbr',
    'Label'
  )

  # Pre-defined abbreviations and labels
  # for data types
  abbr_labels.dat_typ <- rbind(
    c( 'INT', 'Integer' ),
    c( 'DBL', 'Double precision floating point number' ),
    c( 'LGC', 'Logical' ),
    c( 'CHR', 'Character string' ),
    c( 'DAT', 'R Date-class variable for calendar dates' ),
    c( 'FCT', 'Enumerated type - factor' ),
    c( 'DSC', 'Descriptive text field' )
  )
  colnames( abbr_labels.dat_typ ) <- c(
    'Abbr',
    'Label'
  )

  # The primary types of abbreviation/label pairs
  list_of_types <- list(
    category = c(
      'Variable categories', 'variable categories',
      'Variable category', 'variable category',
      'Variable', 'variable',
      'Category', 'category',
      '1'
    ),
    type = c(
      'Data types', 'data types',
      'Data type', 'data type',
      'Data', 'data',
      'Type', 'type',
      '2'
    )
  )

  # Create template for matrix with abbreviations and
  # labels
  if ( column_name == '' & is.null( type ) & !is.na( n ) ) {

    out <- cbind(
      Abbr = rep( 'XXX', n ),
      Label = rep( 'Description of XXX', n )
    )
    return( out )

  }

  # Display pre-defined abbreviations and labels
  if ( column_name == '' & !is.null( type ) ) {

    # Options for variable categories
    if ( type %in% list_of_types$category ) {

      message(
        'Abbreviations and labels for variable categories:\n\n'
      )

      message( paste(
        paste0( abbr_labels.var_cat[,1], ' = ',
                abbr_labels.var_cat[,2], '\n' ),
        collapse = ''
      ) )

    }

    # Options for data types
    if ( type %in% list_of_types$type ) {

      message(
        'Abbreviations and labels for data types:\n\n'
      )

      message( paste(
        paste0( abbr_labels.dat_typ[,1], ' = ',
                abbr_labels.dat_typ[,2], '\n' ),
        collapse = ''
      ) )

    }

  }

  # Match a label to an abbreviation contained within a
  # variable name (for variable categories/data types)
  if ( column_name != '' & !is.null( type ) ) {

    # Split variable name into different parts
    name_parts <- strsplit(
      column_name,
      split = separator,
      fixed = TRUE
    )[[1]]

    # Abbreviations for variable categories are always the
    # first part of the variable name
    if ( type %in% list_of_types$category ) {

      # Check for custom abbreviations/labels
      if ( !is.null( custom ) ) {

        custom_not_specified_correctly <- TRUE

        if ( is.matrix( custom ) ) {
          if ( all( colnames( custom ) %in% c( 'Abbr', 'Label' ) ) ) {
            custom_not_specified_correctly <- FALSE

            abbr_labels.var_cat <- rbind(
              abbr_labels.var_cat,
              custom
            )

          }
        }

        if ( custom_not_specified_correctly ) {
          warning( paste0(
            "Custom abbreviations and labels need to be given as ",
            "a matrix with columns 'Abbr' and 'Label'; ",
            "the command 'column_abbreviations(n=1)' ",
            "will generate a template matrix"
          ) )
        }
      }

      # Extract abbreviation
      cur_abbr <- name_parts[1]
      entries <- abbr_labels.var_cat[,1] %in% cur_abbr

      # If a match is found
      if ( sum(entries) == 1 ) {

        # Return label
        return( abbr_labels.var_cat[entries,2] )

      } else {
        stop( 'Abbreviation not found or has duplicates' )
      }

    }

    # Abbreviations for data types are always the
    # second part of the variable name
    if ( type %in% list_of_types$type ) {

      # Check for custom abbreviations/labels
      if ( !is.null( custom ) ) {

        custom_not_specified_correctly <- TRUE

        if ( is.matrix( custom ) ) {
          if ( all( colnames( custom ) %in% c( 'Abbr', 'Label' ) ) ) {
            custom_not_specified_correctly <- FALSE

            abbr_labels.dat_typ <- rbind(
              abbr_labels.dat_typ,
              custom
            )

          }
        }

        if ( custom_not_specified_correctly ) {
          warning( paste0(
            "Custom abbreviations and labels need to be given as ",
            "a matrix with columns 'Abbr' and 'Label'; ",
            "the command 'column_abbreviations(n=1)' ",
            "will generate a template matrix"
          ) )
        }
      }

      # Extract abbreviation
      cur_abbr <- name_parts[2]
      entries <- abbr_labels.dat_typ[,1] %in% cur_abbr

      # If a match is found
      if ( sum(entries) == 1 ) {

        # Return label
        return( abbr_labels.dat_typ[entries,2] )

      } else {
        stop( 'Abbreviation not found or has duplicates' )
      }

    }

  }

  # Custom abbreviations/labels are always the
  # third part of the variable name
  if ( column_name != '' & is.null( type ) & !is.null( custom ) ) {

    # Split variable name into different parts
    name_parts <- strsplit(
      column_name,
      split = separator,
      fixed = TRUE
    )[[1]]

    custom_not_specified_correctly <- TRUE

    if ( is.matrix( custom ) ) {
      if ( all( colnames( custom ) %in% c( 'Abbr', 'Label' ) ) ) {
        custom_not_specified_correctly <- FALSE

        # Extract abbreviation
        cur_abbr <- name_parts[3]
        entries <- custom[,1] %in% cur_abbr

        # If a match is found
        if ( sum(entries) == 1 ) {

          # Return label
          return( custom[entries,2] )

        } else {
          stop( 'Abbreviation not found or has duplicates' )
        }

      }
    }

    if ( custom_not_specified_correctly ) {
      warning( paste0(
        "Custom abbreviations and labels need to be given as ",
        "a matrix with columns 'Abbr' and 'Label'; ",
        "the command 'column_abbreviations(n=1)' ",
        "will generate a template matrix"
      ) )
    }

  }

}

#### 3) Internal functions ####

#### 1.2) create_summary_for_x ####
# Summaries for Continuous and Categorical Variables
#
# Computes a descriptive statistical summary for
# continuous or categorical variables.
#
# @param 'x_no_missing' A vector of values with
#   missing cases removed.
# @param 'missing_values' A logical vector for
#   all cases marked TRUE when missing.
# @param 'type' The type of summary to compute...
#   'continuous' = The mean, standard deviation,
#     min, 16% quantile, 1st quartile, median,
#     3rd quartile, 84% quantile, and max, along
#     with the total number of cases, and
#     frequency and percent missing.
#   'categorical' = The frequency and percent
#     for each category, along with frequency
#     and percent missing.
#   'range' = The min and max values, along with
#     frequency and percent missing.
# @param 'digits' The number of digits to round to.
#
# @author Kevin Potter
#
# @return A list.

create_summary_for_x <- function( x_no_missing,
                                  missing_values,
                                  type,
                                  digits ) {


  n_obs <- length( missing_values )

  summary_of_x = ''

  if ( type == 'continuous' ) {

    summary_of_x <- list(
      N = sum( !missing_values ),
      Mean = round( mean( x_no_missing ), digits ),
      SD = round( sd( x_no_missing ), digits ),
      Min = round( min( x_no_missing ), digits ),
      Q_16 = round( quantile( x_no_missing, .16 ), digits ),
      Q_25 = round( quantile( x_no_missing, .25 ), digits ),
      Q_50 = round( quantile( x_no_missing, .5 ), digits ),
      Q_75 = round( quantile( x_no_missing, .75 ), digits ),
      Q_84 = round( quantile( x_no_missing, .84 ), digits ),
      Max = round( max( x_no_missing ), digits ),
      Missing = sum( missing_values ),
      digits = digits
    )

  }

  if ( type == 'categorical' ) {

    freq <- table( x_no_missing )

    summary_of_x <- data.frame(
      Category = c( names(freq), 'Missing' ),
      Frequency = c( freq, sum( missing_values ) ),
      Percentage = c( freq, sum( missing_values ) )/n_obs,
      Digits = digits,
      stringsAsFactors = F
    )

  }

  if ( type == 'range' ) {

    summary_of_x <- list(
      N = sum( !missing_values ),
      Min = min( x_no_missing ),
      Max = max( x_no_missing ),
      Missing = sum( missing_values )
    )

  }

  return( summary_of_x )
}

#### 4) Functions for class 'dictionary_meta_data' ####

#### 4.1) new_dictionary_meta_data ####
#' Constructor Function for Dictionary Meta-Data Class
#'
#' A function to create a list with the proper
#' slots for the class \code{dictionary_meta_data}.
#'
#' @param Column_name A standardized column name,
#'   (e.g., 'GGG.TTT.Var_name' or
#'   'GGG.TTT.Optional.Var_name', where 'GGG' is
#'   an abbreviation for the overarching group for a
#'   variable, 'TTT' is an abbreviation for the data type,
#'   and 'Optional' can (if specified) refer to
#'   abbreviations for a sub-category).
#' @param Variable_category The label associated with
#'   the 3-letter abbreviation giving the overarching
#'   variable group to which the column belongs.
#' @param Data_type The label associated with the
#'   3-letter abbreviation for the data type of
#'   the column.
#' @param Sub_category If specified, the label associated
#'   with the optional abbreviation for any sub-categories.
#' @param Description A brief description of what the
#'   variable is.
#' @param REDCap_variables If specified, a character vector
#'   with the variable names from the raw data frame downloaded
#'   from REDCap used to create the current column.
#' @param Values_and_labels If specified, a named list
#'   with two vectors, \code{Values} and \code{Labels},
#'   giving the raw values and the corresponding
#'   labels (e.g., for Likert-scale responses).
#' @param Scale If specified, a list ..
#' @param Subscale If specified, a list...
#' @param Summary If specified, a list with a descriptive
#'   statistical summary of the non-missing values for
#'   the column.
#' @param Units If specified, a character string describing
#'   the units of measurement for the column.
#' @param Codes_for_missing A list of the codes (e.g.,
#'   \code{NA}, \code{''}, etc.) for missing values.
#' @param Groups_collected_over A vector of values
#'   describing the study groups for which the
#'   current variable was collected.
#' @param Times_collected_over A vector of values
#'   describing the study time points during which the
#'   current variable was collected.
#' @param Studies_collected_over A vector of values
#'   describing the studies (e.g., a pilot study,
#'   the active study), for which the current variable
#'   was collected.
#' @param Validated A character string briefly describing
#'   if and how the values in the column were validated
#'   and data checked.
#' @param Notes A character string with any additional
#'   notes.
#'
#' @author Kevin Potter
#'
#' @return A named list of class \code{dictionary_meta_data}.
#'
#' @export

new_dictionary_meta_data <- function( Column_name = '',
                                      Variable_category = '',
                                      Data_type = '',
                                      Sub_category = '',
                                      Description = '',
                                      REDCap_variables = '',
                                      Values_and_labels = '',
                                      Scale = '',
                                      Subscale = '',
                                      Summary = '',
                                      Units = '',
                                      Codes_for_missing = '',
                                      Groups_collected_over = '',
                                      Times_collected_over = '',
                                      Studies_collected_over = '',
                                      Validated = '',
                                      Notes = '' ) {

  # Create names list for new class
  # 'dictionary_meta_data'
  out <- structure(
    list(
      Column_name = Column_name,
      Variable_category = Variable_category,
      Data_type = Data_type,
      Sub_category = Sub_category,
      Description = Description,
      REDCap_variables = REDCap_variables,
      Values_and_labels = Values_and_labels,
      Scale = Scale,
      Subscale = Subscale,
      Summary = Summary,
      Units = Units,
      Codes_for_missing = Codes_for_missing,
      Groups_collected_over = Groups_collected_over,
      Times_collected_over = Times_collected_over,
      Studies_collected_over = Studies_collected_over,
      Validated = Validated,
      Notes = Notes
    ), class = 'dictionary_meta_data'
  )

  return( out )
}

#### 4.2) is.dictionary_meta_data ####
#' Check if a Variable is of Class Dictionary Meta-Data
#'
#' Method to check if an R object is of class
#' \code{dictionary_meta_data}.
#'
#' @param x An R object to be checked.
#'
#' @author Kevin Potter
#'
#' @return A logical value, \code{TRUE} if \code{x}
#' is of class \code{dictionary_meta_data}.
#'
#' @export

is.dictionary_meta_data <- function(x) {
  return( inherits( x, 'dictionary_meta_data' ) )
}

#### 4.3) validate_dictionary_meta_data ####
#' Validate Contents for Dictionary Meta-Data Class Variables
#'
#' A function to check whether the content for
#' R objects of class \code{dictionary_meta_data}
#' have been correctly specified.
#'
#' @param x An R object of class \code{dictionary_meta_data}.
#'
#' @author  Kevin Potter
#'
#' @return If any content is misspecified for \code{x}
#' returns an informative error message.
#'
#' @export

validate_dictionary_meta_data <- function(x) {


  if ( is.dictionary_meta_data(x) ) {

    required_elements <- c(
      "Column_name",
      "Variable_category",
      "Data_type",
      "Description",
      "REDCap_variables",
      "Values_and_labels",
      "Scale",
      "Subscale",
      "Summary",
      "Units",
      "Codes_for_missing",
      "Validated"
    )

    if ( !all( required_elements %in% required_elements ) ) {

      missing_elements <-
        required_elements[ !required_elements %in% required_elements ]

      stop( paste0(
        "Missing the following named elements in the list:\n",
        paste( paste0( "  '", missing_elements, "'\n" ),
               collapse = "" )
      ) )

    }

    ### Check specific elements

    # - Column name
    if ( is.character( x$Column_name ) ) {

      # Error message
      if ( x$Column_name == '' ) {
        stop( "Element 'Column_name' must be specified" )
      }

    } else {
      # Error message
      stop( "Element 'Column_name' should be a character string" )
    }

    # - Variable category
    if ( is.character( x$Variable_category ) ) {
      # Error message
      if ( x$Variable_category == '' ) {
        warning( "Element 'Variable_category' not specified" )
      }
    } else {
      # Error message
      stop( "Element 'Variable_category' should be a character string" )
    }

    # - Data type
    if ( is.character( x$Data_type ) ) {
      # Error message
      if ( x$Data_type == '' ) {
        warning( "Element 'Data_type' not specified" )
      }
    } else {
      # Error message
      stop( "Element 'Data_type' should be a character string" )
    }

    # - Values and labels

    # Check if list
    lst <- x$Values_and_labels
    if ( is.list( lst ) ) {

      nms <- names( lst )

      # Check if list with elements 'Values' and 'Labels'
      if ( !all( nms %in% c( 'Values', 'Labels' ) ) ) {
        # Error message
        stop( paste0(
          "Element 'Values_and_labels' should be list consisting ",
          "of two vectors of matching length, 'Values' and 'Labels'"
        ) )
      }

      # Check if length of 'Values' and 'Labels' match
      if ( any( 'Values' %in% nms ) & any( 'Labels' %in% nms ) ) {

        if ( length( lst$Values ) != length( lst$Labels ) ) {
          # Error message
          stop( paste0(
            "Element 'Values_and_labels' should be list consisting ",
            "of two vectors of matching length, 'Values' and 'Labels'"
          ) )
        }

      }

    } else {
      # Check if empty character set
      if ( lst != '' ) {
        # Error message
        stop( paste0(
          "Element 'Values_and_labels' should be either 1) a list ",
          "consisting of two vectors of matching length, 'Values' ",
          "and 'Labels', or 2) the empty character set ''"
        ) )
      }
    }

  } else {

    stop( "Input must be of class 'dictionary_meta_data'" )

  }

}

#### 4.4) add_dictionary_meta_data ####
#' Add Dictionary Meta-Data to Column Attributes
#'
#' Updates the attributes for a column in a data frame
#' with a standardized list of details on the variable
#' that can be then used to create a data dictionary.
#' Columns are assumed to be named according to a
#' standardized scheme, typically in the style
#' 'GGG.TTT.Var_name' or 'GGG.TTT.Optional.Var_name',
#' where 'GGG' is a 3-letter abbreviation giving the
#' overarching group for a variable
#' (e.g., 'Identifiers', 'Session details', etc.),
#' 'TTT' is a 3-letter abbreviation for the type of data
#' (e.g., 'Integer', 'Character string', etc.),
#' 'Optional' is an optional sub-category (e.g.,
#' indicators for time points like 'BL' for baseline
#' and 'Y1' or 'Y2' or subsequent years), and
#' 'Var_name' is a human-readable label
#' (e.g., 'Treatment_groups', 'Session_dates', etc.).
#'
#' @param dtf A data frame.
#' @param column_name A character string, the name of the
#'   column to add attributes to.
#' @param description A character string, a human-readable
#'   description of the variable.
#' @param variable_category The overarching category to which
#'   a variable belongs (e.g., session details, subject details,
#'   etc.). By default, the function infers the category using
#'   the initial 3-letter abbreviation contained in the column
#'   name.
#' @param data_type The type of data in a column (e.g.,
#'   integer, logical, character string, etc.). By default, the
#'   function infers the data type from the second 3-letter
#'   abbreviation contained in the column name.
#' @param sub_category The label for an optional sub-category
#'   of abbreviation (e.g., time points for wide-form data).
#' @param redcap_variables A character string, the variable
#'   name in the raw REDCap data from which the current
#'   variable was derived.
#' @param values_and_labels An optional named list of two vectors:
#'   \enumerate{
#'     \item Values: A vector of the unique values (e.g., numeric
#'     codes, abbreviations, logical values) stored in the column;
#'     \item Labels: A character vector with the descriptive labels
#'     for each corresponding value.
#'   }
#' @param scale_details An optional list with information for
#'   scale/inventory/questionnaire scores. See the functions
#'   \code{\link{known_scales}} and \code{\link{scale_format}}
#'   on standardized formats for the input.
#' @param subscale_details An optional list with information for
#'   subscale scores. See the functions
#'   \code{\link{known_scales}} and \code{\link{scale_format}}
#'   on standardized formats for the input.
#' @param summary_of_x A character string, either...
#'   \itemize{
#'     \item \code{continuous}: The mean, standard
#'       deviation, min, 16% quantile, 1st quartile,
#'       median, 3rd quartile, 84% quantile, and max,
#'       along with the frequency and percent missing;
#'     \item \code{categorical}: The frequency and percent
#'       for each category, along with frequency and
#'       percent missing.
#'     \item \code{range}: The min and max values, along
#'       with frequency and percent missing.
#'   }
#' @param codes_for_missing A list with the codes used to
#'   indicate missing values (e.g., \code{NA} or \code{''}).
#' @param units_of_x A character string detailing the units
#'   of measurement for the variable (e.g., height in cm,
#'   summed scores, likert-scale response, etc.).
#' @param validated A character string, notes on whether the
#'   variable has been data-checked.
#' @param notes An character string, a free text field for
#'   any additional notes regarding the variable.
#' @param group_var An optional character string giving
#'   the column in \code{dtf} containing the group labels
#'   (e.g., labels for intervention versus control groups).
#' @param time_var An optional character string giving the
#'   the column in \code{dtf} containing the different
#'   time points (e.g., baseline versus follow-up time points).
#' @param study_var An optional character string giving
#'   the column in \code{dtf} containing the study labels
#'   (e.g., labels for pilot versus active studies).
#' @param abbr_labels An optional named list (either
#'   \code{variable_category}, \code{date_type}, or
#'   \code{sub_category}) containing the matrix to
#'   pass to the \code{custom} argument for the
#'   \code{\link{column_abbreviations}} function.
#' @param known_scale An optional argument to pass
#'   along output from the \code{\link{known_scales}}
#'   function, which can be used in lieu of the
#'   \code{scale_details} and \code{subscale_details}
#'   arguments.
#' @param digits Number of digits to round to for summaries.
#'
#' @author Kevin Potter
#'
#' @return An updated data frame where the specified column
#' has with a new attribute of class 'dictionary_meta_data'
#' with the data dictionary information.
#'
#' @export


add_dictionary_meta_data <- function(dtf,
                                     column_name,
                                     description = '',
                                     variable_category = NULL,
                                     data_type = NULL,
                                     sub_category = '',
                                     redcap_variables = '',
                                     values_and_labels = '',
                                     scale_details = '',
                                     subscale_details = '',
                                     summary_of_x = '',
                                     codes_for_missing =
                                       list( NA, '', ' ', as.Date(
                                         '1970-01-01',
                                         format = '%Y-%m-%d' )
                                       ),
                                     units_of_x = '',
                                     validated = '',
                                     notes = '',
                                     group_var = NULL,
                                     time_var = NULL,
                                     study_var = NULL,
                                     abbr_labels = list(),
                                     known_scale = NULL,
                                     digits = 2) {

  # Check if column exists in data frame
  if ( !column_name %in% colnames( dtf ) ) {
    stop( 'Column not found in data frame' )
  }

  # Extract observations
  x <- dtf[[ column_name ]]

  # Determine appropriate codes for missing data and
  # remove missing data in x
  lst <- camrprojects::check_for_missing( x, codes_for_missing )
  x_no_missing <- lst$x_no_missing
  missing_values <- lst$missing_values
  if ( !is.null( lst$codes_for_missing ) ) {
    codes_for_missing <- lst$codes_for_missing
  } else {
    codes_for_missing <- ''
  }

  # Determine overarching category for variable
  if ( is.null( variable_category ) ) {

    variable_category <-
      column_abbreviations(
        column_name,
        type = 'category',
        custom = abbr_labels$variable_category
      )

  }

  # Determine data type
  if ( is.null( data_type ) ) {

    data_type <-
      column_abbreviations(
        column_name,
        type = 'data type',
        custom = abbr_labels$data_type
      )

  }

  if ( is.null( sub_category ) ) {

    if ( !is.null( abbr_labels$sub_category ) ) {
      sub_category <-
        column_abbreviations(
          column_name,
          custom = abbr_labels$sub_category
        )
    } else {
      warning( paste0(
        "Setting 'sub_category' to NULL requires ",
        "passing in to 'abbr_labels' a named list ",
        "with a slot 'sub_category' containing a ",
        "matrix of abbreviations and corresponding ",
        "labels (see 'column_abbreviations')."
      ) )
      sub_category <- ''
    }

  }

  # If applicable, compute summary metrics for variable
  if ( summary_of_x %in% c( 'continuous', 'categorical',
                            'range' ) ) {
    summary_of_x <- create_summary_for_x(
      x_no_missing,
      missing_values,
      type = summary_of_x,
      digits
    )
  }

  # If information for a known scale is provided
  if ( !is.null( known_scale ) ) {
    description <- known_scale$Description
    scale_details <- known_scale$Scale
    subscale_details <- known_scale$Subscale
    units_of_x <- known_scale$Units

    if( is.null( subscale_details ) )
      subscale_details <- ''
  }

  # If specified, indicate which groups, time points,
  # and studies variable has values over

  # Groups
  if ( !is.null( group_var ) ) {
    groups_collected_over =
      unique( dtf[[ group_var ]][ !missing_values ] )
  } else {
    groups_collected_over = ''
  }

  # Time points
  if ( !is.null( time_var ) ) {
    times_collected_over =
      unique( dtf[[ time_var ]][ !missing_values ] )
  } else {
    times_collected_over = ''
  }

  # Studies
  if ( !is.null( study_var ) ) {
    studies_collected_over =
      unique( dtf[[ study_var ]][ !missing_values ] )
  } else {
    studies_collected_over = ''
  }

  # Create object of class 'dictionary_meta_data'
  # with specified inputs
  out <- new_dictionary_meta_data(
    Column_name = column_name,
    Variable_category = variable_category,
    Data_type = data_type,
    Sub_category = sub_category,
    Description = description,
    REDCap_variables = redcap_variables,
    Values_and_labels = values_and_labels,
    Scale = scale_details,
    Subscale = subscale_details,
    Summary = summary_of_x,
    Units = units_of_x,
    Codes_for_missing = codes_for_missing,
    Groups_collected_over = groups_collected_over,
    Times_collected_over = times_collected_over,
    Studies_collected_over = studies_collected_over,
    Validated = validated,
    Notes = notes
  )

  # Check that inputs are correct
  validate_dictionary_meta_data( out )

  # Update attributes for specified column to contain
  # 'dictionary_meta_data' class object
  if ( !is.null( attributes( dtf[[ column_name ]] ) ) ) {

    # Add to existing list of attributes
    attributes( dtf[[ column_name ]] )$dictionary_meta_data <- out

  } else {

    # Create new list of attributes
    attributes( dtf[[ column_name ]] ) <- list(
      dictionary_meta_data = out
    )

  }

  # Return data frame
  return( dtf )
}

#### 4.5) meta ####
#' Extract Dictionary Meta-Data From Attributes
#'
#' A function to extract meta-data intended for
#' data dictionary purposes for a specified column
#' in a data frame. If no column is specified,
#' the function returns a logical vector indicating
#' which columns in the data frame have attributes
#' with the dictionary meta-data and which do not.
#'
#' @param x A data frame.
#' @param column A column name (non-standard evaluation
#'   possible) in \code{x} from which to extract
#'   the attribute of class \code{dictionary_meta_data}.
#'
#' @return Either the extracted list of class
#' \code{dictionary_meta_data} or a logical
#' vector indicating which columns do or
#' do not have dictionary meta-data.
#'
#' @export

meta <- function( x = NULL, column = '' ) {

  if ( is.data.frame( x ) ) {

    V = as.character( substitute( column ) )

    if ( V != '' ) {
      out <- attributes( x[[ V ]] )$dictionary_meta_data
    } else {

      out = sapply( colnames( x ), function(v) {
        res <- FALSE;
        if ( !is.null( attributes( x[[ v ]] ) ) ) {
          if ( any( 'dictionary_meta_data' %in%
                    names( attributes( x[[ v ]] ) ) ) ) {
            res <- TRUE
          }
        }
        return( res )
      } )
      names( out ) = colnames( x )

    }

  } else {

    out <- attributes( x )$dictionary_meta_data

  }

  return( out )
}

#### 4.6) print.dictionary_meta_data ####
#' Display Dictionary Meta-Data Information
#'
#' A print method for the \code{dictionary_meta_data}
#' class, printing to the console key information
#' contained within the dictionary meta-data fields.
#'
#' @param x An R object of class \code{dictionary_meta_data}.
#'
#' @author  Kevin Potter
#'
#' @return The method prints to console, at a minimum, the
#' the variable's overarching group, data type, and
#' associated description. Additional information may include
#' the variable's sub-category, the original REDCap variable
#' used to create the current variable, the unit of
#' measurement, a brief statistical summary when
#' appropriate, and details for scales, inventory, or
#' questionnaire measures.
#'
#' @export

print.dictionary_meta_data <- function(x, digits = 2 ) {

  #### 4.6.1) lines_of_text ####
  # Display Lines of Text in the Console
  #
  # Function to display lines of text via
  # the base function 'message', breaking
  # up longer sentences to be within
  # 50 characters per line.
  #
  # @param 'string' A character string.
  # @param 'spacing' A character string,
  #   the initial start of each line
  #   (typically 5 spaces).
  #
  # @return A vector of character strings.

  lines_of_text <- function( string,
                             spacing = '     ',
                             max_char = 50 ) {

    each_word <- strsplit( string, split = ' ', fixed = TRUE )[[1]]

    n_w <- length( each_word )
    lns <- rep( '', n_w )

    cur_ln <- each_word
    for ( i in 1:n_w ) {

      sel_w <- cumsum( nchar( cur_ln ) ) < max_char

      lns[i] <- paste0(
        spacing,
        paste( cur_ln[ sel_w ], collapse = ' ' )
      )
      if ( any( !sel_w ) ) {
        cur_ln <- cur_ln[ !sel_w ]
      } else {
        break()
      }

    }

    lns <- lns[ lns != '' ]
    lns <- paste0( paste( lns, collapse = '\n' ) )
    message( lns )

  }

  #### 4.6.2) Dictionary meta-data to display ####

  message( x$Column_name )

  message( '  Category:' )
  lines_of_text( x$Variable_category )

  message('  Data type:' )
  lines_of_text( x$Data_type )

  message( '  Description: ' )
  lines_of_text( x$Description )

  if ( x$Sub_category != "" ) {
    message( '  Sub-category:' )
    lines_of_text( x$Sub_category )
  }

  if ( x$REDCap_variables != '' ) {
    msg <- paste( x$REDCap_variables, collapse = ', ' )
    message( '  Original REDCap variable(s):' )
    lines_of_text( msg )
  }

  if ( is.list( x$Summary ) ) {

    lst <- x$Summary

    message( '  Summary:' )

    if ( 'N' %in% names( lst ) )
      message( paste0( '     N = ', lst$N ) )
    if ( 'Missing' %in% names( lst ) )
      message( paste0( '     Missing = ', lst$Missing ) )

    if ( all( c( 'Mean', 'SD' ) %in% names( lst ) ) ) {
      message( paste0( '     Mean = ', round( lst$Mean, digits ) ) )
      message( paste0( '     SD = ', round( lst$SD, digits ) ) )
    }

    if ( all( c( 'Min', 'Max' ) %in% names( lst ) ) ) {

      message( paste0( '     Min = ', round( lst$Min, digits ) ) )

      if ( all( c( 'Q_16', 'Q_25', 'Q_50', 'Q_75', 'Q_84' ) %in%
                names( lst ) ) ) {

        message( paste0( '     16% Q. = ', round( lst$Q_16, digits ) ) )
        message( paste0( '     25% Q. = ', round( lst$Q_25, digits ) ) )
        message( paste0( '     Median = ', round( lst$Q_50, digits ) ) )
        message( paste0( '     75% Q. = ', round( lst$Q_75, digits ) ) )
        message( paste0( '     84% Q. = ', round( lst$Q_84, digits ) ) )

      }

      message( paste0( '     Max = ', round( lst$Max, digits ) ) )
    }

    if ( all( c( 'Category', 'Frequency', 'Percentage' ) %in%
              names( lst ) ) ) {

      p <- round( 100*lst$Percentage, digits )
      nc <- 5 + max( nchar( lst$Category ) ) - nchar( lst$Category )

      val <- sapply( 1:length(nc), function(i)
        paste0( paste( rep( ' ', nc[i] ), collapse = '' ),
                lst$Category[i], ' | ' )
      )

      nc <- 1 + max( nchar( lst$Frequency ) ) - nchar( lst$Frequency )
      val2 <- sapply( 1:length(nc), function(i)
        paste0( paste( rep( ' ', nc[i] ), collapse = '' ),
                lst$Frequency[i], ' | ' )
      )


      nc <- 1 + max( nchar( p ) ) - nchar( p )
      val3 <- sapply( 1:length(nc), function(i)
        paste0( paste( rep( ' ', nc[i] ), collapse = '' ),
                p[i], '% |' )
      )

      message( paste( paste0( val, val2, val3 ), collapse = '\n' ) )

    }

  }

  if ( is.list( x$Scale ) ) {

    message( '  Scale:' )

    msg <- paste0(
      x$Scale$name, ' (',
      x$Scale$abbreviation, ')'
    )
    lines_of_text( msg )

    if ( !is.list( x$Subscale ) ) {

      msg <- paste0(
        x$Scale$n_items, ' items (',
        x$Scale$range[1],
        ' to ',
        x$Scale$range[2], ')'
      )
      lines_of_text( msg )

    } else {

      msg <- paste0(
        'Subscale - ',
        x$Subscale$name
      )
      lines_of_text( msg )

      msg <- paste0(
        x$Subscale$n_items, ' items (',
        x$Subscale$range[1],
        ' to ',
        x$Subscale$range[2], ')'
      )
      lines_of_text( msg )

    }

  }

  if ( x$Units != '' ) {
    message( '  Unit of measurement:' )
    lines_of_text( x$Units )
  }

}

#### 4.7) summary.dictionary_meta_data ####
#' Extract Statistical Summaries From Dictionary Meta-Data
#'
#' Function to extract the numeric summaries (e.g.,
#' mean/standard deviation for continuous variables,
#' frequencies/percentages for categorical variables,
#' etc.) from the dictionary meta-data.
#'
#' @param x A list of class \code{dictionary_meta_data}.
#'
#' @return A list with numeric summaries for
#'   continuous or categorical variables (not
#'   all variables will have summaries).
#'
#' @export

summary.dictionary_meta_data <- function( x ) {

  if ( is.dictionary_meta_data( x ) ) {
    return( x$Summary )
  } else {
    stop( 'Not of class "dictionary_meta_data"' )
  }
}

#### 4.8) subset.dictionary_meta_data ####
#' Extract Components From Dictionary Meta-Data
#'
#' Function to extract a specific element from
#' a list of class \code{dictionary_meta_data}.
#'
#' @param x A list of class \code{dictionary_meta_data}.
#' @param type The type of element to extract. Options
#'   include:
#'   \itemize{
#'     \item \code{'category'}
#'     \item \code{'type'}
#'     \item \code{'sub-category'}
#'     \item \code{'description'}
#'     \item \code{'redcap'}
#'     \item \code{'values and labels'}
#'     \item \code{'scale'}
#'     \item \code{'subscale'}
#'     \item \code{'summary'}
#'     \item \code{'codes for missing'}
#'     \item \code{'units'}
#'   }
#'
#' @return A sub-list, the specified component from the
#' list of class \code{dictionary_meta_data}.
#'
#' @export

subset.dictionary_meta_data <- function( x, type = 'category' ) {

  if ( is.numeric( type ) ) {
    return( x[[ type ]] )
  }

  if ( is.dictionary_meta_data( x ) ) {

    if ( type %in% c( 'Variable category', 'variable category',
                      'Category', 'category' ) ) {
      return( x$Variable_category )
    }

    if ( type %in% c( 'Data type', 'data type',
                      'Type', 'type',
                      'Data', 'data' ) ) {
      return( x$Data_type )
    }

    if ( type %in% c( 'Sub-category', 'sub-category',
                      'Sub category', 'sub category',
                      'Sub', 'sub' ) ) {
      return( x$Sub_category )
    }

    if ( type %in% c( 'Description', 'description',
                      'Desc', 'desc' ) ) {
      return( x$Description )
    }

    if ( type %in% c( 'REDCap variables',
                      'redcap variables',
                      'REDCap', 'redcap',
                      'Original', 'original' ) ) {
      return( x$REDCap_variables )
    }

    if ( type %in% c( 'Values and labels',
                      'values and labels',
                      'Values', 'values',
                      'Labels', 'labels' ) ) {
      return( x$Values_and_labels )
    }

    if ( type %in% c( 'Scale', 'scale',
                      'Inventory', 'inventory',
                      'Questionnaire', 'questionnaire',
                      'Scale details', 'scale details'
    ) ) {
      return( x$Scale )
    }

    if ( type %in% c( 'Subscale', 'subscale',
                      'Subscale details',
                      'subscale details' ) ) {
      return( x$Subscale )
    }

    if ( type %in% c( 'Summary', 'summary',
                      'Range', 'range',
                      'Statistics', 'statistics' ) ) {
      return( x$Summary )
    }

    if ( type %in% c( 'Codes for missing',
                      'codes for missing',
                      'Missing', 'missing',
                      'Codes', 'codes',
                      'Code', 'code' ) ) {
      return( x$Codes_for_missing )
    }

    if ( type %in% c( 'units of measurement',
                      'Units of measurement',
                      'Measurement units',
                      'measurement units',
                      'Units',
                      'units',
                      'Unit',
                      'unit' ) ) {
      return( x$Units )
    }

    stop( 'Incorrect subset type specified' )
  } else {
    stop( "Not of class 'dictionary_meta_data'" )
  }
}

#### 4.9) update_meta_data ####
#' Update Dictionary Meta-Data
#'
#' Function that updates a variable's
#' dictionary meta-data (specifically
#' the numeric summary and codes for
#' missing data). Useful when only
#' a subset of the data will be used.
#'
#' @param dtf A data frame.
#'
#' @return A data frame whose
#' column attributes with dictionary
#' meta-data have been updated.
#'
#' @export

update_dictionary_meta_data <- function( dtf ) {

  # Logical vector of which columns
  # have meta-data
  has_dd_metadata <- meta( dtf )

  # Loop over columns
  K <- ncol( dtf )
  for ( k in 1:K ) {

    if ( has_dd_metadata[k] ) {

      cur_dd_metadata <- meta( dtf[[k]] )

      # Extract values
      x <- dtf[[k]]

      codes_for_missing <- cur_dd_metadata$Codes_for_missing

      # Determine appropriate codes for missing data and
      # remove missing data in x

      lst <- camrprojects::check_for_missing( x, codes_for_missing )

      x_no_missing <- lst$x_no_missing
      missing_values <- lst$missing_values

      # Update codes for missing data
      if ( !is.null( lst$codes_for_missing ) ) {
        codes_for_missing <- lst$codes_for_missing
      } else {
        codes_for_missing <- ''
      }

      cur_dd_metadata$Codes_for_missing <- codes_for_missing

      # If relevant, update summary measures of data
      if ( is.list( cur_dd_metadata$Summary ) ) {

        # If summary of continuous data, will have
        # variable for standard deviation (SD)
        if ( !is.null( cur_dd_metadata$Summary$SD ) ) {

          # Use internal function to update
          # summaries with new values
          sox <- create_summary_for_x(
            x_no_missing,
            missing_values,
            'continuous',
            2
          )
          # Save results
          cur_dd_metadata$Summary <- sox

        }

        # If summary of categorical data, will have
        # variable for frequency
        if ( !is.null( cur_dd_metadata$Summary$Frequency ) ) {

          # Use internal function to update
          # summaries with new values
          sox <- create_summary_for_x(
            x_no_missing,
            missing_values,
            'categorical',
            2
          )
          # Save results
          cur_dd_metadata$Summary <- sox

        }

        # If summary of dates
        if ( cur_dd_metadata$Data_type ==
             'R Date-class variable for calendar dates' ) {


          sox <- create_summary_for_x(
            x_no_missing,
            missing_values,
            'range',
            2
          )

          cur_dd_metadata$Summary <- sox

        }

      }

      # Update attributes with new summaries
      # and codes for missing data
      lst <- attributes( dtf[[k]] )
      lst$dictionary_meta_data <- cur_dd_metadata
      attributes( dtf[[k]] ) <- lst
    }
  }

  return( dtf )
}

#### 4.10) data_frame_from_dictionary_meta_data ####
#' Create Data Frame From Dictionary Meta-Data
#'
#' Function to create a data frame with details
#' from the dictionary meta-data - can then
#' be converted into, for example, a .csv
#' file to easily share a data dictionary
#' with collaborators.
#'
#' @param dtf A data frame with columns whose
#'   attributes include \code{dictionary_meta_data}.
#' @param progress Logical; if \code{TRUE}
#'   tracks progress of extracting meta-data from
#'   columns - useful for debugging purposes.
#'
#' @return A long-form data frame with details
#' (e.g., a variable's category, data type,
#' sub-category, description, units of measurement,
#' codes for missing data, scale/subscale information,
#' and specific notes) for each column with
#' dictionary meta-data.
#'
#' @export

data_frame_from_dictionary_meta_data <- function( dtf,
                                                  progress = FALSE ) {

  column_names <- colnames( dtf )
  NC <- length( column_names )

  vl <- sapply( 1:NC, function(i) {
    lst <- meta( dtf[[i]] );
    length( lst$Values_and_labels[[1]] )
  } )

  content_types <- c(
    "Variable_category",
    "Data_type",
    "Sub_category",
    "Description",
    "Units",
    "Codes_for_missing",
    "Scale",
    "Subscale",
    "Notes",
    rep( "Values_and_labels", max( vl ) )
  )
  CT <- length( content_types )

  out <- data.frame(
    Column_name = rep(
      column_names,
      each = CT
    ),
    Index = rep( 1:CT, NC ),
    Content_type = rep(
      content_types,
      NC
    ),
    Content = '',
    Additional_content = '',
    stringsAsFactors = F
  )
  out$Index_VL <- NA
  out$Index_VL[ out$Content_type == 'Values_and_labels' ] <-
    rep( 1:max( vl ), NC )

  for ( nc in 1:NC ) {

    if ( progress ) {
      message( paste0( '- ', column_names[nc] ) )
    }

    lst <- meta( dtf[[ nc ]] )

    for ( ct in 1:CT ) {

      if ( ct %in% 1:5 ) {

        i <-
          out$Column_name == column_names[nc] &
          out$Index == ct

        out$Content[i] <-
          lst[[ content_types[ct] ]]

      }

      if ( content_types[ct] %in% 'Codes_for_missing' ) {

        if ( is.list( lst$Codes_for_missing ) ) {

          missing_val <- lst$Codes_for_missing

          any_NA <- sapply(
            1:length( missing_val ),
            function(i) is.na( missing_val[[i]] )
          )

          if ( any( any_NA ) ) {
            output <- 'NA'
          } else {
            output <- c()
          }

          if ( any( !any_NA ) ) {
            output <- c(
              output,
              unlist( missing_val[ !any_NA ] )
            )
          }

          i <-
            out$Column_name == column_names[nc] &
            out$Content_type == 'Codes_for_missing'

          out$Content[i] <-
            paste( paste0( '"', output, '"' ), collapse = ' or ' )

        }

      }

      if ( content_types[ct] %in% 'Values_and_labels' ) {

        if ( is.list( lst$Values_and_labels ) ) {

          val <- lst$Values_and_labels$Values
          if ( !is.null( lst$Values_and_labels$Labels ) ) {
            lab <- lst$Values_and_labels$Labels
          } else {
            lab <- rep( "", length( val ) )
          }

          i <-
            out$Column_name == column_names[nc] &
            out$Content_type == 'Values_and_labels' &
            out$Index_VL %in% 1:length( val )

          out$Content[i] <- val
          out$Additional_content[i] <- lab

        }

      }

      if ( content_types[ct] %in% 'Scale' ) {

        i <-
          out$Column_name == column_names[nc] &
          out$Content_type == 'Scale'

        if ( is.list( lst$Scale ) ) {

          out$Content[i] <- paste0(
            lst$Scale$name,
            ' - ',
            lst$Scale$abbreviation
          )

          if ( !is.list( lst$Subscale ) ) {

            out$Additional_content[i] <- paste0(
              lst$Scale$n_items, ' items (',
              lst$Scale$range[1],
              ' to ',
              lst$Scale$range[2], ')'
            )

          }

        }

      }

      if ( content_types[ct] %in% 'Subscale' ) {

        i <-
          out$Column_name == column_names[nc] &
          out$Content_type == 'Subscale'

        if ( is.list( lst$Subscale ) ) {

          out$Content[i] <- lst$Subscale$name

          out$Additional_content[i] <- paste0(
            lst$Subscale$n_items, ' items (',
            lst$Subscale$range[1],
            ' to ',
            lst$Subscale$range[2], ')'
          )

        }

      }

      if ( content_types[ct] %in% 'Notes' ) {

        i <-
          out$Column_name == column_names[nc] &
          out$Content_type == 'Notes'

        out$Content[i] <- lst$Notes

      }

    }

  }

  no_data <- apply(
    out[,c('Content','Additional_content')],
    1, function(x) all(x=="")
  )
  out <- out %>%
    filter( !no_data ) %>%
    select( -Index, -Index_VL )

  return( out )
}
