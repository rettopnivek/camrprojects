# Meta-data functions
# Written by Kevin Potter
# email: kevin.w.potter@gmail.com
# Please email me directly if you
# have any questions or comments
# Last updated 2021-07-21

# Table of contents

### TO DO ###
# - Finish function documentation
# - Alphabetize scales

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
#' @return Either 1) a list with the scale name, number of items,
#' vector with the lowest and largest possible values,
#' abbreviation, a vector of any clinical cut-off values, and
#' a character vector with APA-style references for the scale,
#' or 2) a list with the subscale name, number of items, vector
#' with the lowest and largest possible values, and a vector
#' with any clinical cut-off values.
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
                          subscale = '',
                          description = FALSE,
                          units_of_measurement = FALSE ) {

  if ( is.null( abbreviation ) & subscale == '' ) {

    inputs <- c(
      'Accepted inputs:\n\n',

      'Alcohol Use Disorders Identification Test\n',
      '  abbreviation = "AUDIT"\n\n',

      'Athens Insomnia Scale\n',
      '  abbreviation = "AIS"\n\n',

      'Brief Pain Inventory (Short form)\n',
      '  abbreviation = "BPI"\n',
      '    subscale = Severity\n',
      '    subscale = Interference\n\n',

      'Cannabis Use Disorder Identification Test - Revised\n',
      '  abbreviation = "CUDIT"\n\n',

      'Clinical Global Impression scale\n',
      '  abbreviation = "CGI"\n',
      '    subscale = "Anxiety"\n',
      '    subscale = "Depression"\n\n',

      'Concise Health Risk Tracking scale\n',
      '  abbreviation = "CHRT"\n\n',

      'Hospital Anxiety Depression Scale\n',
      '  abbrevation = "HADS"\n',
      '    subscale = "Anxiety"\n',
      '    subscale = "Depression"\n\n',

      'Marijuana Craving Questionnaire\n',
      '  abbrevation = "MCQ"\n',
      '    subscale = "Compulsitivity"\n',
      '    subscale = "Emotionality"\n',
      '    subscale = "Expectancy"\n',
      '    subscale = "Purposefulness"\n\n',

      'Pain Catastrophizing Scale\n',
      '  abbreviation = "PCS"\n\n',

      'Perceived Stress Scale\n',
      '  abbreviation = "PSS"\n\n',

      'Short Form Health Survey\n',
      '  abbreviation = "SF-12"\n',
      '    subscale = "Physical"\n',
      '    subscale = "Mental"\n\n'
    )
    message( inputs )
  }

  out <- NULL

  #### 1.1.1) HADS ####
  if ( abbreviation %in% c( 'HADS' ) ) {

    ### Overall
    out <- list(
      name = 'Hospital Anxiety Depression Scale',
      n_items = 14,
      range = c( NA, NA ),
      abbreviation = 'HADS',
      cut_off = c( 8 ),
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
      out <- NULL

      ### Anxiety
      if ( subscale %in% c( 'Anxiety', 'anxiety' ) ) {

        if ( description ) {
          out <- paste0(
            'Scores for the HADS anxiety subscale - measure ',
            'of the degree of anxiety in a patient'
          )
        }

        if ( units_of_measurement ) {
          out <- "Summed scores"
        }

        out <- list(
          name = 'Anxiety',
          n_items = 7,
          range = c( 0, 21 ),
          cut_off = c( Borderline = 8, Abnormal = 11 ),
          interpretation = paste0(
            "Higher scores indicate greater anxiety"
          )
        )

      }

      ### Depression
      if ( subscale == 'Depression' ) {


        if ( description ) {
          out <- paste0(
            'Scores for the HADS anxiety subscale - measure ',
            'of the degree of depression experienced by a patient'
          )
        }

        if ( units_of_measurement ) {
          out <- "Summed scores"
        }

        out <- list(
          name = 'Depression',
          n_items = 7,
          range = c( 0, 21 ),
          cut_off = c( Borderline = 8, Abnormal = 11 ),
          interpretation = paste0(
            "Higher scores indicate greater depression"
          )
        )

      }

    }

  }

  #### 1.1.2) CUDIT-R ####
  if ( abbreviation %in% c( 'CUDIT', 'CUDIT-R' ) ) {

    if ( description ) {
      out <- paste0(
        'Scores for the CUDIT-R - measure ',
        'of the degree of problematic cannabis use behavior ',
        'for a subject'
      )
    }

    if ( units_of_measurement ) {
      out <- "Summed scores"
    }

    out <- list(
      name = 'Cannabis Use Disorder Identification Test - Revised',
      n_items = 8,
      range = c( Min = 0, Max = 32 ),
      abbreviation = 'CUDIT-R',
      cut_off = C( Hazardous = 8, Possible_disorder = 12 ),
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

  }

  #### 1.1.3) AUDIT ####
  if ( abbreviation == 'AUDIT' ) {

    if ( description ) {
      out <- paste0(
        'Scores for the AUDIT - measure ',
        'of the degree of problematic alcohol use behavior ',
        'for a subject'
      )
    }

    if ( units_of_measurement ) {
      out <- "Summed scores"
    }

    out <- list(
      name = 'Alcohol Use Disorders Identification Test',
      n_items = 10,
      range = c( Min = 0, Max = 40 ),
      abbreviation = 'AUDIT',
      cut_off = C( Hazardous = 8, Harmful = 16, High_risk = 20 ),
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

  }

  #### 1.1.4) AIS ####
  if ( abbreviation == 'AIS' ) {

    if ( description ) {
      out <- paste0(
        'Scores for the AIS - measure of the degree of insomnia ',
        'experienced by a subject'
      )
    }

    if ( units_of_measurement ) {
      out <- "Summed scores"
    }

    out <- list(
      name = 'Athens Insomnia Scale',
      n_items = 8,
      range = c( Min = 0, Max = 24 ),
      abbreviation = 'AIS',
      cut_off = C( 6 ),
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

  }

  #### 1.1.5) BPI ####
  if ( abbreviation %in% c( 'BPI', 'BPI-SF' ) ) {

    ### Overall
    out <- list(
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
      out <- NULL

      ### Severity
      if ( subscale == 'Severity' ) {

        if ( description ) {
          out <- paste0(
            'Scores for the BPI severity subscale - measure of the ',
            'degree of pain severity experienced by a subject ',
            'within the last 24 hours'
          )
        }

        if ( units_of_measurement ) {
          out <- "Average over items"
        }

        out <- list(
          name = 'Pain severity',
          n_items = 4,
          range = c( Min = 0, Max = 10 ),
          cut_off = NA,
          interpretation = paste0(
            "Higher scores indicate greater pain severity ",
            "in the last 24 hours"
          )
        )

      }

      ### Interference
      if ( subscale == 'Interference' ) {

        if ( description ) {
          out <- paste0(
            'Scores for the BPI interference subscale - measure of ',
            'the extent pain for a participant interfered with daily ',
            'functioning'
          )
        }

        if ( units_of_measurement ) {
          out <- "Average over items"
        }

        out <- list(
          name = 'Pain interference',
          n_items = 7,
          range = c( Min = 0, Max = 10 ),
          cut_off = NA,
          interpretation = paste0(
            "Higher scores indicate greater pain interference ",
            "in daily function"
          )
        )

      }

    }

  }

  #### 1.1.6) PCS ####

  ### Summed scores
  if ( abbreviation == 'PCS' ) {

    if ( description ) {
      out <- paste0(
        "Scores for the PCS - measure of the degree to which ",
        "a subject engages in pain catastrophizing"
      )
    }

    if ( units_of_measurement ) {
      out <- "Summed scores"
    }

    out <- list(
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

  }

  ### Percentages
  if ( abbreviation == 'PCS (%)' ) {

    if ( description ) {
      out <- paste0(
        "Percentages for the PCS - measure of the degree to ",
        "which a subject engages in pain catastrophizing"
      )
    }

    if ( units_of_measurement ) {
      out <- "Percentages"
    }

    out <- list(
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

  }

  #### 1.1.7) PSS ####
  if ( abbreviation == 'PSS' ) {

    if ( description ) {
      out <- paste0(
        "Percentages for the PSS - measure of the degree to ",
        "which subjects view situations in their life ",
        "as stresslful"
      )
    }

    if ( units_of_measurement ) {
      out <- "Summed scores"
    }

    out <- list(
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

  }

  #### 1.1.8) SF-12 ####
  if ( abbreviation == 'SF-12' ) {

    ### Overall
    out <- list(
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
      out <- NULL

      ### Mental
      if ( subscale == 'Mental' ) {

        if ( description ) {
          out <- paste0(
            "Normed scores for the SF-12 mental subscale - ",
            "measure of the degree of general mental health ",
            "of the respondent"
          )
        }

        if ( units_of_measurement ) {
          out <- paste0(
            "Normed score with a population mean of 50 and ",
            "standard deviation of 10"
          )
        }

        out <- list(
          name = 'General mental health',
          n_items = 6,
          range = c( Min = 0, Max = 100 ),
          cut_off = NA,
          interpretation = paste0(
            "Higher scores indicate indicate better degree of ",
            "mental health"
          )
        )

      }

      ### Physical
      if ( subscale == 'Physical' ) {


        if ( description ) {
          out <- paste0(
            "Normed scores for the SF-12 physical subscale - ",
            "measure of the degree of general physical health of ",
            "the respondent"
          )
        }

        if ( units_of_measurement ) {
          out <- paste0(
            "Normed score with a population mean of 50 and ",
            "standard deviation of 10"
          )
        }

        out <- list(
          name = 'General physical health',
          n_items = 6,
          range = c( Min = 0, Max = 100 ),
          cut_off = NA,
          interpretation = paste0(
            "Higher scores indicate better degree of ",
            "physical health"
          )
        )

      }
    }

  }

  #### 1.1.9) MCQ-SF ####
  if ( abbreviation == 'MCQ-sF' ) {

    ### Overall
    out <- list(
      name = 'Marijuana Craving Questionnaire (Short form)',
      n_items = 12,
      range = c( NA, NA ),
      abbreviation = 'MCQ',
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
      out <- NULL

      ### Compulsivity
      if (subscale == 'Compulsivity') {

        if ( description ) {
          out <- paste0(
            'Scores for the MCQ compulsivity subscale - measure ',
            'of the inability to control marijuana use'
          )
        }

        if ( units_of_measurement ) {
          out <- "Summed score"
        }

        out <- list(
          name = 'Compulsivity',
          n_items = 3,
          range = c( Min = 3, Max = 21 ),
          cut_off = NA,
          interpretation = paste0(
            "Higher scores indicate greater difficulty in ",
            "controlling marijuana use"
          )
        )

      }

      ### Emotionality
      if (subscale == 'Emotionality') {

        if ( description ) {
          out <- paste0(
            'Scores for the MCQ emotionality subscale - measure ',
            'of the degree participants use marijuana for relief ',
            'from withdrawal or negative mood'
          )
        }

        if ( units_of_measurement ) {
          out <- "Summed score"
        }

        out <- list(
          name = 'Emotionality',
          n_items = 3,
          range = c( Min = 3, Max = 21 ),
          cut_off = NA,
          interpretation = paste0(
            "Higher scores indicate a greater willingness to ",
            "use marijuana to control withdrawal and negative mood"
          )
        )

      }

      ### Expectancy
      if (subscale == 'Expectancy') {

        if ( description ) {
          out <- paste0(
            'Scores for the MCQ expectancy subscale - measure ',
            'of the degree of anticipation of positive outcomes ',
            'from using marijuana'
          )
        }

        if ( units_of_measurement ) {
          out <- "Summed score"
        }

        out <- list(
          name = 'Expectancy',
          n_items = 3,
          range = c( Min = 3, Max = 21 ),
          cut_off = NA,
          interpretation = paste0(
            "Higher scores indicate higher anticipation of ",
            "postive outcomes from using marijuana"
          )
        )

      }

      ### Purposefulness
      if (subscale == 'Purposefulness') {

        if ( description ) {
          out <- paste0(
            'Scores for the MCQ purposefulness subscale - measure ',
            'of the degree of intention and planning to use ',
            'marijuana for positive outcomes'
          )
        }

        if ( units_of_measurement ) {
          out <- "Summed score"
        }

        out <- list(
          name = 'Purposefulness',
          n_items = 3,
          range = c( Min = 3, Max = 21 ),
          cut_off = NA,
          interpretation = paste0(
            "Higher scores indicate a greater degree of planning ",
            "and intent to use marijuana for postive outcomes"
          )
        )

      }

    }

  }

  #### 1.1.10) CHRT ####
  if ( abbreviation == 'CHRT' ) {

    if ( description ) {
      out <- paste0(
        ""
      )
    }

    if ( units_of_measurement ) {
      out <- "Summed scores"
    }

    out <- list(
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

  }

  #### 1.1.11) CGI ####
  if ( abbreviation == 'CGI' ) {

    ### Overall
    out <- list(
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
      out <- NULL

      ### Severity
      if ( subscale == 'Severity' ) {

        if ( description ) {
          out <- paste0(
            "Rating of illness severity at current time ",
            "based on total clinical experience with particular ",
            "population"
          )
        }

        if ( units_of_measurement ) {
          out <- paste0(
            "Likert scale from 1 = not at all ill to ",
            "7 = among the most extremely ill patients"
          )
        }

        out <- list(
          name = 'Severity of illness',
          n_items = 1,
          range = c( Min = 1, Max = 7 ),
          cut_off = NA,
          interpretation = paste0(
            "Higher scores indicate indicate greater severity ",
            "of illness"
          )
        )

      }

      ### Improvement
      if ( subscale == 'Improvement' ) {


        if ( description ) {
          out <- paste0(
            "Measure of total improvement irrespective of ",
            "drug treatment compared to previous study ",
            "visit for participant"
          )
        }

        if ( units_of_measurement ) {
          out <- paste0(
            "Likert scale from 1 = very much improved to ",
            "7 = very much worse"
          )
        }

        out <- list(
          name = 'Global improvement',
          n_items = 1,
          range = c( Min = 1, Max = 7 ),
          cut_off = NA,
          interpretation = paste0(
            "Higher scores indicate worsening of condition"
          )
        )

      }
    }

  }

  if ( is.null( out ) ) {
    stop( 'Scale or subscale not found' )
  } else {
    return( out )
  }
}

#### 1.2) scale_format ####
#' Standardized Reporting Format for Scales/Inventories/Questionnaires
#'
#' Given a scale's name, number of items, and abbreviation,
#' creates a character string following a standardized
#' reporting format to pass into meta-data for data
#' dictionary purposes. Provides additional options for
#' reporting cut-offs (e.g., clinical cut-offs used for
#' diagnosis purposes) and subscale information.
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


#### 2) meta_data_for_variable ####

meta_data_for_variable <- function(x) {

  meta_data_elements <- c(
    'Column_name',
    'Variable_category',
    'Data_type',
    'Description',
    'REDCap_variables',
    'Values_and_labels',
    'Scale',
    'Subscale',
    'Units',
    'Summary',
    'Codes_for_missing',
    'Groups_collected_over',
    'Times_collected_over',
    'Studies_collected_over',
    'Validated'
  )

  if ( all( meta_data_elements %in% names(x) ) ) {
    class( x ) <- "meta_data_for_variable"
  } else {
    stop( paste0(
      'Input should be a named list with the ',
      'following elements:\n',
      paste( meta_data_elements, collapse = '\n' )
    ) )
  }

  return( x )
}

#### 3) create_meta_data ####
#' Create Meta-data for Data Dictionary Purposes
#'
#' Updates the attributes for a column in a data frame
#' with a standardized list of details on the variable
#' that can be then used to create a data dictionary.
#' Columns are assumed to be named according to a
#' standardized scheme, typically in the style
#' 'AAA.BBB.Description', where 'AAA' is a 3-letter
#' abbreviation giving the overarching category for
#' a variable (e.g., 'Identifiers', 'Session details', etc.),
#' 'BBB' is a 3-letter abbreviation for the type of data
#' (e.g., 'Integer', 'Character string', etc.), and
#' 'Description' is a human-readable label
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
#' @param scale_details ...
#' @param subscale_details ...
#' @param summary_of_x A character string, either...
#'   \itemize{
#'     \item 'continuous': ???;
#'     \item 'categorical' ???;
#'     \item 'range': ???.
#'   }
#' @param category_labels ...
#' @param type_labels ...
#' @param codes_for_missing A list with the codes used to
#'   indicate missing values (e.g., \code{NA} or \code{''}).
#' @param group_var ...
#' @param study_var ...
#' @param units_of_x ...
#' @param validated A character string, notes on whether the
#'   variable has been data-checked.
#' @param digits Number of digits to round to for summaries.
#'
#' @author Kevin Potter
#'
#' @return An updated data frame with new attributes for
#' the specified column.
#'
#' @export

create_meta_data <- function(dtf,
                             column_name,
                             description = '',
                             variable_category = NULL,
                             data_type = NULL,
                             redcap_variables = '',
                             values_and_labels = '',
                             scale_details = NULL,
                             subscale_details = NULL,
                             summary_of_x = '',
                             category_labels = NULL,
                             type_labels = NULL,
                             codes_for_missing = list( NA, '', ' ' ),
                             group_var = NULL,
                             time_var = NULL,
                             study_var = NULL,
                             units_of_x = '',
                             validated = '',
                             digits = 2) {

  # Check if column exists in data frame
  if ( !column_name %in% colnames( dtf ) ) {
    stop( 'Column not found in data frame' )
  }

  # Define functions
  label_for_abbr <- function( column_name,
                              type = 'category',
                              add = NULL,
                              section = NULL,
                              separator = '.' ) {

    if ( grepl( separator, column_name, fixed = TRUE ) ) {
      column_name_parts <- strsplit(
        column_name, split = separator, fixed = TRUE
      )[[1]]
    } else {
      stop( paste0(
        'Column name cannot be broken into separate ',
        'sections - each section must be separated by ',
        separator
      ) )
    }

    if ( type %in% c( '1', 'category', 'variable',
                      'variable category' ) ) {

      tags_labels <- rbind(
        c( 'IDS', 'Identifiers' ),
        c( 'SSS', 'Session details' ),
        c( 'SBJ', 'Subject details' ),
        c( 'STD', 'Student details' ),
        c( 'PNT', 'Patient details' ),
        c( 'DMG', 'Demographic information' ),
        c( 'URN', 'Urine test data' ),
        c( 'INV', 'Inventories and questionnaires' ),
        c( 'QTN', 'Inventories and questionnaires' ),
        c( 'TLF', 'Timeline follow-back data' ),
        c( 'CTB', 'CANTAB battery data' ),
        c( 'DLY', 'Daily diary phone app data' ),
        c( 'MNI', 'MINI diagnoses results' ),
        c( 'CNM', 'Concomitant medication details' ),
        c( 'MRI', 'MRI scan details' ),
        c( 'RMT', 'Remote survey results' ),
        c( 'DTQ', 'Data quality' ),
        c( 'INC', 'Indices for inclusion' )
      )

      section <- 1
    }

    if ( type %in% c( '2', 'data', 'data type', 'type' ) ) {

      tags_labels <- rbind(
        c( 'INT', 'Integer' ),
        c( 'DBL', 'Double precision floating point number' ),
        c( 'LGC', 'Logical' ),
        c( 'CHR', 'Character string' ),
        c( 'DAT', 'R Date-class variable for calendar dates' ),
        c( 'FCT', 'Enumerated type - factor' ),
        c( 'DSC', 'Descriptive text field' )
      )

      section <- 2
    }

    if ( !type %in% c( '0', 'custom' ) ) {

      if ( !is.null( add ) ) {

        tags_labels <- rbind(
          tags_labels,
          add
        )

      }

    } else {
      tags_labels <- add
    }

    if ( is.null( section ) ) {
      stop( paste0(
        "Incorrect input for argument 'type'"
      ) )
    }

    tag <- column_name_parts[ section ]

    n_letter <- nchar( column_name_parts[section] )

    entries <- tag == tags_labels[,1]

    if ( sum( entries ) == 1 ) {
      return( tags_labels[ entries, 2 ] )
    } else {
      stop( paste0(
        n_letter, '-letter abbreviation not found or does ',
        'not correspond to unique label'
      ) )
    }
  }

  create_summary_for_x <- function( x_no_missing,
                                    missing_values,
                                    type,
                                    digits ) {

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
        Missing = sum( missing_values )
      )

    }

    if ( type == 'categorical' ) {

      freq <- table( x_no_missing )

      summary_of_x <- data.frame(
        Category = c( names(freq), 'Missing' ),
        Frequency = c( freq, sum( missing_values ) ),
        Percentage = c( freq, sum( missing_values ) )/n_obs,
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

  # Initialize output
  lst <- list(
    Column_name = column_name,
    Variable_category = '',
    Data_type = '',
    Description = description,
    REDCap_variables = redcap_variables,
    Values_and_labels = values_and_labels,
    Scale = '',
    Subscale = '',
    Summary = '',
    Units = units_of_x,
    Codes_for_missing = '',
    Groups_collected_over = '',
    Times_collected_over = '',
    Studies_collected_over = '',
    Validated = validated
  )

  # Extract observations
  x <- dtf[[ column_name ]]

  # Number of observations
  n_obs <- length( x )

  # Identify missing values
  missing_values <- rep( FALSE, length(x) )
  # Vector to track whether codes for missing
  # values found in variable
  is_missing <- rep( TRUE, length( codes_for_missing ) )

  #< Loop over codes
  for ( k in 1:length( codes_for_missing ) ) {

    #<| Check code for NA
    if ( is.na( codes_for_missing[[k]] ) ) {

      if ( any( is.na(x) ) ) {
        # Update logical vector for missing values
        missing_values[ is.na( x ) ] <- TRUE
      } else {
        # Indicate code not found in variable
        is_missing[k] <- FALSE
      }

      #|> Close 'Check code for NA'
    } else {

      # Only consider non-NA values for missing
      # if not a date variable
      if ( class( x ) != 'Date' ) {
        # Check variable for missing values
        entries <- !is.na(x) & x == codes_for_missing[[k]]
      } else {
        entries <- rep( FALSE, length( x ) )
      }

      if ( any( entries ) ) {
        # Update logical vector for missing values
        missing_values[entries] <- TRUE
      } else {
        # Indicate code not found in variable
        is_missing[k] <- FALSE
      }

      #|> Close else for 'Check code for NA'
    }

    #> Close 'Loop over codes'
  }

  if ( !any( is_missing ) ) {
    # No missing values found
    x_no_missing <- x
    lst$Codes_for_missing <- ''
  } else {
    # Remove missing values
    x_no_missing <- x[ !missing_values ]
    # Include only codes actually found in variable
    lst$Codes_for_missing <- codes_for_missing[ is_missing ]
  }

  # Determine overarching category for variable
  if ( is.null( variable_category ) ) {

    lst$Variable_category <-
      label_for_abbr(
        column_name,
        type = 'category',
        add = category_labels
      )

  } else {
    lst$Variable_category <- variable_category
  }

  # Determine data type
  if ( is.null( data_type ) ) {

    lst$Data_type <-
      label_for_abbr(
        column_name,
        type = 'data type',
        add = type_labels
      )

  } else {
    lst$Data_type <- data_type
  }

  lst$Summary <- create_summary_for_x(
    x_no_missing,
    missing_values,
    type = summary_of_x,
    digits
  )

  # Details for inventory/questionnaires
  if ( !is.null( scale_details ) ) {

    lst$Scale <- c(
      Name = scale_details[1],
      N_items = scale_details[2],
      Abbreviation = scale_details[3],
      Cut_off = scale_details[4]
    )

  } else {
    lst$Scale = ''
  }

  # Details for a subscale of inventory/questionnaires
  if ( !is.null( subscale_details ) ) {

    lst$Subscale <- c(
      Name = subscale_details[1],
      N_items = subscale_details[2]
    )

  } else {
    lst$Subscale = ''
  }

  if ( !is.null( group_var ) ) {
    lst$Groups_collected_over =
      unique( dtf[[ group_var ]][ !missing_values ] )
  } else {
    lst$Groups_collected_over = ''
  }

  if ( !is.null( time_var ) ) {
    lst$Times_collected_over =
      unique( dtf[[ time_var ]][ !missing_values ] )
  } else {
    lst$Times_collected_over = ''
  }

  if ( !is.null( study_var ) ) {
    lst$Studies_collected_over =
      unique( dtf[[ study_var ]][ !missing_values ] )
  } else {
    lst$Studies_collected_over = ''
  }

  lst <- meta_data_for_variable( lst )

  if ( !is.null( attributes( dtf[[ column_name ]] ) ) ) {

    attributes( dtf[[ column_name ]] )$meta_data_for_variable <- lst

  } else {

    attributes( dtf[[ column_name ]] ) <- list(
      meta_data_for_variable = lst
    )

  }

  return( dtf )
}


#### 4) Methods ####

#### 4.1) meta ####
#' Title
#'
#' Description.
#'
#' @param x ...
#'
#' @details
#'
#' @return Output.
#'
#' @examples
#' # Examples
#'
#' @export

meta <- function( x = NULL, column = '' ) {

  if ( is.data.frame( x ) ) {

    V = as.character( substitute( column ) )

    if ( V != '' ) {
      out <- attributes( x[[ V ]] )$meta_data_for_variable
    } else {

      out = sapply( colnames( x ), function(v) {
        res <- FALSE;
        if ( !is.null( attributes( x[[ v ]] ) ) ) {
          if ( any( 'meta_data_for_variable' %in%
                    names( attributes( x[[ v ]] ) ) ) ) {
            res <- TRUE
          }
        }
        return( res )
      } )
      names( out ) = colnames( x )

    }

  } else {

    out <- attributes( x )$meta_data_for_variable

  }

  return( out )
}

#### 4.2) is.meta_data_for_variable ####
#' Title
#'
#' Description.
#'
#' @param x ...
#'
#' @details
#'
#' @return Output.
#'
#' @examples
#' # Examples
#'
#' @export

is.meta_data_for_variable <- function(x) {
  inherits( x, 'meta_data_for_variable' )
}

#### 4.3) print.meta_data_for_variable ####
#' Title
#'
#' Description.
#'
#' @param x ...
#'
#' @details
#'
#' @return Output.
#'
#' @examples
#' # Examples
#'
#' @export

print.meta_data_for_variable <- function(x, digits = 2 ) {

  message( x$Column_name )
  message( '  Category:' )
  message( paste0( '     ', x$Variable_category ) )
  message('  Data type:' )
  message( paste0( '     ', x$Data_type ) )
  message( '  Description: ' )

  dsc <- x$Description
  each_word <- strsplit( dsc, split = ' ', fixed = TRUE )[[1]]

  n_w <- length( each_word )
  lns <- rep( '', n_w )

  cur_ln <- each_word
  for ( i in 1:n_w ) {

    sel_w <- cumsum( nchar( cur_ln ) ) < 50

    lns[i] <- paste0(
      '     ',
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

  if ( x$REDCap_variables != '' ) {
    message( '  Original REDCap variable:' )
    message( paste0( '     ', x$REDCap_variables ) )
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

  if ( x$Units != '' ) {
    message( '  Unit of measurement:' )
    message( paste0( '     ', x$Units ) )
  }

}

#### 4.4) summary.meta_data_for_variable ####
#' Title
#'
#' Description.
#'
#' @param x ...
#'
#' @details
#'
#' @return Output.
#'
#' @examples
#' # Examples
#'
#' @export

summary.meta_data_for_variable <- function( x ) {

  if ( is.meta_data_for_variable( x ) ) {
    return( x$Summary )
  } else {
    stop( 'Not of class "meta_data_for_variable"' )
  }
}

#### 4.5) subset.meta_data_for_variable ####
#' Title
#'
#' Description.
#'
#' @param x ...
#'
#' @details
#'
#' @return Output.
#'
#' @examples
#' # Examples
#'
#' @export

subset.meta_data_for_variable <- function( x, type = 'category' ) {

  if ( is.meta_data_for_variable( x ) ) {

    if ( type %in% c( 'Variable category', 'variable category',
                      'Category', 'category',
                      '2' ) ) {
      return( x$Variable_category )
    }

    if ( type %in% c( 'Data type', 'data type',
                      'Type', 'type',
                      'Data', 'data',
                      '3' ) ) {
      return( x$Data_type )
    }

    if ( type %in% c( 'Description', 'description',
                      'Desc', 'desc',
                      '4' ) ) {
      return( x$Description )
    }

    if ( type %in% c( 'REDCap', 'redcap',
                      'Original', 'original',
                      '5' ) ) {
      return( x$REDCap_variables )
    }

    if ( type %in% c( 'Values', 'values',
                      'Labels', 'labels',
                      '6' ) ) {
      return( x$Values_and_labels )
    }

    if ( type %in% c( 'Scale', 'scale',
                      'Inventory', 'inventory',
                      'Questionnaire', 'questionnaire',
                      '7' ) ) {
      return( x$Scale )
    }

    if ( type %in% c( 'Subscale', 'subscale',
                      '8' ) ) {
      return( x$Subscale )
    }

    if ( type %in% c( 'Summary', 'summary',
                      'Range', 'range',
                      'Statistics', 'statistics',
                      '8' ) ) {
      return( x$Summary )
    }

    if ( type %in% c( 'Missing', 'missing',
                      'Codes', 'codes',
                      'Code', 'code',
                      '11' ) ) {
      return( x$Codes_for_missing )
    }

    stop( 'Incorrect subset type specified' )
  } else {
    stop( 'Not of class "meta_data_for_variable"' )
  }
}

#### 10) transfer_meta ####

# transfer_meta <- function() {}

#### 11) update_meta_data ####

# update_meta_data <- function() {}

#### 12) create_data_dictionary ####

# create_data_dictionary <- function() {}

#### 13) meta_data_template ####





