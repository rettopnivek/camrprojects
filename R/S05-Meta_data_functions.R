# Dictionary meta-data functions
# Written by Kevin Potter
# email: kevin.w.potter@gmail.com
# Please email me directly if you
# have any questions or comments
# Last updated 2021-08-05

# Table of contents

### TO DO ###
# - Finish function documentation
# - Alphabetize scales
# - Add more scales to 'known_scales'

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
#' abbreviation, a vector of any clinical cut-off values,
#' a character vector with APA-style references for the scale,
#' and a brief description of how to interpret higher scores
#' on the scale, or 2) a list with the subscale name, number of
#' items, vector with the lowest and largest possible values,
#' a vector with any clinical cut-off values, and a brief
#' description of how to interpret higher scores.
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

      'Marijuana Craving Questionnaire (Short form)\n',
      '  abbrevation = "MCQ-SF"\n',
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

    # End function call
    return( invisible( NULL ) )
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
          return( out )
        }

        if ( units_of_measurement ) {
          out <- "Summed scores"
          return( out )
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
          return( out )
        }

        if ( units_of_measurement ) {
          out <- "Summed scores"
          return( out )
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
      return( out )
    }

    if ( units_of_measurement ) {
      out <- "Summed scores"
      return( out )
    }

    out <- list(
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

  }

  #### 1.1.3) AUDIT ####
  if ( abbreviation == 'AUDIT' ) {

    if ( description ) {
      out <- paste0(
        'Scores for the AUDIT - measure ',
        'of the degree of problematic alcohol use behavior ',
        'for a subject'
      )
      return( out )
    }

    if ( units_of_measurement ) {
      out <- "Summed scores"
      return( out )
    }

    out <- list(
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

  }

  #### 1.1.4) AIS ####
  if ( abbreviation == 'AIS' ) {

    if ( description ) {
      out <- paste0(
        'Scores for the AIS - measure of the degree of insomnia ',
        'experienced by a subject'
      )
      return( out )
    }

    if ( units_of_measurement ) {
      out <- "Summed scores"
      return( out )
    }

    out <- list(
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
          return( out )
        }

        if ( units_of_measurement ) {
          out <- "Average over items"
          return( out )
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
          return( out )
        }

        if ( units_of_measurement ) {
          out <- "Average over items"
          return( out )
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
      return( out )
    }

    if ( units_of_measurement ) {
      out <- "Summed scores"
      return( out )
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
      return( out )
    }

    if ( units_of_measurement ) {
      out <- "Percentages"
      return( out )
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
      return( out )
    }

    if ( units_of_measurement ) {
      out <- "Summed scores"
      return( out )
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
          return( out )
        }

        if ( units_of_measurement ) {
          out <- paste0(
            "Normed score with a population mean of 50 and ",
            "standard deviation of 10"
          )
          return( out )
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
          return( out )
        }

        if ( units_of_measurement ) {
          out <- paste0(
            "Normed score with a population mean of 50 and ",
            "standard deviation of 10"
          )
          return( out )
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
  if ( abbreviation == 'MCQ-SF' ) {

    ### Overall
    out <- list(
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
      out <- NULL

      ### Compulsivity
      if (subscale == 'Compulsivity') {

        if ( description ) {
          out <- paste0(
            'Scores for the MCQ compulsivity subscale - measure ',
            'of the inability to control marijuana use'
          )
          return( out )
        }

        if ( units_of_measurement ) {
          out <- "Summed score"
          return( out )
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
          return( out )
        }

        if ( units_of_measurement ) {
          out <- "Summed score"
          return( out )
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
          return( out )
        }

        if ( units_of_measurement ) {
          out <- "Summed score"
          return( out )
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
          return( out )
        }

        if ( units_of_measurement ) {
          out <- "Summed score"
          return( out )
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
        "Measure of risk based on suicidal ideation and intent"
      )
      return( out )
    }

    if ( units_of_measurement ) {
      out <- "Summed scores"
      return( out )
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
          return( out )
        }

        if ( units_of_measurement ) {
          out <- paste0(
            "Likert scale from 1 = not at all ill to ",
            "7 = among the most extremely ill patients"
          )
          return( out )
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
          return( out )
        }

        if ( units_of_measurement ) {
          out <- paste0(
            "Likert scale from 1 = very much improved to ",
            "7 = very much worse"
          )
          return( out )
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


#### 2) variable_name_abbreviations ####
#' Title
#'
#' Description.
#'
#' @param var_name ...
#' @param type ...
#' @param custom ...
#' @param n ...
#' @param separator ...
#'
#' @details
#'
#' @return Output.
#'
#' @examples
#' # Examples
#'
#' @export

variable_name_abbreviations <- function( var_name = '',
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
  if ( var_name == '' & is.null( type ) & !is.na( n ) ) {

    out <- cbind(
      Abbr = rep( 'XXX', n ),
      Label = rep( 'Description of XXX', n )
    )
    return( out )

  }

  # Display pre-defined abbreviations and labels
  if ( var_name == '' & !is.null( type ) ) {

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
  if ( var_name != '' & !is.null( type ) ) {

    # Split variable name into different parts
    name_parts <- strsplit(
      var_name,
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
            "the command 'variable_name_abbreviations(n=1)' ",
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
            "the command 'variable_name_abbreviations(n=1)' ",
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
  if ( var_name != '' & is.null( type ) & !is.null( custom ) ) {

    # Split variable name into different parts
    name_parts <- strsplit(
      var_name,
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
        "the command 'variable_name_abbreviations(n=1)' ",
        "will generate a template matrix"
      ) )
    }

  }

}

#### 3) Internal functions


#### 1.2) create_summary_for_x ####
# Title
#
# Description.
#
# @param 'x_no_missing' ...
# @param 'missing_values' ...
# @param 'type' ...
# @param 'digits' ...
#
# @return Output.
#
# @examples
# # Examples

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

#### 4) Functions for class 'dictionary_meta_data' ####

#### 4.1) new_dictionary_meta_data ####
#' Constructor Function for Dictionary Meta-data Class
#'
#' Description.
#'
#' @param Column_name ...
#' @param Variable_category ...
#' @param Data_type ...
#' @param Description ...
#' @param REDCap_variables ...
#' @param Values_and_labels ...
#' @param Scale ...
#' @param Subscale ...
#' @param Summary ...
#' @param Units ...
#' @param Codes_for_missing ...
#' @param Groups_collected_over ...
#' @param Times_collected_over ...
#' @param Studies_collected_over ...
#' @param Validated ...
#' @param Notes ...
#'
#' @details
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
#' Check if a Variable is of Class Dictionary Meta-data
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

is.dictionary_meta_data <- function(x) {
  return( inherits( x, 'dictionary_meta_data' ) )
}

#### 4.3) validate_dictionary_meta_data ####
#' Validate Contents for Dictionary Meta-data Class Variables
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
#' Add Dictionary Meta-data to Column Attributes
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
#'     \item 'continuous': ???;
#'     \item 'categorical' ???;
#'     \item 'range': ???.
#'   }
#' @param category_labels ...
#' @param type_labels ...
#' @param codes_for_missing A list with the codes used to
#'   indicate missing values (e.g., \code{NA} or \code{''}).
#' @param units_of_x A character string detailing the units
#'   of measurement for the variable (e.g., height in cm,
#'   summed scores, likert-scale response, etc.).
#' @param validated A character string, notes on whether the
#'   variable has been data-checked.
#' @param notes ...
#' @param group_var ...
#' @param time_var ...
#' @param study_var ...
#' @param abbr_labels ...
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
      variable_name_abbreviations(
        column_name,
        type = 'category',
        custom = abbr_labels$variable_category
      )

  }

  # Determine data type
  if ( is.null( data_type ) ) {

    data_type <-
      variable_name_abbreviations(
        column_name,
        type = 'data type',
        custom = abbr_labels$data_type
      )

  }

  if ( is.null( sub_category ) ) {

    if ( !is.null( abbr_labels$sub_category ) ) {
      data_type <-
        variable_name_abbreviations(
          column_name,
          custom = abbr_labels$sub_category
        )
    } else {
      warning( paste0(
        "Setting 'sub_category' to NULL requires ",
        "passing in to 'abbr_labels' a named list ",
        "with a slot 'sub_category' containing a ",
        "matrix of abbreviations and corresponding ",
        "labels (see 'variable_name_abbreviations')."
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

  # Details for inventory/questionnaires
  if ( is.null( scale_details ) ) {
    scale_details <- ''
  }

  # Details for a subscale of inventory/questionnaires
  if ( is.null( subscale_details ) ) {
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
#' Extract Dictionary Meta-data From Attributes
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
#' @return Either the extracted list with dictionary
#' meta-data of class \code{dictionary_meta_data} or
#' a logical vector indicating which columns do or
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

print.dictionary_meta_data <- function(x, digits = 2 ) {

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

  if ( is.list( x$Scale ) ) {

    message( '  Scale:' )

    msg <- paste0(
      x$Scale$Name, ' (',
      x$Scale$Abbreviation, ')'
    )

  }

  if ( x$Units != '' ) {
    message( '  Unit of measurement:' )
    message( paste0( '     ', x$Units ) )
  }

}

#### 4.7) summary.dictionary_meta_data ####
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

summary.dictionary_meta_data <- function( x ) {

  if ( is.dictionary_meta_data( x ) ) {
    return( x$Summary )
  } else {
    stop( 'Not of class "dictionary_meta_data"' )
  }
}

#### 4.8) subset.dictionary_meta_data ####
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
                      'values and lables',
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
                      'Statistics', 'statistics',
                      '8' ) ) {
      return( x$Summary )
    }

    if ( type %in% c( 'Codes for missing',
                      'codes for missing',
                      'Missing', 'missing',
                      'Codes', 'codes',
                      'Code', 'code' ) ) {
      return( x$Codes_for_missing )
    }

    stop( 'Incorrect subset type specified' )
  } else {
    stop( "Not of class 'dictionary_meta_data'" )
  }
}

#### 10) transfer_meta ####

# transfer_meta <- function() {}

#### 11) update_meta_data ####

# update_meta_data <- function() {}

#### 12) create_data_dictionary ####

# create_data_dictionary <- function() {}

#### 13) meta_data_template ####





