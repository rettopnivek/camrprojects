# Inventories, scales, and questionnaires
# Written by...
#   Megan Cooke
#   Kevin Potter
#   Jakob Weiss
# Maintained by...
#   Kevin Potter
# Email:
#   kpotter5@mgh.harvard.edu
# Please email us directly if you
# have any questions or comments
# Last updated: 2023-03-02

# Table of contents
# 1) camr_inventories
#   1.0) Templates for adding measures
#   1.1) APSS
#   1.2) ASRS
#   1.3) AUDIT
#   1.4) AUQ
#   1.5) AIS
#   1.6) BIS-11
#   1.7) BRIEF CHECK!!!
#   1.8) BRIEF2 CHECK!!!
#   1.9) BPI
#   1.10) BPAQ
#   1.11) CUDIT-R
#   1.12) CWS
#   1.13) CGI CHECK!!!
#   1.14) CHRT
#   1.15) DOSPERT
#   1.16) DMQ
#   1.17) ERS
#   1.18) GLTEQ
#   1.19) HADS
#   1.20) KSADS CHECK!!!
#   1.21) MCQ-SF
#   1.22) MEEQ CHECK!!!
#   1.23) MMM
#   1.24) MPS
#   1.25) MCQ
#   1.26) MASQ CHECK!!!
#   1.27) MISS
#   1.28) PCS
#   1.29) PSS
#   1.30) PDI
#   1.31) PSQI
#   1.32) PQB
#   1.33) PERS
#   1.34) SCARED
#   1.35) SF-12
#   1.36) TIPI
#   1.37) CES-D
#   1.38) UPPS-P
#   1.39) WTAR
#   1.40) WHODAS 2.0
#   1.41) Y-PSC
# 2) camr_add_inventory_item

#### 1) camr_inventories ####
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
#' @author Megan Cooke, Jacob Weiss, Kevin Potter
#'
#' @return A list consisting of...
#' \itemize{
#'   \item `Description` A brief description of the
#'     scale/subscale and its purpose
#'   \item `Scale` A list with the scale name, abbreviation,
#'     number of items, lowest to highest possible score (if
#'     applicable), clinical cut-offs (if applicable), and
#'     APA-styled references for the scale development/interpretation;
#'   \item `Subscale` A list with the subscale name,
#'     number of items, lowest to highest possible score,
#'     clinical cut-offs (if applicable).
#'   \item `Units` The units of measurement.
#' }
#'
#' @examples
#' # List all possible inputs (i.e., known
#' # scales, inventories, and questionnaires)
#' camr_inventories()
#'
#' # Example input
#' camr_inventories( 'HADS' )
#' camr_inventories( 'HADS', subscale = 'Anxiety' )
#'
#' @export

camr_inventories <- function(
    abbreviation = NULL,
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

      'Behavior Rating Inventory of Executive Function 2nd Edition\n',
      '  abbreviation = "BRIEF2"\n',
      '    subscale = "Inhibit (Raw)"\n',
      '    subscale = "Inhibit (Percentile)"\n',
      '    subscale = "Inhibit (T-score)"\n',
      '    subscale = "Self-Monitor (Raw)"\n',
      '    subscale = "Self-Monitor (Percentile)"\n',
      '    subscale = "Self-Monitor (T-score)"\n',
      '    subscale = "Shift (Raw)"\n',
      '    subscale = "Shift (Percentile)"\n',
      '    subscale = "Shift (T-score)"\n',
      '    subscale = "Emotional Control (Raw)"\n',
      '    subscale = "Emotional Control (Percentile)"\n',
      '    subscale = "Emotional Control (T-score)"\n',
      '    subscale = "Task Completion (Raw)"\n',
      '    subscale = "Task Completion (Percentile)"\n',
      '    subscale = "Task Completion (T-score)"\n',
      '    subscale = "Working Memory (Raw)"\n',
      '    subscale = "Working Memory (Percentile)"\n',
      '    subscale = "Working Memory (T-score)"\n',
      '    subscale = "Plan|Organize (Raw)"\n',
      '    subscale = "Plan|Organize (Percentile)"\n',
      '    subscale = "Plan|Organize (T-score)"\n',
      '    subscale = "Behavioral Regulation (Raw)"\n',
      '    subscale = "Behavioral Regulation (Percentile)"\n',
      '    subscale = "Behavioral Regulation (T-score)"\n',
      '    subscale = "Emotion Regulation (Raw)"\n',
      '    subscale = "Emotion Regulation (Percentile)"\n',
      '    subscale = "Emotion Regulation (T-score)"\n',
      '    subscale = "Cognitive Regulation (Raw)"\n',
      '    subscale = "Cognitive Regulation (Percentile)"\n',
      '    subscale = "Cognitive Regulation (T-score)"\n',
      '    subscale = "General Executive (Raw)"\n',
      '    subscale = "General Executive (Percentile)"\n',
      '    subscale = "General Executive (T-score)"\n\n',

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

      'Emotion Reactivity Scale\n',
      '  abbreviation = "ERS"\n',
      '    subscale = "Sensitivity"\n',
      '    subscale = "Intensity/arousal"\n',
      '    subscale = "Persistence"\n\n',

      'Godin Leisure-Time Exercise Questionnaire\n',
      '  abbreviation = "GLTEQ"\n\n',

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

      'Marijuana Problem Scale\n',
      '  abbrevation = "MPS"\n\n',

      'Monetary Choice Questionnaire\n',
      '  abbrevation = "MCQ"\n\n',

      'Mood and Anxiety Symptom Questionnaire\n',
      '  abbreviation = "MASQ"\n',
      '    subscale = "General distress anxious symptoms"\n',
      '    subscale = "Anxious arousal"\n',
      '    subscale = "General distress depressive symptoms"\n',
      '    subscale = "Anhedonic depression"\n\n',

      'Multidimensional Iowa Suggestibility Scale\n',
      '  abbreviation = "MISS"\n',
      '    subscale = "Consumer Suggestibility"\n',
      '    subscale = "Persuadability"\n',
      '    subscale = "Physiological Suggestibility"\n',
      '    subscale = "Physiological Reactivity"\n',
      '    subscale = "Peer Conformity"\n',
      '    subscale = "Short Suggestibility Scale"\n',
      '    subscale = "Mental Control"\n',
      '    subscale = "Unpersuadability"\n\n',

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

      'Pittsburgh Sleep Quality Index\n',
      '  abbreviation = "PSQI"\n',
      '    subscale = "Component 1"\n',
      '    subscale = "Component 2"\n',
      '    subscale = "Component 3"\n',
      '    subscale = "Component 4"\n',
      '    subscale = "Component 5"\n',
      '    subscale = "Component 6"\n',
      '    subscale = "Component 7"\n\n',

      'Prodromal Questionnaire-Brief\n',
      '  abbreviation = "PQB"\n',
      '    subscale = "Distress"\n\n',

      'Provider Expectations for Recovery Scale\n',
      '  abbreviation = "PERS"\n\n',

      'Screen for Child Anxiety Related Emotional Disorders\n',
      '  abbreviation = "SCARED"\n',
      '    subscale = "Panic Disorder"\n',
      '    subscale = "Separation Anxiety Disorder"\n',
      '    subscale = "Generalized Anxiety Disorder"\n',
      '    subscale = "Social Phobia"\n',
      '    subscale = "Specific Phobias"\n',
      '    subscale = "Obsessive Compulsive Disorder"\n',
      '    subscale = "Traumatic Stress Disorder"\n\n',

      'Short Form Health Survey\n',
      '  abbreviation = "SF-12"\n',
      '    subscale = "Physical"\n',
      '    subscale = "Mental"\n\n',

      'Ten Item Personality Inventory\n',
      '  abbreviation = "TIPI"\n',
      '    subscale = "Extraversion"\n',
      '    subscale = "Agreeableness"\n',
      '    subscale = "Conscientiousness"\n',
      '    subscale = "Emotional Stability"\n',
      '    subscale = "Openness"\n\n',

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

  # Initialize output
  out <- list(
    Description = NULL,
    Scale = NULL,
    Subscale = NULL,
    Units = NULL
  )

  #### 1.0) Templates for adding measures ####

  # Replace content in square brackets []

  # Template for single inventory/scale/questionnaire

  # 1.[Index number]) [Abbreviation]
  # if ( abbreviation == '[Abbreviation]' ) {
  #
  #   out$Description <- paste0(
  #     'Scores for the [Abbreviation] - measure of ',
  #     '[description of measure]'
  #   )
  #
  #   out$Units <- "[Typically either Summed score, Mean score, or T-score]"
  #
  #   out$Scale <- list(
  #     name = '[Full name of measure]',
  #     n_items = [Total number of items for full measure],
  #     range = c( Min = [Lowest score], Max = [Highest score] ),
  #     abbreviation = '[Abbreviation]',
  #     cut_off = c( `[Description]` = [Value] ),
  #     reference = c(
  #       # Add references as separate character strings
  #       paste0(
  #       '[Last name], [First letter of first name], ..., ',
  #       '& [Last name], [First letter of first name] ([Year]).
  #       [Article title]. [Journal title - no abbreviations],
  #       [Journal volume] ([Journal issue]), [1st page]-[Last page].'
  #       )
  #     ),
  #     reference_identifier = c(
  #       DOI = '[DOI number]',
  #       PMID = '[PMID number]',
  #       URL = '[URL link]
  #     ),
  #     interpretation = paste0(
  #       "Higher scores indicate [insert interpretation]"
  #     ),
  #     values_and_labels = c(
  #       content = [Numeric vector with responses],
  #       additional_content = [Character vector with labels for responses]
  #     )
  #   )
  #
  #   # Close '[Abbreviation]'
  # }

  # Template for inventory/scale/questionnaire with subscales

  #### 1.1) APSS ####
  if ( abbreviation == 'APSS' ) {

    out$Description <- paste0(
      'Scores for the APSS - measure of the degree of risk for ',
      'psychotic-like experiences for a subject'
    )

    out$Units <- 'Summed score'

    out$Scale <- list(
      name = 'Adolescent Psychotic Symptom Screener',
      n_items = 7,
      range = c( Min = 0, Max = 7 ),
      abbreviation = 'APSS',
      cut_off = c( `At risk` = 2 ),
      reference = paste0(
        'Kelleher, I., Harley, M., Murtagh, A., & Cannon, M. (2011).',
        ' Are screening instruments valid for psychotic-like ',
        'experiences? A validation study of screening questions ',
        'for psychotic-like experiences using in-depth clinical ',
        'interview. Schizophrenia Bulletin, 37 (2), 362–369.',
        'https://doi.org/10.1093/schbul/sbp057'
      ),
      reference_identifier = c(
        DOI = '10.1093/schbul/sbp057'
      ),
      interpretation = paste0(
        'Higher scores indicate greater risk for psychotic-like experiences'
      ),
      values_and_labels = list(
        content = c( 0, 0.5, 1 ),
        additional_content = c(
          'No - never',
          'Maybe',
          'Yes - definitely'
        )
      )
    )

    # Close 'APSS'
  }

  #### 1.2) ASRS ####
  if ( abbreviation == 'ASRS' ) {

    out$Description <- paste0(
      'Scores for the ASRS - measure of symptoms ',
      'suggesting attention-deficit hyperactivity disorder'
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
      reference_identifier = c(
        DOI = '10.1017/S0033291704002892'
      ),
      interpretation = paste0(
        'Higher scores indicate more severe ADHD-like symptoms'
      ),
      values_and_labels = list(
        content = c( 0, 1, 2, 3, 4 ),
        additional_content = c(
          'Never',
          'Rarely',
          'Sometimes',
          'Often',
          'Very often'
        )
      )
    )

    # Close 'ASRS'
  }

  #### 1.3) AUDIT ####
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
      cut_off = c( Hazardous = 8, Harmful = 16, `High risk` = 20 ),
      reference = paste0(
        'Allen, J. P., Reinert, D. F., Volk, R. J. ',
        '(2001). The alcohol use disorders identification test: ',
        'An aid to recognition of alcohol problems in primary ',
        'care patients. Preventive Medicine, 33 (5), 428-433. ',
        'https://doi.org/10.1006/pmed.2001.0910'
      ),
      reference_identifier = c(
        DOI = '10.1006/pmed.2001.0910'
      ),
      interpretation = paste0(
        "Higher scores indicate more problematic alcohol use ",
        "behavior"
      ),
      values_and_labels = ""
    )

    # Close 'AUDIT'
  }

  #### 1.4) AUQ ####
  if ( abbreviation %in% c( 'AUQ', 'URGE' ) ) {

    ### Overall

    out$Description <- paste0(
      'Scores for the AUQ - measure ',
      'of the degree to which a person craves alcohol'
    )

    out$Units <- "Summed score"

    out$Scale <- list(
      name = 'Alcohol Urge Questionnaire',
      n_items = 8,
      range = c( Min = 8, Max = 56 ),
      abbreviation = 'AUQ',
      cut_off = NA,
      reference = paste0(
        'Bohn, M. J., Krahn, D. D., & Staehler, B. A. (1995). ',
        'Development and initial validation of a measure of ',
        'drinking urges in abstinent alcoholics. Alcoholism: ',
        'clinical and experimental research, 19(3), 600-606. ',
        'https://doi.org/10.1111/j.1530-0277.1995.tb01554.x'
      ),
      reference_identifier = c(
        DOI = '10.1111/j.1530-0277.1995.tb01554.x'
      ),
      interpretation = paste0(
        'Higher scores indicate greater craving of alcohol'
      ),
      values_and_labels = list(
        content = 1:7,
        additional_content = c(
          'Strongly disagree',
          '',
          '',
          '',
          '',
          '',
          'Strongly agree'
        )
      )
    )

    ### Subscales
    if ( subscale != '' ) {

      ### Urge
      if ( subscale %in% c( 'URGE', 'Urge', 'urge' ) ) {

        out$Description <- paste0(
          'Scores for the AUQ Urge subscale - measure of the urge ',
          'to consume alcohol'
        )

        out$Units <- 'Mean score'

        out$Subscale <- list(
          name = 'Alcohol Consumption Urge',
          n_items = 3,
          range = c( Min = 0, Max = 4 ),
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

  #### 1.5) AIS ####
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
      cut_off = c(
        `Insomnia` = 6
      ),
      reference = c(
        paste0(
          'Soldatos, C. R., Dikeos, D.G., & Paparrigopoulos, T.J. ',
          '(2000). Athens insomnia scale: Validation of an ',
          'instrument based on ICD-10 criteria. Journal of ',
          'Psychosomatic Research, 48 (6), 555–560. ',
          'https://doi.org/10.1016/s0022-3999(00)00095-7'
        ),
        paste0(
          'Soldatos, C. R., Dikeos, D. G., & Paparrigopoulos, T. J. ',
          '(2003). The diagnostic validity of the Athens Insomnia Scale. ',
          'Journal of Psychosomatic Research, 55 (3), 263-267. ',
          'https://doi.org/10.1016/S0022-3999(02)00604-9'
        )
      ),
      reference_identifier = c(
        DOI = '10.1016/s0022-3999(00)00095-7',
        DOI = '10.1016/S0022-3999(02)00604-9'
      ),
      interpretation = paste0(
        "Higher scores indicate a greater degree of insomnia"
      ),
      values_and_labels = ""
    )

    # Close 'AIS'
  }

  #### 1.6) BIS-11 ####
  if ( abbreviation %in% c( 'BIS', 'BIS-11' ) ) {

    ### Overall

    out$Description <- paste0(
      'Scores for the AIS - measure of the degree of insomnia ',
      'experienced by a subject'
    )

    out$Units <- "Summed score"

    out$Scale <- list(
      name = 'Barratt Impulsiveness Scale',
      n_items = 30,
      range = c( Min = 30, Max = 120 ),
      abbreviation = 'BIS-11',
      cut_off = c( High = 72 ),
      reference = paste0(
        'Patton, J. H., Stanford, M. S., & Barratt, E. S. (1995). ',
        'Factor structure of the Barratt Impulsiveness Scale. ',
        'Journal of clinical psychology, 51(6), 768-774. ',
        'https://doi.org/10.1002/1097-4679(199511)51:6<',
        '768::AID-JCLP2270510607>3.0.CO;2-1'
      ),
      reference_identifier = c(
        paste0(
          DOI = '10.1002/1097-4679(199511)51:6<',
          '768::AID-JCLP2270510607>3.0.CO;2-1'
        )
      ),
      interpretation = paste0(
        'Higher scores indicate greater impulsivity'
      ),
      values_and_labels = list(
        content = 1:4,
        additional_content = c( "", "", "", "" )
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
          range = c( Min = 8, Max - 32 ),
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
          range = c( Min = 11, Max = 44 ),
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

  #### 1.7) BRIEF CHECK!!! ####
  if ( abbreviation %in% c( 'BRIEF', 'BRIEF-A' ) ) {

    out$Description <- paste0(
      'Scores for the BRIEF - measure of executive ',
      'function impairment for adolescents'
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
      reference_identifier = c(
      ),
      interpretation = paste0(
        'Higher scores indicate poorer executive function'
      )
    )

    # Close 'BRIEF'
  }

  #### 1.8) BRIEF2 CHECK!!! ####
  if ( abbreviation %in% c( 'BRIEF2' ) ) {

    out$Description <- NULL

    out$Units <- NULL

    out$Scale <- list(
      name = 'Behavior Rating Inventory of Executive Function - 2nd Edition',
      n_items = 55,
      range = c( Min = NA, Max = NA ),
      abbreviation = 'BRIEF2',
      cut_off = NA,
      reference = paste0(
        'Hendrickson, N. K., & McCrimmon, A. W. (2019). ',
        'Test Review: Behavior Rating Inventory of Executive ',
        'Function®, Second Edition (BRIEF®2) by Gioia, G. A., ',
        'Isquith, P. K., Guy, S. C., & Kenworthy, L. Canadian ',
        'Journal of School Psychology, 34 (1), 73–78. ',
        'https://doi.org/10.1177/0829573518797762'
      ),
      reference_identifier = c(
        DOI = "10.1177/0829573518797762"
      ),
      interpretation = "",
      values_and_labels = list(
        content = 1:3,
        additional_content = c( 'Never', 'Sometimes', 'Often' )
      )
    )

    ### Subscales
    if ( subscale != '' ) {

      #### 1.8.1) Scores for Inhibit subscale ####

      ### Inhibit (Raw)
      if ( subscale == 'Inhibit (Raw)' ) {

        out$Description <- paste0(
          'Raw score for the BRIEF2 inhibit subscale - ',
          'measure of inhibitory control and impulsivity'
        )

        out$Units <- "Summed score"

        out$Subscale <- list(
          name = 'Inhibit (Raw)',
          n_items = 8,
          range = c( Min = 8, Max = 24 ),
          cut_off = NA,
          interpretation = paste0(
            "Higher scores indicate less inhibitory control and ",
            "greater impulsivity"
          )
        )

        # Close 'Inhibit (Raw)'
      }

      ### Inhibit (Percentile)
      if ( subscale == 'Inhibit (Percentile)' ) {

        out$Description <- paste0(
          'Percentile rank for the BRIEF2 inhibit subscale - ',
          'measure of inhibitory control and impulsivity'
        )

        out$Units <- "Percentile rank"

        out$Subscale <- list(
          name = 'Inhibit (Percentile)',
          n_items = 8,
          range = c( Min = 0, Max = 100 ),
          cut_off = NA,
          interpretation = paste0(
            "Higher scores indicate less inhibitory control and ",
            "greater impulsivity"
          )
        )

        # Close 'Inhibit (Percentile)'
      }

      ### Inhibit (T-score)
      if ( subscale == 'Inhibit (T-score)' ) {

        out$Description <- paste0(
          'T-score for the BRIEF2 inhibit subscale - ',
          'measure of inhibitory control and impulsivity'
        )

        out$Units <- "T-score"

        out$Subscale <- list(
          name = 'Inhibit (T-score)',
          n_items = 8,
          range = c( Mean = 50, SD = 10 ),
          cut_off = c( `Clinically elevated` = 70 ),
          interpretation = paste0(
            "Higher scores indicate less inhibitory control and ",
            "greater impulsivity"
          )
        )

        # Close 'Inhibit (T-score)'
      }

      #### 1.8.2) Scores for Self-Monitor subscale ####

      ### Self-Monitor (Raw)
      if ( subscale == 'Self-Monitor (Raw)' ) {

        out$Description <- paste0(
          'Raw score for the BRIEF2 self-monitor subscale - ',
          'measure of how aware individuals are on the impact of ',
          'their behavior on others and outcomes'
        )

        out$Units <- "Summed score"

        out$Subscale <- list(
          name = 'Self-Monitor (Raw)',
          n_items = 5,
          range = c( Min = 5, Max = 15 ),
          cut_off = NA,
          interpretation = paste0(
            "Higher scores indicate greater difficulty in ",
            "monitoring impact of one's behavior"
          )
        )

        # Close 'Self-Monitor (Raw)'
      }

      ### Self-Monitor (Percentile)
      if ( subscale == 'Self-Monitor (Percentile)' ) {

        out$Description <- paste0(
          'Percentile rank for the BRIEF2 self-monitor subscale - ',
          'measure of how aware individuals are on the impact of ',
          'their behavior on others and outcomes'
        )

        out$Units <- "Percentile rank"

        out$Subscale <- list(
          name = 'Self-Monitor (Percentile)',
          n_items = 5,
          range = c( Min = 0, Max = 100 ),
          cut_off = NA,
          interpretation = paste0(
            "Higher scores indicate greater difficulty in ",
            "monitoring impact of one's behavior"
          )
        )

        # Close 'Self-Monitor (Percentile)'
      }

      ### Self-Monitor (T-score)
      if ( subscale == 'Self-Monitor (T-score)' ) {

        out$Description <- paste0(
          'T-score for the BRIEF2 self-Monitor subscale - ',
          'measure of how aware individuals are on the impact of ',
          'their behavior on others and outcomes'
        )

        out$Units <- "T-score"

        out$Subscale <- list(
          name = 'Self-Monitor (T-score)',
          n_items = 5,
          range = c( Mean = 50, SD = 10 ),
          cut_off = c( `Clinically elevated` = 70 ),
          interpretation = paste0(
            "Higher scores indicate greater difficulty in ",
            "monitoring impact of one's behavior"
          )
        )

        # Close 'Self-Monitor (T-score)'
      }

      #### 1.8.3) Scores for Shift subscale ####

      ### Shift (Raw)
      if ( subscale == 'Shift (Raw)' ) {

        out$Description <- paste0(
          'Raw score for the BRIEF2 shift subscale - ',
          'measure of ability to adapt to different ',
          'situations or activities or problems as ',
          'circumstance demands'
        )

        out$Units <- "Summed score"

        out$Subscale <- list(
          name = 'Shift (Raw)',
          n_items = 8,
          range = c( Min = 8, Max = 24 ),
          cut_off = NA,
          interpretation = paste0(
            "Higher scores indicate a greater tendency ",
            "to get stuck or focused on a given topic or problem"
          )
        )

        # Close 'Shift (Raw)'
      }

      ### Shift (Percentile)
      if ( subscale == 'Shift (Percentile)' ) {

        out$Description <- paste0(
          'Percentile rank for the BRIEF2 shift subscale - ',
          'measure of ability to adapt to different ',
          'situations or activities or problems as ',
          'circumstance demands'
        )

        out$Units <- "Percentile rank"

        out$Subscale <- list(
          name = 'Shift (Percentile)',
          n_items = 8,
          range = c( Min = 0, Max = 100 ),
          cut_off = NA,
          interpretation = paste0(
            "Higher scores indicate a greater tendency ",
            "to get stuck or focused on a given topic or problem"
          )
        )

        # Close 'Shift (Percentile)'
      }

      ### Shift (T-score)
      if ( subscale == 'Shift (T-score)' ) {

        out$Description <- paste0(
          'T-score for the BRIEF2 shift subscale - ',
          'measure of ability to adapt to different ',
          'situations or activities or problems as ',
          'circumstance demands'
        )

        out$Units <- "T-score"

        out$Subscale <- list(
          name = 'Shift (T-score)',
          n_items = 8,
          range = c( Mean = 50, SD = 10 ),
          cut_off = c( `Clinically elevated` = 70 ),
          interpretation = paste0(
            "Higher scores indicate a greater tendency ",
            "to get stuck or focused on a given topic or problem"
          )
        )

        # Close 'Shift (T-score)'
      }

      #### 1.8.4) Scores for Emotional Control subscale ####

      ### Emotional control (Raw)
      if ( subscale == 'Emotional Control (Raw)' ) {

        out$Description <- paste0(
          'Raw score for the BRIEF2 emotional control subscale - ',
          'measure of impact of executive function on emotional ',
          'expression and ability to modulate emotional responses'
        )

        out$Units <- "Summed score"

        out$Subscale <- list(
          name = 'Emotional Control (Raw)',
          n_items = 6,
          range = c( Min = 6, Max = 18 ),
          cut_off = NA,
          interpretation = paste0(
            "Higher scores indicate a greater tendency to ",
            "have overblown emotional reactions to minor events"
          )
        )

        # Close 'Emotional Control (Raw)'
      }

      ### Emotional Control (Percentile)
      if ( subscale == 'Emotional Control (Percentile)' ) {

        out$Description <- paste0(
          'Percentile rank for the BRIEF2 emotional control subscale - ',
          'measure of impact of executive function on emotional ',
          'expression and ability to modulate emotional responses'
        )

        out$Units <- "Percentile rank"

        out$Subscale <- list(
          name = 'Emotional Control (Percentile)',
          n_items = 6,
          range = c( Min = 0, Max = 100 ),
          cut_off = NA,
          interpretation = paste0(
            "Higher scores indicate a greater tendency to ",
            "have overblown emotional reactions to minor events"
          )
        )

        # Close 'Emotional Control (Percentile)'
      }

      ### Emotional Control (T-score)
      if ( subscale == 'Emotional Control (T-score)' ) {

        out$Description <- paste0(
          'T-score for the BRIEF2 emotional control subscale - ',
          'measure of impact of executive function on emotional ',
          'expression and ability to modulate emotional responses'
        )

        out$Units <- "T-score"

        out$Subscale <- list(
          name = 'Emotional Control (T-score)',
          n_items = 6,
          range = c( Mean = 50, SD = 10 ),
          cut_off = c( `Clinically elevated` = 70 ),
          interpretation = paste0(
            "Higher scores indicate a greater tendency to ",
            "have overblown emotional reactions to minor events"
          )
        )

        # Close 'Emotional Control (T-score)'
      }

      #### 1.8.5) Scores for Task Completion subscale ####

      ### Task Completion (Raw)
      if ( subscale == 'Task Completion (Raw)' ) {

        out$Description <- paste0(
          'Raw score for the BRIEF2 task completion subscale - ',
          'measure of ability to independently generate ideas or ',
          'responses or problem-solving strategies and finish a task'
        )

        out$Units <- "Summed score"

        out$Subscale <- list(
          name = 'Task Completion (Raw)',
          n_items = 7,
          range = c( Min = 7, Max = 21 ),
          cut_off = NA,
          interpretation = paste0(
            "Higher scores indicate greater difficulty in generating ",
            "problem-solving approaches and completing tasks"
          )
        )

        # Close 'Task Completion (Raw)'
      }

      ### Task Completion (Percentile)
      if ( subscale == 'Task Completion (Percentile)' ) {

        out$Description <- paste0(
          'Percentile rank for the BRIEF2 task completion subscale - ',
          'measure of ability to independently generate ideas or ',
          'responses or problem-solving strategies and finish a task'
        )

        out$Units <- "Percentile rank"

        out$Subscale <- list(
          name = 'Task Completion (Percentile)',
          n_items = 7,
          range = c( Min = 0, Max = 100 ),
          cut_off = NA,
          interpretation = paste0(
            "Higher scores indicate greater difficulty in generating ",
            "problem-solving approaches and completing tasks"
          )
        )

        # Close 'Task Completion (Percentile)'
      }

      ### Task Completion (T-score)
      if ( subscale == 'Task Completion (T-score)' ) {

        out$Description <- paste0(
          'T-score for the BRIEF2 task completion subscale - ',
          'measure of ability to independently generate ideas or ',
          'responses or problem-solving strategies and finish a task'
        )

        out$Units <- "T-score"

        out$Subscale <- list(
          name = 'Task Completion (T-score)',
          n_items = 7,
          range = c( Mean = 50, SD = 10 ),
          cut_off = c( `Clinically elevated` = 70 ),
          interpretation = paste0(
            "Higher scores indicate greater difficulty in generating ",
            "problem-solving approaches and completing tasks"
          )
        )

        # Close 'Task Completion (T-score)'
      }

      #### 1.8.6) Scores for Working Memory subscale ####

      ### Working Memory (Raw)
      if ( subscale == 'Working Memory (Raw)' ) {

        out$Description <- paste0(
          'Raw score for the BRIEF2 working memory subscale - ',
          'measure of online representational memory - capacity ',
          'to hold or encode information in mind'
        )

        out$Units <- "Summed score"

        out$Subscale <- list(
          name = 'Working Memory (Raw)',
          n_items = 8,
          range = c( Min = 8, Max = 24 ),
          cut_off = NA,
          interpretation = paste0(
            "Higher scores indicate greater difficulty in ",
            "keeping relevant information in mind for problem ",
            "solving"
          )
        )

        # Close 'Working Memory (Raw)'
      }

      ### Working Memory (Percentile)
      if ( subscale == 'Working Memory (Percentile)' ) {

        out$Description <- paste0(
          'Percentile rank for the BRIEF2 working memory subscale - ',
          'measure of online representational memory - capacity ',
          'to hold or encode information in mind'
        )

        out$Units <- "Percentile rank"

        out$Subscale <- list(
          name = 'Working Memory (Percentile)',
          n_items = 8,
          range = c( Min = 0, Max = 100 ),
          cut_off = NA,
          interpretation = paste0(
            "Higher scores indicate greater difficulty in ",
            "keeping relevant information in mind for problem ",
            "solving"
          )
        )

        # Close 'Working Memory (Percentile)'
      }

      ### Working Memory (T-score)
      if ( subscale == 'Working Memory (T-score)' ) {

        out$Description <- paste0(
          'T-score for the BRIEF2 working memory subscale - ',
          'measure of online representational memory - capacity ',
          'to hold or encode information in mind'
        )

        out$Units <- "T-score"

        out$Subscale <- list(
          name = 'Working Memory (T-score)',
          n_items = 8,
          range = c( Mean = 50, SD = 10 ),
          cut_off = c( `Clinically elevated` = 70 ),
          interpretation = paste0(
            "Higher scores indicate greater difficulty in ",
            "keeping relevant information in mind for problem ",
            "solving"
          )
        )

        # Close 'Working Memory (T-score)'
      }

      #### 1.8.7) Scores for Plan|Organize subscale ####

      ### Plan|Organize (Raw)
      if ( subscale == 'Plan|Organize (Raw)' ) {

        out$Description <- paste0(
          'Raw score for the BRIEF2 plan|organize subscale - ',
          'measure of ability to mange current and future-oriented ',
          'task demands'
        )

        out$Units <- "Summed score"

        out$Subscale <- list(
          name = 'Plan|Organize (Raw)',
          n_items = 10,
          range = c( Min = 10, Max = 30 ),
          cut_off = NA,
          interpretation = paste0(
            "Higher scores indicate greater difficulty in planning ",
            "and organizing"
          )
        )

        # Close 'Plan|Organize (Raw)'
      }

      ### Plan|Organize (Percentile)
      if ( subscale == 'Plan|Organize (Percentile)' ) {

        out$Description <- paste0(
          'Percentile rank for the BRIEF2 plan|organize subscale - ',
          'measure of ability to mange current and future-oriented ',
          'task demands'
        )

        out$Units <- "Percentile rank"

        out$Subscale <- list(
          name = 'Plan|Organize (Percentile)',
          n_items = 10,
          range = c( Min = 0, Max = 100 ),
          cut_off = NA,
          interpretation = paste0(
            "Higher scores indicate greater difficulty in planning ",
            "and organizing"
          )
        )

        # Close 'Plan|Organize (Percentile)'
      }

      ### Plan|Organize (T-score)
      if ( subscale == 'Plan|Organize (T-score)' ) {

        out$Description <- paste0(
          'T-score for the BRIEF2 plan|organize subscale - ',
          'measure of ability to mange current and future-oriented ',
          'task demands'
        )

        out$Units <- "T-score"

        out$Subscale <- list(
          name = 'Plan|Organize (T-score)',
          n_items = 10,
          range = c( Mean = 50, SD = 10 ),
          cut_off = c( `Clinically elevated` = 70 ),
          interpretation = paste0(
            "Higher scores indicate greater difficulty in planning ",
            "and organizing"
          )
        )

        # Close 'Plan|Organize (T-score)'
      }

      #### 1.8.8) Scores for Behavioral Regulation subscale ####

      ### Behavioral Regulation (Raw)
      if ( subscale == 'Behavioral Regulation (Raw)' ) {

        out$Description <- paste0(
          'Raw score for the BRIEF2 behavioral regulation subscale - ',
          'measure of ability to regulate and monitor behavior ',
          'effectively'
        )

        out$Units <- "Summed score"

        out$Subscale <- list(
          name = 'Behavioral Regulation (Raw)',
          n_items = 13,
          range = c( Min = 13, Max = 39 ),
          cut_off = NA,
          interpretation = paste0(
            "Higher scores indicate greater difficulty in ",
            "behavioral regulation"
          )
        )

        # Close 'Behavioral Regulation (Raw)'
      }

      ### Behavioral Regulation (Percentile)
      if ( subscale == 'Behavioral Regulation (Percentile)' ) {

        out$Description <- paste0(
          'Percentile rank for the BRIEF2 behavioral regulation subscale - ',
          'measure of ability to regulate and monitor behavior ',
          'effectively'
        )

        out$Units <- "Percentile rank"

        out$Subscale <- list(
          name = 'Behavioral Regulation (Percentile)',
          n_items = 13,
          range = c( Min = 0, Max = 100 ),
          cut_off = NA,
          interpretation = paste0(
            "Higher scores indicate greater difficulty in ",
            "behavioral regulation"
          )
        )

        # Close 'Behavioral Regulation (Percentile)'
      }

      ### Behavioral Regulation (T-score)
      if ( subscale == 'Behavioral Regulation (T-score)' ) {

        out$Description <- paste0(
          'T-score for the BRIEF2 behavioral regulation subscale - ',
          'measure of ability to regulate and monitor behavior ',
          'effectively'
        )

        out$Units <- "T-score"

        out$Subscale <- list(
          name = 'Behavioral Regulation (T-score)',
          n_items = 13,
          range = c( Mean = 50, SD = 10 ),
          cut_off = c( `Clinically elevated` = 70 ),
          interpretation = paste0(
            "Higher scores indicate greater difficulty in ",
            "behavioral regulation"
          )
        )

        # Close 'Behavioral Regulation (T-score)'
      }

      #### 1.8.9) Scores for Emotion Regulation subscale ####

      ### Emotion Regulation (Raw)
      if ( subscale == 'Emotion Regulation (Raw)' ) {

        out$Description <- paste0(
          'Raw score for the BRIEF2 emotion regulation subscale - ',
          'measure of ability to regulate emotional responses ',
          'and to adapt to changes in situations and demands'
        )

        out$Units <- "Summed score"

        out$Subscale <- list(
          name = 'Emotion Regulation (Raw)',
          n_items = 14,
          range = c( Min = 14, Max = 42 ),
          cut_off = NA,
          interpretation = paste0(
            "Higher scores indicate greater difficulty in ",
            "emotional regulation"
          )
        )

        # Close 'Emotion Regulation (Raw)'
      }

      ### Emotion Regulation (Percentile)
      if ( subscale == 'Emotion Regulation (Percentile)' ) {

        out$Description <- paste0(
          'Percentile rank for the BRIEF2 emotion regulation subscale - ',
          'measure of ability to regulate emotional responses ',
          'and to adapt to changes in situations and demands'
        )

        out$Units <- "Percentile rank"

        out$Subscale <- list(
          name = 'Emotion Regulation (Percentile)',
          n_items = 14,
          range = c( Min = 0, Max = 100 ),
          cut_off = NA,
          interpretation = paste0(
            "Higher scores indicate greater difficulty in ",
            "emotional regulation"
          )
        )

        # Close 'Emotion Regulation (Percentile)'
      }

      ### Emotion Regulation (T-score)
      if ( subscale == 'Emotion Regulation (T-score)' ) {

        out$Description <- paste0(
          'T-score for the BRIEF2 emotion regulation subscale - ',
          'measure of ability to regulate emotional responses ',
          'and to adapt to changes in situations and demands'
        )

        out$Units <- "T-score"

        out$Subscale <- list(
          name = 'Emotion Regulation (T-score)',
          n_items = 14,
          range = c( Mean = 50, SD = 10 ),
          cut_off = c( `Clinically elevated` = 70 ),
          interpretation = paste0(
            "Higher scores indicate greater difficulty in ",
            "emotional regulation"
          )
        )

        # Close 'Emotion Regulation (T-score)'
      }

      #### 1.8.10) Scores for Cognitive Regulation subscale ####

      ### Cognitive Regulation (Raw)
      if ( subscale == 'Cognitive Regulation (Raw)' ) {

        out$Description <- paste0(
          'Raw score for the BRIEF2 cognitive regulation subscale - ',
          'measure of ability to control and manage cognitive processes ',
          'and to problem-solve effectively'
        )

        out$Units <- "Summed score"

        out$Subscale <- list(
          name = 'Cognitive Regulation (Raw)',
          n_items = 25,
          range = c( Min = 25, Max = 75 ),
          cut_off = NA,
          interpretation = paste0(
            "Higher scores indicate greater difficulty in ",
            "cognitive regulation"
          )
        )

        # Close 'Cognitive Regulation (Raw)'
      }

      ### Cognitive Regulation (Percentile)
      if ( subscale == 'Cognitive Regulation (Percentile)' ) {

        out$Description <- paste0(
          'Percentile rank for the BRIEF2 cognitive regulation subscale - ',
          'measure of ability to control and manage cognitive processes ',
          'and to problem-solve effectively'
        )

        out$Units <- "Percentile rank"

        out$Subscale <- list(
          name = 'Cognitive Regulation (Percentile)',
          n_items = 25,
          range = c( Min = 0, Max = 100 ),
          cut_off = NA,
          interpretation = paste0(
            "Higher scores indicate greater difficulty in ",
            "cognitive regulation"
          )
        )

        # Close 'Cognitive Regulation (Percentile)'
      }

      ### Cognitive Regulation (T-score)
      if ( subscale == 'Cognitive Regulation (T-score)' ) {

        out$Description <- paste0(
          'T-score for the BRIEF2 cognitive regulation subscale - ',
          'measure of ability to control and manage cognitive processes ',
          'and to problem-solve effectively'
        )

        out$Units <- "T-score"

        out$Subscale <- list(
          name = 'Cognitive Regulation (T-score)',
          n_items = 25,
          range = c( Mean = 50, SD = 10 ),
          cut_off = c( `Clinically elevated` = 70 ),
          interpretation = paste0(
            "Higher scores indicate greater difficulty in ",
            "cognitive regulation"
          )
        )

        # Close 'Cognitive Regulation (T-score)'
      }

      #### 1.8.11) Scores for General Executive subscale ####

      ### General Executive (Raw)
      if ( subscale == 'General Executive (Raw)' ) {

        out$Description <- paste0(
          'Raw score for the BRIEF2 general executive subscale - ',
          'measure of general executive functioning'
        )

        out$Units <- "Summed score"

        out$Subscale <- list(
          name = 'General Executive (Raw)',
          n_items = 55,
          range = c( Min = 55, Max = 165 ),
          cut_off = NA,
          interpretation = paste0(
            "Higher scores indicate greater overall problems in ",
            "general executive functioning"
          )
        )

        # Close 'General Executive (Raw)'
      }

      ### General Executive (Percentile)
      if ( subscale == 'General Executive (Percentile)' ) {

        out$Description <- paste0(
          'Percentile rank for the BRIEF2 general executive subscale - ',
          'measure of general executive functioning'
        )

        out$Units <- "Percentile rank"

        out$Subscale <- list(
          name = 'General Executive (Percentile)',
          n_items = 55,
          range = c( Min = 0, Max = 100 ),
          cut_off = NA,
          interpretation = paste0(
            "Higher scores indicate greater overall problems in ",
            "general executive functioning"
          )
        )

        # Close 'General Executive (Percentile)'
      }

      ### General Executive (T-score)
      if ( subscale == 'General Executive (T-score)' ) {

        out$Description <- paste0(
          'T-score for the BRIEF2 general executive subscale - ',
          'measure of general executive functioning'
        )

        out$Units <- "T-score"

        out$Subscale <- list(
          name = 'General Executive (T-score)',
          n_items = 55,
          range = c( Mean = 50, SD = 10 ),
          cut_off = c( `Clinically elevated` = 70 ),
          interpretation = paste0(
            "Higher scores indicate greater overall problems in ",
            "general executive functioning"
          )
        )

        # Close 'General Executive (T-score)'
      }

      # Close 'Subscales'
    }

    # Close 'BRIEF2'
  }

  #### 1.9) BPI ####
  if ( abbreviation %in% c( 'BPI', 'BPI-SF' ) ) {

    ### Overall

    out$Description = NULL

    out$Units = NULL

    out$Scale <- list(
      name = 'Brief Pain Inventory (Short form)',
      n_items = 11,
      range = c( Min = NA, Max = NA ),
      abbreviation = 'BPI-SF',
      cut_off = NA,
      reference = paste0(
        'Cleeland, C. S., & Ryan, K. M. (1994). Pain assessment: ',
        'Global use of the Brief Pain Inventory. Annals of the ',
        'Academy of Medicine, Singapore, 23(2), 129-138. '
      ),
      reference_identifier = c(
        PMID = "8080219"
      ),
      interpretation = "",
      values_and_labels = ""
    )

    ### Subscales
    if ( subscale != '' ) {

      ### Severity
      if ( subscale == 'Severity' ) {

        out$Description <- paste0(
          'Scores for the BPI severity subscale - measure of the ',
          'degree of pain severity experienced by a subject ',
          'within the last 24 hours'
        )

        out$Units <- "Mean score"

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

        out$Units <- "Mean score"

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

  #### 1.10) BPAQ ####
  if ( abbreviation %in% c( 'BPAQ' ) ) {

    ### Overall

    out$Description = paste0(
      "Total score for the BPAQ - measure of the degree of ",
      "an individual's general aggression"
    )

    out$Units = "Summed score"

    out$Scale <- list(
      name = 'Buss-Perry Aggression Questionnaire',
      n_items = 29,
      range = c( Min = 29, Max = 145 ),
      abbreviation = 'BPAQ',
      cut_off = NA,
      reference = paste0(
        'Buss, A. H., & Perry, M. (1992). The Aggression ',
        'Questionnaire. Journal of Personality and Social ',
        'Psychology, 63 (3), 452-459. ',
        'https://doi.org/10.1037/0022-3514.63.3.452'
      ),
      reference_identifier = c(
        DOI = "10.1037/0022-3514.63.3.452"
      ),
      interpretation = "",
      values_and_labels = list(
        content = 1:5,
        additional_content = c(
          "Extremely uncharacteristic",
          "Somewhat uncharacteristic",
          "Neither uncharacteristic or characteristic",
          "Somewhat characteristic",
          "Extremely characteristic"
        )
      )
    )

    ### Subscales
    if ( subscale != '' ) {

      ### Physical Aggression
      if ( subscale %in% c( 'Physical Aggression', 'BPAQ-PA' ) ) {

        out$Description <- paste0(
          'Scores for the BPAQ-PA – measure of the level of physical ',
          'aggression of a subject'
        )

        out$Units <- 'Summed score'

        out$Subscale <- list(
          name = 'Physical Aggression',
          n_items = 9,
          range = c( Min = 9, Max = 45 ),
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
          'Scores for the BPAQ-VA – measure of the level of verbal ',
          'aggression of a subject'
        )

        out$Units <- 'Mean score'

        out$Subscale <- list(
          name = 'Verbal Aggression',
          n_items = 5,
          range = c( Min = 5, Max = 25 ),
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

        out$Units <- 'Mean score'

        out$Subscale <- list(
          name = 'Anger',
          n_items = 7,
          range = c( Min = 7, Max = 35 ),
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
          'Scores for the BPAQ-H – measure of the level of hostility of ',
          'a subject'
        )

        out$Units <- 'Mean score'

        out$Subscale <- list(
          name = 'Hostility',
          n_items = 8,
          range = c( Min = 8, Max = 40 ),
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

  #### 1.11) CUDIT-R ####
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
      reference_identifier = c(
        DOI = "10.1016/j.drugalcdep.2010.02.017"
      ),
      interpretation = paste0(
        "Higher scores indicate more problematic cannabis use"
      ),
      values_and_labels = ""
    )

    # Close 'CUDIT-R'
  }

  #### 1.12) CWS ####
  if ( abbreviation %in% c( 'CWS' ) ) {

    ### Overall

    out$Description = NULL

    out$Units = NULL

    out$Scale <- list(
      name = 'Cannabis Withdrawal Scale',
      n_items = 38,
      range = c( Min = NA, Max = NA ),
      abbreviation = 'CWS',
      cut_off = NA,
      reference = paste0(
        'Allsop, D. J., Norberg, M. M., Copeland, J., Fu, S., & ',
        'Budney, A. J. (2011). The Cannabis Withdrawal Scale development: ',
        'Patterns and predictors of cannabis withdrawal and distress. ',
        'Drug Alcohol Dependence, 119, 123-129. ',
        'https://doi.org/10.1016/j.drugalcdep.2011.06.003'
      ),
      reference_identifier = c(
        DOI = "10.1016/j.drugalcdep.2011.06.003"
      ),
      interpretation = "",
      values_and_labels = list(
        content = 0:10,
        additional_content = c(
          "Not at all", # 0
          "", "", "", "", # 1 - 4
          "Moderately", # 5
          "", "", "", "", # 6 - 9
          "Strongly agree" # 10
        )
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
          range = c( Min = 0, Max = 190 ),
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
          range = c( Min = 0, Max = 190 ),
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

    # Close 'CWS'
  }

  #### 1.13) CGI CHECK!!! ####
  if ( abbreviation == 'CGI' ) {

    ### Overall

    out$Description = NULL

    out$Units = NULL

    out$Scale <- list(
      name = 'Clinical Global Impression scale',
      n_items = 2,
      range = c( Min = NA, Max = NA ),
      abbreviation = 'CGI',
      cut_off = NA,
      reference = c(
        paste0(
          "Guy, W. (Ed.) (1976). ECDEU assessment manual for ",
          "psychopharmacology. US Department of Heath, ",
          "Education, and Welfare; Public Health Service ",
          "Alcohol, Drug Abuse, and Mental Health Administration."
        )
      ),
      reference_identifier = c(
        ""
      ),
      interpretation = "",
      values_and_labels = list(
        content = c( 1, 7, 1, 7 ),
        additional_content = c(
          "Not at all ill (Severity)",
          "Among the most extremely ill patients (Severity)",
          "Very much improved (Improvement)",
          "Very much worse (Improvement)"
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
          "Likert scale"
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
          "Likert scale"
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

  #### 1.14) CHRT ####
  if ( abbreviation == 'CHRT' ) {

    ### Overall

    out$Description <- paste0(
      "Scores for the CHRT - measure of risk based on ",
      "suicidal ideation and intent"
    )

    out$Units <- "Summed score"

    out$Scale <- list(
      name = 'Concise Health Risk Tracking scale',
      n_items = 12,
      range = c( Min = 0, Max = 48 ),
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
      reference_identifier = c(
        DOI = "10.4088/JCP.11m06837"
      ),
      interpretation = paste0(
        "Higher scores indicate greater risk of suicide"
      ),
      values_and_labels = ""
    )

    ### Subscales
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

  #### 1.15) DOSPERT ####
  if ( abbreviation == 'DOSPERT' ) {

    ### Overall

    out$Description <- paste0(
      "Scores for the DOSPERT - measure of propensity ",
      "for risk-taking"
    )

    out$Units <- "Summed score"

    out$Scale <- list(
      name = 'Domain-Specific Risk-Taking Scale',
      n_items = 90,
      range = c( Min = 90, Max = 630 ),
      abbreviation = 'DOSPERT',
      cut_off = NA,
      reference = paste0(
        'Weber, E. U., Blais, A. R., & Betz, N. E. (2002). ',
        'A domain‐specific risk‐attitude scale: Measuring ',
        'risk perceptions and risk behaviors. Journal of ',
        'behavioral decision making, 15 (4), 263-290. ',
        'https://doi.org/10.1002/bdm.414'
      ),
      reference_identifier = c(
        DOI = "10.1002/bdm.414"
      ),
      interpretation = paste0(
        'Higher scores indicate greater propensity ',
        'for risk-taking'
      ),
      values_and_labels = ""
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
          range = c( Min = 6, Max = 42 ),
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
          range = c( Min = 6, Max = 42 ),
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
          range = c( Min = 6, Max = 42 ),
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
          range = c( Min = 6, Max = 42 ),
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
          range = c( Min = 6, Max = 42 ),
          cut_off = NA,
          interpretation = paste0(
            'Higher scores indicate greater propensity ',
            'to engage in risky recreational activities'
          )
        )

        # Close 'Recreational'
      }

      ### Ethical Risk Perception
      if ( subscale %in% c( 'Ethical Risk Perception',
                            'Ethical risk perception' ) ) {

        out$Description <- paste0(
          'Scores for the DOSPERT (EP) - measure of ',
          'risk perception in ethical situations'
        )

        out$Units <- 'Summed score'

        out$Subscale <- list(
          name = 'Ethical Risk Perception',
          n_items = 6,
          range = c( Min = 6, Max = 42 ),
          cut_off = NA,
          interpretation = paste0(
            'Higher scores indicate greater perception of risk ',
            'in ethical situations'
          )
        )

        # Close 'Ethical Risk Perception'
      }

      ### Financial Risk Perception
      if ( subscale %in% c( 'Financial Risk Perception',
                            'Financial risk perception' ) ) {

        out$Description <- paste0(
          'Scores for the DOSPERT (FP) - measure of ',
          'risk perception in financial situations'
        )

        out$Units <- 'Summed score'

        out$Subscale <- list(
          name = 'Financial Risk Perception',
          n_items = 6,
          range = c( Min = 6, Max = 42 ),
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
          range = c( Min = 6, Max = 42 ),
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
          range = c( Min = 6, Max = 42 ),
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
          range = c( Min = 6, Max = 42 ),
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
          range = c( Min = 6, Max = 42 ),
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
          range = c( Min = 6, Max = 42 ),
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
          range = c( Min = 6, Max = 42 ),
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
          range = c( Min = 6, Max = 42 ),
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
          range = c( Min = 6, Max = 42 ),
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

  #### 1.16) DMQ ####
  if ( abbreviation %in% c( 'DMQ' ) ) {

    ### Overall

    out$Description <- paste0(
      "Scores for the DMQ - measure of a ",
      "patient's motivation to use alcohol"
    )

    out$Units <- 'Composite score'

    out$Scale <- list(
      name = 'The Drinking Motives Questionnaire-Revised',
      n_items = 20,
      range = c( Min = 4, Max = 20 ),
      abbreviation = 'DMQ',
      cut_off = NA,
      reference = paste0(
        'Cooper, M. L. (1994). Motivations for alcohol use among ',
        'adolescents: Development and validation of a four-factor ',
        'model. Psychological Assessment, 6 (2), 117-128. ',
        'https://doi.org/10.1037/1040-3590.6.2.117'
      ),
      reference_identifier = c(
        DOI = "10.1037/1040-3590.6.2.117"
      ),
      interpretation = paste0(
        "Higher scores indicate greater overall motivation to drink"
      ),
      values_and_labels = ""
    )

    ### Subscales
    if ( subscale != '' ) {

      ### Physical Aggression
      if ( subscale %in% c( 'Social', 'SOC' ) ) {

        out$Description <- paste0(
          'Scores for the DMQ Social (SOC) subscale - measure of a ',
          'patient’s social motives for alcohol use '
        )

        out$Units <- 'Mean score'

        out$Subscale <- list(
          name = 'Social',
          n_items = 5,
          range = c( Min = 1, Max = 5 ),
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
          'Scores for the DMQ Coping (COP) subscale – measure of a ',
          'patient’s coping motives for alcohol use '
        )

        out$Units <- 'Mean score'

        out$Subscale <- list(
          name = 'Coping',
          n_items = 5,
          range = c( Min = 1, Max = 5 ),
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
          'Scores for the DMQ Enhancement (ENH) subscale – measure of ',
          'a patient’s enhancement motives for alcohol use '
        )

        out$Units <- 'Mean score'

        out$Subscale <- list(
          name = 'Enhancement',
          n_items = 5,
          range = c( Min = 1, Max = 5 ),
          cut_off = NA,
          interpretation = paste0(
            'Higher scores indicate greater enhancement motives for ',
            'alcohol use'
          )
        )

        # Close 'Enhancement'
      }
      ### Conformity
      if ( subscale %in% c( 'Conformity', 'CON' ) ) {

        out$Description <- paste0(
          'Scores for the DMQ Conformity (CON) subscale – measure of a ',
          'patient’s conformity motives for alcohol use '
        )

        out$Units <- 'Mean score'

        out$Subscale <- list(
          name = 'Conformity',
          n_items = 5,
          range = c( Min = 1, Max = 5 ),
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

  #### 1.17) ERS ####
  if ( abbreviation %in% c( 'ERS' ) ) {

    ### Overall

    out$Description <- paste0(
      "Scores for the ERS - measure of a ",
      "overall emotional reactivity"
    )

    out$Units <- 'Summed score'

    out$Scale <- list(
      name = 'The Emotion Reactivity Scale',
      n_items = 21,
      range = c( Min = 0, Max = 84 ),
      abbreviation = 'ERS',
      cut_off = NA,
      reference = paste0(
        'Nock, M. K., Wedig, M. M., Holmberg, E. B., & Hooley, J. M.',
        ' (2008). The Emotion Reactivity Scale: Development, ',
        'evaluation, and relation to self-injurious thoughts and ',
        'behaviors. Behavior Therapy, 39 (2), 107–116. ',
        'https://doi.org/10.1016/j.beth.2007.05.005'
      ),
      reference_identifier = c(
        DOI = "10.1016/j.beth.2007.05.005"
      ),
      interpretation = paste0(
        "Higher scores indicate a greater degree of emotional reactivity"
      ),
      values_and_labels = ""
    )

    ### Subscales
    if ( subscale != '' ) {

      ### Sensitivity
      if ( subscale %in% c( 'Sensitivity' ) ) {

        out$Description <- paste0(
          'Scores for the ERS Sensitivity subscale – measure of a ',
          'patient’s emotional sensitivity '
        )

        out$Units <- 'Summed score'

        out$Subscale <- list(
          name = 'Sensitivity',
          n_items = 10,
          range = c( Min = 0, Max = 40 ),
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
          'Scores for the ERS Intensity/arousal Subscale - measure of ',
          'a patient’s emotion reaction intensity '
        )

        out$Units <- 'Summed score'

        out$Subscale <- list(
          name = 'Intensity/arousal',
          n_items = 7,
          range = c( Min = 0, Max = 28 ),
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
          'Scores for the ERS persistence subscale – measure of a ',
          'patient’s persistence (duration) of emotional reactions'
        )

        out$Units <- 'Summed score'

        out$Subscale <- list(
          name = 'Persistence',
          n_items = 4,
          range = c( Min = 0, Max = 16 ),
          cut_off = NA,
          interpretation = paste0(
            'Higher scores indicate longer persistence (duration) ',
            'of emotional reactions'
          )
        )

        # Close 'Persistence'
      }
      # Close 'Subscales'
    }

    # Close 'ERS'
  }

  #### 1.18) GLTEQ ####
  if ( abbreviation == 'GLTEQ' ) {

    out$Description <- paste0(
      'Scores for the GLTEQ – measure of the level of physical ',
      'activity of a subject'
    )

    out$Units <- 'Composite score'

    out$Scale <- list(
      name = 'Godin Leisure-Time Exercise Questionnaire',
      n_items = 4,
      range = c( Min = 0, Max = Inf ),
      abbreviation = 'GLTEQ',
      cut_off = c(
        `Active` = 23,
        `Moderately active` = 14,
        `Insufficiently active or sendentary` = 0
      ),
      reference = c(
        paste0(
          'Godin, G., Shephard, R. J. (1985). A simple method to ',
          'assess exercise behavior in the community. ',
          'Canadian Journal of Applied Sport Science, ',
          '10 (3), 141 - 146.'
        ),
        paste0(
          'Pereira, M. A., FitzGerald, S. J., Gregg, E. W., Joswiak, ',
          'M. L., Ryan, W. J., Suminski, R. R., Utter, A. C., Zmuda, ',
          'J. M. (1997). A collection of physical activity questionnaires ',
          'for health-related research. Medicine and Science in Sports ',
          'and Exercise, 29 (6, Suppl.), S1 - S205.'
        )
      ),
      reference_identifier = c(
        PMID = "4053261",
        PMID = "9243481"
      ),
      interpretation = paste0(
        'Higher scores indicate more physically active lifestyle'
      ),
      values_and_labels = list(
        content = "Total leisure activity",
        additional_content = paste0(
          "(9 x times engaged in strenuous exercise per week) + ",
          "(5 x times engaged in moderate exercise per week) + ",
          "(3 x times engaged in light exercise per week)"
        )
      )
    )

    # Close 'GLTEQ'
  }

  #### 1.19) HADS ####
  if ( abbreviation %in% c( 'HADS' ) ) {

    ### Overall

    out$Description = NULL

    out$Units = NULL

    out$Scale <- list(
      name = 'Hospital Anxiety Depression Scale',
      n_items = 14,
      range = c( Min = NA, Max = NA ),
      abbreviation = 'HADS',
      cut_off = NA,
      reference = paste0(
        'Zigmond, A. S., & Snaith, R. P. (1983). The Hospital Anxiety ',
        'and Depression Scale. Acta Psychiatrica Scandinavica, ',
        '67 (6), 361-370. ',
        'https://doi.org/10.1111/j.1600-0447.1983.tb09716.x'
      ),
      reference_identifier = c(
        DOI = "10.1111/j.1600-0447.1983.tb09716.x"
      ),
      interpretation = "",
      values_and_labels = ""
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

  #### 1.20) KSADS CHECK!!! ####
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
        'adolescents by semistructured interview: Test-retest ',
        'reliability of the Schedule for Affective Disorders ',
        'and Schizophrenia for School-Age Children, Present ',
        'Episode Version. Archives of general psychiatry, ',
        '42 (7), 696-702. ',
        'https://doi.org/10.1001/archpsyc.1985.01790300064008'
      ),
      reference_identifier = c(
        DOI = "10.1001/archpsyc.1985.01790300064008"
      ),
      interpretation = paste0(
        'Higher scores indicate a more extensive family history ',
        'of substance abuse'
      ),
      values_and_labels = ""
    )

    # Close 'KSADS'
  }

  #### 1.21) MCQ-SF ####
  if ( abbreviation == 'MCQ-SF' ) {

    ### Overall

    out$Description <- paste0(
      'Global score for the MCQ-SF - measure of ',
      'overall craving for cannabis'
    )

    out$Units <- "Composite score"

    out$Scale <- list(
      name = 'Marijuana Craving Questionnaire (Short form)',
      n_items = 12,
      range = c( Min = 4, Max = 28 ),
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
      reference_identifier = c(
        DOI = "10.1046/j.1360-0443.2001.967102312.x",
        DOI = "10.1016/j.drugalcdep.2008.12.010"
      ),
      interpretation = paste0(
        "Higher scores indicate a greater overall craving for cannabis"
      ),
      values_and_labels = list(
        content = 1:7,
        additional_content = c(
          "Disagree strongly",
          "Disagree moderately",
          "Disagree a little",
          "Neither agree nor disagree",
          "Agree a little",
          "Agree moderately",
          "Agree strongly"
        )
      )
    )

    ### Subscales
    if ( subscale != '' ) {

      ### Compulsivity
      if (subscale == 'Compulsivity') {

        out$Description <- paste0(
          'Scores for the MCQ compulsivity subscale - measure ',
          'of the inability to control marijuana use'
        )

        out$Units <- "Mean score"

        out$Subscale <- list(
          name = 'Compulsivity',
          n_items = 3,
          range = c( Min = 1, Max = 7 ),
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


        out$Units <- "Mean score"

        out$Subscale <- list(
          name = 'Emotionality',
          n_items = 3,
          range = c( Min = 1, Max = 7 ),
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


        out$Units <- "Mean score"

        out$Subscale <- list(
          name = 'Expectancy',
          n_items = 3,
          range = c( Min = 1, Max = 7 ),
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

        out$Units <- "Mean score"

        out$Subscale <- list(
          name = 'Purposefulness',
          n_items = 3,
          range = c( Min = 1, Max = 7 ),
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

  #### 1.22) MEEQ CHECK!!! ####
  if ( abbreviation %in% c( 'MEEQ' ) ) {

    ### Overall

    out$Description <- paste0(
      'Global score for the MEEQ - measure of ',
      'positive expectations about cannabis'
    )

    out$Units <- "Summed score"

    out$Scale <- list(
      name = 'Marijuana Effect Expectancy Questionnaire',
      n_items = 48,
      range = c( Min = 48, Max = 240 ),
      abbreviation = 'MEEQ',
      cut_off = NA,
      reference = c(
        paste0(
          "Schafer, J. B., & Sandra, A. (1991). Marijuana and cocaine ",
          "effect expectancies and drug use patterns. Journal of ",
          "Consulting and Clinical Psychology, 59, (4), 558 - 565. ",
          "https://doi.org/10.1037//0022-006x.59.4.558"
        )
      ),
      reference_identifier = c(
        DOI = "10.1037//0022-006x.59.4.558"
      ),
      interpretation = paste0(
        "Higher scores indicate greater belief in positive effects ",
        "of cannabis"
      ),
      values_and_labels = list(
        content = 1:5,
        additional_content = c(
          "Disagree strongly",
          "Disagree somewhat",
          "Uncertain",
          "Agree somewhat",
          "Agree strongly"
        )
      )
    )

    ### Subscales
    if ( subscale != '' ) {

      ### Cognitive and Behavioral Impairment
      if ( subscale %in% c( 'Cognitive and Behavioral Impairment' ) ) {

        out$Description <- paste0(
          'Scores for the MEEQ Cognitive and Behavioral Impairment ',
          'subscale - measure of the degree of expectancy of cognitive ',
          'and behavioral impairment resulting from marijuana use'
        )

        out$Units <- 'Summed score'

        out$Subscale <- list(
          name = 'Cognitive and Behavioral Impairment',
          n_items = 10,
          range = c( Min = 10, Max = 50 ),
          cut_off = NA,
          interpretation = paste0(
            'Higher scores indicate greater expectancy of cognitive ',
            'and behavioral impairment due to marijuana use '
          )
        )

        # Close 'Cognitive and Behavioral Impairment'
      }

      ### Relaxation and Tension Reduction
      if ( subscale %in% c( 'Relaxation and Tension Reduction' ) ) {

        out$Description <- paste0(
          'Scores for the MEEQ Relaxation and Tension Reduction ',
          'subscale - measure of the degree of expectancy of ',
          'relaxation and tension reduction resulting from marijuana use'
        )

        out$Units <- 'Summed score'

        out$Subscale <- list(
          name = 'Relaxation and Tension Reduction',
          n_items = 8,
          range = c( Min = 8, Max = 40 ),
          cut_off = NA,
          interpretation = paste0(
            'Higher scores indicate greater expectancy of relaxation ',
            'and tension reduction due to marijuana use'
          )
        )

        # Close 'Relaxation and Tension Reduction'
      }

      ### Social and Sexual Facilitation
      if ( subscale %in% c( 'Social and Sexual Facilitation' ) ) {

        out$Description <- paste0(
          'Scores for the MEEQ Social and Sexual Facilitation ',
          'subscale - measure of the degree of expectancy of ',
          'social and sexual facilitation resulting from marijuana use'
        )

        out$Units <- 'Summed score'

        out$Subscale <- list(
          name = 'Social and Sexual Facilitation',
          n_items = 9,
          range = c( Min = 9, Max = 45 ),
          cut_off = NA,
          interpretation = paste0(
            'Higher scores indicate greater expectancy of social and ',
            'sexual facilitation due to marijuana use'
          )
        )

        # Close 'Social and Sexual Facilitation'
      }
      ### Perceptual and Cognitive Enhancement
      if ( subscale %in% c( 'Perceptual and Cognitive Enhancement' ) ) {

        out$Description <- paste0(
          'Scores for the MEEQ Perceptual and Cognitive Enhancement ',
          'subscale - measure of the degree of expectancy of perceptual ',
          'and cognitive enhancement resulting from marijuana use'
        )

        out$Units <- 'Summed score'

        out$Subscale <- list(
          name = 'Perceptual and Cognitive Enhancement',
          n_items = 8,
          range = c( Min = 8, Max = 40 ),
          cut_off = NA,
          interpretation = paste0(
            'Higher scores indicate greater expectancy of ',
            'perceptual and cognitive enhancement due to marijuana use'
          )
        )

        # Close 'Perceptual and Cognitive Enhancement'
      }

      ### Global Negative Effects
      if ( subscale %in% c( 'Global Negative Effects' ) ) {

        out$Description <- paste0(
          'Scores for the MEEQ Global Negative Effects subscale - measure ',
          'of the degree of expectancy of negative effects resulting ',
          'from marijuana use '
        )

        out$Units <- 'Summed score'

        out$Subscale <- list(
          name = 'Global Negative Effects',
          n_items = 9,
          range = c( Min = 9, Max = 45 ),
          cut_off = NA,
          interpretation = paste0(
            'Higher scores indicate greater expectancy of negative ',
            'effects due to marijuana use  '
          )
        )

        # Close 'Global Negative Effects'
      }

      ### Craving and Physical Effects
      if ( subscale %in% c( 'Craving and Physical Effects' ) ) {

        out$Description <- paste0(
          'Scores for the MEEQ Craving and Physical Effects ',
          'subscale - measure of the degree of expectancy of ',
          'craving and physical effects resulting from marijuana use'
        )

        out$Units <- 'Summed score'

        out$Subscale <- list(
          name = 'Craving and Physical Effects',
          n_items = 6,
          range = c( Min = 6, Max = 30 ),
          cut_off = NA,
          interpretation = paste0(
            'Higher scores indicate greater expectancy of craving and ',
            'physical effects due to marijuana use'
          )
        )

        # Close 'Craving and Physical Effects'
      }
      # Close 'Subscales'
    }

    # Close 'MEEQ'
  }

  #### 1.23) MMM ####
  if ( abbreviation == 'MMM' ) {

    ### Overall

    out$Description <- NULL

    out$Units <- NULL

    out$Scale <- list(
      name = 'Marijuana Motives Measure',
      n_items = 25,
      range = c( Min = NA, Max = NA ),
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
      ),
      reference_identifier = c(
        DOI = "10.1037/0022-0167.45.3.265"
      ),
      interpretation = "",
      values_and_labels = list(
        content = 1:5,
        additional_content = c(
          "Almost never/never",
          "Some of the time",
          "Half of the time",
          "Most of the time",
          "Almost always/always"
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

    # Close 'MMM'
  }

  #### 1.24) MPS ####
  if ( abbreviation == 'MPS' ) {

    out$Description <- paste0(
      'Scores for the MPS – measure of potential negative effects of ',
      'marijuana on subject’s wellbeing'
    )

    out$Units <- 'Summed score'

    out$Scale <- list(
      name = 'Marijuana Problem Scale',
      n_items = 19,
      range = c( Min = 0, Max = 19 ),
      abbreviation = 'MPS',
      cut_off = NA,
      reference = c(
        paste0(
          "Stephens, R. S., Roffman, R. A., & Curtin, L. (2000). ",
          "Comparison of extended versus brief treatments for ",
          "marijuana use. Journal of Consulting and Clinical ",
          "Psychology, 68 (5), 898–908. ",
          "https://doi.org/10.1037/0022-006X.68.5.898"
        ),
        paste0(
          "Stephens, R. S., Roffman, R. A., & Simpson, E. E. ",
          "(1994). Treating adult marijuana dependence: A test ",
          "of the relapse prevention model. Journal of Consulting ",
          "and Clinical Psychology, 62 (1), 92–99. ",
          "https://doi.org/10.1037/0022-006X.62.1.92"
        )
      ),
      reference_identifier = c(
        DOI = "10.1037/0022-006X.68.5.898",
        DOI = "10.1037/0022-006X.62.1.92"
      ),
      interpretation = paste0(
        'Higher scores indicate more serious problems associated with ',
        'marijuana use'
      ),
      values_and_labels = ""
    )

    # Close 'MPS'
  }

  #### 1.25) MCQ ####
  if ( abbreviation == 'MCQ' ) {

    out$Description <- paste0(
      'Scores for the MCQ – measure of whether the participant prefers ',
      'smaller immediate rewards over delayed larger rewards'
    )

    out$Units <- 'Rate of Discounting (k)'

    out$Scale <- list(
      name = 'Monetary Choice Questionnaire',
      n_items = 27,
      range = c( Min = 0.0, Max = 0.5 ),
      abbreviation = 'MCQ',
      cut_off = NA,
      reference = c(
        paste0(
          "Kirby, K. N., Petry, N. M., & Bickel, W. K. (1999). Heroin ",
          "addicts have higher discount rates for delayed rewards ",
          "than non-drug-using controls. Journal of Experimental ",
          "Psychology: General, 128 (1), 78-87. ",
          "https://doi.org/10.1037//0096-3445.128.1.78"
        )
      ),
      reference_identifier = c(
        DOI = "10.1037//0096-3445.128.1.78"
      ),
      interpretation = paste0(
        'Higher k values (higher discounting rates) indicate higher ',
        'level of impulsivity (preference for immediate rewards)'
      ),
      values_and_labels = ""
    )

    # Close 'MCQ'
  }

  #### 1.26) MASQ ####
  if ( abbreviation == 'MASQ' ) {

    ### Overall

    out$Description <- NULL

    out$Units <- NULL

    out$Scale <- list(
      name = 'Mood and Anxiety Symptom Questionnaires',
      n_items = 62,
      range = c( Min = NA, Max = NA ),
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
          "symptom questionnaire. Unpublished manuscript. ",
          "https://doi.org/10.1037/t13679-000"
        )
      ),
      reference_identifier = c(
        DOI = "10.1037/0021-843X.104.1.3",
        DOI = "10.1037/t13679-000"
      ),
      interpretation = "",
      values_and_labels = list(
        content = 1:5,
        additional_content = c(
          "Not at all/slightly",
          "A little",
          "Moderately",
          "Quite a bit",
          "Extremely"
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

    # Close 'MASQ'
  }

  #### 1.27) MISS ####
  if ( abbreviation %in% c( 'MISS' ) ) {

    ### Overall

    out$Description <- paste0(
      'Scores for the MISS – measure of suggestibility from ',
      'influence of media or people or immediate events'
    )

    out$Units <- "Summed score"

    out$Scale <- list(
      name = 'Multidimensional Iowa Suggestibility Scale',
      n_items = 64,
      range = c( Min = 64, Max = 320 ),
      abbreviation = 'MISS',
      cut_off = NA,
      reference = c(
        paste0(
          "Kotov, R. I., Bellman, S. B., & Watson, D. B. (2004). ",
          "MISS: Multidimensional Iowa Suggestibility Scale Brief ",
          "Manual. Stony Brook University. https://",
          "medicine.stonybrookmedicine.edu/",
          "system/files/MISSBriefManual.pdf"
        )
      ),
      reference_identifier = c(
        URL = paste0(
          "https://",
          "medicine.stonybrookmedicine.edu/",
          "system/files/MISSBriefManual.pdf"
        )
      ),
      interpretation = paste0(
        "Higher scores indicate a greater degree of overall ",
        "suggestibility from influence of ",
        "media or people or immediate events"
      ),
      values_and_labels = list(
        content = 1:5,
        additional_content = c(
          "Not at all or very slightly",
          "A litte",
          "Somewhat",
          "Quite a bit",
          "A lot"
        )
      )
    )

    ### Subscales
    if ( subscale != '' ) {

      ### Consumer Suggestibility
      if ( subscale %in% c( 'Consumer Suggestibility' ) ) {

        out$Description <- paste0(
          'Scores for the MISS Consumer Suggestibility subscale – measure ',
          'of the level of consumer suggestibility of the subject'
        )

        out$Units <- 'Summed score'

        out$Subscale <- list(
          name = 'Consumer Suggestibility',
          n_items = 11,
          range = c( Min = 11, Max = 55 ),
          cut_off = NA,
          interpretation = paste0(
            'Higher scores indicate higher consumer suggestibility'
          )
        )

        # Close 'Consumer Suggestibility'
      }

      ### Persuadability
      if ( subscale %in% c( 'Persuadability' ) ) {

        out$Description <- paste0(
          'Scores for the MISS Persuadability subscale – measure of the ',
          'persuadability of the subject '
        )

        out$Units <- 'Summed score'

        out$Subscale <- list(
          name = 'Persuadability',
          n_items = 14,
          range = c( Min = 14, Max = 70 ),
          cut_off = NA,
          interpretation = paste0(
            'Higher scores indicate higher persuadability'
          )
        )

        # Close 'Persuadability'
      }

      ### Physiological Suggestibility
      if ( subscale %in% c( 'Physiological Suggestibility' ) ) {

        out$Description <- paste0(
          'Scores for the MISS Consumer Physiological Suggestibility ',
          'subscale – measure of the physiological suggestibility of ',
          'the subject'
        )

        out$Units <- 'Summed score'

        out$Subscale <- list(
          name = 'Physiological Suggestibility',
          n_items = 12,
          range = c( Min = 12, Max = 60 ),
          cut_off = NA,
          interpretation = paste0(
            'higher scores indicate higher physiological suggestibility'
          )
        )

        # Close 'Physiological Suggestibility'
      }

      ### Physiological Reactivity
      if ( subscale %in% c( 'Physiological Reactivity' ) ) {

        out$Description <- paste0(
          'Scores for the MISS Physiological Reactivity subscale – measure ',
          'of the physiological reactivity of subject'
        )

        out$Units <- 'Summed score'

        out$Subscale <- list(
          name = 'Physiological Reactivity',
          n_items = 13,
          range = c( Min = 13, Max = 65 ),
          cut_off = NA,
          interpretation = paste0(
            'Higher scores indicate greater physiological reactivity'
          )
        )

        # Close 'Physiological Reactivity'
      }

      ### Peer Conformity
      if ( subscale %in% c( 'Peer Conformity' ) ) {

        out$Description <- paste0(
          'Scores for the MISS Peer Conformity subscale – measure of the ',
          'peer conformity of subject'
        )

        out$Units <- 'Summed score'

        out$Subscale <- list(
          name = 'Peer Conformity',
          n_items = 14,
          range = c( Min = 14, Max = 70 ),
          cut_off = NA,
          interpretation = paste0(
            'Higher scores indicate greater peer conformity'
          )
        )

        # Close 'Peer Conformity'
      }

      ### Short Suggestibility Scale
      if ( subscale %in% c( 'Short Suggestibility Scale' ) ) {

        out$Description <- paste0(
          'Scores for the MISS Short Suggestibility Scale ',
          'subscale – measure of the subject’s general suggestibility trait'
        )

        out$Units <- 'Summed score'

        out$Subscale <- list(
          name = 'Short Suggestibility Scale',
          n_items = 21,
          range = c( Min = 21, Max = 105 ),
          cut_off = NA,
          interpretation = paste0(
            'Higher scores indicate greater suggestibility'
          )
        )

        # Close 'Short Suggestibility Scale'
      }

      ### Mental Control
      if ( subscale %in% c( 'Mental Control' ) ) {

        out$Description <- paste0(
          'Scores for the MISS Mental Control subscale – measure of ',
          'the subject’s ability for mental control'
        )

        out$Units <- 'Summed score'

        out$Subscale <- list(
          name = 'Mental Control',
          n_items = 15,
          range = c( Min = 15, Max = 75 ),
          cut_off = NA,
          interpretation = paste0(
            'Higher scores indicate greater mental control ability'
          )
        )

        # Close 'Mental Control'
      }

      ### Unpersuadability
      if ( subscale %in% c( 'Unpersuadability' ) ) {

        out$Description <- paste0(
          'Scores for the MISS Unpersuadability subscale – measure ',
          'of the subject’s resistance to persuasion'
        )

        out$Units <- 'Summed score'

        out$Subscale <- list(
          name = 'Unpersuadability',
          n_items = 16,
          range = c( Min = 16, Max = 80 ),
          cut_off = NA,
          interpretation = paste0(
            'Higher scores indicate greater resistance to persuasion'
          )
        )

        # Close 'Unpersuadability'
      }

      # Close 'Subscales'
    }

    # Close 'MISS'
  }

  #### 1.28) PCS ####

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
      reference_identifier = c(
        DOI = "10.1037/1040-3590.7.4.524"
      ),
      interpretation = paste0(
        "Higher scores indicate that a subject engages in ",
        "a greater degree of catastrophizing about pain"
      ),
      values_and_labels = ""
    )

    # Close 'PCS'
  }

  ### Percentages
  if ( abbreviation %in% 'PCS (%)' ) {

    out$Description <- paste0(
      "Percentages for the PCS - measure of the degree to ",
      "which a subject engages in pain catastrophizing"
    )

    out$Units <- "Percentage"

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
      reference_identifier = c(
        DOI = "10.1037/1040-3590.7.4.524"
      ),
      interpretation = paste0(
        "Higher scores indicate that a subject engages in ",
        "a greater degree of catastrophizing about pain"
      ),
      values_and_labels = ""
    )

    # Close 'PCS (%)'
  }

  #### 1.29) PSS ####
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
      reference_identifier = c(
        DOI = "10.2307/2136404",
        "ISBN: 080393162X or 9780803931626"
      ),
      interpretation = paste0(
        "Higher scores indicate a greater degree of perceived stress"
      ),
      values_and_labels = ""
    )

    # Close 'PSS'
  }

  #### 1.30) PDI ####
  if ( abbreviation %in% c( 'PDI' ) ) {

    ### Overall

    out$Description <- paste0(
      "Scores for the PDI - measure of the degree to ",
      "which subjects are proine to delusional ideation"
    )

    out$Units <- "Summed score"

    out$Scale <- list(
      name = 'Peters et al. Delusions Inventory',
      n_items = 63,
      range = c( Min = 0, Max = 336 ),
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
      reference_identifier = c(
        DOI = "10.1093/oxfordjournals.schbul.a033401",
        DOI = "10.1093/oxfordjournals.schbul.a007116"
      ),
      interpretation = paste0(
        "Higher scores indicate greater proneness to delusional ",
        "ideation"
      ),
      values_and_labels = ""
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
          range = c( Min = 0, Max = 105 ),
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
          range = c( Min = 0, Max = 105 ),
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
          range = c( Min = 0, Max = 105 ),
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
          range = c( Min = 0, Max = 21 ),
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
          range = c( Min = 0, Max = 336 ),
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

  #### 1.31) PSQI ####
  if ( abbreviation %in% c( 'PSQI' ) ) {

    ### Overall

    out$Description <- paste0(
      'Global score for the PSQI – measure of the ',
      'subjective sleep quality of a person'
    )

    out$Units <- "Summed score"

    out$Scale <- list(
      name = 'Pittsburgh Sleep Quality Index',
      n_items = 19,
      range = c( Min = 0, Max = 21 ),
      abbreviation = 'PSQI',
      cut_off = NA,
      reference =
        paste0(
          'Buysse,D.J., Reynolds,C.F., Monk,T.H., ',
          'Berman,S.R., & Kupfer,D.J. (1989). The Pittsburgh Sleep',
          ' Quality Index (PSQI): A new instrument for psychiatric',
          ' research and practice. Psychiatry Research, 28 (2), 193-213. ',
          'https://doi.org/10.1016/0165-1781(89)90047-4'
        ),
      reference_identifier = c(
        DOI = "10.1016/0165-1781(89)90047-4"
      ),
      interpretation = paste0(
        "Higher scores indicate worse sleep quality"
      )
    )

    ### Subscales
    if ( subscale != '' ) {

      ### Component 1
      if ( subscale %in% c( 'Component 1' ) ) {

        out$Description <- paste0(
          'Scores for the PSQI component 1 subscale – measure of the ',
          'subjective sleep quality of subject'
        )

        out$Units <- "Ordinal ranking"

        out$Subscale <- list(
          name = 'Component 1',
          n_items = 1,
          range = c( Min = 0, Max = 3 ),
          cut_off = NA,
          interpretation = paste0(
            'A score of 0 indicates subjectively very high sleep ',
            'quality - a score of 3 indicates subjectively very ',
            'poor sleep quality'
          )
        )

        # Close 'Component 1'
      }

      ### Component 2
      if ( subscale %in% c( 'Component 2' ) ) {

        out$Description <- paste0(
          'Scores for the PSQI component 2 subscale – measure of the ',
          'sleep latency of subject '
        )

        out$Units <- "Ordinal ranking"

        out$Subscale <- list(
          name = 'Component 2',
          n_items = 2,
          range = c( Min = 0, Max = 3 ),
          cut_off = NA,
          interpretation = paste0(
            'A score of 0 indicates very low sleep latency - a score ',
            'of 3 indicates very high sleep latency'
          )
        )

        # Close 'Component 2'
      }

      ### Component 3
      if ( subscale %in% c( 'Component 3' ) ) {

        out$Description <- paste0(
          'Scores for the PSQI component 3 subscale – measure of the ',
          'sleep duration of subject'
        )

        out$Units <- "Ordinal ranking"

        out$Subscale <- list(
          name = 'Component 3',
          n_items = 1,
          range = c( Min = 0, Max = 3 ),
          cut_off = NA,
          interpretation = paste0(
            'A score of 0 indicates sufficient sleep duration (>7 ',
            'hours) - a score of 3 indicates insufficient sleep ',
            'duration (<5 hours)'
          )
        )

        # Close 'Component 3'
      }
      ### Component 4
      if ( subscale %in% c( 'Component 4' ) ) {

        out$Description <- paste0(
          'Scores for the PSQI component 4 subscale – measure of the ',
          'habitual sleep efficiency of subject'
        )

        out$Units <- "Ordinal ranking"

        out$Subscale <- list(
          name = 'Component 4',
          n_items = 3,
          range = c( Min = 0, Max = 3 ),
          cut_off = NA,
          interpretation = paste0(
            'A score of 0 indicates high habitual sleep efficiency ',
            '(most time in bed is spent sleeping) - a score of 3 ',
            'indicates low habitual sleep efficiency (less than ',
            '65% of time in bed is spent sleeping)'
          )
        )

        # Close 'Component 4'
      }

      ### Component 5
      if ( subscale %in% c( 'Component 5' ) ) {

        out$Description <- paste0(
          'Scores for the PSQI component 5 subscale – measure of the ',
          'sleep disturbances experienced by subject'
        )

        out$Units <- "Ordinal ranking"

        out$Subscale <- list(
          name = 'Component 5',
          n_items = 9,
          range = c( Min = 0, Max = 3 ),
          cut_off = NA,
          interpretation = paste0(
            'A score of 0 indicates no sleep disturbances - a score ',
            'of 3 indicates very significant sleep disturbances'
          )
        )

        # Close 'Component 5'
      }

      ### Component 6
      if ( subscale %in% c( 'Component 6' ) ) {

        out$Description <- paste0(
          'Scores for the PSQI component 6 subscale – measure of the ',
          'subject’s use of sleeping medication'
        )

        out$Units <- "Ordinal ranking"

        out$Subscale <- list(
          name = 'Component 6',
          n_items = 1,
          range = c( Min = 0, Max = 3 ),
          cut_off = NA,
          interpretation = paste0(
            'A score of 0 indicates no use of any sleep medications in ',
            'the past month - a score of 3 indicates use of sleep ',
            'medication 3 or more times per week'
          )
        )

        # Close 'Component 6'
      }
      ### Component 7
      if ( subscale %in% c( 'Component 7' ) ) {

        out$Description <- paste0(
          'Scores for the PSQI component 7 subscale – measure of the ',
          'daytime dysfunction experienced by subject'
        )

        out$Units <- "Ordinal ranking"

        out$Subscale <- list(
          name = 'Component 7',
          n_items = 2,
          range = c( Min = 0, Max = 3 ),
          cut_off = NA,
          interpretation = paste0(
            'A score of 0 indicates no daytime dysfunction - a ',
            'score of 3 indicates severe daytime dysfunction'
          )
        )

        # Close 'Component 7'
      }

      # Close 'Subscales'
    }

    # Close 'PSQI'
  }

  #### 1.32) PQB ####
  if ( abbreviation %in% c( 'PQB' ) ) {

    ### Overall

    out$Description <- paste0(
      'Total scores for the PBQ - measure of a ',
      'patient’s reported prodromal symptoms'
    )

    out$Units <- "Summed scores"

    out$Scale <- list(
      name = 'Prodromal Questionnaire-Brief',
      n_items = 21,
      range = c( Min = 0, Max = 21 ),
      abbreviation = 'PQB',
      cut_off = NA,
      reference = paste0(
        'Loewy, R. L., Pearson, R., Vinogradov, S., Bearden, C. E., ',
        '& Cannon, T. D. (2011). Psychosis risk screening with the ',
        'Prodromal Questionnaire—brief version (PQ-B). Schizophrenia',
        ' research, 129(1), 42–46.',
        ' https://doi.org/10.1016/j.schres.2011.03.029'
      ),
      reference_identifier = c(
        DOI = "10.1016/j.schres.2011.03.029"
      ),
      interpretation =
        'Higher scores indicate experiencing more prodromal symptoms',
      values_and_labels = ""
    )

    ### Subscales
    if ( subscale != '' ) {

      ### Distress
      if ( subscale %in% c( 'Distress' ) ) {

        out$Description <- paste0(
          'Scores for the PQ-B Distress subscale – measure of the ',
          'level of distress or impairment associated with the ',
          'endorsed positive symptoms'
        )

        out$Units <- 'Summed score'

        out$Subscale <- list(
          name = 'Distress',
          n_items = 21,
          range = c( Min = 21, Max = 105 ),
          cut_off = NA,
          interpretation = paste0(
            'Higher scores indicate higher distress caused by endorsed ',
            'positive symptoms'
          )
        )

        # Close 'Distress'
      }

      # Close 'Subscales'
    }

    # Close 'PQB'
  }

  #### 1.33) PERS ####
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
      abbreviation = 'PERS',
      cut_off = NA,
      reference = paste0(
        'Salyers, M. P., Brennan, M., & Kean, J. (2013). Provider ',
        'Expectations for Recovery Scale: Refining a measure of ',
        'provider attitudes. Psychiatric Rehabilitation Journal, 36(3), 153. ',
        'https://doi.org/10.1037/prj0000010'
      ),
      reference_identifier = c(
        DOI = "10.1037/prj0000010"
      ),
      interpretation = paste0(
        'Higher scores indicate greater clinican optimism about ',
        'patient recovery'
      ),
      values_and_labels = ""
    )

    # Close 'PERS'
  }

  #### 1.34) SCARED ####
  if ( abbreviation %in% c( 'SCARED' ) ) {

    ### Overall

    out$Description <- paste0(
      'Total score for the SCARED - measure of ',
      'overall degree of anxiety'
    )

    out$Units <- "Summed score"

    out$Scale <- list(
      name = 'Screen for Child Anxiety Related Emotional Disorders',
      n_items = 66,
      range = c( Min = 0, Max = 132 ),
      abbreviation = 'SCARED',
      cut_off = NA,
      reference = c(
        paste0(
          "Behren,. B., Swetlitz, C., Pine, D. S., & Pagliaccio, D. ",
          "(2019). The Screen for Child Anxiety Related Emotional ",
          "Disorders (SCARED): Informant discrepancy, measurement ",
          "invariance, and test-retest reliability. Child ",
          "Psychiatry Human Development, 50 (3), 473-482. ",
          "https://doi.org/10.1007/s10578-018-0854-0"
        ),
        paste0(
          'Muris, P., & Steerneman, P. (2001). The revised ',
          'version of the Screen for Child Anxiety Related Emotional ',
          'Disorders (SCARED-R): First evidence for its reliability ',
          'and validity in a clinical sample. The British journal ',
          'of Clinical Psychology, 40 (1), 35–44. ',
          'https://doi.org/10.1348/014466501163463'
        )
      ),
      reference_identifier = c(
        DOI = "10.1348/014466501163463"
      ),
      interpretation = paste0(
        "Higher scores indicate a greater degree of overall anxiety"
      ),
      values_and_labels = list(
        content = 0:2,
        additional_content = c(
          "Almost never",
          'Sometimes',
          "Often"
        )
      )
    )

    ### Subscales
    if ( subscale != '' ) {

      ### Panic Disorder
      if ( subscale %in% c( 'Panic Disorder' ) ) {

        out$Description <- paste0(
          'Scores for the SCARED Panic Disorder subscale– measure of ',
          'the presence of panic disorder symptoms in subject'
        )

        out$Units <- 'Summed score'

        out$Subscale <- list(
          name = 'Panic Disorder',
          n_items = 13,
          range = c( Min = 0, Max = 26 ),
          cut_off = NA,
          interpretation = paste0(
            'Higher scores indicate more severe symptoms of panic disorder'
          )
        )

        # Close 'Panic Disorder'
      }

      ### Separation Anxiety Disorder
      if ( subscale %in% c( 'Separation Anxiety Disorder' ) ) {

        out$Description <- paste0(
          'Scores for the Separation Anxiety Disorder (including ',
          'school phobia) subscale– measure of the presence of ',
          'separation anxiety disorder symptoms in subject'
        )

        out$Units <- 'Summed score'

        out$Subscale <- list(
          name = 'Separation Anxiety Disorder',
          n_items = 12,
          range = c( Min = 0, Max = 24),
          cut_off = NA,
          interpretation = paste0(
            'Higher scores indicate more severe symptoms of ',
            'separation anxiety disorder'
          )
        )

        # Close 'Separation Anxiety Disorder'
      }

      ### Generalized Anxiety Disorder
      if ( subscale %in% c( 'Generalized Anxiety Disorder' ) ) {

        out$Description <- paste0(
          'Scores for the SCARED Generalized Anxiety Disorder ',
          'subscale – measure of the presence of GAD symptoms in subject'
        )

        out$Units <- 'Summed Score'

        out$Subscale <- list(
          name = 'Generalized Anxiety Disorder',
          n_items = 9,
          range = c( Min = 0, Max = 18 ),
          cut_off = NA,
          interpretation = paste0(
            'Higher scores indicate more severe symptoms of generalized ',
            'anxiety disorder'
          )
        )

        # Close 'Generalized Anxiety Disorder'
      }

      ### Social Phobia
      if ( subscale %in% c( 'Social Phobia' ) ) {

        out$Description <- paste0(
          'Scores for the SCARED Social Phobia subscale – measure of ',
          'the presence of social phobia symptoms in subject'
        )

        out$Units <- 'Summed Score'

        out$Subscale <- list(
          name = 'Social Phobia',
          n_items = 4,
          range = c( Min = 0, Max = 8 ),
          cut_off = NA,
          interpretation = paste0(
            'Higher scores indicate more severe symptoms of social phobia'
          )
        )

        # Close 'Social Phobia'
      }

      ### Specific Phobias
      if ( subscale %in% c( 'Specific Phobias' ) ) {

        out$Description <- paste0(
          'Scores for the SCARED Specific Phobias subscale– measure ',
          'of the presence of specific phobias in subject'
        )

        out$Units <- 'Summed score'

        out$Subscale <- list(
          name = 'Specific Phobias',
          n_items = 15,
          range = c( Min = 0, Max = 30 ),
          cut_off = NA,
          interpretation = paste0(
            'Higher scores indicate more severe symptoms of specific phobia'
          )
        )

        # Close 'Specific Phobias'
      }

      ### Obsessive Compulsive Disorder
      if ( subscale %in% c( 'Obsessive Compulsive Disorder' ) ) {

        out$Description <- paste0(
          'Scores for the SCARED Obsessive Compulsive Disorder ',
          'subscale – measure of the presence of OCD symptoms in subject'
        )

        out$Units <- 'Summed score'

        out$Subscale <- list(
          name = 'Obsessive Compulsive Disorder',
          n_items = 9,
          range = c( Min = 0, Max = 18 ),
          cut_off = NA,
          interpretation = paste0(
            'Higher scores indicate more severe symptoms of ',
            'obsessive compulsive disorder'
          )
        )

        # Close 'Obsessive Compulsive Disorder'
      }

      ### Traumatic Stress Disorder
      if ( subscale %in% c( 'Traumatic Stress Disorder' ) ) {

        out$Description <- paste0(
          'Scores for the SCARED Traumatic Stress Disorder ',
          'subscale – measure of the presence of traumatic ',
          'stress disorder symptoms in subject'
        )

        out$Units <- 'Summed score'

        out$Subscale <- list(
          name = 'Traumatic Stress Disorder',
          n_items = 4,
          range = c( Min = 0, Max = 8 ),
          cut_off = NA,
          interpretation = paste0(
            'Higher scores indicate more severe symptoms of ',
            'traumatic stress disorder'
          )
        )

        # Close 'Traumatic Stress Disorder'
      }

      # Close 'Subscales'
    }

    # Close 'SCARED'
  }

  #### 1.35) SF-12 ####
  if ( abbreviation == 'SF-12' ) {

    ### Overall

    out$Description <- NULL

    out$Units <- NULL

    out$Scale <- list(
      name = 'Short-Form 12-item Health Survey',
      n_items = 12,
      range = c( Min = NA, Max = NA ),
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
      ),
      reference_identifier = c(
        DOI = "10.1348/014466501163463"
      ),
      interpretation = "",
      values_and_labels = "10.1097/00005650-199603000-00003"
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
          "T-score"
        )

        out$Subscale <- list(
          name = 'General mental health',
          n_items = 6,
          range = c( Mean = 50, SD = 10 ),
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
          "T-score"
        )

        out$Subscale <- list(
          name = 'General physical health',
          n_items = 6,
          range = c( Mean = 50, SD = 10 ),
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

  #### 1.36) TIPI ####
  if ( abbreviation %in% c( 'TIPI' ) ) {

    ### Overall

    out$Description <- NULL

    out$Units <- NULL

    out$Scale <- list(
      name = 'Ten Item Personality Inventory',
      n_items = 10,
      range = c( Min = NA, Max = NA ),
      abbreviation = 'TIPI',
      cut_off = NA,
      reference = paste0(
        'Gosling, S. D., Rentfrow, P. J., & Swann, W. B., Jr. ',
        '(2003). A very brief measure of the Big-Five personality ',
        'domains. Journal of Research in Personality, 37(6),  ',
        '504–528. https://doi.org/10.1016/S0092-6566(03)00046-1'
      ),
      reference_identifier = c(
        DOI = "10.1016/S0092-6566(03)00046-1"
      ),
      interpretation = "",
      values_and_labels = ""
    )

    ### Subscales
    if ( subscale != '' ) {

      ### Extraversion
      if ( subscale %in% c( 'Extraversion' ) ) {

        out$Description <- paste0(
          'Scores for the TIPI extraversion subscale– measure of ',
          'the subject’s level of extraversion'
        )

        out$Units <- 'Summed score'

        out$Subscale <- list(
          name = 'Extraversion',
          n_items = 2,
          range = c( Min = 2, Max = 14 ),
          cut_off = NA,
          interpretation = paste0(
            'Higher scores indicate higher level of extraversion; ',
            'lower scores indicate higher level of introversion'
          )
        )

        # Close 'Extraversion'
      }

      ### Agreeableness
      if ( subscale %in% c( 'Agreeableness' ) ) {

        out$Description <- paste0(
          'Scores for the TIPI agreeableness subscale – measure of ',
          'the subject’s level of agreeableness'
        )

        out$Units <- 'Summed score'

        out$Subscale <- list(
          name = 'Agreeableness',
          n_items = 2,
          range = c( Min = 2, Max = 14 ),
          cut_off = NA,
          interpretation = paste0(
            'Higher scores indicate higher level of agreeableness'
          )
        )

        # Close 'Agreeableness'
      }

      ### Conscientiousness
      if ( subscale %in% c( 'Conscientiousness' ) ) {

        out$Description <- paste0(
          'Scores for the TIPI conscientiousness subscale – measure ',
          'of the subject’s level of conscientiousness'
        )

        out$Units <- 'Summed Score'

        out$Subscale <- list(
          name = 'Conscientiousness',
          n_items = 2,
          range = c( Min = 2, Max = 14 ),
          cut_off = NA,
          interpretation = paste0(
            'Higher scores indicate higher level of conscientiousness'
          )
        )

        # Close 'Conscientiousness'
      }
      ### Emotional Stability
      if ( subscale %in% c( 'Emotional Stability' ) ) {

        out$Description <- paste0(
          'Scores for the TIPI emotional stability subscale – measure ',
          'of the subject’s level of emotional stability'
        )

        out$Units <- 'Summed Score'

        out$Subscale <- list(
          name = 'Emotional Stability',
          n_items = 2,
          range = c( Min = 2, Max = 14 ),
          cut_off = NA,
          interpretation = paste0(
            'Higher scores indicate higher level of emotional stability'
          )
        )

        # Close 'Emotional Stability'
      }

      ### Openness
      if ( subscale %in% c( 'Openness' ) ) {

        out$Description <- paste0(
          'Scores for the TIPI openness subscale – measure of the ',
          'subject’s level of openness to experiences'
        )

        out$Units <- 'Summed score'

        out$Subscale <- list(
          name = 'Openness',
          n_items = 2,
          range = c( Min = 2, Max = 14 ),
          cut_off = NA,
          interpretation = paste0(
            'Higher scores indicate higher level of openness to experiences'
          )
        )

        # Close 'Openness'
      }

      # Close 'Subscales'
    }

    # Close 'TIPI'
  }

  #### 1.37) CES-D ####
  if ( abbreviation == 'CES-D' ) {

    out$Description <- paste0(
      'Scores for the CES-D - measure ',
      'of depressive symptomatology'
    )

    out$Units <- "Summed score"

    out$Scale <- list(
      name = 'The Center for Epidemiologic Studies Depression Scale',
      n_items = 20,
      range = c( Min = 0, Max = 60 ),
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
      reference_identifier = c(
        DOI = "10.1177/014662167700100306"
      ),
      interpretation = paste0(
        "Higher scores indicate a greater degree of depression"
      ),
      values_and_labels = list(
        content = 0:3,
        additional_content = c(
          "Rarely or none of the time (less than 1 day )",
          "Some or a little of the time (1-2 days)",
          "Occasionally or a moderate amount of time (3-4 days)",
          " Most or all of the time (5-7 days)"
        )
      )
    )

    # Close 'CES-D'
  }

  #### 1.38) UPPS-P ####
  if ( abbreviation == 'UPPS-P' ) {

    ### Overall

    out$Description <- NULL

    out$Units <- NULL

    out$Scale <- list(
      name = 'UPPS-P Impulsive Behavior Scale',
      n_items = 59,
      range = c( Min = NA, Max = NA ),
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
      ),
      reference_identifier = c(
        DOI = "10.1016/S0191-8869(00)00064-7",
        DOI = "10.1037/1040-3590.19.1.107"
      ),
      values_and_labels = list(
        content = 1:4,
        additional_content = c(
          "Agree strongly",
          "Agree somewhat",
          "Disagree somewhat",
          "Disagree strongly"
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

    # Close 'UPPS-P'
  }

  #### 1.39) WTAR ####
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

  #### 1.40) WHODAS 2.0 CHECK!!! ####
  if ( abbreviation %in% c( 'WHODAS', 'WHODAS 2.0' ) ) {

    ### Overall
    out$Scale <- list(
      name = 'World Health Organization Disability Assement Scale 2.0',
      n_items = 36,
      range = c( Min = NA, Max = NA ),
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
          'Item response theory based scores for the ',
          'WHODAS 2.0 - measure of degree of disability ',
          'which negatively impacts daily life'
        )

        out$Units <- "Scaled score"

        out$Subscale <- list(
          name = 'IRT-based scores',
          n_items = 36,
          range = c( Min = 0, Max = 100 ),
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
          range = c( Min = 0, Max = 100 ),
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


  #### 1.41) Y-PSC ####
  if ( abbreviation %in% c( 'Y-PSC' ) ) {

    out$Description <- paste0(
      'Scores for the Y-PSC - measure ',
      'of psychosocial functioning in youth'
    )

    out$Units <- "Summed score"

    out$Scale <- list(
      name = 'Pediatric Symptom Checklist - Youth Report',
      n_items = 35,
      range = c( Min = 0, Max = 70 ),
      abbreviation = 'Y-PSC',
      cut_off = c(
        Psychological_impairment = 30
      ),
      reference = paste0(
        'Jellinek, M. S., J. M. Murphy, J. Robinson, A. Feins, ',
        'S. Lamb and T. Fenton (1988). The Pediatric Symptom ',
        'Checklist: Screening school-age children for ',
        'psychosocial dysfunction. The Journal of Pediatrics, ',
        '(112) (2), 201-209. ',
        'https://doi.org/10.1016/S0022-3476(88)80056-8'
      ),
      interpretation = paste0(
        "Higher scores indicate greater impairment in ",
        "psychosocial functioning"
      )
    )

    # Close 'Y-PSC'
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

#### 2) camr_add_inventory_item ####
#' Add Individual Inventory Item With Codebook Entry
#'
#' Convenience function to add a column with scores
#' for an individual item or question from an
#' inventory, survey, or questionnaire. Can also
#' add the associated codebook entry for the item.
#'
#' @param dtf_original A data frame with the
#'   column of scores for the given item or question.
#' @param dtf_new A data frame to which the column
#'   of scores will be added.
#' @param chr_original_column A character string, the
#'   column name in \code{dtf_original} to add.
#' @param chr_new_column A character string, the
#'   new column name to use in \code{dtf_new}.
#' @param vec_response_options A vector, the original
#'   response options. Used to recode the response
#'   options if needed.
#' @param vec_new_values A vector, the new values to
#'   replace \code{vec_response options} with. Must
#'   match in length to \code{vec_response_options}.
#' @param lst_values_and_labels A named list with
#'   \code{'content'} containing the numeric values
#'   and \code{'additional_content'} containing the
#'   labels or interpretations for the values.
#' @param chr_inventory A character string, the
#'   name of the inventory, survey, or questionnaire
#'   to which the item belongs.
#' @param chr_question A character string, the item's
#'   framing question.
#' @param chr_description A character string,
#'   allows a user to specify a custom description
#'   rather than one auto-filled using \code{chr_inventory}
#'   and \code{chr_question}.
#' @param ... Additional arguments for the
#'   [camrprojects::camr_add_codebook_entry] function.
#'
#' @returns A data frame with an additional column
#' containing the scores for the individual item
#' or question.
#'
#' @export

camr_add_inventory_item <- function(
    dtf_original,
    dtf_new,
    chr_original_column,
    chr_new_column,
    vec_response_options = NULL,
    vec_new_values = NULL,
    lst_values_and_labels = "",
    chr_inventory = "",
    chr_question = "",
    chr_description = "",
    ... ) {

  # Number of rows in data frame
  N_rows <- nrow( dtf_new )

  # Extract scores for specified item
  vec_scores <- dtf_original[[ chr_original_column ]]

  # If response options and new values are provided
  if ( !is.null( vec_new_values ) & !is.null( vec_response_options ) ) {

    # Initialize new vector of scores
    vec_transformed_scores <- rep( NA, N_rows )

    # Number of response options
    N_options <- length( vec_response_options )

    # Check inputs
    if ( length( vec_new_values ) != N_options ) {

      stop(
        paste0(
          "The arguments 'vec_new_values' and 'vec_response_options' ",
          "must match in length"
        )
      )

      # Close 'Check inputs'
    }

    # Loop over response options
    for ( l in 1:N_options ) {

      lgc_rows <- vec_scores %in% vec_response_options[l]
      vec_transformed_scores[ lgc_rows ] <- vec_new_values[l]

      # Close 'Loop over response options'
    }

    lst_values_and_labels <- list(
      content = vec_new_values,
      additional_content = vec_response_options
    )

    # Close 'If values and labels are provided'
  } else {

    vec_transformed_scores <- vec_scores

    # Close else for 'If values and labels are provided'
  }

  mat_new <- cbind(
    vec_transformed_scores
  )
  colnames( mat_new ) <- chr_new_column

  dtf_new <- cbind(
    dtf_new, mat_new
  )

  lgc_codebook <-
    ( chr_inventory != "" ) |
    ( chr_question != "" ) |
    ( chr_description != "" )

  chr_item_number <- ''

  # Extract item number
  if ( grepl( 'Item_', chr_new_column, fixed = TRUE ) ) {

    chr_parts <- strsplit(
      chr_new_column, split = '.', fixed = TRUE
    )[[1]]

    chr_item_number <-
      chr_parts[ grepl( 'Item_', chr_parts, fixed = TRUE ) ]
    chr_item_number <- gsub(
      'Item_', '', chr_item_number, fixed = TRUE
    )
    chr_item_number <- paste0(
      ' ', chr_item_number
    )

    # Close 'Extract item number'
  }

  # If inputs for codebook entry provided
  if ( lgc_codebook ) {

    # Remove commas
    if ( chr_question != '' ) {

      chr_question <-
        gsub( ',', ' - ', chr_question, fixed = TRUE )

      # Close 'Remove commas'
    }

    # Combine item number + inventory + question
    if ( chr_inventory != "" &
         chr_question != "" ) {

      chr_description <- paste0(
        'Response to item',
        chr_item_number,
        ' for the ',
        chr_inventory, ' asking: ',
        chr_question
      )

      # Close 'Combine item number + inventory + question'
    }

    # Combine item number + inventory
    if ( chr_inventory != "" &
         chr_question == "" ) {

      chr_description <- paste0(
        'Response to item',
        chr_item_number,
        ' for the ',
        chr_inventory
      )

      # Close 'Combine item number + inventory'
    }

    # Question
    if ( chr_inventory == "" &
         chr_question != "" ) {

      chr_description <- chr_question

      # Close 'Question'
    }

    # Create codebook entry
    dtf_new <- dtf_new |> camr_add_codebook_entry(
      chr_new_column,
      description = chr_description,
      values_and_labels = lst_values_and_labels,
      created_from = chr_original_column,
      non_standard = FALSE,
      ...
    )

    # Close 'If inputs for codebook entry provided'
  }

  return( dtf_new )
}

