# Meta-data functions
# Written by Kevin Potter
# email: kevin.w.potter@gmail.com
# Please email me directly if you
# have any questions or comments
# Last updated 2021-07-21

# Table of contents

### TO DO ###
# - Finish function documentation

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

known_scales <- function( abbreviation = NULL, subscale = '' ) {

  if ( is.null( abbreviation ) & subscale == '' ) {

    inputs <- c(
      'Accepted inputs:\n\n',
      'Hospital Anxiety Depression Scale\n',
      '  abbrevation = "HADS"\n',
      '    subscale = "Anxiety"\n',
      '    subscale = "Depression"\n\n',
      'Cannabis Use Disorder Identification Test - Revised\n',
      '  abbreviation = "CUDIT"\n\n',
      'Alcohol Use Disorders Identification Test\n',
      '  abbreviation = "AUDIT"\n\n',
      'Athens Insomnia Scale\n',
      '  abbreviation = "AIS"\n\n',
      'Brief Pain Inventory (Short form)\n',
      '  abbreviation = "BPI"\n',
      '    subscale = Severity\n',
      '    subscale = Interference\n\n',
      'Pain Catastrophizing Scale\n',
      '  abbreviation = "PCS"\n\n',
      'Perceived Stress Scale\n',
      '  abbreviation = "PSS"\n\n'
    )
    message( inputs )
  }

  out <- NULL

  #### 1.1.1) HADS ####
  if ( abbreviation %in% c( 'HADS' ) ) {

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
      )
    )

    if ( subscale != '' ) {
      out <- NULL

      if ( subscale %in% c( 'Anxiety', 'anxiety' ) ) {

        out <- list(
          name = 'Anxiety',
          n_items = 7,
          range = c( 0, 21 ),
          cut_off = c( Borderline = 8, Abnormal = 11 )
        )

      }

      if ( subscale == 'Depression' ) {

        out <- list(
          name = 'Depression',
          n_items = 7,
          range = c( 0, 21 ),
          cut_off = c( Borderline = 8, Abnormal = 11 )
        )

      }

    }

  }

  #### 1.1.2) CUDIT-R ####
  if ( abbreviation %in% c( 'CUDIT', 'CUDIT-R' ) ) {

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
      )
    )

  }

  #### 1.1.3) AUDIT ####
  if ( abbreviation == 'AUDIT' ) {

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
      )
    )

  }

  #### 1.1.4) AIS ####
  if ( abbreviation == 'AIS' ) {

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
      )
    )

  }

  #### 1.1.5) BPI ####
  if ( abbreviation %in% c( 'BPI', 'BPI-SF' ) ) {

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
      )
    )

    if ( subscale != '' ) {
      out <- NULL

      if ( subscale == 'Severity' ) {

        out <- list(
          name = 'Pain severity',
          n_items = 4,
          range = c( Min = 0, Max = 10 ),
          cut_off = NA
        )

      }

      if ( subscale == 'Interference' ) {

        out <- list(
          name = 'Pain interference',
          n_items = 7,
          range = c( Min = 0, Max = 10 ),
          cut_off = NA
        )

      }

    }

  }

  #### 1.1.6) PCS ####
  if ( abbreviation == 'PCS' ) {

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
      )
    )

  }

  #### 1.1.7) PSS ####
  if ( abbreviation == 'PSS' ) {

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
      )
    )

  }

  #### 1.1.8) SF-12 ####
  if ( abbreviation == 'SF-12' ) {

    out <- list(
      name = 'Short-Form 12 Health Survey',
      n_items = NA,
      range = c( NA, NA ),
      abbreviation = 'SF-12',
      cut_off = NA,
      reference = c(
      )
    )

    if ( subscale != '' ) {
      out <- NULL

      if ( subscale == 'Mental' ) {

        out <- list(
          name = 'General mental health',
          n_items = NA,
          range = c( Min = 0, Max = 100 ),
          cut_off = NA
        )

      }

      if ( subscale == 'Physical' ) {

        out <- list(
          name = 'General physical health',
          n_items = NA,
          range = c( Min = 0, Max = 100 ),
          cut_off = NA
        )

      }
    }

  }

  if ( is.null( out ) ) {
    stop( 'Scale or subscale not found' )
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
#' @param range ...
#' @param abbreviation A character string with the abbreviation
#'   for the scale (e.g., 'HADS' or 'CUDIT').
#' @param cut_off An optional numeric vector giving any cut-offs
#'   used in the scale (e.g., the CUDIT uses a clinical cut-off
#'   of scores at \code{8} or higher to indicate problematic
#'   cannabis use).
#' @param reference ...
#' @param subscale Logical; if \code{TRUE}, instead outputs
#'   the reporting format for subscales (consisting only
#'   of the subscale name its number of items).
#'
#' @author Kevin Potter
#'
#' @return ...
#'
#' @examples
#' scale_format(
#'   'Cannabis Use Disorder Identification Test',
#'   'CUDIT',
#'   8
#' )
#'
#' @export

scale_format <- function(name,
                         n_items,
                         range,
                         abbreviation = '',
                         cut_off = NA,
                         reference = '',
                         subscale = FALSE ) {

  if ( length( range ) == 1 ) {
    range = rep( range, 2 )
  }

  if ( subscale ) {

    out <- list(
      name = name,
      n_items = n_items,
      range = c( Min = range[1], Max = range[2] ),
      cut_off = cut_off
    )

  } else {
    # Initialize output
    out <- list(
      name = name,
      n_items = n_items,
      range = c( Min = range[1], Max = range[2] ),
      abbreviation = abbreviation,
      cut_off = cut_off,
      reference = reference
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
#' @param column_name ...
#' @param description ...
#' @param variable_category ...
#' @param data_type ...
#' @param redcap_variables ...
#' @param values_and_labels ...
#' @param scale_details ...
#' @param subscale_details ...
#' @param summary_of_x ...
#' @param category_labels ...
#' @param type_labels ...
#' @param codes_for_missing ...
#' @param group_var ...
#' @param study_var ...
#' @param units_of_x ...
#' @param validated ...
#' @param digits ...
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
    Values_and_labels = '',
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

      # Check variable for missing values
      entries <- !is.na(x) & x == codes_for_missing[[k]]

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





