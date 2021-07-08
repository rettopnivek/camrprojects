# Meta-data functions
# Written by Kevin Potter
# email: kevin.w.potter@gmail.com
# Please email me directly if you
# have any questions or comments
# Last updated 2021-07-03

# Table of contents
# 1) label_for_abbr
# 2) find_range_for_x
# 3) create_summary_for_x
# 4) list_of_missing
# 5) scale_format
# 6) values_and_labels
# 7) contains_groups
# 8) add_meta_data
# 9) meta
# 10) transfer_meta
# 11) update_meta_data
# 12) create_data_dictionary

### TO DO ###
# - Finish function documentation

#### 1) label_for_abbr ####
#' Extract Labels Corresponding to Abbreviations
#'
#' Given a standardized naming schemes for columns in
#' a data frame (e.g., \code{AAA.BBB.Description} where
#' 'AAA' is a 3-letter abbreviation for the overarching
#' category to which a variable belongs, and 'BBB' is a
#' 3-letter abbreviation for the type of data in the column,
#' this function extracts the corresponding label associated
#' with a given abbreviation. Additional abbreviations and
#' associated labels can be defined for the categories/data types,
#' and a completely custom set of abbreviations (e.g., 2-letter
#' abbreviations for time points) and associated labels
#' can be defined.
#'
#' @param column_name The column name in a data frame,
#'   assumed to follow a standardized format, such
#'   as 'AAA.BBB.Description' or 'AAA.BBB.CCC.Description'.
#' @param type A character string indicating the type of
#'   abbreviation/label pairings, either...
#'   \itemize{
#'     \item \code{'category'} for abbreviations associated
#'     with a variable's overarching category (e.g., 'Identifier',
#'     'Session details', etc.);
#'     \item \code{'data type'} for abbreviations associated
#'     with the type of data (e.g., 'Integer', 'Character string',
#'     etc.);
#'     \item \code{'custom'} for custom abbreviations and labels
#'     for extensions beyond the typical 'AAA.BBB.Description'
#'     column name construction.
#'   }
#' @param abbr_labels A 2-column character matrix, with the first
#'   column listing abbreviations and the second column listing
#'   associated labels. Can be used to provide additional
#'   abbreviation/label pairings in addition to the default
#'   options for variable categories and data types, or can
#'   be used in conjunction with the argument \code{pst} to
#'   specify completely custom abbreviations and labels.
#' @param pst A vector of two integers giving the starting and
#'   ending positions in the column name character string with
#'   the abbreviation to extract.
#'
#' @author Kevin Potter
#'
#' @return A character string, the corresponding label
#'   to the abbreviation extracted from \code{column_name}.
#'
#' @examples
#' # Determine variable's category
#' label_for_abbr( 'SSS.CHR.Subject' )
#'
#' # Determine variable's data type
#' label_for_abbr( 'SSS.CHR.Subject', type = 'data type' )
#'
#' # Add additional labels
#' al <- rbind(
#'   c( 'MED', 'Medication use' ),
#'   c( 'DRV', 'Driving simulation results' )
#' )
#' label_for_abbr( 'MED.INT.Bupropion_use', abbr_labels = al )
#' label_for_abbr( 'DRV.INT.Divided_attention_task', abbr_labels = al )
#'
#' # Custom abbreviations and labels
#' al <- rbind(
#'   c( 'BL', 'Baseline' ),
#'   c( 'Y1', 'Year 1 of intervention' ),
#'   c( 'Y2', 'Year 2 of intervention' )
#' )
#' pst <- c( 9, 10 )
#' label_for_abbr( 'SMK.INT.BL.Smoking_status',
#'                 type = 'custom', abbr_label = al, pst = pst )
#' label_for_abbr( 'SMK.INT.Y2.Smoking_status',
#'                 type = 'custom', abbr_label = al, pst = pst )
#'
#' @export

label_for_abbr <- function( column_name,
                            type = 'category',
                            abbr_labels = NULL,
                            pst = NULL ) {

  if ( type %in% c( '1', 'category', 'variable' ) ) {

    tags_labels <- rbind(
      c( 'IDS', 'Identifiers' ),
      c( 'SSS', 'Session details' ),
      c( 'SBJ', 'Subject details' ),
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
      c( 'RMT', 'Remote survey results' )
    )

    pst <- c( 1, 3 )

  }

  if ( type %in% c( '2', 'data', 'data type' ) ) {

    tags_labels <- rbind(
      c( 'INT', 'Integer' ),
      c( 'DBL', 'Double precision floating point number' ),
      c( 'LGC', 'Logical' ),
      c( 'CHR', 'Character string' ),
      c( 'DAT', 'R Date-class variable for calendar dates' ),
      c( 'FCT', 'Enumerated type - factor' )
    )

    pst <- c( 5, 7 )

  }

  if ( !type %in% c( '0', 'custom' ) ) {

    if ( !is.null( abbr_labels ) ) {

      tags_labels <- rbind(
        tags_labels,
        abbr_labels
      )

    }

  } else {
    tags_labels <- abbr_labels
  }

  if ( is.null( pst ) ) {
    stop( paste0(
      "Incorrect input for argument 'type'"
    ) )
  }

  tag <- substr( column_name, start = pst[1], stop = pst[2] )

  n_letter <- length( pst[1]:pst[2] )

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

#### 2) find_range_for_x ####
#' Determine the Observed Range for a Numeric Variable
#'
#' Given a numeric vector \code{x}, determines the
#' observed minimum and maximum after removing
#' missing data and outputs a character string to
#' be passed into the meta-data for data dictionary
#' purposes.
#'
#' @param x A numeric vector.
#' @param codes_for_missing A character vector with
#'   additional codes for missing data (e.g.,
#'   \code{''}, \code{' '}, or user-defined codes).
#' @param digits Number of digits to round to.
#'
#' @author Kevin Potter
#'
#' @return A character string, formatted as
#' \code{'Observed = X to Y'} where 'X' is the
#' minimum observed value and 'Y' is the maximum
#' observed value after removing missing data.
#'
#' @examples
#' # Create numeric vector
#' x <- rnorm( 100 )
#' # Add missing data
#' x[ sample( 1:100, size = 10 ) ] <- NA
#' # Determine range
#' find_range_for_x( x, '' )
#'
#' @export

find_range_for_x <- function( x, codes_for_missing, digits = 2 ) {

  no_missing <-
    !is.na( x ) &
    !( as.character(x) %in% codes_for_missing )

  mn <- round( min( x[ no_missing ] ), digits )
  mx <- round( max( x[ no_missing ] ), digits )

  return( paste0( 'Observed = ', mn, ' to ', mx ) )
}

#### 3) create_summary_for_x ####
#' Compute Brief Set of Summary Statistics for a Numeric Variable
#'
#' Given a numeric variable \code{x}, computes either the mean,
#' median, standard deviation, and 1st and 3rd quartiles
#' (for continuous values) or the percentage and observed
#' frequencies (for binary values). Also determines the
#' frequency of missing data.
#'
#' @param x A numeric vector.
#' @param codes_for_missing A character vector with
#'   additional codes for missing data (e.g.,
#'   \code{''}, \code{' '}, or user-defined codes).
#' @param digits Number of digits to round to.
#'
#' @author Kevin Potter
#'
#' @return A character string with the observed summary
#' statistics.
#'
#' @examples
#' # Create numeric vector
#' x <- rnorm( 100 )
#' # Add missing data
#' x[ sample( 1:100, size = 10 ) ] <- NA
#' # Compute summary
#' create_summary_for_x( x, '' )
#'
#' # Binary values
#' x <- rbinom( 100, 1, .5 )
#' # Add missing data
#' x[ sample( 1:100, size = 10 ) ] <- NA
#' # Compute summary
#' create_summary_for_x( x, '' )
#'
#' @export

create_summary_for_x <- function( x, codes_for_missing, digits = 2 ) {

  out = ''

  no_missing <-
    !is.na( x ) &
    !( as.character(x) %in% codes_for_missing )
  x <- x[ no_missing ]

  if ( length( unique( x ) ) == 2 ) {

    u = sort( unique( x ) )
    p <- round( 100*mean( x == u[2] ), digits )
    freq <- sum( x == u[2] )
    n <- length( x )

    out <- paste0(
      '<',
      p, '% of ', u[2], '|', freq, ' of ', n, '|',
      'Missing = ', sum( !no_missing ),
      ' of ', length( no_missing ),
      '>'
    )

  } else {

    m <- round( mean( x ), digits )
    md <- round( median( x ), digits )
    s <- round( mean( x ), digits )
    q1 <- round( quantile( x, .25 ), digits )
    q3 <- round( quantile( x, .75 ), digits )

    out <- paste0(
      '<',
      'M = ', m, '|Md = ', md, '|',
      'SD = ', s, '|', 'Q1 = ', q1, '|',
      'Q3 = ', q3, '|Missing = ', sum( !no_missing ),
      ' of ', length( no_missing ),
      '>'
    )

  }

  return( out )
}

#### 4) list_of_missing ####
#' Creates List of Missing Data Found in a Vector
#'
#' Creates a formatted character string listing the
#' different types of missing data found in a vector
#' to then pass into the meta-data used for data
#' dictionary purposes.
#'
#' @param x A vector of values.
#' @param codes_for_missing A character vector with
#'   additional codes for missing data (e.g.,
#'   \code{''}, \code{' '}, or user-defined codes).
#'
#' @author Kevin Potter
#'
#' @return A character string listing the types of missing data
#' found. For example, if a vector uses R's standard \code{NA}
#' and \code{NaN} values to indicate missing values, the
#' output would be: \code{'<NA|NaN>'}. Different codes for
#' missing values are separated by the pipe symbol \code{|}.
#'
#' @examples
#' # Numeric example
#' x <- rnorm( 10 ); x[1] <- NA
#' list_of_missing( x )
#'
#' # Character vector with '' and ' ' indicating missingness
#' x <- LETTERS; x[1] <- ''; x[2] <- ' '
#' list_of_missing( x )
#'
#' @export

list_of_missing <- function(x, codes_for_missing = c( '', ' ' )) {

  missing_values <- c()

  # NA or NaN values
  if ( any( is.na( x ) ) ) {

    missing_values <- c(
      missing_values,
      c( 'NA', 'NaN' )
    )

  }

  if ( length( codes_for_missing ) > 0 ) {

    for ( i in 1:length( codes_for_missing ) ) {

      if ( any( as.character( x[ !is.na( x ) ] ) ==
                codes_for_missing[i] ) ) {
        missing_values <- c(
          missing_values,
          codes_for_missing[i]
        )
      }

    }

  }

  if ( length( missing_values ) > 0 ) {

    out <- paste0(
      '<',
      paste( missing_values, collapse = '|' ),
      '>'
    )
    return( out )

  } else {
    return( '' )
  }

}

#### 5) scale_format ####
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
#' @param abbreviation A character string with the abbreviation
#'   for the scale (e.g., 'HADS' or 'CUDIT').
#' @param cut_off An optional numeric value giving a cut-off
#'   used in the scale (e.g., the CUDIT uses a clinical cut-off
#'   of scores at \code{8} or higher to indicate problematic
#'   cannabis use).
#' @param subscale Logical; if \code{TRUE}, instead outputs
#'   the reporting format for subscales (consisting only
#'   of the subscale name its number of items).
#'
#' @author Kevin Potter
#'
#' @return A character string, either '<Scale name> (<Scale
#' abbreviation>) - <N> items - Cut-off of <x>' or
#' '<Subscale name> - <N> items'.
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
                         abbreviation = '',
                         cut_off = '',
                         subscale = FALSE) {

  if ( !subscale ) {

    if ( cut_off != '' ) {
      cut_off = paste0(
        ' - Cut-off of ', cut_off
      )
    }

    out = paste0(
      name,
      ' (',
      abbreviation,
      ') - ',
      n_items,
      ' items',
      cut_off
    )
  } else {

    out = paste0(
      name,
      ' - ',
      n_items,
      ' items'
    )

  }

  return( out )
}

#### 6) values_and_labels ####
#' Standardized Reporting for Matching Values to Labels
#'
#' Outputs character strings with standardized reporting
#' formats to link codes to their associated labels
#' (e.g., for likert-type responses) that can then be
#' passed into meta-data for data dictionary purposes.
#'
#' @param values A vector of codes matching in length to
#'   \code{labels}.
#' @param labels A vector of labels matching in length to
#'   \code{values}.
#'
#' @author Kevin Potter
#'
#' @return A vector of 2 character strings.
#'
#' @examples
#' # Example likert-type response
#' v <- 1:5
#' l <- c( 'Never', 'Sometimes', 'Half of the time',
#'         'Often', 'All of the time' )
#' values_and_labels( v, l )
#'
#' @export

values_and_labels <- function( values, labels ) {

  if ( length( values ) != length( labels ) ) {
    stop( paste0(
      'Vector length for values must match ',
      'vector length for labels'
    ) )
  }

  out <- c(
    values = paste0(
      '<',
      paste( values, collapse = '|' ),
      '>'
    ),
    labels = paste0(
      '<',
      paste( labels, collapse = '|' ),
      '>'
    )
  )

  return( out )
}

#### 7) contains_groups ####
#' Determine Levels of a Grouping Factor Associated With a Variable
#'
#' Determines the available levels of a grouping factor
#' for all non-missing values of another variable,
#' to be passed into meta-data for data dictionary purposes.
#'
#' @param dtf A data frame.
#' @param column_name A character string for the column
#'   name over which to determine all non-missing values.
#' @param grouping_column A character string for the column
#'   name with the grouping factor.
#' @param codes_for_missing A character vector with
#'   additional codes for missing data (e.g.,
#'   \code{''}, \code{' '}, or user-defined codes).
#'
#' @author Kevin Potter
#'
#' @return A character string.
#'
#' @examples
#' # Example data frame
#' dtf <- data.frame(
#'   x = rnorm( 10 ),
#'   # Grouping factor
#'   g = rep( LETTERS[1:5], each = 2 )
#' )
#' dtf$x[1:2] <- NA # Create missing values
#' contains_groups( dtf, 'x', 'g', '' )
#'
#' @export

contains_groups <- function(dtf, column_name, grouping_column,
                            codes_for_missing) {

  x <- dtf[[ column_name ]]

  no_missing <-
    !is.na( x ) &
    !( as.character(x) %in% codes_for_missing )

  out <- unique( dtf[[ grouping_column ]][ no_missing ] )
  out <- sort( out )

  out <- paste0(
    '<',
    paste( out, collapse = '|' ),
    '>'
  )

  return( out )
}

#### 8) add_meta_data ####
#' Add Meta-data to a Column for Data Dictionary Purposes
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
#' @param values ...
#' @param labels_for_values ...
#' @param scales ...
#' @param subscales ...
#' @param range_of_x ...
#' @param category_labels ...
#' @param type_labels ...
#' @param codes_for_missing ...
#' @param group_var ...
#' @param time_points_var ...
#' @param additional_abbr ...
#' @param units_of_x ...
#' @param digits ...
#'
#' @author Kevin Potter
#'
#' @return An updated data frame with new attributes for
#' the specified column.
#'
#' @export

add_meta_data <- function(dtf,
                          column_name,
                          description = '',
                          variable_category = NULL,
                          data_type = NULL,
                          values = '',
                          labels_for_values = '',
                          scales = NULL,
                          subscales = NULL,
                          range_of_x = NULL,
                          category_labels = NULL,
                          type_labels = NULL,
                          codes_for_missing = c( '', ' ' ),
                          group_var = NULL,
                          time_points_var = NULL,
                          additional_abbr = NULL,
                          units_of_x = '',
                          digits = 2) {

  if ( !column_name %in% colnames( dtf ) ) {
    stop( 'Column not found in data frame' )
  }

  # Extract observations
  x <- dtf[[ column_name ]]

  # Determine overarching category for variable
  if ( is.null( variable_category ) ) {

    variable_category <-
      label_for_abbr(
        column_name,
        type = 'category',
        abbr_labels = category_labels
      )

  }

  # Determine data type
  if ( is.null( data_type ) ) {

    data_type <-
      label_for_abbr(
        column_name,
        type = 'data type',
        abbr_labels = type_labels
      )

  }

  # Details for inventory/questionnaires
  if ( !is.null( scales ) ) {

    scales <- scale_format(
      names = scales[1],
      n_items = scales[2],
      abbreviation = scales[3],
      cut_off = scales[4]
    )

  } else {
    scales = ''
  }

  # Details for a subscale of inventory/questionnaires
  if ( !is.null( subscales ) ) {

    subscales <- scale_format(
      names = scales[1],
      n_items = scales[2],
      subscale = TRUE
    )

  } else {
    subscales = ''
  }

  # Determine observed range for observations
  if ( is.null( range_of_x ) ) {

    if ( grepl( '.INT.', column_name ) |
         grepl( '.DBL.', column_name ) |
         grepl( '.LGC.', column_name ) ) {

      range_of_x <- find_range_for_x( x, codes_for_missing, digits )

    } else {
      range_of_x = ''
    }

  } else {
    if ( grepl( 'Expected', range_of_x ) ) {
      range_of_x <- paste0(
        range_of_x, '|',
        range_for_meta_data( x, codes_for_missing )
      )
    }
  }

  # Generate descriptive summary of observations
  # if integer, double, or logical
  if ( grepl( '.INT.', column_name ) |
       grepl( '.DBL.', column_name ) |
       grepl( '.LGC.', column_name ) ) {

    summary_of_x =
      create_summary_for_x(x, codes_for_missing, digits)

  } else {
    summary_of_x = ''
  }

  if ( !is.null( group_var ) ) {
    groups_assessed = contains_groups(
      dtf, column_name, group_var,
      codes_for_missing
    )
  } else {
    groups_assessed = ''
  }

  if ( !is.null( time_points_var ) ) {
    time_points_assessed = contains_groups(
      dtf, column_name, time_points_var,
      codes_for_missing
    )
  } else {
    time_points_assessed = ''
  }

  if ( length( values ) > 1 ) {

    code_for_values_and_labels <- values_and_labels(
      values,
      labels_for_values
    )

    values <- code_for_values_and_labels[1]
    labels_for_values <- code_for_values_and_labels[2]

  }

  # Determine indicators for missing data
  codes_for_missing <- list_of_missing( x, codes_for_missing )

  if ( is.null( attributes( dtf[[ column_name ]] ) ) ) {

    lst <- list(
      Column_name = column_name,
      Variable_category = variable_category,
      Data_type = data_type,
      Description = description,
      Values = values,
      Labels_for_values = labels_for_values,
      Scale = scales,
      Subscale = subscales,
      Codes_for_missing = codes_for_missing,
      Range = range_of_x,
      Summary = summary_of_x,
      Units = units_of_x,
      Groups_assessed = groups_assessed,
      Time_points_assessed = time_points_assessed
    )

  } else {

    lst <- attributes( dtf[[ column_name ]] )

    lst$Column_name = column_name
    lst$Variable_category = variable_category
    lst$Data_type = data_type
    lst$Description = description
    lst$Values = values
    lst$Labels_for_values = labels_for_values
    lst$Scale = scales
    lst$Subscale = subscales
    lst$Codes_for_missing = codes_for_missing
    lst$Range = range_of_x
    lst$Summary = summary_of_x
    lst$Units = units_of_x
    lst$Groups_assessed = groups_assessed
    lst$Time_points_assessed = time_points_assessed

  }

  attributes( dtf[[ column_name ]] ) <- lst

  return( dtf )
}

#### 9) meta ####
#' Extract Meta-data for Data Frame
#'
#' Extracts a specified attribute in the list of meta-data
#' for data dictionary purposes.
#'
#' @param dtf A data frame.
#' @param column_name A character string for the name of
#'   the column with the meta-data to extract.
#' @param element The attribute in the meta-data list
#'   to extract. Use \code{element = 'all'} to list
#'   all attributes.
#' @param parts Logical; if \code{TRUE} splits
#'   multi-part elements (i.e., lists with elements
#'   separated by the pipe symbol '|').
#'
#' @author Kevin Potter
#'
#' @return A character vector.
#'
#' @export

meta <- function(dtf,
                 column_name,
                 element = 'description',
                 parts = FALSE) {

  lst <- attributes( dtf[[ column_name ]] )

  if ( element %in% c( 'All elements', 'all elements',
                       'All', 'all' ) ) {
    return( lst )
  }

  elem_types <- list(
    Variable_category = c(
      'Variable category', 'variable category',
      'Variable', 'variable',
      'Category', 'category',
      'Var', 'var', 'Cat', 'cat',
      '2'
    ),
    Data_type = c(
      'Data type', 'data type',
      'Data', 'data',
      'type', 'type',
      '3'
    ),
    Description = c(
      'Description', 'description',
      'Desc', 'desc',
      '4'
    ),
    Values = c(
      'Values', 'values',
      'Val', 'val',
      '5'
    ),
    Labels_for_values = c(
      'Labels for values', 'labels for values',
      'Labels', 'labels',
      'Labels_for_values',
      '6'
    ),
    Scale = c(
      'Scale', 'scale',
      'Scales', 'scales',
      '7'
    ),
    Subscale = c(
      'Subscale', 'subscale',
      'Subscales', 'subscales',
      '8'
    ),
    Codes_for_missing = c(
      'Codes for missing', 'codes for missing',
      'Missing data', 'missing data',
      'Missing', 'missing',
      'Codes_for_missing',
      '9'
    ),
    Range = c(
      'Range', 'range',
      '10'
    ),
    Summary = c(
      'Summary', 'summary',
      '11'
    ),
    Units = c(
      'Units', 'units',
      '12'
    ),
    Groups_assessed = c(
      'Groups assessed', 'groups assessed',
      'Groups', 'groups',
      'Groups_assessed',
      '13'
    ),
    Time_points_assessed = c(
      'Time points assessed', 'time points assessed',
      'Time points', 'time points',
      'Visits', 'visits',
      'Time_points_assessed',
      '14'
    )
  )

  which_element = sapply( 1:length( elem_types ), function(i) {
    return( element %in% elem_types[[i]] )
  } )

  if ( sum( which_element ) == 1 ) {

    elem <- lst[[ names( elem_types )[ which_element ] ]]

    if ( !parts ) {
      return( elem )
    } else {

      if ( grepl( '<', elem, fixed = TRUE ) ) {

        out <- elem
        out <- gsub( '<', '', out, fixed = TRUE )
        out <- gsub( '>', '', out, fixed = TRUE )
        out <- strsplit(out, split =  '|', fixed = TRUE )[[1]]

        return( out )
      }

    }

  } else {
    stop( 'Specified attribute not found' )
  }

}

#### 10) transfer_meta ####

# transfer_meta <- function() {}

#### 11) update_meta_data ####

# update_meta_data <- function() {}

#### 12) create_data_dictionary ####

# create_data_dictionary <- function() {}

#### 13) meta_data_template ####

meta_data_template <- function( dtf,
                                group_var = NULL,
                                time_points_var = NULL ) {

  column_names = colnames( dtf )

  arg_for_md <- ""

  in_s <- function(x, s) {
    grepl( x, s, fixed = TRUE )
  }

  for ( i in 1:length( column_names ) ) {

    lst_val <- '    description = paste0("")'

    if ( in_s( '.INT.', column_names[i] ) &
         !in_s( 'IDS.', column_names[i] ) &
         !in_s( 'SSS.', column_names[i] ) ) {
      lst_val <- paste0(
        lst_val, ',\n',
        '    units_of_x = ""'
      )
    }

    if ( in_s( '.DBL.', column_names[i] ) &
         !in_s( 'IDS.', column_names[i] ) &
         !in_s( 'SSS.', column_names[i] )) {
      lst_val <- paste0(
        lst_val, ',\n',
        '    units_of_x = ""'
      )
    }

    if ( in_s( '.LGC.', column_names[i] ) ) {
      lst_val <- paste0(
        lst_val, ',\n',
        '    values = c( TRUE, FALSE ),\n',
        '    labels_for_values = c( "", "" )'
      )
    }


    arg_for_md <- paste0(
      arg_for_md,
      '  ', column_names[i], ' = list(\n',
      lst_val, '\n',
      '  )'
    )
    if ( i < length( column_names ) ) {
      arg_for_md <- paste0(
        arg_for_md,
        ',\n'
      )
    }

  }
  arg_for_md <- paste0(
    'inputs <- list(\n',
    arg_for_md,
    '\n)'
  )

  update_md <- paste0(
    'for ( i in 1:length( inputs ) ) {\n',
    '  lst <- inputs[[i]]\n',
    '  lst$dtf <- dtf\n',
    '  lst$column_name <- names(inputs)[i]\n',
    '  dtf <- do.call( add_meta_data, lst )\n',
    '}\n'
  )

  message( paste0( arg_for_md, "\n\n", update_md ) )

}



