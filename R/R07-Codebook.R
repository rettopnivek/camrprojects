# Codebook functions
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
# Last updated: 2022-11-21

# Table of contents
# 1) Helper functions
#   1.1) camr_column_abbreviations
#   1.2) camr_collected_over
#   1.3) camr_descriptive_summary
# 2) Functions for codebook entries
#   2.1) camr_new_codebook_entry
#     2.1.1) is.codebook_entry
#   2.2) camr_add_codebook_entry
#     2.2.1) camr_ace
#   2.3) camr_in_codebook_entry
#     2.3.1) camr_ice
#   2.4) camr_pull_codebook_entry
#     2.4.1) camr_puce
#   2.5) camr_display_codebook_entry
#     2.5.1) camr_dice
#   2.6) camr_deidentified_codebook_entry
#   2.7) camr_data_frame_from_codebook_entry
#   2.8) camr_update_codebook_entry
#   2.9) camr_data_frame_to_codebook_entry

#### 1) Helper functions ####

#### 1.1) camr_column_abbreviations ####
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
#'   pass to the `custom` argument.
#' @param separator The punctuation used
#'   to separate abbreviations, typically
#'   a period.
#'
#' @author Kevin Potter
#'
#' @returns A character string. If only `n` is
#'   provided, a matrix with `n` rows with
#'   columns 'Abbr' and 'Label' to use as a
#'   template for custom abbreviations/labels.
#'
#' @examples
#' # List pre-defined abbreviations and labels
#' camr_column_abbreviations( type = 'category' )
#' camr_column_abbreviations( type = 'data type' )
#'
#' # Match abbreviation for variable categories
#' camr_column_abbreviations(
#'   'IDS.CHR.Subject', type = 'category'
#' )
#'
#' # Match abbreviation for data type
#' camr_column_abbreviations(
#'   'IDS.CHR.Subject', type = 'data type'
#' )
#'
#' # Create template for custom abbreviations
#' # and labels
#' M <- camr_column_abbreviations( n = 3 )
#' M[,1] <- c( 'BL', 'Y1', 'Y2' )
#' M[,2] <- c( 'Baseline', 'Year 1', 'Year 2' )
#' camr_column_abbreviations( 'SMK.INT.BL.Quit_status', custom = M )
#' camr_column_abbreviations( 'SMK.INT.Y2.Quit_status', custom = M )
#'
#' @export

camr_column_abbreviations <- function(
    column_name = '',
    type = NULL,
    custom = NULL,
    n = NA,
    separator = "." ) {

  # Pre-defined abbreviations and
  # labels for variable categories
  abbr_labels.var_cat <- rbind(
    c( 'IDS', 'Identifiers' ),
    c( 'IDX', 'Variables for indexing' ),
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
    c( 'INC', 'Indices for inclusion' ),
    c( 'DGN', 'Clinical diagnoses' )
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
        "the command 'camr_column_abbreviations(n=1)' ",
        "will generate a template matrix"
      ) )
    }

  }

}

#### 1.2) camr_collected_over ####
#' Categories over which Data was Collected
#'
#' Function to list the categories (e.g., experimental
#' groups, study time points) over which data was collected.
#' Useful for denoting the specific subset of categories data
#' was collected over (e.g., if data was only collected at a
#' baseline time point). Output can be passed to the
#' \code{Collected_over} argument for the
#' [camr_new_codebook_entry] function.
#'
#' @param dtf A data frame.
#' @param x The variable for the collected data (non-standard
#'   evaluation possible).
#' @param y The variable with the categories (non-standard
#'   evaluation possible).
#' @param missing_values An optional logical vector indicating
#'   missing data (must match in length to \code{x}). If
#'   \code{NULL} assumes \code{NA} values are the only
#'   indicator of missing data.
#' @param non_standard A logical value; if \code{TRUE}
#'   uses non-standard evaluation for \code{x} and \code{y},
#'   otherwise assumes they are character strings.
#'
#' @author Kevin Potter
#'
#' @returns A list with two vectors: \code{content} with the
#'   categories from \code{y} over which there was non-missing
#'   data, and \code{additional_content} listing the column
#'   name for \code{y}.
#'
#' @examples
#' data( example_CAM_data_set )
#'
#' example_CAM_data_set |>
#'   camr_collected_over( INV.INT.HADS_anxiety, SSS.CHR.Session )
#'
#' example_CAM_data_set |>
#'   camr_collected_over( INV.DBL.BPI_severity, SSS.CHR.Event )
#'
#' @export

camr_collected_over <- function(
    dtf,
    x,
    y,
    missing_values = NULL,
    non_standard = TRUE ) {

  if ( non_standard ) {
    variable_of_interest <- as.character( substitute( x ) )
    variable_collected_over <- as.character( substitute( y ) )
  } else {
    variable_of_interest <- x
    variable_collected_over <- y
  }

  voi <- dtf[[ variable_of_interest ]]
  vco <- dtf[[ variable_collected_over ]]

  if ( is.null( missing_values ) ) {
    non_missing <- !is.na( voi ) & !is.na( vco )
  }

  values_of_vco <- unique( vco[ non_missing ] )
  n <- length( values_of_vco )

  out <- list(
    content = values_of_vco,
    additional_content = rep( variable_collected_over, n )
  )

  return( out )
}


#### 1.3) camr_descriptive_summary ####
#' Create Descriptive Summary of Data
#'
#' Function to generate descriptive summaries
#' (means, percentages, ranges, etc.) of a
#' variable that can then be passed to the
#' \code{descriptive_summary} argument of the
#' [new_codebook_entry] function.
#'
#' @param x A variable.
#' @param type The type of summary to return,
#'   either \code{continuous} (mean, SD, etc.),
#'   \code{categorical} (percentages, etc.), or
#'   \code{range} (date ranges).
#' @param missing_values An optional logical
#'   vector matching in length to \code{x}
#'   indicating missing data.
#' @param digits The number of digits to round to.
#'
#' @author Kevin Potter
#'
#' @returns A list with two vectors: \code{content}
#'   with the summary statistics and \code{additional_content}
#'   with the labels for each type of statistic.
#'
#' @examples
#' data( example_CAM_data_set )
#'
#' example_CAM_data_set$INV.INT.HADS_anxiety |>
#'   camr_descriptive_summary( "continuous") |>
#'   data.frame()
#'
#' example_CAM_data_set$SBJ.INT.Biological_sex |>
#'   camr_descriptive_summary( "categorical") |>
#'   data.frame()
#'
#' example_CAM_data_set$SSS.DAT.Date_of_visit |>
#'   camr_descriptive_summary( "range") |>
#'   data.frame()
#'
#' @export

camr_descriptive_summary <- function(
    x,
    type,
    missing_values = NULL,
    digits = 2 ) {

  if ( is.null( missing_values ) ) {
    missing_values <- camr_missing( x )
  }

  n_obs <- length( missing_values )

  summary_of_x = list(
    content = '',
    additional_content = ''
  )

  x_no_missing <- x[ !missing_values ]

  if ( type == 'continuous' ) {

    summary_of_x <- list(
      content = c(
        sum( !missing_values ),
        round( mean( x_no_missing ), digits ),
        round( sd( x_no_missing ), digits ),
        round( min( x_no_missing ), digits ),
        round( quantile( x_no_missing, .25 ), digits ),
        round( quantile( x_no_missing, .5 ), digits ),
        round( quantile( x_no_missing, .75 ), digits ),
        Max = round( max( x_no_missing ), digits ),
        Missing = sum( missing_values )
      ),
      additional_content = c(
        "N",
        "Mean",
        "SD",
        "Min",
        "1st quartile",
        "Median",
        "3rd quartile",
        "Max",
        "N missing"
      )
    )

  }

  if ( type == 'categorical' ) {

    freq <- table( x_no_missing )

    summary_of_x <- list(
      content = c(
        paste0(
          c( freq, sum( missing_values ) ),
          " (",
          round( 100*c( freq, sum( missing_values ) )/n_obs, digits ),
          "%)"
        )
      ),
      additional_content = c(
        paste0(
          c( names(freq), 'Missing' ),
          ": n (%)"
        )
      )
    )

  }

  if ( type == 'range' ) {

    if ( any( class( x ) %in% c( "Date", "POSIXct", "POSIXt" ) ) ) {

      summary_of_x <- list(
        content = c(
          sum( !missing_values ),
          as.character( min( x_no_missing ) ),
          as.character( max( x_no_missing ) ),
          sum( missing_values )
        ),
        additional_content = c(
          "N",
          "Min",
          "Max",
          "N missing"
        )
      )

    } else {

      summary_of_x <- list(
        content = c(
          sum( !missing_values ),
          min( x_no_missing ),
          max( x_no_missing ),
          sum( missing_values )
        ),
        additional_content = c(
          "N",
          "Min",
          "Max",
          "N missing"
        )
      )

    }

  }

  return( summary_of_x )
}

#### 2) Functions for codebook entries ####

#### 2.1) camr_new_codebook_entry ####
#' Create New Codebook Entry
#'
#' Function to create a data frame of additional
#' class \code{codebook_entry}.
#'
#' @param Variable_name A character string, the column
#'   name for the variable of interest.
#' @param Category The label associated with the 3-letter
#'   abbreviation for the variable's category (see
#'   [camr_column_abbreviations]).
#' @param Data_type The label associated with the 3-letter
#'   abbreviation for the variable's data type (see
#'   [camr_column_abbreviations]).
#' @param Subcategory The label associated with the custom
#'   abbreviation for the variable's subcategory (see
#'   [camr_column_abbreviations]).
#' @param Description A character string, a brief human-readable
#'   description of what the variable is.
#' @param Values_and_labels A named list of two vectors,
#'   (1) \code{content} containing the distinct values for
#'   the variable, and (2) \code{additional_content} containing
#'   the label associated with each value in (1).
#' @param Inventory A named list of two vectors,
#'   (1) \code{content} containing the details for
#'   the inventory, scale, or questionnaire, and (2)
#'   \code{additional_content} containing the label
#'   associated with each detail given in (1).
#' @param Units_of_measurement A character string, the
#'   units of measurement for the variable.
#' @param Descriptive_summary A named list of two vectors,
#'   (1) \code{content} containing summary statistics for
#'   the variable, and (2) \code{additional_content} containing
#'   the label associated with each statistic in (1).
#' @param Codes_for_missing_data A character vector, the
#'   codes (surrounding by back-ticks) for missing data.
#' @param Deidentified Either \code{FALSE} if the variable
#'   is not deidentified (i.e., contains potential patient
#'   health information) or \code{TRUE} otherwise.
#' @param Collected_over A named list of two vectors,
#'   (1) \code{content} containing distinct values for
#'   groups, time points, etc., over which the variable was
#'   collected, and (2) \code{additional_content} containing
#'   the column name associated with the values in (1).
#' @param Created_from A character vector, the original
#'   REDCap variables from which the variable was created.
#' @param Data_quality_checks A character string, notes
#'   on any data quality checks relevant to the variable.
#' @param Notes A character vector, any additional notes
#'   to include in the codebook entry.
#'
#' @details The use of commas should be avoided for
#' codebook entries in order to ensure easy conversion
#' to a .csv file.
#'
#' @author Kevin Potter
#'
#' @return A data frame of the additional class
#' \code{codebook_entry}.
#'
#' @export

camr_new_codebook_entry <- function(
    Variable_name = '',
    Category = '',
    Data_type = '',
    Subcategory = '',
    Description = '',
    Values_and_labels = '',
    Inventory = '',
    Units_of_measurement = '',
    Descriptive_summary = '',
    Codes_for_missing_data = '',
    Deidentified = 'FALSE',
    Collected_over = '',
    Created_from = '',
    Data_quality_checks = '',
    Notes = '' ) {

  # Check inputs
  checkmate::assert_string( Variable_name )
  checkmate::assert_string( Category )
  checkmate::assert_string( Data_type )
  checkmate::assert_string( Subcategory )
  checkmate::assert_string( Description )
  checkmate::assert_string( Units_of_measurement )

  # Values and associated labels
  if ( is.list( Values_and_labels ) ) {

    # Check inputs
    checkmate::assert_list(
      Values_and_labels,
      len = 2
    )
    checkmate::assert_names(
      names( Values_and_labels ),
      permutation.of = c( "content", "additional_content" )
    )

    # Extract content
    Values_and_labels.content <-
      Values_and_labels$content
    Values_and_labels.additional_content <-
      Values_and_labels$additional_content

    # Check vectors match in length
    n_content <- length( Values_and_labels.content )
    n_add_content <- length( Values_and_labels.additional_content )

    if ( n_content != n_add_content ) {
      stop( "Vectors within 'Values_and_labels' must match in length" )
    }

    # Close 'Values and associated labels'
  } else {

    Values_and_labels.content <- ""
    Values_and_labels.additional_content <- ""

    # Close else for 'Values and associated labels'
  }

  # Inventory details
  if ( is.list( Inventory ) ) {

    # Check inputs
    checkmate::assert_list(
      Inventory,
      len = 2
    )
    checkmate::assert_names(
      names( Inventory ),
      permutation.of = c( "content", "additional_content" )
    )

    Inventory.content <- Inventory$content
    Inventory.additional_content <- Inventory$additional_content

    # Check vectors match in length
    n_content <- length( Inventory.content )
    n_add_content <- length( Inventory.additional_content )

    if ( n_content != n_add_content ) {
      stop( "Vectors within 'Inventory' must match in length" )
    }

    # Close 'Inventory details'
  } else {

    Inventory.content <- ""
    Inventory.additional_content <- ""

    # Close else for 'Inventory details'
  }

  # Descriptive summary of variable
  if ( is.list( Descriptive_summary ) ) {

    # Check inputs
    checkmate::assert_list(
      Descriptive_summary,
      len = 2
    )
    checkmate::assert_names(
      names( Descriptive_summary ),
      permutation.of = c( "content", "additional_content" )
    )

    Descriptive_summary.content <-
      Descriptive_summary$content
    Descriptive_summary.additional_content <-
      Descriptive_summary$additional_content

    # Check vectors match in length
    n_content <- length( Descriptive_summary.content )
    n_add_content <- length( Descriptive_summary.additional_content )

    if ( n_content != n_add_content ) {
      stop( "Vectors within 'Descriptive_summary' must match in length" )
    }

    # Close 'Descriptive summary of variable'
  } else {

    Descriptive_summary.content <- ""
    Descriptive_summary.additional_content <- ""

    # Close else for 'Descriptive summary of variable'
  }

  # Categories data collected over
  if ( is.list( Collected_over ) ) {

    # Check inputs
    checkmate::assert_list(
      Collected_over,
      len = 2
    )
    checkmate::assert_names(
      names( Collected_over ),
      permutation.of = c( "content", "additional_content" )
    )

    Collected_over.content <-
      Collected_over$content
    Collected_over.additional_content <-
      Collected_over$additional_content

    # Check vectors match in length
    n_content <- length( Collected_over.content )
    n_add_content <- length( Collected_over.additional_content )

    if ( n_content != n_add_content ) {
      stop( "Vectors within 'Collected_over' must match in length" )
    }

    # Close 'Categories data collected over'
  } else {

    Collected_over.content <- ""
    Collected_over.additional_content <- ""

    # Close else for 'Categories data collected over'
  }

  n_values_and_labels <- length( Values_and_labels.content )
  n_inventory <- length( Inventory.content )
  n_descriptive_summary <- length( Descriptive_summary.content )
  n_codes_for_missing_data <- length( Codes_for_missing_data )
  n_collected_over <- length( Collected_over.content )
  n_created_from <- length( Created_from )
  n_notes <- length( Notes )

  content_type <- c(
    "Category",
    "Data type",
    "Subcategory",
    "Description",
    rep( "Values and labels", n_values_and_labels ),
    rep( "Inventory details", n_inventory ),
    "Units of measurement",
    rep( "Descriptive summary", n_descriptive_summary ),
    rep( "Codes for missing data", n_codes_for_missing_data ),
    "De-identified",
    rep( "Collected over", n_collected_over ),
    rep( "Created from", n_created_from ),
    "Data quality checks",
    rep( "Notes", n_notes )
  )

  content <- c(
    Category,
    Data_type,
    Subcategory,
    Description,
    Values_and_labels.content,
    Inventory.content,
    Units_of_measurement,
    Descriptive_summary.content,
    Codes_for_missing_data,
    Deidentified,
    Collected_over.content,
    Created_from,
    Data_quality_checks,
    Notes
  )

  additional_content <- c(
    "", # Category
    "", # Data type
    "", # Subcategory
    "", # Description
    Values_and_labels.additional_content,
    Inventory.additional_content,
    "", # Units of measurement
    Descriptive_summary.additional_content,
    rep( "", n_codes_for_missing_data ),
    "", # De-identified
    Collected_over.additional_content,
    rep( "", n_created_from ),
    "", # Data quality checks
    rep( "", n_notes )
  )

  cdbk_entry <- data.frame(
    Variable = Variable_name,
    Content_type = content_type,
    Content = content,
    Additional_content = additional_content,
    Entry_type = ""
  )

  # Create class 'codebook_entry' without
  # overwriting 'data.frame' class
  class( cdbk_entry ) <-
    c( class(cdbk_entry), 'codebook_entry' )

  return( cdbk_entry )
}

#### 2.1.1) is.codebook_entry ####
#' Check if a Variable is of Class Codebook Entry
#'
#' Method to check if an R object is of class
#' `codebook_entry`.
#'
#' @param x An R object to be checked.
#'
#' @author Kevin Potter
#'
#' @return A logical value, `TRUE` if `x`
#' is of class `codebook_entry`.
#'
#' @export

is.codebook_entry <- function(
    x ) {

  return( inherits( x, 'codebook_entry' ) )
}

#### 2.2) camr_add_codebook_entry ####
#' Add a Codebook Entry to a Data Frame
#'
#' Function to update the attributes for a column
#' in a data frame to contain a codebook entry.
#' This ensures easy portability (at least within the
#' framework of R) of the codebook for a data set.
#'
#' @param dtf A data frame, preferably one using
#'   standardized naming conventions for columns
#'   (see [camr_column_abbreviations]).
#' @param variable The column name for the variable
#'   whose attributes should be updated to include
#'   a codebook entry. Non-standard evaluation possible.
#' @param description A character string, a brief human-readable
#'   description of what the variable is.
#' @param category An optional character string, the
#'   category label for the variable. If left blank, the
#'   function automatically determines the category via
#'   [camr_column_abbreviations].
#' @param data_type An optional character string, the
#'   data type label for the variable. If left blank, the
#'   function automatically determines the data type via
#'   [camr_column_abbreviations].
#' @param subcategory An optional character string, the
#'   subcategory label for the variable. If left blank, the
#'   function automatically determines the subcategory via
#'   [camr_column_abbreviations].
#' @param values_and_labels Either the empty character set or
#'   a named list of two vectors, (1) \code{content} containing
#'   the distinct values for the variable, and (2)
#'   \code{additional_content} containing the label associated
#'   with each value in (1).
#' @param inventory Either (1) output from the
#'   [camr_inventories] function, (2) the empty character
#'   set, or (3) a named list of two vectors, where \code{content}
#'   contains details on inventory an \code{additional_content}
#'   labels each type of detail (name, number of items, etc.).
#' @param units_of_measurement A character string, the
#'   units of measurement for the variable.
#' @param descriptive_summary Either the empty character set or
#'   a named list of two vectors, (1) \code{content} containing
#'   summary statistics for the variable, and (2)
#'   \code{additional_content} containing the label associated
#'    with each statistic in (1).
#' @param codes_for_missing_data An optional vector with
#'   codes for missing data. If \code{NULL} uses default
#'   codes. If \code{variable} has an attribute with the
#'   codes, the codes in the attribute list override
#'   this argument.
#' @param deidentified Either \code{FALSE} if the variable
#'   is not deidentified (i.e., contains potential patient
#'   health information) or \code{TRUE} otherwise.
#' @param collected_over Either the empty character set or
#'   a named list of two vectors, (1) \code{content} containing
#'   distinct values for groups, time points, etc., over which
#'   the variable was collected, and (2) \code{additional_content}
#'   containing the column name associated with the values in (1).
#' @param created_from An optional character vector, the original
#'   REDCap variables from which the variable was created.
#' @param data_quality_checks An optional character string, notes
#'   on any data quality checks relevant to the variable.
#' @param notes An optional character vector, any additional notes
#'   to include in the codebook entry.
#' @param custom_category An optional matrix with two
#'   columns, 'Abbr' and 'Label', for custom abbreviations
#'   and labels for variable categories.
#' @param custom_data_type An optional matrix with two
#'   columns, 'Abbr' and 'Label', for custom abbreviations
#'   and labels for data types.
#' @param custom_subcategory An optional matrix with two
#'   columns, 'Abbr' and 'Label', for custom abbreviations
#'   and labels for variable subcategories.
#' @param missing_attribute_label A character string, the
#'   name of the element in the list of attributes
#'   containing codes for missing data.
#' @param digits The number of decimal places to round to
#'   for the descriptive summaries.
#' @param add_missing_to_attr An optional logical value;
#'   if \code{FALSE} will not add codes for missing data
#'   to the attributes of \code{Variable}. If \code{NULL}
#'   defaults to adding if no pre-existing attribute is found.
#' @param non_standard A logical value; if \code{TRUE}
#'   uses non-standard evaluation for \code{variable}.
#'
#' @author Kevin Potter
#'
#' @return The data frame \code{dtf}, with the attributes for
#'   the column specified by \code{variable} updated to
#'   contain a data frame of additional class
#'   \code{codebook_entry}.
#'
#' @name camr_add_codebook_entry
#'
#' @examples
#' data( "example_CAM_data_set" )
#' dtf <- example_CAM_data_set
#'
#' # Add codebook entry to data frame
#' dtf <- dtf |>
#'   camr_add_codebook_entry(
#'     IDS.INT.Screening,
#'     description = "Identifier assigned at screening session"
#'   )
#'
#' dtf |> camr_pull_codebook_entry( IDS.INT.Screening )
#'
#' dtf <- dtf |>
#'   camr_add_codebook_entry(
#'     IDS.CHR.Participant,
#'     "Identifier assigned at enrollment"
#'   )
#'
#' dtf |> camr_pull_codebook_entry( IDS.CHR.Participant )
#'
#' dtf <- dtf |>
#'   camr_add_codebook_entry(
#'     INV.INT.HADS_anxiety,
#'     inventory = camr_inventories( "HADS", "Anxiety" ),
#'     descriptive_summary = "continuous",
#'     collected_over = camr_ice(
#'       "combine",
#'       dtf |> camr_collected_over( INV.INT.HADS_anxiety, SSS.CHR.Session ),
#'       dtf |> camr_collected_over( INV.INT.HADS_anxiety, SSS.CHR.Event )
#'     )
#'   )
#' dtf |> camr_pull_codebook_entry( INV.INT.HADS_anxiety )
#'
#' @export

camr_add_codebook_entry <- function(
    dtf,
    variable,
    description = "",
    category = "",
    data_type = "",
    subcategory = "",
    values_and_labels = "",
    inventory = "",
    units_of_measurement = "",
    descriptive_summary = "",
    codes_for_missing_data = NULL,
    deidentified = "FALSE",
    collected_over = "",
    created_from = "",
    data_quality_checks = "",
    notes = "",
    custom_category = NULL,
    custom_data_type = NULL,
    custom_subcategory = NULL,
    missing_attribute_label = "missing",
    digits = 2,
    add_missing_to_attr = NULL,
    non_standard = TRUE ) {

  # Check first input is a data frame
  checkmate::assert_data_frame( dtf )

  if ( non_standard ) {
    column_name <- as.character( substitute( variable ) )
  } else {
    column_name <- variable
  }

  # Functions for checking inputs

  assert_string_or_list <- function( x,
                                     label,
                                     string_content = "" ) {

    err_msg <- paste0(
      "Argument '",
      label,
      "' must be either a list of two vectors ",
      "matching in length or { ",
      paste( paste0( "`", string_content, "`" ), collapse = ", " ),
      " }."
    )

    is_fine <- FALSE

    # Check if list
    if ( is.list(x) ) {

      # Check if two elements in list
      if ( length(x) == 2 ) {
        is_fine <- TRUE

        # Close 'Check if two elements in list'
      }

      # If two-element list
      if ( is_fine ) {

        # Check that element 1 matches in length to element 2
        if ( length( x[[1]] ) != length( x[[2]] ) ) {

          is_fine <- FALSE

          # Close 'Check that element 1 matches in length to element 2'
        }

        # Close 'If two-element list'
      }

      # Close 'Check if list'
    } else {

      # Check if string
      if ( checkmate::check_character( x ) ) {

        is_fine <- TRUE

        # If string
        if ( is_fine ) {

          # If correct options not listed
          if ( any( x %not_in% string_content ) ) {

            is_fine <- FALSE

            # Close 'If correct options not listed'
          }

          # Close 'If string'
        }

        # Close 'Check if string'
      }

      # Close else for 'Check if list'
    }

    if ( !is_fine ) stop( err_msg )

  }

  assert_string_of_length_1 <- function( x, label ) {

    err_msg <- paste0(
      "Argument '",
      label,
      "' must be a unitary character string"
    )

    is_fine <- checkmate::check_character( x )
    if ( is_fine & length( x ) != 1 ) {
      is_fine <- FALSE
    }

    if ( !is_fine ) stop( err_msg )

  }

  # Check column exists in data frame
  if ( column_name %not_in% colnames( dtf ) ) {
    err_msg <- "Variable not found in data frame"
    stop( err_msg )
  }

  # Check 'description'
  assert_string_of_length_1( description, 'description' )

  # Check 'category'
  assert_string_of_length_1( category, 'category' )

  # Check 'data_type'
  assert_string_of_length_1( data_type, 'data_type' )

  # Check 'subcategory'
  assert_string_of_length_1( subcategory, 'subcategory' )

  # Check 'values_and_labels'
  assert_string_or_list( values_and_labels, "values_and_labels" )

  # Check 'inventory'

  # Check 'units_of_measurement'
  assert_string_of_length_1( units_of_measurement, 'units_of_measurement' )

  # Check 'descriptive_summary'
  assert_string_or_list(
    descriptive_summary, 'descriptive_summary',
    string_content = c( '', 'continuous', 'categorical', 'range' )
  )

  # Check 'codes_for_missing_data'

  # Check 'deidentified'
  deidentified <- as.character( deidentified )
  if ( deidentified %not_in% c( "TRUE", "FALSE" ) ) {
    stop(
      "Argument 'deidentified' must be TRUE or FALSE."
    )
  }

  # Check 'collected_over'
  assert_string_or_list(
    collected_over,
    "collected_over",
    string_content = c( "", colnames( dtf ) )
  )

  # Check 'created_from'
  checkmate::assert_character( created_from )

  # Check 'data_quality_checks'
  assert_string_of_length_1( data_quality_checks, 'data_quality_checks' )

  # Check 'notes'
  checkmate::assert_string( notes )

  # Extract variable
  x <- dtf[[ column_name ]]

  x_attributes <- attributes( x )

  # Initialize missing codes for descriptive summary
  codes_for_desc <- NULL

  # Check attributes for variable
  if ( !is.null( x_attributes ) ) {

    # Check for pre-existing codes
    if ( !is.null( x_attributes[[ missing_attribute_label ]] ) ) {

      codes_for_missing_data <- x_attributes[[ missing_attribute_label ]]

      if ( is.null( add_missing_to_attr ) ) {
        add_missing_to_attr <- FALSE
      }

      # Close 'Check for pre-existing codes'
    }

    # Close 'Check attributes for variable'
  }

  # Check for missing data using custom codes
  if ( !is.null( codes_for_missing_data ) ) {

    codes_for_missing_data <- camr_missing(
      x,
      codes = codes_for_missing_data,
      return_codes = TRUE
    )
    if ( is.null( add_missing_to_attr ) ) {
      add_missing_to_attr <- TRUE
    }

    # Close 'Check for missing data using custom codes'
  } else {

    codes_for_missing_data <- camr_missing(
      x,
      return_codes = TRUE
    )
    if ( is.null( add_missing_to_attr ) ) {
      add_missing_to_attr <- TRUE
    }

    # Close else for 'Check for missing data using custom codes'
  }

  # Convert codes for missing data to character string
  codes_for_desc <- codes_for_missing_data
  codes_for_missing_data <-
    camr_in_codebook_entry( "missing", codes_for_missing_data )

  # If specified add codes for missing as attribute
  if ( add_missing_to_attr ) {

    dtf[[ column_name ]] <- camr_add_attribute(
      dtf[[ column_name ]],
      codes_for_desc,
      missing_attribute_label
    )

    # Close 'If specified add codes for missing as attribute'
  }

  # Extract any custom abbreviations
  # and labels for variable names, data
  # types, and subcategories
  custom_abbr_labels <- list(
    category = custom_category,
    data_type = custom_data_type,
    subcategory = custom_subcategory
  )

  # Convert category abbreviations to labels
  if (category == "") {

    category <- camr_column_abbreviations(
      column_name,
      type = 'Category',
      custom = custom_abbr_labels$category
    )

    # Close 'Convert category abbreviations to labels'
  }

  # Convert data type abbreviations to labels
  if (data_type == "") {

    data_type <- camr_column_abbreviations(
      column_name,
      type = 'Data type',
      custom = custom_abbr_labels$data_type
    )

    # Close 'Convert data type abbreviations to labels'
  }

  # Convert subcategory abbreviations to labels
  if ( subcategory == "" ) {

    # Check for custom abbreviations and labels
    if ( !is.null( custom_abbr_labels$subcategory ) ) {

      subcategory <- camr_column_abbreviations(
        column_name,
        type = NULL,
        custom = custom_abbr_labels$subcategory
      )

      # Close 'Check for custom abbreviations and labels'
    }

    # Close 'Convert subcategory abbreviations to labels'
  }

  names_for_lists_of_content <- c( "content", "additional_content" )

  # Check and add list with values and their labels
  if ( is.list( values_and_labels ) ) {

    list_names <- names( values_and_labels )

    # Ensure list elements named correctly
    if ( !all( list_names %in% names_for_lists_of_content ) ) {

      values_and_labels <- list(
        content = values_and_labels[[1]],
        additional_content = values_and_labels[[2]]
      )

      # Close 'Ensure list elements named correctly'
    }

    # Close 'Check and add list with values and their labels'
  }

  # Check if output from dedicated function
  if ( is.list( inventory ) ) {

    list_names <- names( inventory )
    inventory_output <- c( "Description", "Scale", "Subscale", "Units" )

    # If output from "camr_inventories" function
    if ( all( list_names %in% inventory_output ) ) {

      description <- inventory$Description
      units_of_measurement <- inventory$Units
      inventory <- camr_in_codebook_entry(
        "inventory", inventory
      )

      # Close 'If output from "camr_inventories" function'
    }


    # Close 'Check if output from dedicated function'
  }

  # Check inputs for inventory
  assert_string_or_list(
    inventory,
    'inventory'
  )

  # Check and add list with descriptive summary
  if ( is.list( descriptive_summary ) ) {

    list_names <- names( descriptive_summary )

    # Ensure list elements named correctly
    if ( !all( list_names %in% names_for_lists_of_content ) ) {

      descriptive_summary <- list(
        content = descriptive_summary[[1]],
        additional_content = descriptive_summary[[2]]
      )

      # Close 'Ensure list elements named correctly'
    }

    # Close 'Check and add list with descriptive summary'
  } else {

    type_arguments <- c(
      "continuous",
      "categorical",
      "range"
    )

    # Create list with descriptive summary
    if ( descriptive_summary %in% type_arguments ) {

      # If codes for missing data are available
      if ( !is.null( codes_for_desc ) ) {

        descriptive_summary <- camr_descriptive_summary(
          x,
          type = descriptive_summary,
          missing_values = camr_missing( x, codes = codes_for_desc ),
          digits = digits
        )

        # Close 'If codes for missing data are available'
      } else {

        descriptive_summary <- camr_descriptive_summary(
          x,
          type = descriptive_summary,
          digits = digits
        )

        # Close else for 'If codes for missing data are available'
      }

      # Close 'Create list with descriptive summary'
    }

    # Close else for 'Check and add list with descriptive summary'
  }

  # Check and add list with details on collection
  if ( is.list( collected_over ) ) {

    list_names <- names( collected_over )

    # Ensure list elements named correctly
    if ( !all( list_names %in% names_for_lists_of_content ) ) {

      collected_over <- list(
        content = collected_over[[1]],
        additional_content = collected_over[[2]]
      )

      # Close 'Ensure list elements named correctly'
    }

    # Close 'Check and add list with details on collection'
  }

  if ( is.character( collected_over ) ) {

    lsts <- lapply(
      collected_over, function(y) {
        dtf |> camr_collected_over( column_name, y, non_standard = FALSE )
      }
    )

    collected_over <- list(
      content =
        unlist( lapply( seq_along( lsts ), function(i) lsts[[i]]$content ) ),
      additional_content =
        unlist( lapply( seq_along( lsts ),
                        function(i) lsts[[i]]$additional_content ) )
    )

  }

  # Create output
  cdbk_entry <- camr_new_codebook_entry(

    Variable_name = column_name,
    Category = category,
    Data_type = data_type,
    Subcategory = subcategory,
    Description = description,
    Values_and_labels = values_and_labels,
    Inventory = inventory,
    Units_of_measurement = units_of_measurement,
    Descriptive_summary = descriptive_summary,
    Codes_for_missing_data = codes_for_missing_data,
    Deidentified = deidentified,
    Collected_over = collected_over,
    Created_from = created_from,
    Data_quality_checks = data_quality_checks,
    Notes = notes

  )

  no_details <-
    cdbk_entry$Content %in% "" &
    cdbk_entry$Additional_content %in% ""

  cdbk_entry <- cdbk_entry[ !no_details, ]

  cdbk_entry$Entry_type <- 'Pipeline'

  # Update attributes
  dtf[[ column_name ]] <- camr_add_attribute(
    dtf[[ column_name ]],
    cdbk_entry,
    "codebook_entry"
  )

  return( dtf )
}

#### 2.2.1) camr_ace ####

#' @rdname camr_add_codebook_entry
#' @export

camr_ace <- function( ... ) {
  return( camr_add_codebook_entry( ... ) )
}

#### 2.3) camr_in_codebook_entry ####
#' Specify Content for Codebook Entries
#'
#' Convenience function to ensure inputs for
#' [camr_new_codebook_entry] and [camr_add_codebook_entry]
#' are correctly formatted.
#'
#' @param type The type of formatting to carry out, where...
#'   * \code{content} Converts two vectors into a list
#'     with \code{content} and \code{additional_content}.
#'   * \code{custom_abbr} Converts two vectors of (a) abbreviations
#'     and (b) their associated labels into a matrix that
#'     can passed to the \code{custom_category},
#'     \code{custom_data_type}, or \code{custom_subcategory}
#'     arguments.
#'   * \code{inventory} Takes output from the
#'     [camr_inventories] function and converts it
#'     into a list with \code{content} and \code{additional_content}.
#'   * \code{missing} Takes a vector of codes for missing
#'     data and converts it into a character vector.
#' @param ... Additional arguments to be formatted.
#'
#' @author Kevin Potter
#'
#' @return Either (1) a named list with two elements, \code{content}
#' and \code{additional_content}, (2) a matrix with abbreviations
#' and associated labels, or (3) a character vector.
#'
#' @name camr_in_codebook_entry
#'
#' @export

camr_in_codebook_entry <- function(
    type,
    ... ) {

  args <- list(...)

  types <- list(
    content = c(
      "Content", "content",
      "C", "c"
    ),
    custom_abbr = c(
      "Custom abbreviations", "custom abbreviations",
      "custom_abbr",
      "CA", "ca"
    ),
    combine = c(
      "Combine", "combine",
      "Comb", "comb", "+"
    ),
    inventory = c(
      "Inventory details", "inventory details",
      "Inventory", "inventory",
      "I", "i"
    ),
    missing = c(
      "Codes for missing data",
      "codes for missing data",
      'Missingness codes',
      'missingness codes',
      "Missing data",
      "missing data",
      "Missing values",
      "missing values",
      "Missingness",
      "missingness",
      "Missing",
      "missing",
      "M", "m"
    ),
    units_of_measurement = c(
      "Units of measurement", "units of measurement",
      "units_of_measurement",
      "Units", "units",
      "U", "u"
    )
  )

  # Create list with content + additional content
  if ( type %in% types$content ) {

    out <- list(
      content = args[[1]],
      additional_content = args[[2]]
    )
    if ( length( out[[1]] ) != length( out[[2]] ) ) {
      stop( "Vectors must match in length" )
    }

    return( out )

    # Close 'Create list with content + additional content'
  }

  # Create inputs for custom abbreviations
  if ( type %in% types$custom_abbr ) {

    a <- args[[1]]
    l <- args[[2]]

    if ( length(a) != length(l) ) {
      stop("Abbreviations and labels must match in length")
    }

    n_rows <- length(a)

    out <- camr_column_abbreviations( n = n_rows )

    out[,1] <- a; out[,2] <- l

    return( out )

    # Close 'Create inputs for custom abbreviations'
  }

  # Combine separate lists with content
  if ( type %in% types$combine ) {

    N_args <- length( args )
    Max_elments <- sapply(
      1:N_args, function(j) length( args[[j]] )
    )
    Max_elments <- max( Max_elments )

    out <- list(
      content = rep( NA, N_args*Max_elments ),
      additional_content = NA
    )

    inc <- 1
    for ( j in 1:N_args ) {

      for ( k in 1:length( args[[j]][[1]] ) ) {
        out$content[[inc]] <- args[[j]][[1]][k]
        out$additional_content[[inc]] <- args[[j]][[2]][k]

        inc <- inc + 1
      }

    }
    inc <- inc - 1
    out$content <- out$content[1:inc]
    out$additional_content <- out$additional_content[1:inc]

    return( out )

    # Close 'Combine separate lists with content'
  }

  # Extract info to include in codebook for inventories
  if ( type %in% types$inventory ) {

    # args <- list( camr_inventories( "AIS" ) )
    # args <- list( camr_inventories( "BPI", "Severity" ) )

    # If for a subscale
    if ( is.list( args[[1]]$Subscale ) ) {

      range_label <- c( "Min", "Max" )

      if ( all( !is.na( args[[1]]$Subscale$range ) ) ) {

        if ( !is.null( names( args[[1]]$Subscale$range ) ) ) {

          range_label <- names( args[[1]]$Scale$range )

        }

      }

      ref_id <- args[[1]]$Scale$reference_identifier

      n_ref <- length( ref_id )
      if ( is.null( names( ref_id ) ) ) {
        ref_type <- rep( 'No reference', n_ref )
      } else {
        ref_type <- paste0( 'Reference ', names( ref_id ) )
      }

      out <- list(
        content = c(
          args[[1]]$Scale$name,
          args[[1]]$Scale$abbreviation,
          args[[1]]$Scale$n_items,
          args[[1]]$Subscale$name,
          args[[1]]$Subscale$n_items,
          args[[1]]$Subscale$range[1],
          args[[1]]$Subscale$range[2],
          args[[1]]$Subscale$interpretation,
          ref_id
        ),
        additional_content = c(
          "Name",
          "Abbreviation",
          "Number of items",
          "Subscale name",
          "Subscale number of items",
          range_label[1],
          range_label[2],
          "Interpretation",
          ref_type
        )
      )

      # Close 'If for a subscale'
    } else {

      range_label <- c( "Min", "Max" )

      if ( all( !is.na( args[[1]]$Scale$range ) ) ) {

        if ( !is.null( names( args[[1]]$Scale$range ) ) ) {

          range_label <- names( args[[1]]$Scale$range )

        }

      }

      ref_id <- args[[1]]$Scale$reference_identifier

      n_ref <- length( ref_id )
      if ( is.null( names( ref_id ) ) ) {
        ref_type <- rep( 'No reference', n_ref )
      } else {
        ref_type <- paste0( 'Reference ', names( ref_id ) )
      }

      out <- list(
        content = c(
          args[[1]]$Scale$name,
          args[[1]]$Scale$abbreviation,
          args[[1]]$Scale$n_items,
          args[[1]]$Scale$range[1],
          args[[1]]$Scale$range[2],
          args[[1]]$Scale$interpretation,
          ref_id
        ),
        additional_content = c(
          "Name",
          "Abbreviation",
          "Number of items",
          range_label[1],
          range_label[2],
          "Interpretation",
          ref_type
        )
      )

      # Close else for 'If for a subscale'
    }

    return( out )

    # Close 'Extract info to include in codebook for inventories'
  }

  # Convert codes for missing values to string
  if ( type %in% types$missing ) {

    if ( is.null( args[[1]] ) ) {
      out <- ""
    } else {
      out <- as.character( args[[1]] )
      out[ is.na( out ) ] <- "NA"
      out <- paste0(
        "`", out, "`"
      )
    }

    return( out )

    # Close 'Convert codes for missing values to string'
  }

  # Pre-defined labels for units of measurement
  if ( type %in% types$units_of_measurement ) {

    lookup <- args[[1]]

    predefined_units <- list(
      inventory_summed_score = c(
        "Summed score", "summed score", "Sum", "sum",
        "Total", "total"
      ),
      inventory_mean_score = c(
        "Mean score", "mean score",
        "Mean", "mean",
        "Average score", "average score",
        "Average", "average"
      ),
      inventory_composite_score = c(

      ),
      parts_per_million = c(
        "Parts per million", "parts per million",
        "PPM", "ppm"
      ),
      inches = c(
        "Inches", "inches", "in"
      ),
      centimeters = c(
        "Centimeters", "centimeters", "cm"
      ),
      meters = c(
        "Meters"
      ),
      feet = c(
        "Feet", "ft"
      ),
      milliseconds = c(
        "Milliseconds", "milliseconds", "ms"
      ),
      seconds = c(
        "Seconds", "seconds", "s"
      )
    )

    if ( lookup %in% predefined_units$inventory_summed_score ) {
      out <- "Sum over item scores"
    }

    if ( lookup %in% predefined_units$inventory_summed_score ) {
      out <- "Average over item scores"
    }

    if ( lookup %in% predefined_units$inventory_summed_score ) {
      out <- "Composite score computed from items"
    }

    return( out )

    # Close 'Pre-defined labels for units of measurement'
  }

  stop( "Check argument 'type'" )
}

#### 2.3.1) camr_ice ####

#' @rdname camr_in_codebook_entry
#' @export

camr_ice <- function( type, ... ) {
  return( camr_in_codebook_entry( type, ...) )
}

#### 2.4) camr_pull_codebook_entry ####
#' Extract Codebook Entry for a Variable
#'
#' Function to extract a codebook entry for
#' a specified variable, or indicate all
#' variables in a data frame that have
#' codebook entries.
#'
#' @param x Either (a) a data frame or (b) a column
#'   in a data frame.
#' @param variable If \code{x} is a data frame, the column
#'   in the data frame from which the codebook entry
#'   should be extracted.
#'
#' @author Kevin Potter
#'
#' @return Either (a) the data frame of additional
#' class \code{codebook_entry} for the specified
#' variable, or (b) a logical vector equal to
#' \code{TRUE} for columns with codebook entries.
#'
#' @name camr_pull_codebook_entry
#'
#' @examples
#' data( "example_CAM_data_set" )
#' dtf <- example_CAM_data_set[,1:2]
#'
#' # Add codebook entry to data frame
#' dtf <- dtf |>
#'   camr_add_codebook_entry(
#'     IDS.INT.Screening,
#'     description = "Identifier assigned at screening session"
#' )
#' # Identify columns with and without codebook entries
#' camr_pull_codebook_entry( dtf )
#'
#' # Extract codebook entry
#' dtf |> camr_pull_codebook_entry( IDS.INT.Screening )
#' # Alternative approach
#' camr_pull_codebook_entry( dtf$IDS.INT.Screening )
#'
#' @export

camr_pull_codebook_entry <- function(
    x = NULL,
    variable = '' ) {

  if ( is.data.frame( x ) ) {

    # Non-standard evaluation
    column_name = as.character( substitute( variable ) )

    if ( column_name != '' ) {
      out <- attributes( x[[ column_name ]] )$codebook_entry
    } else {

      out = sapply( colnames( x ), function(v) {
        res <- FALSE;
        if ( !is.null( attributes( x[[ v ]] ) ) ) {
          if ( any( 'codebook_entry' %in%
                    names( attributes( x[[ v ]] ) ) ) ) {
            res <- TRUE
          }
        }
        return( res )
      } )
      names( out ) = colnames( x )

    }

  } else {

    out <- attributes( x )$codebook_entry

  }

  return( out )
}

#### 2.4.1) camr_puce ####

#' @rdname camr_pull_codebook_entry
#' @export

camr_puce <- function( ... ) {
  return( camr_pull_codebook_entry( ... ) )
}

#### 2.5) camr_display_codebook_entry ####
#' Display Codebook Entry
#'
#' Print details from a codebook entry to the console window.
#'
#' @param x Either (a) a data frame or (b) a variable with
#'   a \code{codebook_entry} attribute.
#' @param variable If \code{x} is a data frame, the column
#'   in the data frame from which the codebook entry
#'   should be extracted and displayed.
#'
#' @author Kevin Potter
#'
#' @returns Output to the console window detailing the
#' codebook entry.
#'
#' @name camr_display_codebook_entry
#'
#' @examples
#' data( "example_CAM_data_set" )
#' dtf <- example_CAM_data_set[,1:2]
#'
#' # Add codebook entry to data frame
#' dtf <- dtf |>
#'   camr_add_codebook_entry(
#'     IDS.INT.Screening,
#'     description = "Identifier assigned at screening session"
#' )
#'
#' # Display entry
#' dtf |> camr_display_codebook_entry( IDS.INT.Screening )
#'
#' # Alternative approach
#' camr_display_codebook_entry( dtf$IDS.INT.Screening )
#'
#' @export

camr_display_codebook_entry <- function(
    x,
    variable = '' ) {

  # If a data frame is provided
  if ( is.data.frame(x) ) {

    # Non-standard evaluation
    column_name = as.character( substitute( variable ) )

    # If column is found
    if ( column_name %in% colnames(x) ) {

      cdbk_entry <- attributes( x[[ column_name ]] )$codebook_entry

      # Close 'If column is found'
    } else {

      stop( "Column not found in 'x'" )

      # Close else for 'If column is found'
    }

    # Close 'If a data frame is provided'
  } else {

    # If codebook entry is directly provided
    if ( is.codebook_entry(x) ) {

      cdbk_entry <- x

      # Close 'If codebook entry is directly provided'
    } else {

      cdbk_entry <- attributes( x )$codebook_entry

      # Close else for 'If codebook entry is directly provided'
    }

    # Close else for 'If a data frame is provided'
  }

  if ( !is.codebook_entry(cdbk_entry) ) {

    err_msg <- paste0(
      "Not of class 'codebook_entry'"
    )

    stop( err_msg )
  }

  break_into_lines <- function( string ) {

    lines_to_print <- lapply( seq_along( string ), function(i) "" )

    for ( j in seq_along( string) ) {

      lines_to_print[[j]] <- stringr::str_wrap( string[j], width = 60 )
      lines_to_print[[j]] <-
        strsplit( lines_to_print[[j]], split = '\n', fixed = TRUE )[[1]]
    }

    lines_to_print <- unlist( lines_to_print )

    return( lines_to_print )
  }

  print_entry <- function( lines_to_print ) {

    lines_to_print <- paste0( '    ', lines_to_print )
    for ( k in seq_along( lines_to_print ) ) {
      message( lines_to_print[k] )
    }

  }

  message( 'Variable' )
  print_entry( cdbk_entry$Variable[1] )

  index <- cdbk_entry$Content_type == 'Category'
  lines_to_print <- cdbk_entry$Content[index]
  if ( any( lines_to_print != "" ) ) {
    message( 'Category' )
    lines_to_print |> break_into_lines() |> print_entry()
  }

  index <- cdbk_entry$Content_type == 'Data type'
  lines_to_print <- cdbk_entry$Content[index]
  if ( any( lines_to_print != "" ) ) {
    message( 'Data type' )
    lines_to_print |> break_into_lines() |> print_entry()
  }

  index <- cdbk_entry$Content_type == 'Subcategory'
  lines_to_print <- cdbk_entry$Content[index]
  if ( any( lines_to_print != "" ) ) {
    message( 'Subcategory' )
    lines_to_print |> break_into_lines() |> print_entry()
  }

  index <- cdbk_entry$Content_type == 'Description'
  lines_to_print <- cdbk_entry$Content[index]
  if ( any( lines_to_print != "" ) ) {
    message( 'Description' )
    lines_to_print |> break_into_lines() |> print_entry()
  }

  index <- cdbk_entry$Content_type == 'Values and labels'
  if ( any( cdbk_entry$Content[index] != "" ) ) {
    message( 'Values and labels' )
    lines_to_print <- paste0(
      cdbk_entry$Content[index], ' = ',
      cdbk_entry$Additional_content[index]
    )
    lines_to_print |> break_into_lines() |> print_entry()
  }

  index <- cdbk_entry$Content_type == 'Inventory details'
  if ( any( cdbk_entry$Content[index] != "" ) ) {
    message( 'Inventory details' )
    lines_to_print <- paste0(
      cdbk_entry$Additional_content[index], " = ",
      cdbk_entry$Content[index]
    )
    lines_to_print |> break_into_lines() |> print_entry()
  }

  index <- cdbk_entry$Content_type == 'Units of measurement'
  lines_to_print <- cdbk_entry$Content[index]
  if ( any( lines_to_print != "" ) ) {
    message( 'Units of measurement' )
    lines_to_print |> break_into_lines() |> print_entry()
  }

  index <- cdbk_entry$Content_type == 'Descriptive summary'
  if ( any( cdbk_entry$Content[index] != "" ) ) {
    message( 'Descriptive summary' )
    lines_to_print <- paste0(
      cdbk_entry$Additional_content[index], " = ",
      cdbk_entry$Content[index]
    )
    lines_to_print |> break_into_lines() |> print_entry()
  }

  index <- cdbk_entry$Content_type == 'Codes for missing data'
  lines_to_print <- cdbk_entry$Content[index]
  if ( any( lines_to_print != "" ) ) {
    message( 'Codes for missing data' )
    lines_to_print |> break_into_lines() |> print_entry()
  }

  index <- cdbk_entry$Content_type == 'De-identified'
  lines_to_print <- cdbk_entry$Content[index]
  if ( any( lines_to_print != "" ) ) {
    message( 'De-identified' )
    lines_to_print |> break_into_lines() |> print_entry()
  }

  index <- cdbk_entry$Content_type == 'Collected over'
  if ( any( cdbk_entry$Content[index] != "" ) ) {
    message( 'Collected over' )
    lines_to_print <- paste0(
      cdbk_entry$Content[index],
      " (", cdbk_entry$Additional_content[index], ")"
    )
    lines_to_print |> break_into_lines() |> print_entry()
  }

  index <- cdbk_entry$Content_type == 'Created from'
  lines_to_print <- cdbk_entry$Content[index]
  if ( any( lines_to_print != "" ) ) {
    message( 'Created from' )
    lines_to_print |> break_into_lines() |> print_entry()
  }

  index <- cdbk_entry$Content_type == 'Data quality checks'
  lines_to_print <- cdbk_entry$Content[index]
  if ( any( lines_to_print != "" ) ) {
    message( 'Data quality checks' )
    lines_to_print |> break_into_lines() |> print_entry()
  }

  index <- cdbk_entry$Content_type == 'Notes'
  lines_to_print <- cdbk_entry$Content[index]
  if ( any( lines_to_print != "" ) ) {
    message( 'Notes' )
    lines_to_print |> break_into_lines() |> print_entry()
  }

}

#### 2.5.1) camr_dice ####

#' @rdname camr_display_codebook_entry
#' @export

camr_dice <- function( ... ) {
  return( camr_display_codebook_entry( ... ) )
}

#### 2.6) camr_deidentified_codebook_entry ####
#' Flag De-identified Variables
#'
#' Function to flag whether a variable in a data frame
#' is de-identified (i.e., does not risk exposure of
#' patient health information) based on codebook entries.
#'
#' @param dtf A data frame.
#'
#' @details According to the Health Insurance Portability
#' and Accountability Act of 1996 (HIPAA), under the
#' 'Safe Harbor' method of de-identification, there are
#' 18 types of identifiers that should be removed for
#' a data set to avoid risk of exposing patient health
#' information (PHI):
#'   * Names
#'   * All geographic subdivisions smaller than a state
#'   * All elements of dates (except year) directly
#'     related to an individual
#'   * Telephone numbers
#'   * Vehicle identifiers and serial numbers
#'   * Fax numbers
#'   * Device identifiers and serial numbers
#'   * Email addresses
#'   * Web Universal Resource Locators (URLs)
#'   * Social security numbers
#'   * Internet Protocol (IP) addresses
#'   * Medical record numbers
#'   * Biometric identifiers
#'   * Health plan beneficiary numbers
#'   * Full-face photographs and comparable images
#'   * Account numbers
#'   * Any other unique identifying number, characteristic, or code
#'   * Certificate/license numbers
#'
#' @author Kevin Potter
#'
#' @returns A logical vector matching in length to the number
#' of colunns in \code{dtf}, equal to \code{TRUE} for
#' any variables with codebook entries indicating the variable
#' is de-identified.
#'
#' @examples
#' data( "example_CAM_data_set" )
#'
#' @export

camr_deidentified_codebook_entry <- function(
    dtf ) {

  all_columns <- rep( FALSE, ncol(dtf) )
  names( all_columns ) <- colnames( dtf )

  columns_with_entries <- camr_pull_codebook_entry( dtf )

  if ( any( columns_with_entries ) ) {

    N_entries <- sum( columns_with_entries )
    index <- which( columns_with_entries )

    any_deidentified <- sapply( 1:N_entries, function(j) {

      cdbk_entry <- camr_pull_codebook_entry( dtf[[ index[j] ]] )

      out <- as.logical(
        cdbk_entry$Content[ cdbk_entry$Content_type == "De-identified" ]
      )

      return( out )

    } )

    all_columns[ colnames( dtf )[ index ] ] <- any_deidentified

    return( all_columns )
  } else {
    stop( "No codebook entries found" )
  }

}

#### 2.7) camr_data_frame_from_codebook_entry ####
#' Create Data Frame From Codebook Entries
#'
#' Function that creates a data frame containing
#' all codebook entries - useful for creating
#' a .csv codebook.
#'
#' @param dtf A data frame whose variables have
#'   codebook entries.
#'
#' @author Kevin Potter
#'
#' @return A data frame with all codebook entries.
#'
#' @export

camr_data_frame_from_codebook_entry <- function(
    dtf ) {

  columns_with_entries <- camr_pull_codebook_entry( dtf )
  N_entries <- sum( columns_with_entries )

  if ( N_entries == 0 ) {
    stop( "No codebook entries found" )
  }

  index <- which( columns_with_entries )

  N_rows <- sapply( 1:N_entries, function(j) {

    nrow( camr_pull_codebook_entry( dtf[[ index[j] ]] ) ) + 1

  } )

  index_rows <- lapply( seq_along( N_rows ), function(j) {

    return( rep( j, N_rows[j] ) )

  } ) |> unlist()

  out <- data.frame(
    Variable = rep( "---", length( index_rows ) ),
    Content_type = "",
    Content = "",
    Additional_content = "",
    Entry_type = ""
  )

  for ( j in 1:N_entries ) {

    current_entry <- camr_pull_codebook_entry( dtf[[ index[j] ]] )

    i <- which( index_rows == j )

    out[ i[1:nrow(current_entry)], ] <- current_entry

  }

  return( out )
}

#### 2.8) camr_update_codebook_entry ####
#' Update Codebook Entries for Data Frame
#'
#' Function that updates the descriptive summaries,
#' codes for misssing data, and cases collected over
#' for relevant codebook entries for a data frame
#' that is a subset of a larger data set.
#'
#' @param dtf A data frame whose variables have
#'   codebook entries.
#'
#' @author Kevin Potter
#'
#' @return A data frame with updated codebook entries.
#'
#' @export

camr_update_codebook_entry <- function(
    dtf ) {

  columns_with_entries <- camr_pull_codebook_entry( dtf )
  N_entries <- sum( columns_with_entries )

  if ( N_entries == 0 ) {
    stop( "No codebook entries found" )
  }

  index <- which( columns_with_entries )

  # Loop over codebook entries
  for ( j in seq_along( index ) ) {

    # Extract codebook entry
    cdbk_entry <- camr_puce( dtf[[ index[j] ]] )

    #### 2.8.1) Descriptive summary ####

    # Check for descriptive summary
    dsc_sm <- cdbk_entry$Content_type %in% 'Descriptive summary'

    # If entry includes descriptive summary
    if ( any( dsc_sm ) ) {

      is_continuous <-
        '1st quartile' %in% cdbk_entry$Additional_content[ dsc_sm ]
      is_range <-
        'Min' %in% cdbk_entry$Additional_content[ dsc_sm ] &
        '1st quartile' %not_in% cdbk_entry$Additional_content[ dsc_sm ]
      is_categorical <- FALSE
      if ( !is_continuous & !is_range ) is_categorical <- TRUE

      # If summary for continuous variable
      if ( is_continuous ) {

        # Determine number of digits
        val <- cdbk_entry$Content[ dsc_sm ][2]

        if ( val %pm% "." ) {
          dgt <-
            nchar( strsplit( val, split = ".", fixed = TRUE )[[1]][2] )
        } else {
          dgt <- 0
        }

        # Update descriptive summary
        cdbk_cnt <- camr_descriptive_summary(
          dtf[[ index[j] ]],
          type = 'continuous',
          digits = dgt
        )

        # Close 'If summary for continuous variable'
      }

      # If summary for categorical variable
      if ( is_categorical ) {

        # Update descriptive summary
        cdbk_cnt <- camr_descriptive_summary(
          dtf[[ index[j] ]],
          type = 'categorical'
        )

        # Close 'If summary for categorical variable'
      }

      # If summary for range
      if ( is_range ) {

        # Update descriptive summary
        cdbk_cnt <- camr_descriptive_summary(
          dtf[[ index[j] ]],
          type = 'range'
        )

        # Close 'If summary for range'
      }

      # Update codebook entry
      to_keep <-
        cdbk_entry$Additional_content[ dsc_sm ] %in%
        cdbk_cnt$additional_content

      if ( any(to_keep) ) {
        cdbk_entry$Content[ dsc_sm ][ to_keep ] <- cdbk_cnt$content
        cdbk_entry$Additional_content[ dsc_sm ][ to_keep ] <-
          cdbk_cnt$additional_content
      }

      # Close 'If entry includes descriptive summary'
    }

    #### 2.8.2) Codes for missing data ####

    # Check for missing data codes
    cds_md <- cdbk_entry$Content_type %in% 'Codes for missing data'

    # If any codes
    if ( any( cds_md ) ) {

      missing_codes <- cdbk_entry$Content[cds_md]

      # Loop over codes
      for ( k in 1:length( missing_codes ) ) {

        # NA values
        if ( missing_codes[k] == '`NA`' ) {

          # Remove code
          if ( !any( is.na( dtf[[ index[j] ]] ) ) ) {

            cdbk_entry$Content[ cds_md ][k] <- ""
            cdbk_entry$Additional_content[ cds_md ][k] <- ""

            # Close 'Remove code'
          }

          # Close 'NA values'
        } else {

          # Remove code
          if ( missing_codes[k] %not_in% dtf[[ index[j] ]] ) {

              cdbk_entry$Content[ cds_md ][k] <- ""
              cdbk_entry$Additional_content[ cds_md ][k] <- ""

            # Close 'Remove code'
          }

          # Close else for 'NA values'
        }

        # Close 'Loop over codes'
      }

      # Close 'If any codes'
    }

    #### 2.8.3) Collected over ####

    # Check for cases
    cll_ov <- cdbk_entry$Content_type %in% 'Collected over'

    # If any cases
    if ( any( cll_ov ) ) {

      cases_collected_over <- cdbk_entry$Content[cll_ov]
      relevant_variables <- cdbk_entry$Additional_content[cll_ov]

      # Loop over cases
      for ( k in 1:length( cases_collected_over ) ) {

        no_case_found <-
          cases_collected_over[k] %not_in% dtf[[ relevant_variables[k] ]]

        # Remove case
        if ( no_case_found ) {

          cdbk_entry$Content[ cll_ov ][k] <- ""
          cdbk_entry$Additional_content[ cll_ov ][k] <- ""

          # Close 'Remove case'
        }

        # Close 'Loop over cases'
      }

      # Close 'If any cases'
    }

    #### 2.8.4) Update entry in data frame ####

    no_details <-
      cdbk_entry$Content %in% "" &
      cdbk_entry$Additional_content %in% ""

    cdbk_entry <- cdbk_entry[ !no_details, ]

    lst <- attributes( dtf[[ index[j] ]] )
    lst$codebook_entry <- cdbk_entry
    attributes( dtf[[ index[j] ]] ) <- lst

    # Close 'Loop over codebook entries'
  }

  return( dtf )
}

#### 2.9) camr_data_frame_to_codebook_entry ####
#' Convert Data Frame to Codebook Entry
#'
#' Function to extract codebook entries from a
#' data frame and add them to the attributes for
#' the relevant variables in a data frame of
#' observations.
#'
#'
#' @param dtf_main A data frame of observations.
#' @param dtf_codebook A data frame with codebook
#'   entries (see [camr_data_frame_from_codebook_entry]).
#'
#' @author Kevin Potter
#'
#' @return A data frame with codebook entries as
#' attributes for the relevant variables.
#'
#' @export

camr_data_frame_to_codebook_entry <- function(
    dtf_main,
    dtf_codebook ) {

  chr_variable <- unique( dtf_codebook$Variable )
  chr_variable <-
    chr_variable[ chr_variable %not_in% c( "", "---" ) ]

  J <- length( chr_variable )

  for ( j in 1:J ) {

    temp_add_cont <- list(
      content = list(),
      additional_content = list()
    )

    cur_category <- ""
    cur_data_type <- ""
    cur_subcat <- ""
    cur_desc <- ""
    cur_val_and_lab <- ""
    cur_inv <- ""
    cur_units <- ""
    cur_desc_sum <- ""
    cur_codes <- ""
    cur_deid <- "FALSE"
    cur_coll <- ""
    cur_create <- ""
    cur_dqc <- ""
    cur_notes <- ""

    dtf_entry <- dtf_codebook %rows% which(
      dtf_codebook$Variable %in% chr_variable[j]
    )

    content_type <- c(
      "Category", # 1
      "Data type", # 2
      "Subcategory", # 3
      "Description", # 4
      "Values and labels", # 5
      "Inventory details", # 6
      "Units of measurement", # 7
      "Descriptive summary", # 8
      "Codes for missing data", # 9
      "De-identified", # 10
      "Collected over", # 11
      "Created from", # 12
      "Data quality checks", # 13
      "Notes" # 14
    )

    # Category
    entries <- dtf_entry$Content_type %in% content_type[1]
    if ( any( entries ) ) {
      cur_category <- dtf_entry$Content[entries]
    }

    # Data type
    entries <- dtf_entry$Content_type %in% content_type[2]
    if ( any( entries ) ) {
      cur_data_type <- dtf_entry$Content[entries]
    }

    # Subcategory
    entries <- dtf_entry$Content_type %in% content_type[3]
    if ( any( entries ) ) {
      cur_subcat <- dtf_entry$Content[entries]
    }

    # Description
    entries <- dtf_entry$Content_type %in% content_type[4]
    if ( any( entries ) ) {
      cur_desc <- dtf_entry$Content[entries]
    }

    # Values and labels
    entries <- dtf_entry$Content_type %in% content_type[5]
    if ( any( entries ) ) {

      cur_val_and_lab <- temp_add_cont
      cur_val_and_lab$content <-
        dtf_entry$Content[entries]
      cur_val_and_lab$additional_content <-
        dtf_entry$Additional_content[entries]

    }

    # Inventory details
    entries <- dtf_entry$Content_type %in% content_type[6]
    if ( any( entries ) ) {

      cur_inv <- temp_add_cont
      cur_inv$content <-
        dtf_entry$Content[entries]
      cur_inv$additional_content <-
        dtf_entry$Additional_content[entries]

    }

    # Units of measurement
    entries <- dtf_entry$Content_type %in% content_type[7]
    if ( any( entries ) ) {
      cur_units <- dtf_entry$Content[entries]
    }

    # Descriptive summary
    entries <- dtf_entry$Content_type %in% content_type[8]
    if ( any( entries ) ) {

      cur_desc_sum <- temp_add_cont
      cur_desc_sum$content <-
        dtf_entry$Content[entries]
      cur_desc_sum$additional_content <-
        dtf_entry$Additional_content[entries]

    }

    # Codes for missingness
    entries <- dtf_entry$Content_type %in% content_type[9]
    if ( any( entries ) ) {
      cur_codes <- dtf_entry$Content[entries]
    }

    # Deidentified
    entries <- dtf_entry$Content_type %in% content_type[10]
    if ( any( entries ) ) {
      cur_deid <- dtf_entry$Content[entries]
    }

    # Collected over
    entries <- dtf_entry$Content_type %in% content_type[11]
    if ( any( entries ) ) {

      cur_coll <- temp_add_cont
      cur_coll$content <-
        dtf_entry$Content[entries]
      cur_coll$additional_content <-
        dtf_entry$Additional_content[entries]

    }

    # Created from
    entries <- dtf_entry$Content_type %in% content_type[12]
    if ( any( entries ) ) {

      cur_create <- temp_add_cont
      cur_create$content <-
        dtf_entry$Content[entries]
      cur_create$additional_content <-
        dtf_entry$Additional_content[entries]

    }

    # Data-quality checks
    entries <- dtf_entry$Content_type %in% content_type[13]
    if ( any( entries ) ) {
      cur_dqc <- dtf_entry$Content[entries]
    }

    # Notes
    entries <- dtf_entry$Content_type %in% content_type[14]
    if ( any( entries ) ) {
      cur_notes <- dtf_entry$Content[entries]
    }

    cdbk_entry <- camr_new_codebook_entry(
        Variable_name = chr_variable[j],
        Category = cur_category,
        Data_type = cur_data_type,
        Subcategory = cur_subcat,
        Description = cur_desc,
        Values_and_labels = cur_val_and_lab,
        Inventory = cur_inv,
        Units_of_measurement = cur_units,
        Descriptive_summary = cur_desc_sum,
        Codes_for_missing_data = cur_codes,
        Deidentified = cur_deid,
        Collected_over = cur_coll,
        Created_from = cur_create,
        Data_quality_checks = cur_dqc,
        Notes = cur_notes
      )

    # Update attributes
    dtf_main[[ chr_variable[j] ]] <- camr_add_attribute(
      dtf_main[[ chr_variable[j] ]],
      cdbk_entry,
      "codebook_entry"
    )

  }

  return( dtf_main )
}

