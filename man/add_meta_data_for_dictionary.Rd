% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/S16-add_meta_data_for_dictionary.R
\name{add_meta_data_for_dictionary}
\alias{add_meta_data_for_dictionary}
\title{Add Meta-Data for Data Dictionaries}
\usage{
add_meta_data_for_dictionary(
  Column_name = " ",
  Variable_category = " ",
  Data_type = " ",
  Time_points_assessed = "N/A",
  Studies_assessed = "N/A",
  Groups_assessed = "N/A",
  Description = " ",
  Values = " ",
  Labels_for_values = " ",
  Scale = " ",
  Subscale = " ",
  Range = " ",
  Missing_data = "<NA|NaN|>",
  x = NULL,
  prev_attributes = NULL,
  time_points = NULL,
  studies = NULL,
  groups = NULL,
  variable_categories = c(IDS = "Identifier", SSS = "Session details", SBJ =
    "Subject details", INV = "Inventory measures", QTN = "Inventory measures", TLF =
    "Time-line follow-back measures", URN = "Urine test data"),
  data_types = c(INT = "Integer", DBL = "Double float", CHR = "Character string", LGC =
    "Logical", FCT = "Enumerated type", DAT = "R Date-class variable"),
  custom_attr = NULL
)
}
\arguments{
\item{Column_name}{The column name in the main data frame
(e.g., IDS.CHR.Subject, SSS.CHR.Group).}

\item{Variable_category}{The category (i.e., associated
processing script) for the variable. Can be determined
from the initial three-letter abbreviation with
standardized column names (change defaults via the
argument \code{variable_categories}).}

\item{Data_type}{The data type of the values in the column.
Can be determined from the second three-letter abbreviation
with standardized column names (change defaults via the
argument \code{data_types}).}

\item{Time_points_assessed}{For multi-session data, the list
of all time points at which measure was collected, separated
by the pipe symbol (e.g., 'Baseline|2nd time point|3rd time point').
Can also inferred from inputs to the argument \code{time_points}.}

\item{Studies_assessed}{For data collected from multiple
studies, or over multiple phases for a study (e.g., combination
of data from pilot and active studies), the list of all
studies (or phases) during which the measure was collected,
separated by the pipe symbol (e.g., 'Pilot|Active').
Can be inferred from inputs to the argument \code{studies}.}

\item{Groups_assessed}{For data with multiple randomization
groups, the list of groups over which the measure was collected,
separated by the pipe symbol (e.g., 'Group 1|Group 2|Group 3').
Can be inferred from inputs to the argument \code{groups}.}

\item{Description}{Human-readable description of what the variable
is and what it is for.}

\item{Values}{Internally stored values REDcap uses for paired
values and labels (e.g., likert-scale type variables)
separated by the pipe symbol (e.g., '1|2|3|4|5').}

\item{Labels_for_values}{The labels associated with internally
stored values REDcap uses for paired values and labels (e.g.,
likert-scale type variables) separated by the pipe symbol
(e.g., 'Very low|Low|Medium|High|Very high'). Should match in
length to the input for the argument \code{Values}.}

\item{Scale}{If applicable, the full name, abbreviation, and
total number of items for a scale or inventory measure (e.g.,
'Hospital Anxiety Depression SCale (HADS) - 14 items').}

\item{Subscale}{If applicable, the subscale name and its number
of items (e.g., 'Anxiety - 7 items').}

\item{Range}{For numeric variables, the minimum and maximum
observed values. Can be inferred based on input to the
argument \code{x}.}

\item{Missing_data}{The values and/or labels used for missing data.
Must be in the format '<Value 1|Value 2|...|Value N>'. For
example, if missing values are represented by \code{NA},
\code{NAN}, " ", and "", this would be written as '<NA|NaN| |>'.}

\item{x}{The vector of values for the specified variable.}

\item{time_points}{A vector of labels for the time points.}

\item{studies}{A vector of labels for the studies or study phases.}

\item{groups}{A vector of labels for the subject groups.}

\item{variable_categories}{A named vector matching 3-letter
abbreviations with different variable categories.}

\item{data_types}{A named vector matching 3-letter
abbreviations with different data types.}

\item{custom_attr}{An optional named list of lists, which allows
users to specify additional meta-data entries
besides the default ones. Each element in the list corresponds
to a new entry, the name of the entry determined from the name of
the element in the list. Additional information is given in the
details and examples section.}

\item{lst}{A list with the existing attributes/meta-data
assigned to the variable.}
}
\value{
A list with the completed meta-data fields.
}
\description{
A function that adds meta-data (see \code{\link[base]{attributes}})
to variables in a data frame or list that can be used for data
dictionary purposes. For standardized column names, many
fields can be auto-completed.
}
\details{
At a minimum, standardized column names follow the format:
'XXX.YYY.Variable_Description'. Here, 'XXX' is a 3-letter abbreviation
specifying a general content category for a variable (e.g.,
variables for subject or patient identification, variables
that deal with experimental session details, variables for
inventory measures, etc.). Next, 'YYY' is a 3-letter abbreviation
specifying the data type of the variable (e.g., integer-based
values, double float precision numbers, character strings, etc.).
Finally, 'Variable_Description' is a brief human readable description
(e.g., 'Session_dates', or 'Time_points'). With this format,
auto-completion of several fields for meta-data (and by extension
future data dictionaries) become possible as per the
abbreviations and labels defined via the arguments
\code{variable_categories} and \code{data_types}.

For cases that require additional meta-data entries not included
in the default arguments provided by \code{add_meta_data_for_dictionary},
a user can define new entries via the \code{custom_attr} argument.
Furthermore, with careful formatting of the list passed to
the \code{custom_attr} argument, one can create custom meta-data
entries that auto-complete based on abbreviations included in
the column names, just like the \code{Variable_category} and
\code{Data_type} options. The final demonstration in the examples
sections details how this can be done.
}
\examples{
# Example data frame with standardized column names
df <- data.frame(
  IDS.CHR.Subject = rep( LETTERS[1:3], each = 3 ),
  SSS.INT.Time_point = rep( 1:3, 3 ),
  SSS.CHR.Group = rep( c( 'ACT', 'ACT', 'WLC' ), each = 3 ),
  SSS.DAT.Date = rep( Sys.Date(), 9 ),
  INV.DBL.Fake_scores = round( rnorm( 9 ), 1 ),
  stringsAsFactors = F
)
# Assume missing data for first time point
df$INV.DBL.Fake_scores[ seq( 1, 9, 3 ) ] = NA

# Add meta-data for subject ID and time point columns
# Auto-completes variable category and data type
vrb <- colnames( df )[1]
attributes( df[[ vrb ]] ) <- add_meta_data_for_dictionary(
  Column_name = vrb,
  Description = 'Example subject identifier'
)
vrb <- colnames( df )[2]
attributes( df[[ vrb ]] ) <- add_meta_data_for_dictionary(
  Column_name = vrb,
  Description = 'Example study time points'
)

# Add meta-data for details on randomization group
# Details on values and full group label included
vrb <- colnames( df )[3]
attributes( df[[ vrb ]] ) <- add_meta_data_for_dictionary(
  Column_name = vrb,
  Description = 'Example group membership',
  Values = 'ACT|WLC',
  Labels_for_values = 'Active group|Waitlist control'
)

# Add meta-data for date data was collected
# Example of a variable with existing meta-data that
# should not be over-written
vrb <- colnames( df )[4]
attributes( df[[ vrb ]] ) <- add_meta_data_for_dictionary(
  Column_name = vrb,
  Description = 'Example date variable',
  # Prevent 'Date' class from being over-written
  prev_attributes = attributes( df[[ vrb ]] )
)

# Add meta-data for inventory measure
# Specify scale details in format
# 'Inventory name (Abbreviation) - N items'
# Entries on time points, group, and range
# accurately completed by providing vector for scores,
# and columns defining time points and groups
vrb <- colnames( df )[5]
no_na <- !is.na( df[[ vrb ]] )
attributes( df[[ vrb ]] ) <- add_meta_data_for_dictionary(
  Column_name = vrb,
  Description = 'Example inventory scores',
  Scale = 'Fake Inventory (FI) - 10 items'
  x = df[[ vrb ]],
  time_points = unique( df$SSS.INT.Time_point[ no_na ] ),
  groups = unique( df$SSS.CHR.Group[ no_na ] )
)

# --- Example for custom meta-data entries ---

# Create example wide-form data set
df <- data.frame(
  IDS.CHR.AT.Subject = LETTERS[1:4],
  SSS.INT.AT.Group = rep( 1:2, each = 4 ),
  MED.INT.Y1.Took_meds = c( 0, 1, 1, 0 ),
  MED.INT.Y2.Took_meds = c( 0, 1, 1, 0 ),
  stringsAsFactors = F
)

# Create named list with specific elements
# to allow auto-fill of time point info
# based on abbreviations in column names
custom_attr <- list(
# Name for meta-data entry
Study_year = list(
  # Content for entries
  Content = c( 'Year 1 data', 'Year 2 data' ),
  # Abbreviation to match in column name
  Abbreviation = c( 'Y1', 'Y2' ),
  # Start and end of abbreviation in column name
  Position = c( 9, 10 )
) )

# Loop over column names for the time points
# and auto-fill based on 'custom_attr' list
for ( i in 1:2 ) {
  vrb <- paste0( 'MED.INT.Y', i, '.Took_meds' )
  attributes( df[[vrb]] ) <- add_meta_data_for_dictionary(
    Column_name = vrb,
    Description = 'Did patient take meds',
    Values = '0|1',
    Labels_for_values = 'No|Yes',
    prev_attributes = attributes( df[[ vrb ]] ),
    groups = unique( df$SSS.INT.AT.Group ),
    custom_attr = custom_attr
  )
}

}
\author{
Kevin Potter
}
