# School-wide assessment linking code
# Written by...
#   Michael Pascale
#   Kevin Potter
# Maintained by...
#   Kevin Potter
# Email:
#   kpotter5@mgh.harvard.edu
# Please email us directly if you
# have any questions or comments
# Last updated: 2025-04-01

# Table of contents
# 1) camr_SWA_linking_code_simulate
#   1.1) Setup
#   1.2) Linking questions
#   1.3) Generate data
#     1.3.1) Testing/debugging
#       1.3.1.1) Initialize data frame
#       1.3.1.2) Standard linking + no links
#       1.3.1.3) Dissimilarity = 1 [Base]
#       1.3.1.4) Dissimilarity = 1 [Add]
#       1.3.1.5) Duplicate records [Base]
#       1.3.1.6) Duplicate records [Add]
#       1.3.1.7) Subset dissimilarity = 0
#       1.3.1.8) Dissimilarity off by 1
#       1.3.1.9) Duplicate records w/ NA [Base]
#       1.3.1.10) Duplicate records w/ NA [Add]
#       1.3.1.11) Test of priority [School ID over questions]
#       1.3.1.12) Link using different items by time point
#       1.3.1._) Final processing
#     1.3.2) Accuracy testing
#     1.3.3) Duplicate records
#       1.3.3.1) Initialize data frame
#       1.3.3.2) Define linking items
#     1.3.4) Demonstration
#       1.3.4.1) Initialize data frame
#       1.3.4.2) Standard linking + no links
# 2) camr_SWA_linking_code_inputs
#   2.1) Setup
#   2.2) lst_link_across
#   2.3) obj_link_using
#   2.4) lst_link_combo
#   2.5) lst_ignore_nonmissing
# 3) camr_SWA_linking_code
#   3.1) Setup
#     3.1.1) Initialize variables for linking
#   3.2) Link over sets
#     3.2.1) Compute dissimilarity scores
#   3.3) Create linked IDs
# 4) camr_SWA_linking_code_performance
# 5) Helper functions
#   5.1) camr_SWA_linking_code_items
#   5.2) camr_SWA_linking_code_rows
#   5.3) camr_SWA_linking_code_select

#### 1) camr_SWA_linking_code_simulate ####
#' Simulate Data to Link
#'
#' Function to simulate different types of data
#' to pass to the linking code function.
#' Provides (a) data for debugging purposes, (b)
#' realistic data for accuracy assessment, (c)
#' data at a single time point with duplicate
#' records, and (d) a small data set for
#' demonstration purposes.
#'
#' @param chr_type A character string, the type of
#'   data set to generate, where \code{'debug'}
#'   generates data for debugging purposes,
#'   \code{'demo'} generates a small data set for
#'   demonstration purposes,
#'   \code{'accuracy'} generates data for accuracy
#'   assessment, \code{'duplicate'} generates data
#'   for a single time point with duplicate records,
#'   and \code{'demo'} generates a small data set
#'   for demonstration purposes.
#' @param int_RNG_seed A integer value, passed to
#'   [base::set.seed] to ensure reproducible
#'   results when simulating values.
#'
#' @author Kevin Potter
#'
#' @returns A data frame.
#'
#' @examples
#' dtf_demo <- camr_SWA_linking_code_simulate( 'demo' )
#'
#' @export

camr_SWA_linking_code_simulate <- function(
    chr_type = 'debug',
    int_RNG_seed = 20250314 ) {

  #### 1.1) Setup ####

  # Set RNG seed
  set.seed( int_RNG_seed )

  lst_types <- list(
    Accuracy = c(
      'Accuracy',
      'accuracy'
    ),
    Demo = c(
      'Demonstration',
      'demonstration',
      'Demostrate',
      'demostrate',
      'Demo',
      'demo'
    ),
    Duplicate = c(
      'Duplicate',
      'duplicate'
    ),
    Test = c(
      'Test',
      'test',
      'Debug',
      'debug'
    )
  )

  #### 1.2) Linking questions ####

  # Function to help create responses
  if ( FALSE ) {

    fun_create_data_frame <- function(vec_values) {

      vec_values <- vec_values[ !is.na(vec_values) ]

      int_freq <- table(vec_values)

      chr_values <-
        paste0( '    "', names(int_freq),
                c( rep( '",\n', length(int_freq) - 1 ), '"\n' ) ) |>
        paste( collapse = '' )
      chr_probabilities <-
        paste0( '    ', round( int_freq / sum(int_freq), 5 ),
                c( rep( ',\n', length(int_freq) - 1 ), '\n' ) ) |>
        paste( collapse = '' )

      chr_code <- paste0(
        'dtf_possible <- data.frame(\n',
        '  Values = c(\n',
        chr_values,
        "  ),\n",
        "  Prob = c(\n",
        chr_probabilities,
        '  )\n',
        ')\n'
      )

      lst_nuisance <- sapply( chr_code, message )
    }

    # Close 'Function to help create responses'
  }

  # Define possible responses with marginal rates of occurrence

  dtf_possible.SBJ.FCT.Sex <- data.frame(
    Values = c(
      "Female",
      "Male"
    ),
    Prob = c(
      0.51281,
      0.48719
    )
  )

  dtf_possible.SBJ.FCT.Link.BirthMonth <- data.frame(
    Values = c(
      "April",
      "August",
      "December",
      "February",
      "January",
      "July",
      "June",
      "March",
      "May",
      "November",
      "October",
      "September"
    ),
    Prob = c(
      0.08232,
      0.08993,
      0.07893,
      0.07361,
      0.08099,
      0.08947,
      0.08795,
      0.0825,
      0.08811,
      0.07911,
      0.08315,
      0.08394
    )
  )

  dtf_possible.SBJ.FCT.Link.OlderSiblings <- data.frame(
    Values = c(
      "1 older sibling born in April",
      "1 older sibling born in August",
      "1 older sibling born in December",
      "1 older sibling born in February",
      "1 older sibling born in January",
      "1 older sibling born in July",
      "1 older sibling born in June",
      "1 older sibling born in March",
      "1 older sibling born in May",
      "1 older sibling born in November",
      "1 older sibling born in October",
      "1 older sibling born in September",
      "2 older siblings, the oldest born in April",
      "2 older siblings, the oldest born in August",
      "2 older siblings, the oldest born in December",
      "2 older siblings, the oldest born in February",
      "2 older siblings, the oldest born in January",
      "2 older siblings, the oldest born in July",
      "2 older siblings, the oldest born in June",
      "2 older siblings, the oldest born in March",
      "2 older siblings, the oldest born in May",
      "2 older siblings, the oldest born in November",
      "2 older siblings, the oldest born in October",
      "2 older siblings, the oldest born in September",
      "3 or more older siblings, the oldest born in April",
      "3 or more older siblings, the oldest born in August",
      "3 or more older siblings, the oldest born in December",
      "3 or more older siblings, the oldest born in February",
      "3 or more older siblings, the oldest born in January",
      "3 or more older siblings, the oldest born in July",
      "3 or more older siblings, the oldest born in June",
      "3 or more older siblings, the oldest born in March",
      "3 or more older siblings, the oldest born in May",
      "3 or more older siblings, the oldest born in November",
      "3 or more older siblings, the oldest born in October",
      "3 or more older siblings, the oldest born in September",
      "no older siblings"
    ),
    Prob = c(
      0.02652,
      0.03045,
      0.02854,
      0.02439,
      0.02605,
      0.0294,
      0.02486,
      0.02658,
      0.02843,
      0.02605,
      0.02708,
      0.03017,
      0.01222,
      0.01411,
      0.01286,
      0.01198,
      0.01228,
      0.01322,
      0.01319,
      0.01231,
      0.01267,
      0.01187,
      0.01289,
      0.01422,
      0.00846,
      0.01012,
      0.00982,
      0.00877,
      0.00951,
      0.0091,
      0.00938,
      0.00882,
      0.00951,
      0.00871,
      0.01023,
      0.0117,
      0.4035
    )
  )

  dtf_possible.SBJ.FCT.Link.EyeColor <- data.frame(
    Values = c(
      "Black",
      "Blue",
      "Brown",
      "Gray",
      "Green",
      "Hazel"
    ),
    Prob = c(
      0.08253,
      0.20336,
      0.48487,
      0.01889,
      0.08305,
      0.12731
    )
  )

  dtf_possible.SBJ.FCT.Link.MiddleInitial <- data.frame(
    Values = c(
      "a",
      "b",
      "c",
      "d",
      "e",
      "f",
      "g",
      "h",
      "i",
      "j",
      "k",
      "l",
      "m",
      "n",
      "no middle name",
      "o",
      "p",
      "q",
      "r",
      "s",
      "t",
      "u",
      "v",
      "w",
      "x",
      "y",
      "z"
    ),
    Prob = c(
      0.09918,
      0.02545,
      0.04793,
      0.03828,
      0.05895,
      0.02054,
      0.03622,
      0.0153,
      0.01596,
      0.09507,
      0.02377,
      0.04917,
      0.11936,
      0.02487,
      0.10338,
      0.0102,
      0.03068,
      0.00154,
      0.06759,
      0.04722,
      0.02602,
      0.00115,
      0.01275,
      0.01508,
      0.00244,
      0.00814,
      0.00376
    )
  )

  dtf_possible.SBJ.INT.Link.KindergartenYearEst <- data.frame(
    Values = c(
      "2012",
      "2013",
      "2014",
      "2015",
      "2016"
    ),
    Prob = c(
      0.13301,
      0.32219,
      0.39593,
      0.13384,
      0.01504
    )
  )

  # Over 2,000 unique cases so take sample of 200
  dtf_possible.SBJ.CHR.Link.Streetname <- data.frame(
    Values = c(
      "adi",
      "ald",
      "alg",
      "alm",
      "als",
      "ame",
      "ash",
      "asp",
      "bac",
      "bak",
      "bas",
      "bay",
      "bel",
      "bes",
      "bla",
      "bre",
      "bri",
      "bro",
      "cab",
      "can",
      "cap",
      "cat",
      "ced",
      "cen",
      "che",
      "cla",
      "cle",
      "col",
      "com",
      "con",
      "cou",
      "cre",
      "cur",
      "d",
      "dai",
      "dav",
      "dev",
      "dip",
      "dov",
      "dri",
      "eas",
      "edg",
      "edn",
      "elm",
      "end",
      "exc",
      "fai",
      "fir",
      "for",
      "fos",
      "fou",
      "fra",
      "fre",
      "ful",
      "gar",
      "ger",
      "gra",
      "gro",
      "gtm",
      "ham",
      "har",
      "hat",
      "hig",
      "hil",
      "hol",
      "hoo",
      "hoy",
      "hud",
      "hyd",
      "idk",
      "ind",
      "irv",
      "jas",
      "kar",
      "kel",
      "kno",
      "lak",
      "law",
      "lew",
      "lin",
      "lol",
      "lyo",
      "mad",
      "mai",
      "man",
      "map",
      "mas",
      "mca",
      "mon",
      "mor",
      "nai",
      "nor",
      "nou",
      "oak",
      "old",
      "ora",
      "orc",
      "orv",
      "osb",
      "pad",
      "par",
      "pec",
      "per",
      "pin",
      "ple",
      "pom",
      "pon",
      "pri",
      "que",
      "qui",
      "rev",
      "ric",
      "riv",
      "ros",
      "sag",
      "sar",
      "sch",
      "she",
      "six",
      "sou",
      "ste",
      "str",
      "sum",
      "thi",
      "tic",
      "tim",
      "tra",
      "uni",
      "van",
      "ver",
      "vin",
      "vio",
      "vmt",
      "vos",
      "wal",
      "was",
      "wat",
      "wav",
      "wes",
      "wil",
      "win",
      "wit",
      "wol",
      "woo"
    ),
    Prob = c(
      0.00552,
      0.00552,
      0.00552,
      0.00552,
      0.00552,
      0.00552,
      0.00552,
      0.00552,
      0.00552,
      0.00552,
      0.00552,
      0.00552,
      0.00552,
      0.00552,
      0.00552,
      0.01105,
      0.00552,
      0.02762,
      0.00552,
      0.00552,
      0.00552,
      0.00552,
      0.00552,
      0.01105,
      0.00552,
      0.00552,
      0.00552,
      0.00552,
      0.00552,
      0.00552,
      0.00552,
      0.01105,
      0.00552,
      0.00552,
      0.00552,
      0.01105,
      0.00552,
      0.00552,
      0.00552,
      0.00552,
      0.01105,
      0.00552,
      0.00552,
      0.00552,
      0.00552,
      0.00552,
      0.00552,
      0.00552,
      0.00552,
      0.01105,
      0.01105,
      0.01105,
      0.00552,
      0.01657,
      0.00552,
      0.00552,
      0.00552,
      0.00552,
      0.00552,
      0.00552,
      0.00552,
      0.00552,
      0.00552,
      0.00552,
      0.01105,
      0.00552,
      0.00552,
      0.00552,
      0.00552,
      0.01657,
      0.00552,
      0.00552,
      0.00552,
      0.00552,
      0.00552,
      0.00552,
      0.00552,
      0.01105,
      0.00552,
      0.01105,
      0.00552,
      0.00552,
      0.00552,
      0.00552,
      0.00552,
      0.00552,
      0.00552,
      0.00552,
      0.01105,
      0.00552,
      0.00552,
      0.00552,
      0.00552,
      0.00552,
      0.00552,
      0.00552,
      0.00552,
      0.00552,
      0.01105,
      0.00552,
      0.01105,
      0.00552,
      0.00552,
      0.00552,
      0.01105,
      0.00552,
      0.00552,
      0.00552,
      0.00552,
      0.00552,
      0.00552,
      0.00552,
      0.00552,
      0.00552,
      0.00552,
      0.00552,
      0.00552,
      0.00552,
      0.01105,
      0.02762,
      0.00552,
      0.01105,
      0.01105,
      0.00552,
      0.00552,
      0.00552,
      0.00552,
      0.00552,
      0.00552,
      0.00552,
      0.00552,
      0.00552,
      0.00552,
      0.00552,
      0.01657,
      0.01657,
      0.00552,
      0.00552,
      0.00552,
      0.01105,
      0.00552,
      0.00552,
      0.00552,
      0.01657
    )
  )

  chr_linking_questions <- c(
    'SBJ.FCT.Sex',
    'SBJ.FCT.Link.BirthMonth',
    'SBJ.FCT.Link.OlderSiblings',
    'SBJ.FCT.Link.EyeColor',
    'SBJ.FCT.Link.MiddleInitial',
    'SBJ.CHR.Link.Streetname',
    'SBJ.INT.Link.KindergartenYearEst'
  )

  #### 1.3) Generate data ####

  # Generate data for testing/debugging
  if ( chr_type %in% lst_types$Test ) {

    #### 1.3.1) Testing/debugging ####

    #### 1.3.1.1) Initialize data frame ####

    dtf_long <- data.frame(
      IDX.CHR.Origin.ID = '',
      IDX.INT.Origin.LASID = c(
        # Standard linking
        1, rep( NA, 8 ),
        1, rep( NA, 8 ),
        1, rep( NA, 8 ),
        # No links
        2, rep( NA, 8 ),
        3, rep( NA, 8 ),
        4, rep( NA, 8 ),
        # Specific tests
        # + Dissimilarity = 1 [Base]
        rep( NA, 3 ) |> rep(7),
        # + Dissimilarity = 1 [Add]
        rep( NA, 3 ) |> rep(7),
        # + Duplicate records [Base]
        rep( NA, 3 ),
        # + Duplicate records [Add]
        rep( NA, 3 ),
        # + Subset dissimilarity = 0
        rep( NA, 7 ) |> rep(2),
        # + Dissimilarity off by 1
        rep( NA, 7 ) |> rep(2),
        # + Duplicate records w/ NA [Base]
        rep( NA, 3 ),
        # + Duplicate records w/ NA [Add]
        rep( NA, 3 ),
        # + Test of priority [School ID over questions]
        c( 5, 5 ),
        # + Link using different items by time point
        c( 6, NA, 6 )
      ),
      SSS.INT.School.Code = c(
        # Standard linking
        rep( 1, 9*3 ),
        # No links
        rep( 1, 9*3 ),
        # Specific tests
        # + Dissimilarity = 1 [Base]
        c(1, 1, 1) |> rep(7),
        # + Dissimilarity = 1 [Add]
        c(1, 1, 1) |> rep(7),
        # + Duplicate records [Base]
        rep( 1, 3 ),
        # + Duplicate records [Add]
        rep( 1, 3 ),
        # + Subset dissimilarity = 0
        rep( 1, 7 ) |> rep(2),
        # + Dissimilarity off by 1
        rep( 1, 7 ) |> rep(2),
        # + Duplicate records w/ NA [Base]
        rep( 1, 3 ),
        # + Duplicate records w/ NA [Add]
        rep( 1, 3 ),
        # + Test of priority [School ID over questions]
        c( 1, 1 ),
        # + Link using different items by time point
        c( 1, 1, 1 )
      ),
      SSS.INT.SurveyYear = c(
        # Standard linking
        rep( 2023, 9 ),
        rep( 2024, 18 ),
        # No links
        rep( 2023, 9 ),
        rep( 2024, 18 ),
        # Specific tests
        # + Dissimilarity = 1 [Base]
        c(2023, 2023, 2024) |> rep(7),
        # + Dissimilarity = 1 [Add]
        c(2023, 2024, 2024) |> rep(7),
        # + Duplicate records [Base]
        c(2023, 2023, 2024),
        # + Duplicate records [Add]
        c(2023, 2024, 2024),
        # + Subset dissimilarity = 0
        c( 2023, 2024 ) |> rep(7),
        # + Dissimilarity off by 1
        c( 2023, 2024 ) |> rep(7),
        # + Duplicate records w/ NA [Base]
        c(2023, 2023, 2024),
        # + Duplicate records w/ NA [Add]
        c(2023, 2024, 2024),
        # + Test of priority [School ID over questions]
        c( 2023, 2024 ),
        # + Link using different items by time point
        c(2023, 2024, 2024)
      ),
      SSS.CHR.Semester = c(
        # Standard linking
        rep( 'Fall', 9 ),
        rep( 'Spring', 9 ),
        rep( 'Fall', 9 ),
        # No links
        rep( 'Fall', 9 ),
        rep( 'Spring', 9 ),
        rep( 'Fall', 9 ),
        # Specific tests
        # + Dissimilarity = 1 [Base]
        c('Fall', 'Fall', 'Spring') |> rep(7),
        # + Dissimilarity = 1 [Add]
        c('Fall', 'Spring', 'Spring') |> rep(7),
        # + Duplicate records [Base]
        c('Fall', 'Fall', 'Spring'),
        # + Duplicate records [Add]
        c('Fall', 'Spring', 'Spring'),
        # + Subset dissimilarity = 0
        c( 'Fall', 'Spring' ) |> rep(7),
        # + Dissimilarity off by 1
        c( 'Fall', 'Spring' ) |> rep(7),
        # + Duplicate records w/ NA [Base]
        c('Fall', 'Fall', 'Spring'),
        # + Duplicate records w/ NA [Add]
        c('Fall', 'Spring', 'Spring'),
        # + Test of priority [School ID over questions]
        c( 'Fall', 'Spring' ),
        # + Link using different items by time point
        c( 'Fall', 'Spring', 'Fall' )
      ),
      SSS.CHR.Time_point = '',
      SSS.INT.Time_point = c(
        # Standard linking
        rep( 0, 9 ),
        rep( 1, 9 ),
        rep( 2, 9 ),
        # No links
        rep( 0, 9 ),
        rep( 1, 9 ),
        rep( 2, 9 ),
        # Specific tests
        # + Dissimilarity = 1 [Base]
        c(0, 0, 1) |> rep(7),
        # + Dissimilarity = 1 [Add]
        c(0, 1, 1) |> rep(7),
        # + Duplicate records [Base]
        c(0, 0, 1),
        # + Duplicate records [Add]
        c(0, 1, 1),
        # + Subset dissimilarity = 0
        c( 0, 1 ) |> rep(7),
        # + Dissimilarity off by 1
        c( 0, 1 ) |> rep(7),
        # + Duplicate records w/ NA [Base]
        c(0, 0, 1),
        # + Duplicate records w/ NA [Add]
        c(0, 1, 1),
        # + Test of priority [School ID over questions]
        c( 0, 1 ),
        # + Link using different items by time point
        c( 0, 1, 2 )
      ),
      SSS.INT.Grade = c(
        # Standard linking
        rep( 9, 18 ),
        rep( 10, 9 ),
        # No links
        rep( 9, 18 ),
        rep( 10, 9 ),
        # Specific tests
        # + Dissimilarity = 1 [Base]
        c(9, 9, 10) |> rep(7),
        # + Dissimilarity = 1 [Add]
        c(9, 10, 10) |> rep(7),
        # + Duplicate records [Base]
        c(9, 9, 10),
        # + Duplicate records [Add]
        c(9, 10, 10),
        # + Subset dissimilarity = 0
        c( 9, 9 ) |> rep(7),
        # + Dissimilarity off by 1
        c( 9, 9 ) |> rep(7),
        # + Duplicate records w/ NA [Base]
        c(9, 9, 10),
        # + Duplicate records w/ NA [Add]
        c(9, 10, 10),
        # + Test of priority [School ID over questions]
        c( 9, 9 ),
        # + Link using different items by time point
        c( 9, 9, 10 )
      ),
      # Linking questions
      SBJ.FCT.Sex = NA,
      SBJ.FCT.Link.BirthMonth = NA,
      SBJ.FCT.Link.OlderSiblings = NA,
      SBJ.FCT.Link.EyeColor = NA,
      SBJ.FCT.Link.MiddleInitial = NA,
      SBJ.CHR.Link.Streetname = NA,
      SBJ.INT.Link.KindergartenYearEst = NA,
      # Extra variables for true linked status
      QCC.LGC.Linked.True = c(
        # Standard linking
        rep( TRUE, 9*3 ),
        # No links
        rep( FALSE, 9*3 ),
        # Specific tests
        # + Dissimilarity = 1 [Base]
        c(TRUE, FALSE, TRUE) |> rep(7),
        # + Dissimilarity = 1 [Add]
        c(TRUE, FALSE, TRUE) |> rep(7),
        # + Duplicate records [Base]
        rep( TRUE, 3 ),
        # + Duplicate records [Add]
        rep( TRUE, 3 ),
        # + Subset dissimilarity = 0
        c( FALSE, FALSE ) |> rep(7),
        # + Off-by-one error
        c( TRUE, TRUE ) |> rep(7),
        # + Duplicate records w/ NA [Base]
        rep( TRUE, 3 ),
        # + Duplicate records w/ NA [Add]
        rep( TRUE, 3 ),
        # + Test of priority [School ID over questions]
        c( TRUE, TRUE ),
        # + Link using different items by time point
        rep( TRUE, 3 )
      ),
      IDX.INT.Linked.True = 0,
      SSS.CHR.Linked.Test_type = ''
    )
    dtf_long$SSS.CHR.Time_point <- paste0(
      dtf_long$SSS.CHR.Semester, ' ',
      dtf_long$SSS.INT.SurveyYear
    )

    dtf_long$IDX.CHR.Origin.ID <-
      paste0(
        'Fake', 1:nrow(dtf_long)
      )
    dtf_long$IDX.INT.Row <- 1:nrow(dtf_long)

    #### 1.3.1.2) Standard linking + no links ####

    # Standard linking + no links
    for ( l in 1:7 ) {

      dtf_possible <- get(
        paste0( 'dtf_possible.', chr_linking_questions[l] )
      )

      int_unique <- c(
        # Standard linking
        2:9,
        # No link
        (9*3 + 1):(9*6)
      )

      dtf_long[[ chr_linking_questions[l] ]][int_unique] <- sample(
        dtf_possible[[1]],
        size = length(int_unique),
        replace = TRUE,
        prob = dtf_possible[[2]]
      )

      # Copy cases that should be linked
      dtf_long[[ chr_linking_questions[l] ]][2:9 + 9] <-
        dtf_long[[ chr_linking_questions[l] ]][2:9]
      dtf_long[[ chr_linking_questions[l] ]][2:9 + 9*2] <-
        dtf_long[[ chr_linking_questions[l] ]][2:9]

      # Close 'Standard linking'
    }

    # Track actual links
    dtf_long$IDX.INT.Linked.True[ 1:9 ] <- 1:9
    dtf_long$IDX.INT.Linked.True[ 1:9 + 9 ] <- 1:9
    dtf_long$IDX.INT.Linked.True[ 1:9 + 9*2 ] <- 1:9

    # Label test type
    dtf_long$SSS.CHR.Linked.Test_type[ 1:(9*3) ] <-
      'Standard linking'
    dtf_long$SSS.CHR.Linked.Test_type[ (9*3) + 1:(9*3) ] <-
      'Standard no link'

    # Update indices
    int_old <- 9*6
    int_ID_old <- 9

    #### 1.3.1.3) Dissimilarity = 1 [Base] ####

    int_ID <- c(
      c( 1, 2, 1 ),
      c( 1, 2, 1 ) + 2,
      c( 1, 2, 1 ) + 2*2,
      c( 1, 2, 1 ) + 2*3,
      c( 1, 2, 1 ) + 2*4,
      c( 1, 2, 1 ) + 2*5,
      c( 1, 2, 1 ) + 2*6
    )
    int_new <- seq_along(int_ID)
    lst_new <- lapply(
      1:7, function(l) {
        1:3 + 3*(l-1)
      }
    )

    # Loop over variables to differ
    for ( v in 1:7 ) {

      # Loop over linking items
      for (l in 1:7) {

        dtf_possible <- get(
          paste0( 'dtf_possible.', chr_linking_questions[l] )
        )

        # Item to differ
        if ( l == v ) {

          dtf_long[[ chr_linking_questions[l] ]][
            int_old + lst_new[[v]]
          ] <- sample(
            dtf_possible[[1]],
            size = 2,
            replace = FALSE,
            prob = dtf_possible[[2]]
          )[c(1, 2, 1)]

          # Close 'Item to differ'
        } else {

          dtf_long[[ chr_linking_questions[l] ]][
            int_old + lst_new[[v]]
          ] <- sample(
            dtf_possible[[1]],
            size = 1,
            replace = FALSE,
            prob = dtf_possible[[2]]
          )

          # Close else for 'Item to differ'
        }

        # Close 'Loop over linking items'
      }

      # Close 'Loop over variables to differ'
    }

    # Update ID
    dtf_long$IDX.INT.Linked.True[int_old + int_new] <-
      int_ID_old + int_ID

    # Label test type
    dtf_long$SSS.CHR.Linked.Test_type[int_old + int_new] <-
      'Dissimilarity = 1 [Base]'

    # Update indices
    int_old <- int_old + max(int_new)
    int_ID_old <- int_ID_old + max(int_ID)

    #### 1.3.1.4) Dissimilarity = 1 [Add] ####

    int_ID <- c(
      c( 1, 2, 1 ),
      c( 1, 2, 1 ) + 2,
      c( 1, 2, 1 ) + 2*2,
      c( 1, 2, 1 ) + 2*3,
      c( 1, 2, 1 ) + 2*4,
      c( 1, 2, 1 ) + 2*5,
      c( 1, 2, 1 ) + 2*6
    )
    int_new <- seq_along(int_ID)
    lst_new <- lapply(
      1:7, function(l) {
        1:3 + 3*(l-1)
      }
    )

    # Loop over variables to differ
    for ( v in 1:7 ) {

      # Loop over linking items
      for (l in 1:7) {

        dtf_possible <- get(
          paste0( 'dtf_possible.', chr_linking_questions[l] )
        )

        # Item to differ
        if ( l == v ) {

          dtf_long[[ chr_linking_questions[l] ]][
            int_old + lst_new[[v]]
          ] <- sample(
            dtf_possible[[1]],
            size = 2,
            replace = FALSE,
            prob = dtf_possible[[2]]
          )[c(1, 2, 1)]

          # Close 'Item to differ'
        } else {

          dtf_long[[ chr_linking_questions[l] ]][
            int_old + lst_new[[v]]
          ] <- sample(
            dtf_possible[[1]],
            size = 1,
            replace = FALSE,
            prob = dtf_possible[[2]]
          )

          # Close else for 'Item to differ'
        }

        # Close 'Loop over linking items'
      }

      # Close 'Loop over variables to differ'
    }

    # Update ID
    dtf_long$IDX.INT.Linked.True[int_old + int_new] <-
      int_ID_old + int_ID

    # Label test type
    dtf_long$SSS.CHR.Linked.Test_type[int_old + int_new] <-
      'Dissimilarity = 1 [Add]'

    # Update indices
    int_old <- int_old + max(int_new)
    int_ID_old <- int_ID_old + max(int_ID)

    #### 1.3.1.5) Duplicate records [Base] ####

    int_ID <- 1
    int_new <- 1:3

    # Loop over linking items
    for (l in 1:7) {

      dtf_possible <- get(
        paste0( 'dtf_possible.', chr_linking_questions[l] )
      )

      dtf_long[[ chr_linking_questions[l] ]][
        int_old + int_new
      ] <- sample(
        dtf_possible[[1]],
        size = 1,
        replace = FALSE,
        prob = dtf_possible[[2]]
      )

      # Close 'Loop over linking items'
    }

    # Update ID
    dtf_long$IDX.INT.Linked.True[int_old + int_new] <-
      int_ID_old + int_ID

    # Label test type
    dtf_long$SSS.CHR.Linked.Test_type[int_old + int_new] <-
      'Duplicate records [Base]'

    # Update indices
    int_old <- int_old + max(int_new)
    int_ID_old <- int_ID_old + max(int_ID)

    #### 1.3.1.6) Duplicate records [Add] ####

    int_ID <- 1
    int_new <- 1:3

    # Loop over linking items
    for (l in 1:7) {

      dtf_possible <- get(
        paste0( 'dtf_possible.', chr_linking_questions[l] )
      )

      dtf_long[[ chr_linking_questions[l] ]][
        int_old + int_new
      ] <- sample(
        dtf_possible[[1]],
        size = 1,
        replace = FALSE,
        prob = dtf_possible[[2]]
      )

      # Close 'Loop over linking items'
    }

    # Update ID
    dtf_long$IDX.INT.Linked.True[int_old + int_new] <-
      int_ID_old + int_ID

    # Label test type
    dtf_long$SSS.CHR.Linked.Test_type[int_old + int_new] <-
      'Duplicate records [Add]'

    # Update indices
    int_old <- int_old + max(int_new)
    int_ID_old <- int_ID_old + max(int_ID)

    #### 1.3.1.7) Subset dissimilarity = 0 ####

    int_ID <- 1:14
    lst_new <- list(
      1:2,
      3:4,
      5:6,
      7:8,
      9:10,
      11:12,
      13:14
    )

    # Loop over variables to differ
    for ( v in 1:7 ) {

      # Loop over linking items
      for (l in 1:7) {

        dtf_possible <- get(
          paste0( 'dtf_possible.', chr_linking_questions[l] )
        )

        # Item to differ
        if ( l == v ) {

          dtf_long[[ chr_linking_questions[l] ]][
            int_old + lst_new[[v]]
          ] <- sample(
            dtf_possible[[1]],
            size = 2,
            replace = FALSE,
            prob = dtf_possible[[2]]
          )

          # Close 'Item to differ'
        } else {

          dtf_long[[ chr_linking_questions[l] ]][
            int_old + lst_new[[v]]
          ] <- sample(
            dtf_possible[[1]],
            size = 1,
            replace = FALSE,
            prob = dtf_possible[[2]]
          )

          # Close else for 'Item to differ'
        }

        # Close 'Loop over linking items'
      }

      # Close 'Loop over variables to equate'
    }

    # Update ID
    int_new <- unlist(lst_new)
    dtf_long$IDX.INT.Linked.True[int_old + int_new] <-
      0

    # Label test type
    dtf_long$SSS.CHR.Linked.Test_type[int_old + int_new] <-
      'Subset dissimilarity = 0'

    # Update indices
    int_old <- int_old + max(int_new)
    int_ID_old <- int_ID_old + max(int_ID)

    #### 1.3.1.8) Dissimilarity off by 1 ####

    int_ID <- rep( 1:7, each = 2 )
    lst_new <- list(
      1:2,
      3:4,
      5:6,
      7:8,
      9:10,
      11:12,
      13:14
    )

    # Loop over variables to differ
    for ( v in 1:7 ) {

      # Loop over linking items
      for (l in 1:7) {

        dtf_possible <- get(
          paste0( 'dtf_possible.', chr_linking_questions[l] )
        )

        # Item to differ
        if ( l == v ) {

          dtf_long[[ chr_linking_questions[l] ]][
            int_old + lst_new[[v]]
          ] <- sample(
            dtf_possible[[1]],
            size = 2,
            replace = FALSE,
            prob = dtf_possible[[2]]
          )

          # Close 'Item to differ'
        } else {

          dtf_long[[ chr_linking_questions[l] ]][
            int_old + lst_new[[v]]
          ] <- sample(
            dtf_possible[[1]],
            size = 1,
            replace = FALSE,
            prob = dtf_possible[[2]]
          )

          # Close else for 'Item to differ'
        }

        # Close 'Loop over linking items'
      }

      # Close 'Loop over variables to equate'
    }

    # Update ID
    int_new <- unlist(lst_new)
    dtf_long$IDX.INT.Linked.True[int_old + int_new] <-
      int_ID_old + int_ID

    # Label test type
    dtf_long$SSS.CHR.Linked.Test_type[int_old + int_new] <-
      'Dissimilarity off by 1'

    # Update indices
    int_old <- int_old + max(int_new)
    int_ID_old <- int_ID_old + max(int_ID)

    #### 1.3.1.9) Duplicate records w/ NA [Base] ####

    int_ID <- 1
    int_new <- 1:3

    # Select variable to be missing
    int_missing <- sample( 1:7, size = 1 )

    # Loop over linking items
    for (l in 1:7) {

      dtf_possible <- get(
        paste0( 'dtf_possible.', chr_linking_questions[l] )
      )

      dtf_long[[ chr_linking_questions[l] ]][
        int_old + int_new
      ] <- sample(
        dtf_possible[[1]],
        size = 1,
        replace = FALSE,
        prob = dtf_possible[[2]]
      )

      # Set missing value in 2nd base record
      if ( l == int_missing ) {

        dtf_long[[ chr_linking_questions[l] ]][
          int_old + int_new
        ][2] <- NA

        # Close 'Set missing value in 2nd base record'
      }

      # Close 'Loop over linking items'
    }

    # Update ID
    dtf_long$IDX.INT.Linked.True[int_old + int_new] <-
      int_ID_old + int_ID

    # Label test type
    dtf_long$SSS.CHR.Linked.Test_type[int_old + int_new] <-
      'Duplicate records w/ NA [Base]'

    # Update indices
    int_old <- int_old + max(int_new)
    int_ID_old <- int_ID_old + max(int_ID)

    #### 1.3.1.10) Duplicate records w/ NA [Add] ####

    int_ID <- 1
    int_new <- 1:3

    # Select variable to be missing
    int_missing <- sample( 1:7, size = 1 )

    # Loop over linking items
    for (l in 1:7) {

      dtf_possible <- get(
        paste0( 'dtf_possible.', chr_linking_questions[l] )
      )

      dtf_long[[ chr_linking_questions[l] ]][
        int_old + int_new
      ] <- sample(
        dtf_possible[[1]],
        size = 1,
        replace = FALSE,
        prob = dtf_possible[[2]]
      )

      # Set missing value in 1st add record
      if ( l == int_missing ) {

        dtf_long[[ chr_linking_questions[l] ]][
          int_old + int_new
        ][2] <- NA

        # Close 'Set missing value in 1st add record'
      }

      # Close 'Loop over linking items'
    }

    # Update ID
    dtf_long$IDX.INT.Linked.True[int_old + int_new] <-
      int_ID_old + int_ID

    # Label test type
    dtf_long$SSS.CHR.Linked.Test_type[int_old + int_new] <-
      'Duplicate records w/ NA [Add]'

    # Update indices
    int_old <- int_old + max(int_new)
    int_ID_old <- int_ID_old + max(int_ID)

    #### 1.3.1.11) Test of priority [School ID over questions] ####

    int_ID <- 1
    int_new <- 1:2

    # Loop over linking items
    for (l in 1:7) {

      dtf_possible <- get(
        paste0( 'dtf_possible.', chr_linking_questions[l] )
      )

      dtf_long[[ chr_linking_questions[l] ]][
        int_old + int_new
      ] <- sample(
        dtf_possible[[1]],
        size = 2,
        replace = FALSE,
        prob = dtf_possible[[2]]
      )

      # Close 'Loop over linking items'
    }

    # Update ID
    dtf_long$IDX.INT.Linked.True[int_old + int_new] <-
      int_ID_old + int_ID

    # Label test type
    dtf_long$SSS.CHR.Linked.Test_type[int_old + int_new] <-
      'Test of priority [School ID over questions]'

    # Update indices
    int_old <- int_old + max(int_new)
    int_ID_old <- int_ID_old + max(int_ID)

    #### 1.3.1.12) Link using different items by time point ####

    int_ID <- 1
    int_new <- 1:3

    # Loop over linking items
    for (l in 1:7) {

      dtf_possible <- get(
        paste0( 'dtf_possible.', chr_linking_questions[l] )
      )

      dtf_long[[ chr_linking_questions[l] ]][
        int_old + int_new[-1]
      ] <- sample(
        dtf_possible[[1]],
        size = 1,
        replace = FALSE,
        prob = dtf_possible[[2]]
      )

      # Close 'Loop over linking items'
    }

    # Update ID
    dtf_long$IDX.INT.Linked.True[int_old + int_new] <-
      int_ID_old + int_ID

    # Label test type
    dtf_long$SSS.CHR.Linked.Test_type[int_old + int_new] <-
      'Link using different items by time point'

    # Update indices
    int_old <- int_old + max(int_new)
    int_ID_old <- int_ID_old + max(int_ID)

    #### 1.3.1._) Final processing ####

    # Ensure unlinkable true ID is 0
    dtf_long$IDX.INT.Linked.True[
      !dtf_long$QCC.LGC.Linked.True
    ] <- 0

    # Ensure IDs increment by 1
    int_old_IDs <- dtf_long$IDX.INT.Linked.True |>
      unique() |> sort()
    int_new_IDs <- as.numeric(
      as.factor( int_old_IDs )
    ) - 1
    # Loop over IDs
    for ( i in seq_along(int_old_IDs) ) {

      dtf_long$IDX.INT.Linked.True[
        dtf_long$IDX.INT.Linked.True == int_old_IDs[i]
      ] <- int_new_IDs[i]

      # Close 'Loop over IDs'
    }

    # Index for easy filtering
    dtf_long$SSS.INT.Linked.Test_type <- as.numeric(
      as.factor( dtf_long$SSS.CHR.Linked.Test_type )
    )

    return(dtf_long)

    # Close 'Generate data for test/debugging'
  }

  # Generate data for accuracy testing
  if ( chr_type %in% lst_types$Accuracy ) {

    #### 1.3.2) Accuracy testing ####


    # Close 'Generate data for accuracy testing'
  }

  # Generate data with duplicate records
  if ( chr_type %in% lst_types$Duplicate ) {

    #### 1.3.3) Duplicate records ####

    #### 1.3.3.1) Initialize data frame ####

    dtf_long <- data.frame(
      IDX.CHR.Origin.ID = '',
      IDX.INT.Origin.LASID = c(
        # Duplicates
        1, rep( NA, 8 ),
        1, rep( NA, 8 ),
        # Unique
        2, rep( NA, 8 ),
        3, rep( NA, 8 )
      ),
      SSS.INT.School.Code = c(
        # Duplicates
        rep( 1, 9*2 ),
        # Unique
        rep( 1, 9*2 )
      ),
      SSS.INT.SurveyYear = c(
        # Duplicates
        rep( 2023, 18 ),
        # Unique
        rep( 2023, 18 )
      ),
      SSS.CHR.Semester = c(
        # Duplicates
        rep( 'Fall', 18 ),
        # Unique
        rep( 'Fall', 18 )
      ),
      SSS.CHR.Time_point = c(
        # Duplicates
        rep( '2023 Fall', 18 ),
        # Unique
        rep( 'Fall', 18 )
      ),
      SSS.INT.Time_point = c(
        # Duplicates
        rep( 0, 18 ),
        # Unique
        rep( 0, 18 )
      ),
      SSS.INT.Grade = c(
        # Duplicates
        rep( 9, 18 ),
        # Unique
        rep( 9, 18 )
      ),
      # Linking questions
      SBJ.FCT.Sex = NA,
      SBJ.FCT.Link.BirthMonth = NA,
      SBJ.FCT.Link.OlderSiblings = NA,
      SBJ.FCT.Link.EyeColor = NA,
      SBJ.FCT.Link.MiddleInitial = NA,
      SBJ.CHR.Link.Streetname = NA,
      SBJ.INT.Link.KindergartenYearEst = NA,
      # Extra variables for true linked status
      QCC.LGC.Linked.True = c(
        # Duplicates
        rep( TRUE, 9*2 ),
        # Unique
        rep( FALSE, 9*2 )
      ),
      IDX.INT.Linked.True = 0
    )

    dtf_long$IDX.CHR.Origin.ID <-
      paste0(
        'Fake', 1:nrow(dtf_long)
      )
    dtf_long$IDX.INT.Row <- 1:nrow(dtf_long)

    #### 1.3.3.2) Define linking items ####

    # Standard linking + no links
    for ( l in 1:7 ) {

      dtf_possible <- get(
        paste0( 'dtf_possible.', chr_linking_questions[l] )
      )

      int_unique <- c(
        # Standard linking
        2:9,
        # No link
        (9*2 + 1):(9*2)
      )

      dtf_long[[ chr_linking_questions[l] ]][int_unique] <- sample(
        dtf_possible[[1]],
        size = length(int_unique),
        replace = TRUE,
        prob = dtf_possible[[2]]
      )

      # Copy cases that should be linked
      dtf_long[[ chr_linking_questions[l] ]][2:9 + 9] <-
        dtf_long[[ chr_linking_questions[l] ]][2:9]

      # Close 'Standard linking'
    }

    # Track actual links
    dtf_long$IDX.INT.Linked.True[ 1:9 ] <- 1:9
    dtf_long$IDX.INT.Linked.True[ 1:9 + 9 ] <- 1:9

    return(dtf_long)

    # Close 'Generate data with duplicate records'
  }

  # Generate data for demonstration purposes
  if ( chr_type %in% lst_types$Demo ) {

    #### 1.3.4) Demonstration ####

    #### 1.3.4.1) Initialize data frame ####

    dtf_long <- data.frame(
      IDX.CHR.Origin.ID = '',
      IDX.INT.Origin.LASID = c(
        # Standard linking
        1, rep( NA, 8 ),
        1, rep( NA, 8 ),
        1, rep( NA, 8 ),
        # No links
        2, rep( NA, 8 ),
        3, rep( NA, 8 ),
        4, rep( NA, 8 )
      ),
      SSS.INT.School.Code = c(
        # Standard linking
        rep( 1, 9*3 ),
        # No links
        rep( 1, 9*3 )
      ),
      SSS.INT.SurveyYear = c(
        # Standard linking
        rep( 2023, 18 ),
        rep( 2024, 9 ),
        # No links
        rep( 2023, 18 ),
        rep( 2024, 9 )
      ),
      SSS.CHR.Semester = c(
        # Standard linking
        rep( 'Fall', 9 ),
        rep( 'Spring', 9 ),
        rep( 'Fall', 9 ),
        # No links
        rep( 'Fall', 9 ),
        rep( 'Spring', 9 ),
        rep( 'Fall', 9 )
      ),
      SSS.CHR.Time_point = c(
        # Standard linking
        rep( '2023 Fall', 9 ),
        rep( '2023 Spring', 9 ),
        rep( '2024 Fall', 9 ),
        # No links
        rep( 'Fall', 9 ),
        rep( 'Spring', 9 ),
        rep( 'Fall', 9 )
      ),
      SSS.INT.Time_point = c(
        # Standard linking
        rep( 0, 9 ),
        rep( 1, 9 ),
        rep( 2, 9 ),
        # No links
        rep( 0, 9 ),
        rep( 1, 9 ),
        rep( 2, 9 )
      ),
      SSS.INT.Grade = c(
        # Standard linking
        rep( 9, 18 ),
        rep( 10, 9 ),
        # No links
        rep( 9, 18 ),
        rep( 10, 9 )
      ),
      # Linking questions
      SBJ.FCT.Sex = NA,
      SBJ.FCT.Link.BirthMonth = NA,
      SBJ.FCT.Link.OlderSiblings = NA,
      SBJ.FCT.Link.EyeColor = NA,
      SBJ.FCT.Link.MiddleInitial = NA,
      SBJ.CHR.Link.Streetname = NA,
      SBJ.INT.Link.KindergartenYearEst = NA,
      # Extra variables for true linked status
      QCC.LGC.Linked.True = c(
        # Standard linking
        rep( TRUE, 9*3 ),
        # No links
        rep( FALSE, 9*3 )
      ),
      IDX.INT.Linked.True = 0
    )

    dtf_long$IDX.CHR.Origin.ID <-
      paste0(
        'Fake', 1:nrow(dtf_long)
      )
    dtf_long$IDX.INT.Row <- 1:nrow(dtf_long)

    #### 1.3.4.2) Standard linking + no links ####

    # Standard linking + no links
    for ( l in 1:7 ) {

      dtf_possible <- get(
        paste0( 'dtf_possible.', chr_linking_questions[l] )
      )

      int_unique <- c(
        # Standard linking
        2:9,
        # No link
        (9*3 + 1):(9*6)
      )

      dtf_long[[ chr_linking_questions[l] ]][int_unique] <- sample(
        dtf_possible[[1]],
        size = length(int_unique),
        replace = TRUE,
        prob = dtf_possible[[2]]
      )

      # Copy cases that should be linked
      dtf_long[[ chr_linking_questions[l] ]][2:9 + 9] <-
        dtf_long[[ chr_linking_questions[l] ]][2:9]
      dtf_long[[ chr_linking_questions[l] ]][2:9 + 9*2] <-
        dtf_long[[ chr_linking_questions[l] ]][2:9]

      # Close 'Standard linking'
    }

    # Track actual links
    dtf_long$IDX.INT.Linked.True[ 1:9 ] <- 1:9
    dtf_long$IDX.INT.Linked.True[ 1:9 + 9 ] <- 1:9
    dtf_long$IDX.INT.Linked.True[ 1:9 + 9*2 ] <- 1:9

    return(dtf_long)

    # Close 'Generate data for demonstration purposes'
  }

  stop( "Input must be either 'accuracy', 'debug', 'demo', or 'duplicates" )
}

#### 2) camr_SWA_linking_code_inputs ####

camr_SWA_linking_code_inputs <- function(
    dtf_long,
    chr_input = 'lst_link_across',
    lst_pairs = NULL,
    obj_extra = NULL,
    lgc_code = FALSE ) {

  #### 2.1) Setup ####

  lst_input_types <- list(
    across = c(
      'lst_link_across',
      'link_across',
      'link across',
      'across'
    ),
    using = c(
      'obj_link_using',
      'link_using',
      'link using',
      'using',
      'linking questions',
      'link questions',
      'questions',
      'linking items',
      'link items',
      'items'
    ),
    combo = c(
      'lst_link_combo',
      'link_combo',
      'link combo',
      'combo',
      'lst_link_combos',
      'link_combos',
      'link combos',
      'combos'
    )
  )

  # Default linking items
  chr_default_items <- c(
    "SSS.INT.School.Code",
    "IDX.INT.Origin.LASID",
    "SBJ.CHR.Link.Streetname",
    "SBJ.FCT.Link.BirthMonth",
    "SBJ.FCT.Link.EyeColor",
    "SBJ.FCT.Link.MiddleInitial",
    "SBJ.FCT.Link.OlderSiblings",
    "SBJ.FCT.Sex",
    "SBJ.INT.Link.KindergartenYearEst"
  )

  # Default pairs for time points
  if ( is.null( lst_pairs ) ) {

    # Time points
    int_times <- unique(
      dtf_long$SSS.INT.Time_point
    ) |> sort()

    # Total number of possible pairs
    lst_base <- lapply(
      int_times[ -length(int_times) ],
      function(i) { i:max(int_times) }
    )
    int_sets <- sapply(
      lst_base, length
    ) - 1

    # Initialize list
    lst_pairs <- rep(
      list( c( NA, NA ) ),
      sum(int_sets)
    )

    int_inc <- 1

    # Loop over base combos
    for ( j in seq_along(lst_base) ) {

      # Loop over pairs
      for ( k in 2:length( lst_base[[j]] ) ) {

        lst_pairs[[int_inc]][1] <- lst_base[[j]][1]
        lst_pairs[[int_inc]][2] <- lst_base[[j]][k]

        int_inc <- int_inc + 1

        # Close 'Loop over pairs'
      }

      # Close 'Loop over base pairs'
    }

    # Close 'Default pairs for time points'
  }

  #### 2.2) lst_link_across ####

  # Input for linking across time points
  if ( chr_input %in% lst_input_types$across ) {

    chr_code <- paste0(
      'list(\n',
      sapply(
        seq_along(lst_pairs), function(s) {
          paste0( '  TP', lst_pairs[[s]][1],
                  'tTP', lst_pairs[[s]][2],
                  ' = list(\n',
                  '    Base = dtf_long$SSS.INT.Time_point %in% ',
                  lst_pairs[[s]][1], ',\n',
                  '    Add = dtf_long$SSS.INT.Time_point %in% ',
                  lst_pairs[[s]][1], '\n  )' )
        }
      ) |> paste( collapse = ',\n' ),
      '\n)'
    )

    lst_link_across <- eval( parse(text = chr_code ) )

    # Display code in console
    if (lgc_code) {

      chr_code <- paste0(
        'lst_link_across <- ',
        chr_code
      )
      message(chr_code)

      # Close 'Display code in console'
    } else {

      return(lst_link_across)

      # Close else for 'Display code in console'
    }

    # Close 'Input for linking across time points'
  }

  #### 2.3) obj_link_using ####

  # Input for items to link over
  if ( chr_input %in% lst_input_types$using |
       chr_input %in% lst_input_types$combo ) {

    # Initialize output
    obj_link_using <- lapply(
      seq_along(lst_pairs), function(s) return( list() )
    )
    names( obj_link_using ) <- names( lst_pairs )

    chr_all_items <- chr_default_items[
      chr_default_items %in% colnames(dtf_long)
    ]

    # Loop over sets
    for (s in seq_along(obj_link_using) ) {

      # Identify base and add rows
      lgc_base <-
        dtf_long$SSS.INT.Time_point %in% lst_pairs[[s]][1]
      lgc_add <-
        dtf_long$SSS.INT.Time_point %in% lst_pairs[[s]][2]

      # Determine which linking items are non-missing
      lgc_missing_base <- apply(
        dtf_long[lgc_base, chr_all_items], 2, function(x) {
          all( is.na(x) )
        }
      )
      lgc_missing_add <- apply(
        dtf_long[lgc_add, chr_all_items], 2, function(x) {
          all( is.na(x) )
        }
      )

      chr_items <- chr_all_items[
        !lgc_missing_base & !lgc_missing_add
      ]

      obj_link_using[[s]] <- chr_items

      # Close 'Loop over sets'
    }

    # Return results
    if ( chr_input %in% lst_input_types$using ) {

      # Display code in console
      if (lgc_code) {

        chr_code <- paste0(
          'obj_link_using <- list(\n',
          sapply(
            seq_along(obj_link_using), function(s) {
              paste0(
                '  ', names(obj_link_using)[s], ' = c(\n',
                paste(
                  paste0( '    "', obj_link_using[[s]], '"',
                          ' # ', seq_along(obj_link_using[[s]] ) ),
                  collapse = ',\n'
                ),
                '\n  )'
              )
            }
          ) |> paste( collapse = ',\n'),
          '\n)\n'
        )
        message(chr_code)

        # Close 'Display code in console'
      } else {

        return(obj_link_using)

        # Close else for 'Display code in console'
      }

      # Close 'Return results'
    }

    # Close 'Input for items to link over'
  }

  #### 2.4) lst_link_combo ####

  # Input for combinations of items
  if ( chr_input %in% lst_input_types$combo ) {

    # Check if items were already provided
    if ( !is.null(obj_extra) ) {

      # Confirm extra input is...

      # A list
      lgc_is_combo <- is.list(obj_extra)

      # Matches length of lst_pairs
      if ( lgc_is_combo ) {

        lgc_is_combo <- length(lst_pairs) == length(obj_extra)

        # Close 'Matches length of lst_pairs'
      }

      # Contains columns in dtf_long
      if ( lgc_is_combo ) {

        lgc_is_combo <- sapply(
          seq_along(obj_extra), function(s) {
            all( obj_extra[[s]] %in% colnames(dtf_long) )
          }
        ) |> all()

        # Close 'Contains columns in dtf_long'
      }

      # Error if wrong input
      if ( !lgc_is_combo ) {

        chr_error <- paste0(
          "The argument 'obj_extra' must be a list equal ",
          "in length to 'lst_pairs' with the columns to ",
          "use as linking items for each pair of time points"
        )

        stop( chr_error )

        # Close 'Error if wrong input'
      }

      # Update input
      obj_link_using <- obj_extra

      # Close 'Check if items were already provided'
    }

    # # Loop over sets
    # for ( s in seq_along(lst_pairs) ) {
    #
    #   # Loop over standard combos
    #   for (j in 1:) {
    #
    #   }
    #
    #   # Close 'Loop over sets'
    # }

    # Construct indices for standard combos


    # Close 'Input for combinations of items'
  }

  #### 2.5) lst_ignore_nonmissing ####

  return(NULL)
}

#### 3) camr_SWA_linking_code ####
#' Link Records for School-Wide Assessment Data
#'
#' Function to link records (e.g., across different
#' time points) using a set of linking items.
#'
#' @param dtf_long A data frame, must have a column with
#'   integer values for time points (\code{'SSS.INT.Time_point'})
#'   and the relevant columns for the linking items.
#' @param lst_link_across A list of lists, with each sublist specifying
#'   \code{'Base'} and \code{'Add'} logical vectors for the pair of data
#'   subsets in \code{dtf_long} to link over (e.g., \code{'Base'} would
#'   subset the first time point and \code{'Add'} would subset the second
#'   time point). If \code{NULL} the functions infers all possible
#'   pairings over time points from the \code{'SSS.INT.Time_point'}
#'   variable. If the \code{'Base'} and \code{'Add'} logical vectors
#'   are for the same subset, the function checks for duplicate records
#'   instead.
#' @param obj_link_using Either a character vector with the column
#'   names for the linking items, or a list of character vectors,
#'   one vector for each set defined in \code{lst_link_across}.
#'   Passing a list with separate vectors allows using different
#'   linking items for different sets when necessary. If
#'   \code{NULL} assumes the standard set of linking items:
#'   \code{SSS.INT.School.Code}, \code{IDX.INT.Origin.LASID},
#'   \code{SBJ.FCT.Sex}, \code{SBJ.FCT.Link.BirthMonth},
#'   \code{SBJ.FCT.Link.OlderSiblings}, \code{SBJ.FCT.Link.OlderSiblings},
#'   \code{SBJ.FCT.Link.EyeColor}, \code{SBJ.FCT.Link.EyeColor},
#'   \code{SBJ.FCT.Link.MiddleInitial}, \code{SBJ.CHR.Link.Streetname},
#'   and \code{SBJ.INT.Link.KindergartenYearEst}.
#' @param lst_link_combo A list of lists, where each sublist consists of
#'   an integer vector indexing the combination of linking items to
#'   consider in order of priority. One sublist of integer vectors must
#'   be defined for each set defined by \code{lst_link_across}. For a
#'   given sublist, indices apply to the character vector defined
#'   for the relevant set in \code{obj_link_using}, meaning that if
#'   character vectors differ across sets, indices should be defined
#'   accordingly.
#' @param lst_ignore_nonmissing A list of lists, similar to
#'   \code{lst_link_combo}, indicating items to ignore even if they
#'   are not missing when computing a dissimilarity score over a
#'   given combination (thereby allowing records to be linked even if
#'   some items do not match). If \code{c()} (the default) the function
#'   will not ignore non-missing mismatches.
#' @param chr_progress A character string, used to specify how progress
#'   of the function is tracked. If \code{'section'}, prints the completed
#'   sections for the different parts of the linking process to the
#'   console window; if \code{'bar'}, a simple progress bar is shown
#'   on the console window (default); if \code{''} no progress is displayed.
#' @param lgc_matches_only A logical value; if TRUE only computes
#'   returns dissimilarity scores for confirmed matches (results in
#'   faster computation).
#'
#' @author Michael Pascale and Kevin Potter
#'
#' @returns A data frame.
#'
#' @examples
#' # Linking across time points
#' dtf_demo <- camr_SWA_linking_code_simulate('demo')
#' dtf_demo_linked <- camr_SWA_linking_code(dtf_demo)
#'
#' # Identifying duplicate records
#' dtf_dup <- camr_SWA_linking_code_simulate( 'duplicate' )
#' dtf_dup_linked <- camr_SWA_linking_code(
#'   dtf_dup,
#'   lst_link_across = list(
#'     DR2023F = list(
#'       Base = rep( TRUE, nrow(dtf_dup) ),
#'       Add = rep( TRUE, nrow(dtf_dup) )
#'     )
#'   )
#' )
#'
#' @export

camr_SWA_linking_code <- function(
    dtf_long,
    lst_link_across = NULL,
    obj_link_using = NULL,
    lst_link_combo = NULL,
    lst_ignore_nonmissing = NULL,
    chr_progress = 'bar',
    lgc_matches_only = TRUE ) {

  # Progress bar
  lgc_progress <- FALSE
  if ( chr_progress %in% c( 'label', 'labels', 'section', 'sections' ) )
    lgc_progress <- TRUE
  if ( chr_progress == 'bar' )
    lgc_progress_bar <- TRUE else lgc_progress_bar <- FALSE

  if (lgc_progress)
    message( 'Start: camr_SWA_linking_code' )

  # Track run time
  dtt_start <- Sys.time()

  #### 3.1) Setup ####

  # Debugging
  if ( FALSE ) {

    # TO DO:
    # - Check impact of excluding 'already' links
    # - Improve default input creation
    # - Add min dissimilarity score for non-linked
    # - Update documentation

    dtf_long <- camr_SWA_linking_code_simulate( 'debug' )

    lst_link_across <- list(
      TP0tTP1 = list(
        Base = dtf_long$SSS.INT.Time_point %in% 0,
        Add = dtf_long$SSS.INT.Time_point %in% 1
      ),
      TP0tTP2 = list(
        Base = dtf_long$SSS.INT.Time_point %in% 0,
        Add = dtf_long$SSS.INT.Time_point %in% 2
      ),
      TP1tTP2 = list(
        Base = dtf_long$SSS.INT.Time_point %in% 1,
        Add = dtf_long$SSS.INT.Time_point %in% 2
      )
    )

    dtf_linked <- camr_SWA_linking_code(
      dtf_long,
      lst_link_across = lst_link_across
    )

    lst_perf <- camr_SWA_linking_code_performance( dtf_linked )

    lst_ignore_nonmissing <- lapply(
      1:3, function(l) {
        list(
          SLQ_______ = c(),
          S_Q1234567 = c(),
          S_Q_234567 = c(),
          S_Q1_34567 = c(),
          S_Q12_4567 = c(),
          S_Q123_567 = c(),
          S_Q1234_67 = c(),
          S_Q12345_7 = c(),
          S_Q123456_ = c()
        )
      }
    )

    dtf_linked <- camr_SWA_linking_code(
      dtf_long,
      lst_link_across = lst_link_across,
      lst_ignore_nonmissing = lst_ignore_nonmissing
    )

    lst_perf <- camr_SWA_linking_code_performance( dtf_linked )



    # Close 'Debugging'
  }

  if (lgc_progress)
    message( '  Check inputs' )

  lgc_is_dtf <- is.data.frame(dtf_long)
  lgc_has_columns <- all(
    c(
    'SSS.INT.Time_point'
    ) %in% colnames(dtf_long)
  )

  # Make sure row index corresponds to actual rows
  if ( !is.null( dtf_long$IDX.INT.Row) ) {

    dtf_long$IDX.INT.Row.Old <- dtf_long$IDX.INT.Row

    # Close 'Make sure row index exists'
  }

  dtf_long$IDX.INT.Row <- 1:nrow(dtf_long)

  if (lgc_progress)
    message( '  Default options' )

  # Default sets to link across
  if ( is.null(lst_link_across) ) {

    int_times <- unique(
      dtf_long$SSS.INT.Time_point
    ) |> sort()

    lst_base <- lapply(
      int_times[ -length(int_times) ],
      function(i) { i:max(int_times) }
    )
    int_sets <- sapply(
      lst_base, length
    ) - 1

    mat_base_add <- matrix(
      NA, sum(int_sets), 2
    )

    int_inc <- 1

    # Loop over base combos
    for ( j in seq_along(lst_base) ) {

      # Loop over pairs
      for ( k in 2:length( lst_base[[j]] ) ) {

        mat_base_add[int_inc, 1] <- lst_base[[j]][1]
        mat_base_add[int_inc, 2] <- lst_base[[j]][k]

        int_inc <- int_inc + 1

        # Close 'Loop over pairs'
      }

      # Close 'Loop over base pairs'
    }

    lst_link_across <- lapply(
      1:nrow(mat_base_add), function(r) {

        list(
          Base = dtf_long$SSS.INT.Time_point %in% mat_base_add[r, 1],
          Add = dtf_long$SSS.INT.Time_point %in% mat_base_add[r, 2]
        )

      }
    )
    names(lst_link_across) <- paste0(
      'TP', mat_base_add[, 1],
      't',
      'TP', mat_base_add[, 2]
    )

    # Close 'Default sets to link across'
  }

  # Default definitions
  chr_default_items <- c(
    'SSS.INT.School.Code',
    'IDX.INT.Origin.LASID',
    'SBJ.FCT.Sex',
    'SBJ.FCT.Link.BirthMonth',
    'SBJ.FCT.Link.OlderSiblings',
    'SBJ.FCT.Link.EyeColor',
    'SBJ.FCT.Link.MiddleInitial',
    'SBJ.CHR.Link.Streetname',
    'SBJ.INT.Link.KindergartenYearEst'
  )
  lst_standard_combos <- list(
    c( 1:2 ),
    c( 1, 3:9 ), # All
    c( 1, 4:9 ), # -1
    c( 1, 3, 5:9 ), # -2
    c( 1, 3:4, 6:9 ), # -3
    c( 1, 3:5, 7:9 ), # -4
    c( 1, 3:6, 8:9 ), # -5
    c( 1, 3:7, 9 ), # -6
    c( 1, 3:8 ) # -7
  )
  lst_standard_ignore <- list(
    c( 1:2 ),
    c( 1, 3:9 ), # All
    c( 1, 3:9 ), # -1
    c( 1, 3:9 ), # -2
    c( 1, 3:9 ), # -3
    c( 1, 3:9 ), # -4
    c( 1, 3:9 ), # -5
    c( 1, 3:9 ), # -6
    c( 1, 3:9 ) # -7
  )

  # Default items to use to link
  if ( is.null(obj_link_using) ) {

    obj_link_using <- chr_default_items[
      chr_default_items %in% colnames(dtf_long)
    ]

    # Close 'Default linking questions'
  }

  # If a character vector convert to list
  if ( is.character(obj_link_using) ) {

    obj_link_using <- rep(
      list(obj_link_using), length(lst_link_across)
    )

    # Close 'If a character vector'
  }

  # Loop over list elements
  for ( l in seq_along(obj_link_using) ) {

    obj_link_using[[l]] <- obj_link_using[[l]][
      obj_link_using[[l]] %in% colnames(dtf_long)
    ]

    # No linking items
    if ( length( obj_link_using[[l]] ) == 0 ) {

      stop(
        paste0(
          "No linking items found - check column names or ",
          "specify manually using 'obj_link_using' argument"
        )
      )

      # Close 'No linking items'
    }

    # Close 'Loop over list elements'
  }

  # Confirm lengths match for lists
  if ( length(lst_link_across) != length(obj_link_using) ) {

    stop(

      paste0(
        'List of sets of items to link across must match number of ',
        'sets to link over'
      )

    )

    # Close 'Confirm lengths match for lists'
  }

  # Match names
  names(obj_link_using) <- names(lst_link_across)

  # Default combinations of linking items
  if ( is.null(lst_link_combo) ) {

    lst_link_combo <- lapply(
      seq_along(lst_link_across), function(s) {

        chr_items <- obj_link_using[[s]]

        # Using subset of default items
        if ( all( chr_items %in% chr_default_items) ) {

          lst_combo <- c()

          # Loop over combos
          for ( j in seq_along(lst_standard_combos) ) {

            chr_check_items <-
              chr_default_items[ lst_standard_combos[[j]] ]

            # Subset of items present
            if ( all( chr_check_items %in% chr_items ) ) {

              lst_combo <- c(
                lst_combo,
                list( which( chr_items %in% chr_check_items ) )
              )

              # Close 'Subset of items present'
            }

            # Close 'Loop over combos'
          }

          names(lst_combo) <- sapply(
            seq_along(lst_combo), function(j) {
              paste0(
                'I', paste( lst_combo[[j]], collapse = '' )
              )
            }
          )

          # Close 'Using subset of default items'
          } else {

            # Just use all items
            lst_combo <- list(
              ALL = seq_along(chr_items)
            )

            # Close else for 'Using subset of default items'
          }

        return(lst_combo)

      }
    )
    names(lst_link_combo) <- names(lst_link_across)

    # Close 'Default combinations of linking items'
  }

  # Default missingness conditions to skip linking
  if ( is.null(lst_ignore_nonmissing) ) {

    lst_ignore_nonmissing <- lapply(
      seq_along(lst_link_across), function(s) {

        chr_items <- obj_link_using[[s]]

        # Using subset of default items
        if ( all( chr_items %in% chr_default_items) ) {

          lst_combo <- rep(
            list( c() ), length(lst_link_combo[[s]])
          )

          # Loop over combos
          for ( j in seq_along(lst_standard_combos) ) {

            chr_check_items <-
              chr_default_items[ lst_standard_combos[[j]] ]

            # Subset of items present
            if ( all( chr_check_items %in% chr_items ) ) {

              lst_combo[[j]] <- lst_standard_ignore[[j]]

              # Close 'Subset of items present'
            }

            # Close 'Loop over combos'
          }

          names(lst_combo) <- names(lst_link_combo[[s]])

          # Close 'Using subset of default items'
        } else {

          # Ignore all
          lst_combo <- list(
            ALL = c()
          )

          # Close else for 'Using subset of default items'
        }

        return(lst_combo)

      }
    )
    names(lst_ignore_nonmissing) <- names(lst_link_across)

    # Close 'Default missingness conditions to skip linking'
  }

  #### 3.1.1) Initialize variables for linking ####

  chr_linking_columns <- c(
    "IDX.CHR.Linked.ID",
    "QCC.LGC.Linked.Attempted",
    "QCC.LGC.Linked",
    "QCC.LGC.Linked.No_issues",
    "QCC.CHR.Linked.Rows",
    "QCC.CHR.Linked.Set_and_combo",
    "QCC.CHR.Linked.Duplicates",
    "QCC.LGC.Linked.Duplicates",
    "QCC.CHR.Linked.Dissimilarity",
    "QCC.CHR.Linked.Parameters"
  )
  int_set_start <- 0
  int_set_end <- length(lst_link_across)

  dtf_long$IDX.CHR.Linked.ID <- paste0(
    'NL TP',
    dtf_long$SSS.INT.Time_point,
    ' ',
    1:nrow(dtf_long)
  )
  dtf_long$QCC.CHR.Linked.Sets <- ''
  dtf_long$QCC.LGC.Linked.Attempted <- FALSE
  dtf_long$QCC.LGC.Linked <- FALSE
  dtf_long$QCC.LGC.Linked.No_issues <- FALSE
  dtf_long$QCC.CHR.Linked.Rows <- ''
  dtf_long$QCC.CHR.Linked.Set_and_combo <- ''
  dtf_long$QCC.CHR.Linked.Duplicates <- ''
  dtf_long$QCC.LGC.Linked.Duplicates <- FALSE
  dtf_long$QCC.CHR.Linked.Dissimilarity <- ''
  dtf_long$QCC.CHR.Linked.Parameters <-
    'See column attribute [[camr_SWA_linking_code]]'

  #### 3.2) Link over sets ####

  if (lgc_progress)
    message( '  Link over sets' )

  int_prog <- 0

  # Create progress bar parameters
  if (lgc_progress_bar) {

    int_items <- sapply(
      seq_along(obj_link_using), function(s) {
        length( obj_link_using[[s]] )
      }
    )
    int_combo <- sapply(
      seq_along(lst_link_combo), function(s) {
        length(lst_link_combo[[s]])
      }
    )

    int_total <- sum( length(lst_link_across)*2 + sum(int_combo) )

    obj_pb <- txtProgressBar(
      min = 1, max = int_total, style = 3
    )

    # Close 'Create progress bar parameters'
  }

  # Loop over sets
  for ( s in seq_along(lst_link_across) ) {

    if (lgc_progress)
      message( paste0( '    Set: ', names(lst_link_across)[s] ) )

    #### 3.2.1) Compute dissimilarity scores ####

    if ( lgc_progress )
      message( '    Compute dissimilarity scores' )

    lgc_base <-
      lst_link_across[[s]]$Base

    lgc_add <-
      lst_link_across[[s]]$Add

    # Check if assessing for duplicates
    lgc_duplicates <- all( lgc_base == lgc_add )

    # Indicate that linkage was attempted
    dtf_long$QCC.LGC.Linked.Attempted[
      lgc_base | lgc_add
    ] <- TRUE

    # Indicate that linkage was attempted
    dtf_long$QCC.CHR.Linked.Sets[ lgc_base | lgc_add ] <- paste0(
      dtf_long$QCC.CHR.Linked.Sets[ lgc_base | lgc_add ],
      '|', s
    )

    # All possible linking items
    chr_all_items <-
      obj_link_using |> unlist() |> unique() |> sort()
    int_items <- length(chr_all_items)

    int_total_comparisons <-
      sum(lgc_base)*sum(lgc_add)

    # Row indices for comparison pairs
    mat_rows <- matrix(
      NA, int_total_comparisons, 2
    )
    colnames(mat_rows) <- c( 'base', 'add' )
    mat_rows[, 1] <- rep(
      which(lgc_base), each = sum(lgc_add)
    )
    mat_rows[, 2] <- rep(
      which(lgc_add), sum(lgc_base)
    )

    if ( lgc_progress )
      message( '      Computes matches over linking items' )

    # Update progress bar
    int_prog <- int_prog + 1
    if (lgc_progress_bar)
      setTxtProgressBar(obj_pb, int_prog)

    mat_items_base <- matrix(
      NA, int_total_comparisons, int_items
    )
    colnames(mat_items_base) <- chr_all_items
    mat_items_add <- mat_items_base

    # Loop over items
    for ( i in seq_along(chr_all_items) ) {

      if ( lgc_progress )
        message( paste0( '      + Copying (', i, ')' ) )

      mat_items_base[, i] <- rep(
        dtf_long[[ chr_all_items[i] ]][lgc_base],
        each = sum(lgc_add)
      )

      mat_items_add[, i] <- rep(
        dtf_long[[ chr_all_items[i] ]][lgc_add],
        sum(lgc_base)
      )

      # Close 'Loop over items'
    }

    if ( lgc_progress )
      message( paste0( '      + Matching' ) )

    mat_matches <-
      mat_items_base == mat_items_add
    colnames(mat_matches) <- chr_all_items

    # Missing cases
    mat_missing <- is.na( mat_matches )

    # Update progress bar
    int_prog <- int_prog + 1
    if (lgc_progress_bar)
      setTxtProgressBar(obj_pb, int_prog)

    # Exclude comparisons to same record
    if (lgc_duplicates) {

      mat_matches[
        mat_rows[, 1] == mat_rows[, 2],
      ] <- FALSE

      # Close 'Exclude comparisons to same record'
    }

    # Track which pairs have been matched already
    lgc_already <- rep( FALSE, int_total_comparisons )

    # Clean up workspace
    rm( mat_items_base, mat_items_add)
    # Force garbage collection
    gc()

    mat_diss_scores <- matrix(
      NA,
      int_total_comparisons,
      length(lst_link_combo[[s]])
    )

    # Loop over combos
    for ( j in seq_along(lst_link_combo[[s]] ) ) {

      if ( lgc_progress )
        message( paste0( '      + Dissimilarity scores (', j, ')' ) )

      chr_current_items <- obj_link_using[[s]][
        lst_link_combo[[s]][[j]]
      ]
      mat_diss_scores[, j] <-
        length(chr_current_items) -
        rowSums( mat_matches[, chr_current_items] )

      # Avoid linking if larger subset not missing
      if ( length( lst_ignore_nonmissing[[s]][[j]] ) > 0 ) {

        chr_any_missing <-
          obj_link_using[[s]][
            lst_ignore_nonmissing[[s]][[j]]
          ]
        mat_diss_scores[, j] <-
          mat_diss_scores[, j] +
          ( ( length( chr_any_missing ) -
              rowSums( as.matrix( mat_missing[, chr_any_missing] ) ) ) -
              length(chr_current_items) )

        # Close 'Avoid linking if larger subset not missing'
      }

      lgc_zero <-
        mat_diss_scores[, j] %in% 0 &
        !lgc_already

      # Any matches
      if ( any(lgc_zero) ) {

        lgc_already[lgc_zero] <- TRUE

        mat_pairs <- mat_rows[lgc_zero, ]

        # Make sure is matrix
        if ( is.null( dim(mat_pairs) ) ) {

          mat_pairs <- rbind( mat_pairs )

          # Close 'Make sure is matrix'
        }

        # Check for duplicate matches
        int_unique_base <- table( mat_pairs[, 1] )
        int_unique_add <- table( mat_pairs[, 2] )

        int_unique_base <- as.numeric(
          names(int_unique_base)[int_unique_base == 1]
        )
        int_unique_add <- as.numeric(
          names(int_unique_add)[int_unique_add == 1]
        )

        lgc_unique <-
          mat_pairs[, 1] %in% int_unique_base &
          mat_pairs[, 2] %in% int_unique_add

        # Update data set

        # Loop over base and add subsets
        for ( k in 1:2 ) {

          int_rows <- mat_pairs[lgc_unique, k]
          dtf_long$QCC.CHR.Linked.Rows[int_rows] <- paste0(
            dtf_long$QCC.CHR.Linked.Rows[int_rows],
            mat_pairs[lgc_unique, 1],
            ',',
            mat_pairs[lgc_unique, 2],
            ';'
          )
          dtf_long$QCC.CHR.Linked.Set_and_combo[int_rows] <- paste0(
            dtf_long$QCC.CHR.Linked.Set_and_combo[int_rows],
            s,
            ',',
            j,
            ';'
          )
          dtf_long$QCC.LGC.Linked[int_rows] <- TRUE
          dtf_long$QCC.CHR.Linked.Dissimilarity[int_rows] <- paste0(
            dtf_long$QCC.CHR.Linked.Dissimilarity[int_rows],
            '0;'
          )

          # Close 'Loop over base and add subsets'
        }

        # Duplicate matches
        if ( any(!lgc_unique) ) {

          # Loop over individual pairs
          for ( p in which(!lgc_unique) ) {

            # Update data set
            int_rows <- mat_pairs[p, 1]

            # Loop over base and add subsets
            for ( k in 1:2 ) {

              int_rows <- mat_pairs[p, k]
              dtf_long$QCC.CHR.Linked.Duplicates[int_rows] <- paste0(
                dtf_long$QCC.CHR.Linked.Duplicates[int_rows],
                mat_pairs[p, 1],
                ',',
                mat_pairs[p, 2],
                ';'
              )
              dtf_long$QCC.LGC.Linked.Duplicates[int_rows] <- TRUE

              # Close 'Loop over base and add subsets'
            }

            # Close 'Loop over individual pairs'
          }

          # Close 'Duplicate matches'
        }

        # Close 'Any matches'
      }

      # Update progress bar
      if (lgc_progress_bar)
        setTxtProgressBar(obj_pb, int_prog + j)

      # Close 'Loop over combos'
    }
    int_prog <- int_prog + length(lst_link_combo[[s]])

    # Compute dissimilarity scores for non-matches
    if (!lgc_matches_only) {

      # Close 'Compute dissimilarity scores for non-matches'
    }

    # Clean up workspace
    rm(
      mat_matches,
      mat_missing, lgc_already,
      mat_diss_scores, lgc_zero
    )
    # Force garbage collection to reduce memory load
    gc()

    # Close 'Loop over sets'
  }

  #### 3.3) Create linked IDs ####

  lgc_linked <- dtf_long$QCC.LGC.Linked

  # Extract unique linked pairs
  chr_linked_rows <-
    dtf_long$QCC.CHR.Linked.Rows[
      lgc_linked & dtf_long$QCC.CHR.Linked.Rows != ''
    ]

  chr_linked_rows <- lapply(
    chr_linked_rows, function(s) {
      chr_out <- strsplit( s, split = ';', fixed = TRUE )[[1]]
      chr_out <- chr_out[ chr_out != '' ]
      return(chr_out )
    }
  ) |> unlist() |> unique()
  mat_linked_rows <- sapply(
    chr_linked_rows, function(r) {
      strsplit( r, split = ',', fixed = TRUE )[[1]] |> as.numeric()
    }
  )

  # Define all possible links
  int_all_rows <- unique( as.vector( mat_linked_rows[1:2, ] ) ) |> sort()

  lst_all_links <- rep(
    list(NULL), length(int_all_rows)
  )

  int_to_check <- int_all_rows
  int_inc <- 1

  # Loop over possible links
  for (i in seq_along(int_all_rows)) {

    lgc_col <-
      mat_linked_rows[1, ] %in% int_all_rows[i] |
      mat_linked_rows[2, ] %in% int_all_rows[i]
    int_all_combos <-
      mat_linked_rows[1:2, lgc_col] |>
      as.vector() |> unique() |> sort()
    lgc_col <-
      mat_linked_rows[1, ] %in% int_all_combos |
      mat_linked_rows[2, ] %in% int_all_combos

    # First time
    if ( i == 1 ) {

      int_rows_to_consider <- as.vector(
        mat_linked_rows[1:2, lgc_col]
      ) |> unique() |> sort()

      lst_all_links[[int_inc]] <- int_rows_to_consider

      int_to_check <- int_to_check[
        !int_to_check %in% lst_all_links[[int_inc]]
      ]
      int_inc <- int_inc + 1

      # Close 'First time'
    } else {

      # Check if row has not already been included
      if ( int_all_rows[i] %in% int_to_check) {

        int_rows_to_consider <- as.vector(
          mat_linked_rows[1:2, lgc_col]
        ) |> unique() |> sort()

        lst_all_links[[int_inc]] <- int_rows_to_consider

        int_to_check <- int_to_check[
          !int_to_check %in% lst_all_links[[int_inc]]
        ]
        int_inc <- int_inc + 1

        # Close 'Check if row has not already been included'
      }

      # Close else for 'First time'
    }

    # Close 'Loop over possible links'
  }

  # Remove empty slots
  lst_all_links <- lst_all_links[
    !sapply( lst_all_links, is.null )
  ]

  # Loop over possible links
  for ( l in seq_along(lst_all_links) ) {

    dtf_long$IDX.CHR.Linked.ID[ lst_all_links[[l]] ] <-
      gsub(
        'NL', 'YL', dtf_long$IDX.CHR.Linked.ID[ lst_all_links[[l]] ]
      )[1]
    dtf_long$QCC.LGC.Linked.No_issues[ lst_all_links[[l]] ] <- TRUE

    # Close 'Loop over possible links'
  }

  # Check for duplicate records post-hoc
  lgc_linked <- dtf_long$QCC.LGC.Linked.No_issues
  dtf_IDs <- aggregate(
    dtf_long$SSS.INT.Time_point[lgc_linked],
    list(
      dtf_long$IDX.CHR.Linked.ID[lgc_linked]
    ),
    function(x) any( table(x) > 1 )
  )

  # Undo linkage for post-hoc duplicates
  lgc_posthoc_dup <-
    dtf_long$IDX.CHR.Linked.ID %in% dtf_IDs[[1]][ dtf_IDs[[2]] ]
  dtf_long$QCC.LGC.Linked.Duplicates[
    lgc_posthoc_dup
  ] <- TRUE
  dtf_long$QCC.LGC.Linked.No_issues[
    lgc_posthoc_dup
  ] <- FALSE
  dtf_long$IDX.CHR.Linked.ID[ lgc_posthoc_dup ] <-
    gsub(
      "YL", "NL",
      dtf_long$IDX.CHR.Linked.ID[ lgc_posthoc_dup ], fixed = TRUE
    )

  fun_gsub <- function(
    chr_string,
    chr_pattern,
    chr_with ) {

    return(
      gsub( chr_pattern, chr_with, chr_string, fixed = TRUE )
    )

  }

  # Match IDs but do not mark as no issues for remaining duplicates
  lgc_dup <-
    dtf_long$QCC.LGC.Linked.Duplicates
  int_freq <-
    table( dtf_long$IDX.CHR.Linked.ID[lgc_dup] )
  chr_ID_dup <- names( int_freq )
  chr_ID_dup <- chr_ID_dup[ int_freq == 1 ]

  # Loop over IDs
  for ( i in seq_along(chr_ID_dup) ) {

    lgc_ID <- dtf_long$IDX.CHR.Linked.ID %in% chr_ID_dup[i]

    if ( any( lgc_ID ) ) {

      int_rows <- as.numeric(
        strsplit(
          dtf_long$QCC.CHR.Linked.Duplicates[lgc_ID] |>
            fun_gsub( ',', ' ' ) |>
            fun_gsub( ';', ' ' ),
          split = ' ', fixed = TRUE
        )[[1]]
      )

      lgc_poss_dup <-
        dtf_long$IDX.INT.Row %in% int_rows &
        dtf_long$QCC.LGC.Linked.Duplicates

      # Update duplicate IDs for later checking
      if ( sum(lgc_poss_dup) > 1 ) {

        int_most <- table(
          dtf_long$IDX.CHR.Linked.ID[
            lgc_poss_dup
          ]
        )

        dtf_long$IDX.CHR.Linked.ID[
          lgc_poss_dup
        ] <- rev( names( int_most ) )[1]

        # Close 'Update duplicate IDs for later checking'
      }

    }

    # Close 'Loop over IDs'
  }

  # Add details about linking parameters as attributes
  attributes(dtf_long$QCC.CHR.Linked.Parameters) <- list(
    camr_SWA_linking_code = list(
      lst_link_across = lst_link_across,
      obj_link_using = obj_link_using,
      lst_link_combo = lst_link_combo,
      lst_ignore_nonmissing = lst_ignore_nonmissing
    )
  )

  # Track run time
  dtt_end <- Sys.time()

  # Display run time
  if ( lgc_progress | lgc_progress_bar ) {

    message('')
    print(dtt_end - dtt_start)

    # Close 'Display run time'
  }

  return( dtf_long )
}

#### 4) Helper functions ####

#### 4.1) camr_SWA_linking_code_items ####
#' Extract Linking Item Column Names
#'
#' Function to extract column names for all
#' variables used as linking items.
#'
#' @param dtf_linked A data frame, output from
#'   the [camrprojects::camr_SWA_linking_code]
#'   function.
#'
#' @returns A character vector, the variables used
#' as linking items.
#'
#' @export

camr_SWA_linking_code_items <- function(
    dtf_linked ) {

  chr_linking_items <- attributes(
    dtf_linked$QCC.CHR.Linked.Parameters
  )$camr_SWA_linking_code$obj_link_using |>
    unlist() |> unique()

  return( chr_linking_items )
}

#### 4.2) camr_SWA_linking_code_rows ####
#' Extract Rows Identified as Possible Links
#'
#' Extract rows identified as possible links for a
#' given record.
#'
#' @param dtf_linked A data frame, output from
#'   the [camrprojects::camr_SWA_linking_code]
#'   function.
#' @param int_row The row from \code{dtf_linked} to
#'   process.
#' @param lgc_filter A logical value; if \code{TRUE}
#'   returns the data frame rows, otherwise
#'   returns the extract rows.
#'
#' @returns Either a data frame or a vector of row indices.
#'
#' @export

camr_SWA_linking_code_rows <- function(
    dtf_linked,
    int_row = 1,
    lgc_filter = TRUE ) {

  dtf_current <- dtf_linked

  # Subset to specified row
  if ( nrow(dtf_linked) > 1 ) {

    dtf_current <- dtf_linked[int_row, ]

    # Close 'Subset to specified row'
  }

  fun_gsub <- function(
    chr_string,
    chr_pattern,
    chr_with ) {

    return(
      gsub( chr_pattern, chr_with, chr_string, fixed = TRUE )
    )

  }

  int_rows <- as.numeric(
    strsplit(
      dtf_current$QCC.CHR.Linked.Rows |>
        fun_gsub( ',', ' ' ) |>
        fun_gsub( ';', ' ' ),
      split = ' ', fixed = TRUE
    )[[1]]
  )

  # Return data frame
  if (lgc_filter) {

    lgc_rows <-
      dtf_linked$IDX.INT.Row %in% c(
        int_rows,
        dtf_current$IDX.INT.Row
      )

    dtf_output <- dtf_linked[lgc_rows, ]

    # Pass on attributes
    if ( !is.null( dtf_linked$QCC.CHR.Linked.Parameters ) ) {

      lst_attr <- attributes(
        dtf_linked$QCC.CHR.Linked.Parameters
      )

      # If attribute exists
      if ( !is.null(lst_attr$camr_SWA_linking_code) ) {

        attributes(dtf_output$QCC.CHR.Linked.Parameters) <-
          lst_attr

        # Close 'If attribute exists'
      }

      # Close 'Pass on attributes'
    }

    return(
      dtf_output
    )

    # Close 'Return data frame'
  }

  return( int_rows )
}

#### 4.3) camr_SWA_linking_code_select ####
#' Select Columns for Linking Items and Useful Information
#'
#' Extracts columns with row indices, time points,
#' linked IDs, and potential linkable rows, along
#' with linking item variables (assuming standard
#' column names).
#'
#' @param dtf_linked A data frame, output from
#'   the [camrprojects::camr_SWA_linking_code]
#'   function.
#' @param chr_extra An optional character vector,
#'   additional columns to include.
#'
#' @returns A data frame,
#'
#' @export

camr_SWA_linking_code_select <- function(
    dtf_linked,
    chr_extra = '' ) {

  chr_linking_items <- camrprojects::camr_SWA_linking_code_items(
    dtf_linked
  )

  chr_useful <- c(
    'IDX.INT.Row',
    'SSS.INT.Time_point',
    'IDX.CHR.Linked.ID',
    'QCC.CHR.Linked.Rows'
  )

  chr_final <- c( chr_useful, chr_extra, chr_linking_items )

  chr_final <- chr_final[
    chr_final %in% colnames(dtf_linked)
  ]

  return( dtf_linked[, chr_final] )
}

#### 4.4) camr_SWA_linking_code_by_ID ####
#' Linkage Patterns by Linked Identifier
#'
#' Function to determine the pattern of
#' linked time points for each linked ID.
#'
#' @param dtf_linked A data frame, output from
#'   the [camrprojects::camr_SWA_linking_code]
#'   function. Must have the columns
#'   \code{IDX.CHR.Linked.ID} and
#'   \code{SSS.INT.Time_point}.
#'
#' @returns A wide-form data frame with one row
#' per linked ID along with the pattern of linked
#' time points (e.g., \code{'0-1'} means a link
#' from the baseline to first time point).
#'
#' @export

camr_SWA_linking_code_by_ID <- function(
    dtf_linked,
    lgc_update = FALSE ) {

  dtf_IDs <- aggregate(
    dtf_linked$SSS.INT.Time_point,
    list( dtf_linked$IDX.CHR.Linked.ID ),
    function(x) {
      paste( sort(x), collapse = '-' )
    }
  )
  colnames(dtf_IDs) <- c(
    'IDX.CHR.Linked.ID', 'SSS.CHR.Linked.Linkage_patterns'
  )

  # If indicator for duplicates found
  if ( 'QCC.LGC.Linked.Duplicates' %in% colnames(dtf_linked) ) {

    # Add indicator for duplicates
    dtf_IDs$QCC.LGC.Duplicates <- sapply(
      1:nrow( dtf_IDs ), function(r) {

        lgc_rows <-
          dtf_linked$IDX.CHR.Linked.ID %in% dtf_IDs$ID[r]

        return( all( dtf_linked$QCC.LGC.Linked.Duplicates[lgc_rows] ) )

      }
    )

    # Close 'If indicator for duplicates found'
  }

  # Linkage status per time point
  chr_TP <- sort( unique( dtf_linked$SSS.INT.Time_point) )

  mat_TP <- matrix(
    0, nrow(dtf_IDs), length( chr_TP )
  )
  colnames( mat_TP ) <- paste0(
    'SSS.INT.Linked.Records.TP.', chr_TP
  )

  # Loop over each time point
  for ( j in 1:ncol(mat_TP) ) {

    lgc_any <- grepl(
      chr_TP[j], dtf_IDs$SSS.CHR.Linked.Linkage_patterns,
      fixed = TRUE
    )

    mat_TP[lgc_any, j] <-
      dtf_IDs$SSS.CHR.Linked.Linkage_patterns[
        lgc_any
      ] |> sapply(
        function(x) {
          grepl( '-', strsplit( x, '' )[[1]], fixed = TRUE ) |> sum()
        }
      ) + 1

    # Close 'Loop over each time point'
  }

  dtf_IDs <- cbind( dtf_IDs, mat_TP )

  # Update original data set with linkage patterns
  if ( lgc_update ) {

    dtf_linked$SSS.CHR.Linked.Linkage_patterns <- sapply(
      1:nrow(dtf_linked), function(r) {

        chr_out <- ''

        lgc_rows <-
          dtf_IDs$IDX.CHR.Linked.ID %in%
            dtf_linked$IDX.CHR.Linked.ID[r]

        # Return pattern
        if ( any(lgc_rows) ) {

          chr_out <- dtf_IDs$SSS.CHR.Linked.Linkage_patterns[
            lgc_rows
          ]

          # Close 'Return pattern'
        }

        return( chr_out )
      }
    )

    # Close 'Update original data set with linkage patterns'
  }

  return( dtf_IDs )
}

#### 5) camr_SWA_linking_code_performance ####
#' Summary of Linking of Records
#'
#' Function to summarize the performance of
#' the linking code for the school-wide
#' assessment. Provides summary statistics
#' for what records were linked, and if
#' columns with the 'true' IDs are detected,
#' provides details on hits and correct
#' rejections.
#'
#' @param dtf_linked A data frame, the output
#'   from [camrprojects::camr_SWA_linking_code].
#' @param lst_groups A named list of column names,
#'   the grouping factors to consider when summarizing
#'   the number of records linked.
#' @param lgc_display A logical value; if \code{TRUE}
#'   prints the results to the console in addition
#'   to returning output as a list.
#'
#' @returns A list of data frames.
#'
#' @examples
#' # Linking across time points
#' dtf_long <- camr_SWA_linking_code_simulate('demo')
#' dtf_linked <- camr_SWA_linking_code(dtf_long)
#' lst_summary <- camr_SWA_linking_code_performance(
#'   dtf_linked, lgc_display = FALSE
#' )
#'
#' @export

camr_SWA_linking_code_performance <- function(
    dtf_linked,
    lst_groups = list(
      Time = 'SSS.INT.Time_point'
    ) ) {

  #### 5.1) Setup ####

  fun_count_percent <- function(
    lgc_x,
    int_num = NULL,
    int_denom = NULL ) {

    # Numerator/Denominator
    if ( !is.null(lgc_x) ) {

      int_num <- sum(lgc_x)
      int_denom <- length(x)

      # Close 'Numerator/Denominator'
    }

    chr_out <- paste0(
      int_num, '/', int_denom,
      ' (',
      format(
        round( 100*int_num/int_denom, 1 ),
        nsmall = 1
      ), '%)'
    )

    return(chr_out)
  }

  # Wide-form data with linkage patterns
  dtf_IDs <- camrprojects::camr_SWA_linking_code_by_ID(
    dtf_linked
  )

  # Initialize output
  lst_output <- list()

  #### 5.2) Linkage patterns [Overall] ####

  dtf_summary.linkage_patterns <- aggregate(
    rep( TRUE, nrow(dtf_IDs) ),
    list( Patterns = dtf_IDs$SSS.CHR.Linked.Linkage_patterns,
          Duplicates = dtf_IDs$QCC.LGC.Duplicates ),
    function(x) sum(x)
  )
  colnames(dtf_summary.linkage_patterns)[3] <- 'N'
  dtf_summary.linkage_patterns$CP <- sapply(
    1:nrow(dtf_summary.linkage_patterns), function(r) {

      fun_count_percent(
        NULL,
        dtf_summary.linkage_patterns$N[r],
        dtf_summary.linkage_patterns$N |> sum()
      )

    }
  )

  lst_output$linkage_patterns <- list(
    overall = dtf_summary.linkage_patterns
  )

  #### 5.3) Linkage patterns [Groups] ####

  #### 5.4) Any linked [Overall] ####

  dtf_IDs$Current <- dtf_IDs$SSS.INT.Linked.Records.TP.0
  dtf_IDs$Current[
    dtf_IDs$Current > 1
  ] <- '2+'

  dtf_summary.linked_with <- aggregate(
    rep( TRUE, nrow(dtf_IDs) ),
    list( Linked_with = dtf_IDs$Current ),
    function(x) sum(x)
  )

  colnames(dtf_summary.linked_with)[2] <- 'N'
  dtf_summary.linked_with$CP <- sapply(
    1:nrow(dtf_summary.linked_with), function(r) {

      fun_count_percent(
        NULL,
        dtf_summary.linked_with$N[r],
        dtf_summary.linked_with$N |> sum()
      )

    }
  )

  lst_output$linked_with <- list(
    overall = dtf_summary.linked_with
  )

  # If column with true IDs detected
  if ( 'IDX.INT.Linked.True' %in% colnames(dtf_linked) ) {

    dtf_summary.true_ID <- data.frame(
      Type = unique(
        dtf_linked$SSS.CHR.Linked.Test_type
      ),
      Records = NA,
      Linked = NA,
      Hits_true = NA,
      Hits = NA,
      Rejects_true = NA,
      Rejects = NA,
      Duplicates = NA
    )

    # Loop over types
    for ( r in 1:nrow(dtf_summary.true_ID) ) {

      lgc_rows <-
        dtf_linked$SSS.CHR.Linked.Test_type %in%
        dtf_summary.true_ID$Type[r]

      int_ID <- dtf_linked$IDX.INT.Linked.True[lgc_rows]
      lgc_zero <- int_ID == 0
      chr_ID <- dtf_linked$IDX.CHR.Linked.ID[lgc_rows]

      dtf_summary.true_ID$Linked[r] <-
        length( unique( chr_ID ) )

      dtf_summary.true_ID$Records[r] <- sum(lgc_rows)
      dtf_summary.true_ID$Hits_true[r] <-
        length( unique( int_ID[!lgc_zero] ) )
      dtf_summary.true_ID$Rejects_true[r] <-
        sum( lgc_zero )

      n_hits <- 0

      # Hits for records that should be linked
      if ( any(!lgc_zero) ) {

        dtf_H <- aggregate(
          chr_ID[!lgc_zero],
          list( int_ID[!lgc_zero] ),
          function(x) {
            length( unique(x) )
          }
        )
        n_hits <- sum( dtf_H[[2]] == 1 )

        # Close 'Hits for records that should be linked'
      }

      n_rejects <- 0

      # False alarms for records that should not be linked
      if ( any(lgc_zero) ) {

        dtf_FA <- aggregate(
          int_ID,
          list( chr_ID ),
          function(x) {
            sum( x == 0 )
          }
        )
        n_rejects <- sum( dtf_FA[[2]] == 1 )

        # Close 'False alarms for records that should not be linked'
      }

      dtf_summary.true_ID$Duplicates[r] <- 0

      lgc_dup <-
        dtf_linked$QCC.LGC.Linked.Duplicates[lgc_rows]

      # Check if duplicate records
      if ( any( lgc_dup ) ) {

        dtf_summary.true_ID$Duplicates[r] <-
          length( unique( chr_ID[lgc_dup] ) )

        # Close 'Check if duplicate records'
      }

      dtf_summary.true_ID$Hits[r] <- n_hits
      dtf_summary.true_ID$Rejects[r] <- n_rejects

      # Close 'Loop over types'
    }

    lst_output$true_ID <- dtf_summary.true_ID

    # Close 'If column with true IDs detected'
  }

  return( lst_output )
}
