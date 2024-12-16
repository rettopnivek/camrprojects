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
# Last updated: 2024-12-16

# Table of contents
# 1) camr_SWA_linking_code_simulate
# 2) ...
# 3) camr_SWA_linking_code

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
#'   \code{'accuracy'} generates data for accuracy
#'   assessment, \code{'duplicate'} generates data
#'   for a single time point with duplicate records,
#'   and \code{'demo'} generates a small data set
#'   for demonstration purposes.
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
    chr_type = 'debug' ) {

  #### 1.1) Setup ####

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
        rep( NA, 3 )

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
        rep( 1, 3 )
      ),
      SSS.INT.SurveyYear = c(
        # Standard linking
        rep( 2023, 18 ),
        rep( 2024, 9 ),
        # No links
        rep( 2023, 18 ),
        rep( 2024, 9 ),
        # Specific tests
        # + Dissimilarity = 1 [Base]
        c(2023, 2023, 2023) |> rep(7),
        # + Dissimilarity = 1 [Add]
        c(2023, 2023, 2023) |> rep(7),
        # + Duplicate records [Base]
        c(2023, 2023, 2023),
        # + Duplicate records [Add]
        c(2023, 2023, 2023)
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
        c('Fall', 'Spring', 'Spring')
      ),
      SSS.CHR.Time_point = c(
        # Standard linking
        rep( '2023 Fall', 9 ),
        rep( '2023 Spring', 9 ),
        rep( '2024 Fall', 9 ),
        # No links
        rep( 'Fall', 9 ),
        rep( 'Spring', 9 ),
        rep( 'Fall', 9 ),
        # Specific tests
        # + Dissimilarity = 1 [Base]
        c('2023 Fall', '2023 Fall', '2023 Spring') |> rep(7),
        # + Dissimilarity = 1 [Add]
        c('2023 Fall', '2023 Spring', '2023 Spring') |> rep(7),
        # + Duplicate records [Base]
        c('2023 Fall', '2023 Fall', '2023 Spring'),
        # + Duplicate records [Add]
        c('2023 Fall', '2023 Spring', '2023 Spring')
      ),
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
        c(0, 1, 1)
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
        c(9, 10, 10)
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
        rep( TRUE, 3 )
      ),
      IDX.INT.Linked.True = 0,
      SSS.CHR.Linked.Test_type = ''
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
      'Duplicate records [Base]'

    # Update indices
    int_old <- int_old + max(int_new)
    int_ID_old <- int_ID_old + max(int_ID)

    # + Ignore school code

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
    obj_additional_input = NULL,
    lgc_code = FALSE ) {

  # No specification given
  if ( is.null(obj_additional_input) ) {

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

    # Close 'No specification given'
  } else {

    mat_base_add <- obj_additional_input$mat_base_add

    # Close else for 'No specification given'
  }

  chr_code <- paste0(
    'list(\n',
    sapply(
      1:nrow(mat_base_add), function(r) {
        paste0( '  TP', mat_base_add[r, 1],
                'tTP', mat_base_add[r, 2],
                ' = list(\n',
                '    Base = dtf_long$SSS.INT.Time_point %in% ',
                mat_base_add[r, 1], ',\n',
                '    Add = dtf_long$SSS.INT.Time_point %in% ',
                mat_base_add[r, 2], '\n  )' )
      }
    ) |> paste( collapse = ',\n' ),
    '\n)'
  )

  lst_link_across <- eval( parse(text = chr_code ) )

  # Return input details
  if ( chr_input == 'lst_link_across' ) {

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

    # Close 'Return input details'
  }

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
#'   Pass a list with separate vectors allows using different
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
#' @param lst_link_threshold A list of lists, where each sublist consists
#'   of integer vectors for each combination of linking items defined in
#'   \code{lst_link_combo}. Integer vectors consist of 4 values, the
#'   minimum dissimilarity score and unique count of that score for the
#'   base set and linking set, respectively.
#' @param lgc_progress A logical value; if \code{TRUE} displays
#'   progress of the function call.
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
    lst_link_threshold = NULL,
    lgc_progress = TRUE ) {

  if (lgc_progress)
    message( 'Start: camr_SWA_linking_code' )

  #### 3.1) Setup ####

  # Debugging
  if ( FALSE ) {

    dtf_debug <- camr_SWA_linking_code_simulate( 'debug' )

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

  # Make sure row index exists
  if ( is.null( dtf_long$IDX.INT.Row) ) {

    dtf_long$IDX.INT.Row <- 1:nrow(dtf_long)

    # Close 'Make sure row index exists'
  }

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

    # Close 'Default sets to link across'
  }

  # Default items to use to link
  if ( is.null(obj_link_using) ) {

    obj_link_using <- c(
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

    # Close 'Default linking questions'
  }

  obj_link_using <- obj_link_using[
    obj_link_using %in% colnames(dtf_long)
  ]

  # No linking items
  if ( length(obj_link_using) == 0 ) {

    stop(
      paste0(
        "No linking items found - check column names or ",
        "specify manually using 'obj_link_using' argument"
      )
    )

    # Close 'No linking items'
  }

  # If a character vector convert to list
  if ( is.character(obj_link_using) ) {

    obj_link_using <- rep(
      list(obj_link_using), length(lst_link_across)
    )

    # Close 'If a character vector'
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
      seq_along(lst_link_across), function(l) {

        return(
          list(
            SLQ_______ = c( 1:2 ),
            S_Q1234567 =c( 1, 3:9 ),
            S_Q_234567 =c( 1, 4:9 ),
            S_Q1_34567 = c( 1, 3, 5:9 ),
            S_Q12_4567 = c( 1, 3:4, 6:9 ),
            S_Q123_567 = c( 1, 3:5, 7:9 ),
            S_Q1234_67 = c( 1, 3:6, 8:9 ),
            S_Q12345_7 = c( 1, 3:7, 9 ),
            S_Q123456_ = c( 1, 3:8 )
          )
        )

      }
    )
    names(lst_link_combo) <- names(lst_link_across)

    # Close 'Default combinations of linking items'
  }

  # Default thresholds for matches
  if ( is.null(lst_link_threshold) ) {

    lst_link_threshold <- lst_link_combo

    # Loop over sets
    for ( s in seq_along(lst_link_combo) ) {

      # Loop over combos
      for ( l in seq_along(lst_link_combo[[s]] ) ) {

        lst_link_threshold[[s]][[l]] <- c(
          0, 1, 0, 1
        )

        # Close 'Loop over combos'
      }

      # Close 'Loop over sets'
    }

    # Close 'Default thresholds for matches'
  }

  #### 3.1.1) fun_match_matrices ####
  fun_match_matrices <- function(
    dtf_data,
    chr_column,
    lgc_base,
    lgc_add ) {

    mat_base <- matrix(
      dtf_data[[ chr_column ]][lgc_base],
      sum( lgc_add ), sum( lgc_base ), byrow = TRUE
    )
    mat_add <- matrix(
      dtf_data[[ chr_column ]][lgc_add],
      sum( lgc_add ), sum( lgc_base ), byrow = FALSE
    )
    mat_comparison <-
      mat_base == mat_add
    # mat_comparison[ is.na(mat_comparison) ] <- FALSE

    return( mat_comparison )
  }

  #### 3.1.2) Initialize variables for linking ####

  dtf_long$IDX.CHR.Linked.ID <- paste0(
    'NL TP',
    dtf_long$SSS.INT.Time_point,
    ' ',
    1:nrow(dtf_long)
  )
  dtf_long$QCC.LGC.Linked.Attempted <- FALSE
  dtf_long$QCC.LGC.Linked <- FALSE
  dtf_long$QCC.LGC.Linked.No_issues <- FALSE
  dtf_long$QCC.CHR.Linked.Score.Base <- ''
  dtf_long$QCC.CHR.Linked.Score.Add <- ''
  dtf_long$QCC.CHR.Linked.Rows <- ''
  dtf_long$QCC.CHR.Linked.Dissimilarity <- ''

  #### 3.1.3) fun_split_by_semicolon ####

  fun_split_by_semicolon <- function(
    chr_content,
    chr_addition = '',
    lgc_unique = TRUE ) {

    # Check that any content exists
    if ( chr_content != '' ) {

      # Split into separate components
      chr_components <- strsplit(
        chr_content, split = ';', fixed = TRUE
      )[[1]]

      # If addition then add and combine
      if (chr_addition != '') {

        # Unique components only
        if ( lgc_unique ) {

          chr_components <-
            c( chr_components, chr_addition ) |> unique()

          # Close 'Unique components only'
        } else {

          chr_components <-
            c( chr_components, chr_addition )

          # Close else for 'Unique components only'
        }

        chr_content <- paste(
          chr_components, collapse = ';'
        )

        return( chr_content )

        # Close 'If addition then add and combine'
      }

      return( chr_components )

      # Close 'Check that any content exists'
    } else {

      # If addition then add and combine
      if (chr_addition != '') {

        chr_content <- chr_addition

        return( chr_content )

        # Close 'If addition then add and combine'
      }

      return( '' )

      # Close else for 'Check that any content exists'
    }

  }

  #### 3.2) Link over sets ####

  if (lgc_progress)
    message( '  Link over sets' )

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

    # Max number of link items
    int_max_link <- length(
      unlist( obj_link_using[[s]] ) |> unique()
    )

    # Array for matches over link items
    arr_match <- array(
      NA,
      dim = c(
        sum(lgc_add), sum(lgc_base), length( obj_link_using[[s]] )
      )
    )

    # Loop over link items
    for ( l in seq_along(obj_link_using[[s]] ) ) {

      if (lgc_progress)
        message( paste0( '      Matches for ', obj_link_using[[s]][l] ) )

      arr_match[, , l] <- fun_match_matrices(
        dtf_long,
        obj_link_using[[s]][l],
        lgc_base,
        lgc_add
      )

      # Close 'Loop over link items'
    }

    if (lgc_progress) message( '      Computing dissimilarity patterns' )


    arr_match[
      is.na(arr_match)
    ] <- ' '
    arr_match[
      arr_match == 'TRUE'
    ] <- '0'
    arr_match[
      arr_match == 'FALSE'
    ] <- '1'

    chr_code <- paste0(
      'paste0(',
      paste(
        paste0( 'arr_match[, , ', 1:dim(arr_match)[3], ']' ),
        collapse = ', '
      ),
      ')'
    )

    mat_diss_pattern <- eval(
      parse(text = chr_code)
    )
    dim(mat_diss_pattern) <- dim(arr_match)[1:2]

    chr_default_pattern <- rep( ' ', length(obj_link_using[[s]]) )


    # mat_diss_pattern <- matrix(
    #   '',
    #   dim( arr_match )[1],
    #   dim( arr_match )[2]
    # )
    #
    # fun_pattern <- function(lgc_match) {
    #
    #   chr_out <- chr_default_pattern
    #   chr_out[ !is.na(lgc_match) ] <-
    #     1 - as.numeric( lgc_match[!is.na(lgc_match)] )
    #
    #   chr_out <- paste( chr_out, collapse = '' )
    #
    #   return(chr_out)
    # }
    #
    # # Loop over rows
    # for ( j in 1:nrow( mat_diss_pattern ) ) {
    #
    #   # Loop over columns
    #   for ( k in 1:ncol( mat_diss_pattern ) ) {
    #
    #     mat_diss_pattern[j, k] <- fun_pattern(
    #       arr_match[j, k, ]
    #     )
    #
    #     # Close 'Loop over columns'+
    #     }
    #
    #   # Update the progress bar
    #   if (lgc_progress)
    #     setTxtProgressBar(obj_pb, j)
    #
    #   # Close 'Loop over rows'
    # }

    # Progress bar
    if (lgc_progress) {

      obj_pb <- txtProgressBar(
        min = 1, max = ncol(mat_diss_pattern), style = 3
      )

      # Close 'Progress bar'
    }

    # Loop over base records
    for ( b in 1:ncol(mat_diss_pattern) ) {

      lgc_matched <- FALSE
      chr_min_diss_base <- paste0(
        s, '.', length(lst_link_combo[[s]]),
        ':x/x:x'
      )
      chr_min_diss_add <- chr_min_diss_base
      chr_min_diss <- paste0(
        s, '.', length(lst_link_combo[[s]]),
        ':',
        paste( chr_default_pattern, collapse = '' )
      )
      int_add_row <- 1

      # Columns [base] to consider
      int_b <- 1:ncol(mat_diss_pattern)
      # Rows [add] to consider
      int_a <- 1:nrow(mat_diss_pattern)

      # Remove diagonal for duplicates
      if (lgc_duplicates) {

        int_b <- int_b[-b]
        int_a <- int_a[-b]

        # Close 'Remove diagonal for duplicates'
      }

      # Loop over combinations
      for ( l in seq_along(lst_link_combo[[s]] ) ) {

        # Not yet matched
        if ( !lgc_matched ) {

          # Compute dissimilarity score for current combo
          int_diss_score_base <- sapply(
            mat_diss_pattern[int_a, b], function(chr_pattern) {

              strsplit( chr_pattern, split = '', fixed = TRUE )[[1]][
                lst_link_combo[[s]][[l]]
              ] |> as.numeric() |> sum()

            }
          )

          # If any non-missing dissimilarity scores
          if ( any( !is.na(int_diss_score_base) ) ) {

            int_min_diss_base <- min( int_diss_score_base, na.rm = T )

            int_min_diss_count_base <- sum(
              int_diss_score_base %in% int_min_diss_base
            )

            chr_min_diss_base <- paste0(
              s, '.', l, ':',
              int_min_diss_base,
              '/', length( lst_link_combo[[s]][[l]] ),
              ':', int_min_diss_count_base
            )

            lgc_threshold_min_base <-
              int_min_diss_base <= lst_link_threshold[[s]][[l]][1]
            lgc_threshold_count_base <-
              int_min_diss_count_base <= lst_link_threshold[[s]][[l]][2]

            # Identify row in add [Take first if multiple]
            int_add_row <- which(
              int_diss_score_base %in% int_min_diss_base
            )[1]
            int_add_row <- int_a[int_add_row]

            int_diss_score_add <- sapply(
              mat_diss_pattern[int_add_row, int_b], function(chr_pattern) {

                strsplit( chr_pattern, split = '', fixed = TRUE )[[1]][
                  lst_link_combo[[s]][[l]]
                ] |> as.numeric() |> sum()

              }
            )

            int_min_diss_add <- min( int_diss_score_add, na.rm = T )

            int_min_diss_count_add <- sum(
              int_diss_score_add %in% int_min_diss_add
            )

            chr_min_diss_add <- paste0(
              s, '.', l, ':',
              int_min_diss_add,
              '/', length( lst_link_combo[[s]][[l]] ),
              ':', int_min_diss_count_add
            )
            chr_min_diss <- paste0(
              s, '.', length(lst_link_combo[[s]]),
              ':',
              mat_diss_pattern[int_add_row, b]
            )

            # Meets threshold for min score/count/excluded [Base]
            if ( lgc_threshold_min_base &
                 lgc_threshold_count_base ) {

              lgc_threshold_min_add <-
                int_min_diss_add <= lst_link_threshold[[s]][[l]][3]
              lgc_threshold_count_add <-
                int_min_diss_count_add <= lst_link_threshold[[s]][[l]][4]

              # Meets threshold for min score and count [Add]
              if ( lgc_threshold_min_add &
                   lgc_threshold_count_add ) {

                # Indicate successful match
                lgc_matched <- TRUE

                dtf_long$QCC.CHR.Linked.Rows[lgc_base][b] <-
                  fun_split_by_semicolon(
                    dtf_long$QCC.CHR.Linked.Rows[lgc_base][b],
                    paste0(
                      dtf_long$IDX.INT.Row[lgc_base][b],
                      ',',
                      dtf_long$IDX.INT.Row[lgc_add][int_add_row]
                    ),
                    lgc_unique = TRUE
                  )

                dtf_long$QCC.CHR.Linked.Rows[lgc_add][int_add_row] <-
                  fun_split_by_semicolon(
                    dtf_long$QCC.CHR.Linked.Rows[lgc_add][int_add_row],
                    paste0(
                      dtf_long$IDX.INT.Row[lgc_base][b],
                      ',',
                      dtf_long$IDX.INT.Row[lgc_add][int_add_row]
                    ),
                    lgc_unique = TRUE
                  )

                dtf_long$QCC.LGC.Linked[lgc_base][b] <- TRUE
                dtf_long$QCC.LGC.Linked[lgc_add][int_add_row] <- TRUE

                # Close 'Meets threshold for min score and count [Add]'
              }

              # Close 'Meets threshold for min score and count [Base]'
            }

            # Close 'If any non-missing dissimilarity scores'
          }

          # Close 'Not yet matched'
        }

        # Close 'Loop over combinations'
      }

      dtf_long$QCC.CHR.Linked.Score.Base[lgc_base][b] <-
        fun_split_by_semicolon(
          dtf_long$QCC.CHR.Linked.Score.Base[lgc_base][b],
          chr_min_diss_base,
          lgc_unique = FALSE
        )
      dtf_long$QCC.CHR.Linked.Score.Add[lgc_add][int_add_row] <-
        fun_split_by_semicolon(
          dtf_long$QCC.CHR.Linked.Score.Add[lgc_add][int_add_row],
          chr_min_diss_add,
          lgc_unique = FALSE
        )
      dtf_long$QCC.CHR.Linked.Dissimilarity[lgc_base][b] <-
        fun_split_by_semicolon(
          dtf_long$QCC.CHR.Linked.Dissimilarity[lgc_base][b],
          chr_min_diss,
          lgc_unique = FALSE
        )

      # Update the progress bar
      if (lgc_progress)
        setTxtProgressBar(obj_pb, b)

      # Close 'Loop over base records'
    }
    if (lgc_progress) { close(obj_pb); rm(obj_pb) }

    # Close 'Loop over sets'
  }

  #### 3.3) Create linked IDs ####

  lgc_linked <- dtf_long$QCC.LGC.Linked

  # Extract unique linked pairs
  chr_linked_rows <-
    dtf_long$QCC.CHR.Linked.Rows[ lgc_linked ]
  chr_linked_rows <- lapply(
    chr_linked_rows, function(s) {
      strsplit( s, split = ';', fixed = TRUE )[[1]]
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

  return( dtf_long )
}
