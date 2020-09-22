#' build_demo_table
#'
#' A function that will build a data frame that is well-
#' formatted to easily create a flextable object.
#'
#' @param df A data frame that contains variables named
#'   according to the specification created by Kevin
#'   Potter (i.e. XXX.FFF.Var_name), where XXX is a 3-letter
#'   subset code and FFF is a 3-letter format identifier.
#' @param grp A variable name that contains the grouping
#'   variable for the demographics table.
#' @param funcs A named list of purrr-style lambda functions
#'   where the names follow one of 3 conventions (below). Note:
#'   for all CHR variables, the function operates on the unique
#'   values within that variable, NOT on the variable itself.
#'   Further, the value \code{n} is available for use within these
#'   functions. All functions must return a CHR type.
#'   \itemize{
#'   \item{FFF}{which applies the function to all variables (with
#'     the exception of IDS.CHR...) with that format identifier.}
#'   \item{XXX.FFF}{which applies the function to all variables (with
#'     the exception of IDS.CHR...) with that subset code and
#'     format identifier.}
#'   \item{XXX.FFF.Var_name}{which applies the function to that
#'     variable}
#'     }
#'
#' @author William Schmitt
#'
#' @return
#'
#' @examples
#' # Example data frame
#' df <- data.frame(
#'   SSS.CHR.Group = c('G1', 'G1', 'G2', 'G2'),
#'   SBJ.INT.Age = c(45, 47, 48, 52),
#'   SBJ.CHR.Race = c('White', 'White', 'Asian', 'Black')
#' )
#'
#' # Runs function on CHR and INT variables
#' dt <- build_demo_table(
#'   df,
#'   SSS.CHR.Group,
#'   list(
#'     'INT' = ~paste0(round(mean(.), 2)),
#'     'CHR' = ~paste0(./n*100, '%')
#'   )
#' )
#'
#' # Runs function on SBJ.INT and SBJ.CHR variables
#' dt <- build_demo_table(
#'   df,
#'   SSS.CHR.Group,
#'   list(
#'     'SBJ.INT' = ~paste0(round(mean(.), 2)),
#'     'SBJ.CHR' = ~paste0(./n*100, '%')
#'   )
#' )
#'
#' # Runs function on individual columns
#' dt <- build_demo_table(
#'   df,
#'   SSS.CHR.Group,
#'   list(
#'     'SBJ.INT.Age' = ~paste0(round(mean(.), 2)),
#'     'SBJ.CHR.Race' = ~paste0(./n*100, '%')
#'   )
#' )
#' @export
#' @importFrom magrittr %>%
#' @import dplyr
#' @import stringr
#' @import tidyr


build_demo_table <- function(df, grp, funcs) {

  eqGrp <- rlang::enquo(grp)

  eqFuncs <- rlang::enquo(funcs)

  # Calculate n for grp
  n_tb <- df %>%
    group_by(
      !!eqGrp
    ) %>%
    summarise(
      n = n()
    )
  tb <- n_tb

  for (i in 1:length(funcs)) {

    col <- names(funcs)[i]

    if (str_detect(col, '^\\w{3}\\.\\w{3}\\..+')) {
      cur_df <- df %>%
        select(
          !!eqGrp,
          col
        )
    } else if (str_detect(col, '^\\w{3}\\.\\w{3}')) {
      cur_df <- df %>%
        select(
          !!eqGrp,
          contains(col)
        )
    } else if (str_detect(col, '^\\w{3}$')) {
      cur_df <- df %>%
        select(
          !!eqGrp,
          contains(str_c('.', col, '.'))
        )
    } else {
      warning(str_c(col, ' specification invalid. Skipping.'))
      next
    }

    if (str_detect(col, 'INT')) {
      int_df <- cur_df %>%
        group_by(
          !!eqGrp
        ) %>%
        summarise(
          across(
            contains('.INT.'),
            (!!funcs)[[i]]
          )
        )

      tb <- left_join(tb, int_df)
    } else if (str_detect(col, 'CHR')) {

      chr_df <- cur_df %>%
        select(
          matches('(?<!^IDS)\\.CHR\\.', perl=T),
          !!eqGrp
        ) %>%
        pivot_longer(
          -!!eqGrp,
          names_to = 'Category',
          values_to = 'Value'
        ) %>%
        count(
          !!eqGrp,
          Category,
          Value
        ) %>%
        pivot_wider(
          names_from = c('Category', 'Value'),
          values_from = n,
          names_sep = '...',
          values_fill = 0
        )

      chr_df <- chr_df %>%
        left_join(
          n_tb
        ) %>%
        mutate(
          across(
            contains('.CHR.') & -!!eqGrp,
            (!!eqFuncs)[[i]]
          )
        ) %>%
        select(
          -n
        )

      tb <- left_join(tb, chr_df)
    }
  }

  tb <- tb %>%
    select(
      -n
    ) %>%
    pivot_longer(
      -!!eqGrp,
      names_to = 'Category',
      values_to = 'Value'
    ) %>%
    pivot_wider(
      names_from = !!eqGrp,
      values_from = Value
    ) %>%
    mutate(
      Category = str_replace(Category, '\\w{3}\\.(INT|CHR)\\.', ''),
      Category = str_replace(Category, '\\.{3}$', '...Not Reported')
    ) %>%
    separate(
      Category,
      c('Header', 'Category'),
      sep = '\\.{3}'
    ) %>%
    mutate(
      Category = if_else(
        is.na(Category),
        Header,
        Category
      )
    )

  head_vals <- unique(tb$Header[duplicated(tb$Header)])
  blank_rw <- data.frame(matrix(ncol=length(names(tb)), nrow = length(head_vals)))
  names(blank_rw) <- names(tb)
  blank_rw$Header <- head_vals
  blank_rw$Category <- head_vals

  if (any(str_detect(names(tb), 'Total'))) {
    total_col <- names(tb)[str_detect(names(tb), 'Total')]
    other_idx <- names(tb)
    other_idx <- other_idx[!(other_idx %in% total_col)]
    tb <- tb[c(other_idx, total_col)]
  } else {
    total_col <- names(tb)[3]
  }

  tb <- bind_rows(tb, blank_rw) %>%
    arrange(
      Header,
      !is.na(!!as.name(total_col))
    ) %>%
    mutate(
      Category = if_else(
        duplicated(Header) & !is.na(Category),
        str_c('    ', Category),
        Category
      )
    ) %>%
    select(
      -Header
    )

  return(tb)
}
