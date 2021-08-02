#' Shorthand to create a for loop wrapped in a progress bar.
#'
#' @param l A vector or list to loop over.
#'
#' @param f A function to apply to each item.
#'
#' @return NULL
#'
#' @author Michael Pascale
#'
#' @export
#' @md

camr_for <- function (l, f) {
  p <- txtProgressBar(min = 1, max = length(l), style=3)

  for (i in 1:length(l)) {
    f(l[i])
    setTxtProgressBar(p, i)
  }
}
