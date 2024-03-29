#' @title Unique Names in a Parameter Table
#'
#' @description Gets all the unique names form a [lavaan] parameter table.
#'
#' @details It currently supports `lavaan` output only.
#'
#' @return A vector of unique characters
#'
#' @param ptable A lavaan parameter table (e.g., an object returned by
#'  [lavaan::parameterTable()])
#'
#' @examples
#'
#' \dontrun{
#' library(lavaan)
#' mod <-
#' "
#' y ~ x + m
#' z ~ a * y + m
#' "
#' ptable <- lavaanify(mod)
#' get_names_from_ptable(ptable)
#' }
#' @noRd

get_names_from_ptable <- function(ptable) {
    if (!is.data.frame(ptable)) {
        stop("ptable is not a data frame")
      }
    if (!all(c("lhs", "rhs", "label", "plabel") %in% names(ptable))) {
        stop("At least one of these columns are absent. \n
              lhs, rhs, label, plabel.\n
              Check whether ptable is a parameter table.")
      }
    out <- unique(c(ptable$lhs,
                    ptable$rhs,
                    ptable$label,
                    ptable$plabel))
    out[!(out == "")]
  }