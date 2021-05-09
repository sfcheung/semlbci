#' @title Get all the unique names from a parameter table
#'
#' @description Get all the unique names form a [lavaan] parameter table
#'
#' @details
#' These columns will be extracted: `lhs`, `rhs`, `label`, and `plabel`.
#'
#' @return
#' A vector of unique characters
#'
#' @param ptable A lavaan parameter table (e.g., an object returned by
#'                [lavaan::parameterTable()])
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
#' @keywords internal

get_names_from_ptable <- function(ptable) {
    if (!is.data.frame(ptable)) {
        stop("ptable is not a data frame")
      }
    if (!all(c("lhs", "rhs", "label", "plabel") %in% names(ptable))) {
        stop("At least one of these columns are absent. \n
              lhs, rhs, label, plabel.\n
              Check whethere ptable is a parameter table.")
      }
    out <- unique(c(ptable$lhs,
                    ptable$rhs,
                    ptable$label,
                    ptable$plabel))
    out[!(out == "")]
  }