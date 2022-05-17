#' @title Get the row number of a parameter from lhs, op, rhs, block, and group
#'
#' @description Get the row number of a parameter in a parameter table
#'              from lhs, op, rhs, block, and group
#'
#' @details
#' These columns will be extracted: `lhs`, `rhs`, `label`, and `plabel`.
#'
#' @return
#' A vector of unique characters
#'
#' @param ptable A lavaan parameter table (e.g., an object returned by
#'                [lavaan::parameterTable()])
#' @param lor A one row data frame with these columns, lhs, op, rhs, block, and
#'            group. lhs, op, and rhs are characters, and block and group are
#'            numeric. block and group are optional. If they are omitted, an
#'            error will be raised if there are more than one match.
#'            If `NULL`, the data frame will be constructed from the following
#'            arguments. Default is `NULL`.
#' @param lhs A string. The `lhs` in the parameter table.
#' @param op  A string. The `op` in the parameter table.
#' @param rhs A string. The `rhs` in the parameter table.
#' @param block  A number. The `block` in the parameter table. Optional
#' @param group  A number. The `group` in the parameter table. Optional
#'
#' @examples
#'
#' # TODO
#'
#' @keywords internal

get_i_from_lor <- function(ptable,
                           lor = NULL,
                           lhs,
                           op,
                           rhs,
                           block = NULL,
                           group = NULL
                           ) {
    if (!is.data.frame(ptable)) {
        stop("ptable is not a data frame")
      }
    if (!all(c("lhs", "rhs", "op") %in% names(ptable))) {
        stop(paste0(
              "At least one of these columns are absent: ",
              "lhs, rhs, op. ",
              "Check whethere ptable is a parameter table."))
      }
    if (sum(c(is.null(block), is.null(group))) == 1) {
        stop(paste0(
              "Only either block or group is supplied. ",
              "If supplied, they need to be supplied together."))
      }
    if (!is.null(block) && !is.null(group)) {
        block_group <- TRUE
      } else {
        block_group <- FALSE
      }
    if (is.null(lor) && any(missing(lhs), 
                            missing(op),
                            missing(rhs))) {
        stop("If lor is not supplied, then lhs, op, and rhs must be supplied.")
      }
    if (!is.null(lor)) {
        if (!is.data.frame(lor)) {
            stop("lor is not a data frame.")
          }
        if (nrow(lor) > 1) {
            stop("lor has more than one row.")
          }
        if (!all(c("lhs", "rhs", "op") %in% names(lor))) {
            stop(paste0(
                  "At least one of these columns are absent from lor",
                  "lhs, rhs, op."))
          }
      }

    if (!is.null(lor)) {
        lhs <- lor$lhs
        op  <- lor$op
        rhs <- lor$rhs
        if (!is.null(lor$block)) {
            block <- lor$block
          } else {
            block <- NA
          }
        if (!is.null(lor$group)) {
            group <- lor$group
          } else {
            group <- NA
          }
        if (is.numeric(block) & is.numeric(group)) {
            block_group <- TRUE
          } else {
            block_group <- FALSE
          }
      }

    if (block_group) {
        i_tmp <- which((ptable$lhs == lhs) &
                       (ptable$op  == op) &
                       (ptable$rhs == rhs) &
                       (ptable$block == block) &
                       (ptable$group == group))
      } else {
        i_tmp <- which((ptable$lhs == lhs) &
                       (ptable$op  == op) &
                       (ptable$rhs == rhs))
      }
    if (length(i_tmp) > 1) {
        stop("More than one match of the parameter was found.")
      }
    if (length(i_tmp) == 0) {
        stop("No match was found in the parameter table.")
      }
    i_tmp
  }