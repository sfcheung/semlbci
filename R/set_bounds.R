#' @title Bounds for Parameters
#'
#' @description Use the new `bounds` option in [lavaan()]
#'
#' @details It uses [update()] to get the bounds.
#'
#' @return A lavaan parameter table in which the lower and upper bounds
#'         are set.
#'
#' @param sem_out The SEM output. Currently \code{lavaan} output only.
#'
#' @param bounds This argument will be passed to [lavaan::lavaan()].
#'
#' @noRd

set_bounds <- function(sem_out = NULL,
                      bounds = NULL) {
    # Support the bounds argument in lavaan.
    # Not ready. Not used for now.
    sem_out_b <- lavaan::update(sem_out,
                              bounds = bounds,
                              do.fit = FALSE)
    lavaan::parameterTable(sem_out_b)
  }