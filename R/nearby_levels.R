#' @title LBCI Bounds of Nearby Levels of Confidence
#'
#' @description Find LBCIs with levels of confidence
#' different from those stored in a `semlbci`- class
#' object.
#'
#' @details It receives a `semlbci`-class object, gets
#' the original level of confidence, generates one or
#' more levels of confidence different from this level
#' by certain amounts, and repeats the original call
#' to [semlbci()] with these levels of confidence.
#' The results are returned as a list of class
#' `semlbci_list`, with the original`semlbci`-class
#' included.
#'
#' @return A `semlbci_list`-class object, which is
#'  simply a named list of `semlbci`-class object,
#'  names being the levels of confidence.
#'
#' @param x The output of [semlbci()].
#'
#' @param ciperc_levels A numeric vector of deviations
#'  from the original level of confidence. The default
#'  is `c(-.025, .025)`. Therefore, if the original level
#'  is .95, the levels to be used is `c(-.025, .025) + .95`
#'  or `c(.925, .975)`.
#'
#' @param ciperc_range A numeric vector of two numbers,
#'  which are the minimum and maximum levels of confidence
#'  to be used, respectively. Default is `c(.60, .99)`.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @seealso [semlbci()]
#'
#' @examples
#'
#' library(lavaan)
#' mod <-
#' "
#' m ~ a*x
#' y ~ b*m
#' ab := a * b
#' "
#' fit_med <- sem(mod, simple_med, fixed.x = FALSE)
#' lbci_fit <- semlbci(fit_med)
#' lbci_fit_nb <- nearby_levels(lbci_fit,
#'                  ciperc_levels = c(-.050, .050))
#' names(lbci_fit_nb)
#'
#' @export

nearby_levels <- function(x,
                          ciperc_levels = c(-.025, .025),
                          ciperc_range = c(.60, .99)) {
    call_org <- attr(x, "call")
    ciperc_org <- get_ciperc_all(x)
    if (is.na(ciperc_org)) {
        ciperc_org <- formals(semlbci)$ciperc
      }
    cipercs <- ciperc_org + sort(unique(c(ciperc_levels, 0)))
    cipercs[cipercs > max(ciperc_range)] <- max(ciperc_range)
    cipercs[cipercs < min(ciperc_range)] <- min(ciperc_range)
    cipercs <- unique(cipercs)
    k <- length(cipercs)
    calls <- lapply(cipercs,
                    function(x) {call_org$ciperc <- x
                                 call_org})
    names(calls) <- cipercs
    i0 <- which(cipercs == ciperc_org)
    # The following lines are inefficient
    # However, FUN = eval is preferred, to avoid
    # any potential scoping issues.
    env <- parent.frame()
    out <- sapply(calls[-i0],
                  FUN = eval,
                  envir = env,
                  simplify = FALSE,
                  USE.NAMES = TRUE)
    attr(x, which = "call") <- calls[[i0]]
    out <- c(out, list(x))
    names(out) <- c(names(calls)[-i0], names(calls)[i0])
    out <- out[order(as.numeric(names(out)))]
    class(out) <- "semlbci_list"
    out
  }

#' @title Check The Order of Bounds in a List of `semlbci`
#'  Objects
#'
#' @description Check whether the LBCIs in a list of
#'  `semlbci`-class of objects are consistent with their
#'  levels of confidence.
#'
#' @param semlbci_list An object of class `semlbci_list`,
#'  such as the output of [nearby_levels()].
#'
#' @return
#' A `ci_order`-class object with a `print` method
#' [print.ci_order()]. The number of rows is equal to the
#' number of
#' parameters in `semlbci_list`, and the columns stores the
#' confidence limits from the list, ordered according to the
#' level of confidence.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @seealso [nearby_levels()], [semlbci()]
#'
#' @examples
#'
#' library(lavaan)
#' mod <-
#' "
#' m ~ a*x
#' y ~ b*m
#' ab := a * b
#' "
#' fit_med <- sem(mod, simple_med, fixed.x = FALSE)
#' lbci_fit <- semlbci(fit_med)
#' lbci_fit_nb <- nearby_levels(lbci_fit,
#'                  ciperc_levels = c(-.050, .050))
#' ci_order(lbci_fit_nb)
#'
#' @export

ci_order <- function(semlbci_list) {
    cipercs <- sapply(semlbci_list, function(xx) {attr(xx, which = "call")$ciperc})
    k <- length(cipercs)
    pl <- order(cipercs, decreasing = TRUE)
    pu <- order(cipercs, decreasing = FALSE)
    est <- ifelse("est.std" %in% colnames(semlbci_list[[1]]),
                  "est.std",
                  "est")
    cis <- lapply(semlbci_list, stats::confint)
    out <- do.call(cbind,
                    c(lapply(cis[pl], function(xx) {xx[, 1, drop = FALSE]}),
                      lapply(cis[pu], function(xx) {xx[, 2, drop = FALSE]})))
    ci_names <- c(paste0("lb_", names(semlbci_list)[pl]),
                  paste0("ub_", names(semlbci_list)[pu]))
    colnames(out) <- ci_names
    class(out) <- c("ci_order", class(out))
    out
  }

#' @param x The output of [ci_order()].
#'
#' @param digits The number of decimal places in the printout.
#'
#' @param ... Additional arguments. Not used.
#'
#' @return
#' `x` is returned invisibly. Called for its side effect.
#'
#' @describeIn ci_order The print method of the output of
#' [ci_order()].
#'
#' @export

print.ci_order <- function(x, digits = 3, ...) {
    out <- data.frame(lapply(x, formatC, digits = digits, format = "f"),
                      row.names = row.names(x))
    chk <- apply(x, 1, function(xx) identical(order(xx), seq_len(ncol(x))))
    for (i in seq_len(ncol(out))[-1]) {
        out[, i] <- paste0(ifelse(out[, i] > out[, i - 1],
                                  "< ",
                                  "! "), out[, i])
      }
    out$Order <- ifelse(chk, "OK", "Please check!")
    print(out, ...)
  }

#' @title Level of Confidence in a `semlbci`-Class Object
#'
#' @description Return a number only if the levels are the
#'  same for all LBCIs.
#'
#' @noRd

get_ciperc_all <- function(x) {
    if (!inherits(x, "semlbci")) {
        stop("x not a 'semlbci'-class object.")
      }
    lb_out <- attr(x, which = "lb_out")
    ub_out <- attr(x, which = "ub_out")
    cb_out <- c(lb_out[!is.na(lb_out)],
                ub_out[!is.na(ub_out)])
    if (length(cb_out) == 0) {
        return(NA)
      }
    out <- sapply(cb_out, function(xx) xx$diag$ciperc)
    if (!is.numeric(out) || length(out) == 0) {
        return(NA)
      }
    if (all(out[1] == out)) {
        return(unname(out[1]))
      } else {
        return(NA)
      }
    return(NA)
  }
