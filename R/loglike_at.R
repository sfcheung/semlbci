#' @title Loglikelihood When a Parameter ss Fixed to Values in a Range
#'
#' @description ...
#'
#' @details ...
#'
#' @return ...
#'
#' @param sem_out The SEM output. Currently \code{lavaan} output only.
#'
#' @param par_i The row number of the parameter in the output of
#'              [lavaan::parameterTable()].
#'
#' @param confidence The level of confidence of the Wald confidence
#'                   interval. If `interval` is `NULL`, this confidence
#'                   is used as the interval.
#'
#' @param n_points The number of points to be evaluated in the interval.
#'
#' @param interval A vector of two numbers. If provided, this will be used
#'                 as the interval. Default is `NULL`.
#'
#' @param verbose Whether some diagnostic information will be printed.
#'                Default is `FALSE`.
#'
#' @examples
#'
#' library(lavaan)
#' data(simple_med)
#' mod <-
#' "
#' m ~ a*x
#' y ~ b*m
#' ab:= a*b
#' asq:= a^2
#' "
#' fit_med <- sem(mod, simple_med, fixed.x = FALSE)
#' p_table <- parameterTable(fit_med)
#'
#' pars <- c("m ~ x",
#'           "y ~ m",
#'           "asq := 1",
#'           "ab  := 2",
#'           "not in table")
#' out <- syntax_to_i(pars, fit_med)
#' out
#' p_table[out, ]
#'
#' @describeIn loglike_range Description of this function
#' @order 1

loglike_range <- function(sem_out, par_i,
                          confidence = .95,
                          n_points = 20,
                          interval = NULL,
                          verbose = FALSE) {
    ptable <- lavaan::parameterTable(sem_out)
    if (is.null(interval)) {
        est <- ptable$est[par_i]
        se <- ptable$se[par_i]
        z <- stats::qnorm(1 - (1 - confidence) / 2)
        zs <- seq(-z, z, length.out = n_points)
        thetas <- est + se * zs
      } else {
        thetas <- seq(interval[1], interval[2], length.out = n_points)
      }
    out <- lapply(thetas, loglike_point, sem_out = sem_out,
                                     par_i = par_i,
                                     verbose = verbose)
    out_final <- data.frame(theta = thetas,
                            loglike = sapply(out, function(x) x$loglike),
                            pvalue = sapply(out, function(x) x$pvalue))
    out_final
  }

#' @param theta0 The value at which the parameter is fixed to.
#'
#' @describeIn loglike_range Description of this function
#' @order 2

loglike_point <- function(theta0,
                          sem_out,
                          par_i,
                          verbose = FALSE) {
    ptable <- lavaan::parameterTable(sem_out)
    op_i <- ptable[par_i, "op"]
    if (op_i != ":=") {
        ptable[par_i, "free"] <- 0
        ptable[par_i, "start"] <- theta0
        ptable[par_i, "est"] <- theta0
        fit_i <- lavaan::update(sem_out, model = ptable, se = "none")
      } else {
        par_plabel <- ptable$label[par_i]
        fit_i <- lavaan::update(sem_out,
                                add = paste0(par_plabel, " == ", theta0),
                                se = "none")
      }
    lrt <- lavaan::lavTestLRT(fit_i, sem_out)
    if (verbose) print(lrt)
    loglike <- lavaan::logLik(fit_i)
    p <- lrt[2, "Pr(>Chisq)"]
    out <- list(loglike = loglike,
                pvalue = p,
                fit = fit_i,
                lrt = lrt)
    out
  }

#' @describeIn loglike_range Description of this function
#' @order 3


loglike_quad_range <- function(sem_out,
                               par_i,
                               confidence = .95,
                               n_points = 20) {
    ptable <- lavaan::parameterTable(sem_out)
    est <- ptable$est[par_i]
    se <- ptable$se[par_i]
    z <- stats::qnorm(1 - (1 - confidence) / 2)
    zs <- seq(-z, z, length.out = n_points)
    thetas <- est + se * zs
    out <- sapply(thetas, loglike_quad_point, sem_out = sem_out, par_i = par_i)
    out_final <- data.frame(theta = thetas,
                            loglike = out)
    out_final
  }

#' @describeIn loglike_range Description of this function
#' @order 4


loglike_quad_point <- function(theta0,
                               sem_out,
                               par_i) {
    # p_info <- 1 / lavaan::lavInspect(fit, "vcov")[par_i, par_i]
    est <- lavaan::parameterEstimates(sem_out)[par_i, "est"]
    p_info <- 1 / lavaan::parameterEstimates(sem_out)[par_i, "se"]^2
    -.5 * p_info * (theta0 - est) ^ 2 + lavaan::fitMeasures(sem_out, "logl")
  }
