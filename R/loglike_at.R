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
#' @param interval A vector of numbers. If provided and has two elements, this
#'                 will be used
#'                 as the end points of the interval. If it has more than
#'                 two elements, the elements will be used directly to form
#'                 the range of values in the interval.
#'                 Default is `NULL`.
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
#' @name loglikelihood
NULL

#' @describeIn loglikelihood Description of this function
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
        if (length(interval) == 2) {
            thetas <- seq(interval[1], interval[2], length.out = n_points)
          } else {
            thetas <- interval
          }
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
#' @describeIn loglikelihood Description of this function
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

#' @describeIn loglikelihood Description of this function
#' @order 3


loglike_quad_range <- function(sem_out,
                               par_i,
                               confidence = .95,
                               n_points = 20,
                               interval = NULL) {
    if (is.null(interval)) {
        ptable <- lavaan::parameterTable(sem_out)
        est <- ptable$est[par_i]
        se <- ptable$se[par_i]
        z <- stats::qnorm(1 - (1 - confidence) / 2)
        zs <- seq(-z, z, length.out = n_points)
        thetas <- est + se * zs
      } else {
        if (length(interval) == 2) {
            thetas <- seq(interval[1], interval[2], length.out = n_points)
          } else {
            thetas <- interval
          }
      }
    out <- sapply(thetas, loglike_quad_point, sem_out = sem_out, par_i = par_i)
    pvalues <- sapply(thetas, function(x) {
                          loglike_point(x, sem_out = sem_out, par_i = par_i)$lrt[2, "Pr(>Chisq)"]
                        })
    out_final <- data.frame(theta = thetas,
                            loglike = out,
                            pvalue = pvalues)
    out_final
  }

#' @describeIn loglikelihood Description of this function
#' @order 4


loglike_quad_point <- function(theta0,
                               sem_out,
                               par_i) {
    # p_info <- 1 / lavaan::lavInspect(fit, "vcov")[par_i, par_i]
    est <- lavaan::parameterEstimates(sem_out)[par_i, "est"]
    p_info <- 1 / lavaan::parameterEstimates(sem_out)[par_i, "se"]^2
    -.5 * p_info * (theta0 - est) ^ 2 + lavaan::fitMeasures(sem_out, "logl")
  }

#' @describeIn loglikelihood Description of this function
#' @order 5

loglike_compare <- function(sem_out,
                            par_i,
                            confidence = .95,
                            n_points = 20) {
    ptable <- lavaan::parameterTable(sem_out)
    est <- ptable$est[par_i]
    se <- ptable$se[par_i]
    z <- stats::qnorm(1 - (1 - confidence) / 2)
    zs <- seq(-z, z, length.out = n_points)
    thetas_q <- est + se * zs
    lbci_i <- semlbci(sem_out, pars = par_i, ciperc = confidence)
    lbci_range <- unlist(unname(confint(lbci_i)[1, ]))
    thetas_l <- seq(lbci_range[1], lbci_range[2], length.out = n_points)
    thetas_0 <- sort(c(thetas_q, thetas_l))
    int_q <- thetas_0[which(thetas_0 == min(thetas_q)):which(thetas_0 == max(thetas_q))]
    int_l <- thetas_0[which(thetas_0 == min(thetas_l)):which(thetas_0 == max(thetas_l))]
    ll_q <- loglike_quad_range(sem_out, par_i = par_i,
                               interval = int_q)
    ll <- loglike_range(sem_out, par_i = par_i,
                        interval = int_l)
    # ll_q <- loglike_quad_range(sem_out, par_i = par_i,
    #                            confidence = confidence, n_points = n_points)
    # ll <- loglike_range(sem_out, par_i = par_i,
    #                     interval = theta_int,
    #                     n_points = n_points)
    # # theta_range <- range(c(ll_q$theta, ll$theta))
    # loglik_range <- range(c(ll_q$loglike, ll$loglik))
    pvalue_q_lb <- loglike_point(ll_q[1, "theta"], sem_out = sem_out, par_i = par_i)$lrt[2, "Pr(>Chisq)"]
    pvalue_q_ub <- loglike_point(ll_q[nrow(ll_q), "theta"], sem_out = sem_out, par_i = par_i)$lrt[2, "Pr(>Chisq)"]
    pvalue_l_lb <- loglike_point(ll[1, "theta"], sem_out = sem_out, par_i = par_i)$lrt[2, "Pr(>Chisq)"]
    pvalue_l_ub <- loglike_point(ll[nrow(ll), "theta"], sem_out = sem_out, par_i = par_i)$lrt[2, "Pr(>Chisq)"]
    out <- list(quadratic = ll_q,
                loglikelihood = ll,
                pvalue_quadratic = c(pvalue_q_lb, pvalue_q_ub),
                pvalue_loglikelihood = c(pvalue_l_lb, pvalue_l_ub))
    class(out) <- "loglike_compare"
    out
  }

#' @title Plot the Output of 'loglike_compare()'
#'
#' @description ...
#'
#' @details ...
#'
#' @return ...
#'
#' @param x The output of [loglike_compare()].
#' @param y Not used.
#' @param type Character. If `"ggplot2"`, will use [ggplot2] to plot the graph.
#'             Default is `"default"` and R base graphics is used.
#' @param ... Optional arguments. Ignored.
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
#' @export

plot.loglike_compare <- function(x, y, type = "default", ...) {
    if (type == "ggplot2") {
        dat <- rbind(data.frame(x$quadratic, type = "quadratic"),
                     data.frame(x$loglikelihood, type = "true"))
        dat$loglike <- dat$loglike - max(dat$loglike)
        p <- ggplot2::ggplot() +
                ggplot2::geom_line(data = dat,
                                   ggplot2::aes(x = theta, y = loglike, color = type)) +
                ggplot2::geom_hline(yintercept = min(dat$loglike)) +
                ggplot2::ylab("Scaled Loglikelihood") +
                ggplot2::xlab("Parameter Value") +
                ggplot2::labs(title = "Loglikelihood") +
                ggplot2::theme(legend.position = "top")
        return(p)
      } else {
        theta_range <- range(c(x$quadratic$theta, x$loglikelihood$theta))
        loglik_range <- range(c(x$quadratic$loglike, x$loglikelihood$loglik))
        loglik_max <- max(c(x$quadratic$loglike, x$loglikelihood$loglik))
        loglik_range <- loglik_range - loglik_max
        plot(x$quadratic$theta, x$quadratic$loglike - loglik_max, type = "l", col = "blue",
            xlim = theta_range, ylim = loglik_range)
        # Plot true loglikelihood
        points(x$loglikelihood$theta, x$loglikelihood$loglike - loglik_max, type = "l", col = "red")
      }
    invisible(NULL)
  }