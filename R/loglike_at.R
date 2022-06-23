#' @title Log-likelihood When a Parameter is Fixed to a Value
#'
#' @description These functions compute the log-likelihood of a model
#'              when a parameter is fixed to a value or a range of
#'              values
#'
#' @details The methods presented in Pawitan (2013) are used to
#'          compute and visualize the log-likelihood of a structural
#'          equation model when a parameter is fixed to a value.
#'          [loglike_range()] and [loglike_point()] compute the
#'          so-called "true" log-likelihood, while
#'          [loglike_quad_range()] and [loglike_quad_point()] compute
#'          the log-likelihood using quadratic assumption.
#'
#' These functions are for the purpose of illustration and learning.
#' Therefore, they are not a versatile as [semlbci()] in the types of
#' models and parameters supported. They can be used for free
#' parameters and user-defined parameters not involved in any
#' constraints. Only a model fitted by maximum likelihood is
#' supported.
#'
#' @return [loglike_range()] returns a data frame with these columns:
#'
#' - `theta`: The values to which the parameter is fixed to.
#' - `loglike`: The log-likelihood values of the model.
#' - `pvalue`: The *p*-values based on the likelihood ratio difference
#'             test between the original model and model with the
#'             parameter fixed to theta.
#'
#' @param sem_out The SEM output. Currently \code{lavaan} output only.
#'
#' @param par_i The row number of the parameter in the output of
#'              [lavaan::parameterTable()]. Can also be a [lavaan::model.syntax]
#'              specification for a parameter, e.g., `"y ~ x"`. It will be
#'              converted to the row number by [syntax_to_i()].
#'
#' @param confidence The level of confidence of the Wald confidence
#'                   interval. If `interval` is `NULL`, this
#'                   confidence is used as the interval.
#'
#' @param n_points The number of points to be evaluated in the
#'                 interval.
#'
#' @param interval A vector of numbers. If provided and has two
#'                 elements, this will be used as the end points of
#'                 the interval. If it has more than two elements, the
#'                 elements will be used directly to form the range of
#'                 values in the interval. Default is `NULL`.
#'
#' @param verbose Whether some diagnostic information will be printed.
#'                Default is `FALSE`.
#'
#' @param start How the start values are set in [lavaan::lavaan()].
#'              See [lavaan::lavOptions()] on this argument.
#'              Default is `"default"`. If the plot is too irregular,
#'              try setting it to `"simple"`.
#'
#' @param parallel If `TRUE`, will use parallel processing.
#'                 A cluster will be created by [parallel::makeCluster()],
#'                 with the number of workers equal to `ncpus`.
#'
#' @param ncpus The number of workers if `parallel` is `TRUE`.
#'              Default is `parallel::detectCores(logical = FALSE) - 1`.
#'
#' @param use_pbapply If `TRUE` and `pbapply`
#'  is installed, `pbapply` will be used to display a
#'  progress bar when finding the intervals. Default is `TRUE`.
#'
#' @references Pawitan, Y. (2013). *In all likelihood: Statistical
#' modelling and inference using likelihood*. Oxford University Press.
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
#' @export


loglike_range <- function(sem_out, par_i,
                          confidence = .95,
                          n_points = 20,
                          interval = NULL,
                          verbose = FALSE,
                          start = "default",
                          parallel = FALSE,
                          ncpus = parallel::detectCores(logical = FALSE) - 1,
                          use_pbapply = TRUE) {
    # Parallel processing not yet supported
    # parallel <- FALSE
    if (is.character(par_i)) {
        par_i <- syntax_to_i(par_i, sem_out)
        if (length(par_i) != 1) {
            stop("par_i must denote one parameter only.")
          }
      }
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
    if (parallel) {
        cl <- parallel::makeCluster(ncpus)
        pkgs <- .packages()
        pkgs <- rev(pkgs)
        parallel::clusterExport(cl, "pkgs", envir = environment())
        parallel::clusterEvalQ(cl, {sapply(pkgs,
                        function(x) library(x, character.only = TRUE))
                      })
        parallel::clusterExport(cl, ls(envir = parent.frame()),
                                       envir = parent.frame())
        parallel::clusterExport(cl, ls(envir = environment()),
                                       envir = environment())
        if (requireNamespace("pbapply", quietly = TRUE) &
                    use_pbapply) {
            cat("\n", "Finding p-values for LR test", "\n",
                sep = "")
            flush.console()
            out <- pbapply::pblapply(thetas,
                                     semlbci::loglike_point,
                                     sem_out = sem_out,
                                     par_i = par_i,
                                     verbose = verbose,
                                     start = start,
                                     cl = cl)
          } else {
            out <- parallel::parLapplyLB(cl = cl,
                                         thetas,
                                         semlbci::loglike_point,
                                         sem_out = sem_out,
                                         par_i = par_i,
                                         verbose = verbose,
                                         start = start)
          }
        parallel::stopCluster(cl)
      } else {
        if (requireNamespace("pbapply", quietly = TRUE) &
                    use_pbapply) {
            cat("\n", "Finding p-values for LR test", "\n",
                sep = "")
            flush.console()
            out <- pbapply::pblapply(thetas,
                                     semlbci::loglike_point,
                                     sem_out = sem_out,
                                     par_i = par_i,
                                     verbose = verbose,
                                     start = start)
          } else {
            out <- lapply(thetas, loglike_point, sem_out = sem_out,
                                            par_i = par_i,
                                            verbose = verbose,
                                            start = start)
          }
      }
    out_final <- data.frame(theta = thetas,
                            loglike = sapply(out, function(x) x$loglike),
                            pvalue = sapply(out, function(x) x$pvalue))
    out_final
  }

#' @param theta0 The value at which the parameter is fixed to.
#'
#' @return [loglike_point()] returns a list with these elements:
#'
#' - `loglike`: The log-likelihood of the model when the parameter is
#'              fixed to `theta0`.
#' - `pvalue`: The *p*-values based on the likelihood ratio difference
#'             test between the original model and model with the
#'             parameter fixed to `theta0`.
#' - `fit`: A [lavaan::lavaan-class] object. The original model with
#'          the parameter fixed to `theta0`.
#' - `lrt`: The output of [lavaan::lavTestLRT()], comparing the
#'          original model with the model with the parameter fixed to
#'          `theta0`.
#'
#' @describeIn loglikelihood Description of this function
#' @order 2
#' @export

loglike_point <- function(theta0,
                          sem_out,
                          par_i,
                          verbose = FALSE,
                          start = "default") {
    if (is.character(par_i)) {
        par_i <- syntax_to_i(par_i, sem_out)
        if (length(par_i) != 1) {
            stop("par_i must denote one parameter only.")
          }
      }
    ptable <- lavaan::parameterTable(sem_out)
    op_i <- ptable[par_i, "op"]
    slot_opt2 <- sem_out@Options
    slot_pat2 <- sem_out@ParTable
    slot_mod2 <- sem_out@Model
    slot_smp2 <- sem_out@SampleStats
    slot_dat2 <- sem_out@Data
    slot_opt3 <- slot_opt2
    # slot_opt3$do.fit <- FALSE
    slot_opt3$se <- "none"
    if (op_i != ":=") {
        ptable[par_i, "free"] <- 0
        ptable[par_i, "start"] <- theta0
        ptable[par_i, "est"] <- theta0
        suppressWarnings(fit_i <- lavaan::update(sem_out, model = ptable, se = "none",
                                baseline = FALSE,
                                h1 = FALSE,
                                start = start,
                                slotOptions = slot_opt3,
                                slotSampleStats = slot_smp2,
                                slotData = slot_dat2))
      } else {
        par_plabel <- ptable$label[par_i]
        suppressWarnings(fit_i <- lavaan::update(sem_out,
                                add = paste0(par_plabel, " == ", theta0),
                                se = "none",
                                baseline = FALSE,
                                h1 = FALSE,
                                start = start,
                                slotOptions = slot_opt3,
                                slotSampleStats = slot_smp2,
                                slotData = slot_dat2))
       suppressWarnings(fit_i <- try_more(fit_i, attempts = 5))
      }
    # Suppress the warning that may occur if theta0 is close the
    # the estimate in sem_out
    lrt <- suppressWarnings(lavaan::lavTestLRT(fit_i, sem_out))
    if (verbose) print(lrt)
    loglike <- lavaan::logLik(fit_i)
    p <- lrt[2, "Pr(>Chisq)"]
    out <- list(loglike = loglike,
                pvalue = p,
                fit = fit_i,
                lrt = lrt)
    out
  }

#' @return [loglike_quad_range()] returns a data frame with these
#' columns:
#'
#' - `theta`: The values to which the parameter is fixed to.
#' - `loglike`: The log-likelihood values of the model using quadratic
#'              approximation.
#' - `pvalue`: The *p*-values based on the likelihood ratio difference
#'             test between the original model and model with the
#'             parameter fixed to theta.
#'
#' @describeIn loglikelihood Description of this function
#' @order 3
#' @export


loglike_quad_range <- function(sem_out,
                               par_i,
                               confidence = .95,
                               n_points = 20,
                               interval = NULL,
                               parallel = FALSE,
                               ncpus = parallel::detectCores(logical = FALSE) - 1,
                               use_pbapply = TRUE) {
    if (is.character(par_i)) {
        par_i <- syntax_to_i(par_i, sem_out)
        if (length(par_i) != 1) {
            stop("par_i must denote one parameter only.")
          }
      }
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
    out <- sapply(thetas, loglike_quad_point,
                  sem_out = sem_out, par_i = par_i)
    if (parallel) {
        cl <- parallel::makeCluster(ncpus)
        pkgs <- .packages()
        pkgs <- rev(pkgs)
        parallel::clusterExport(cl, "pkgs", envir = environment())
        parallel::clusterEvalQ(cl, {sapply(pkgs,
                        function(x) library(x, character.only = TRUE))
                      })
        parallel::clusterExport(cl, ls(envir = parent.frame()),
                                       envir = parent.frame())
        parallel::clusterExport(cl, ls(envir = environment()),
                                       envir = environment())
        if (requireNamespace("pbapply", quietly = TRUE) &
                    use_pbapply) {
            cat("\n", "Finding p-values for quadratic approximation", "\n",
                sep = "")
            flush.console()
            pvalues <- pbapply::pbsapply(thetas, function(x, sem_out, par_i) {
                                  loglike_point(x,
                                                sem_out = sem_out,
                                                par_i = par_i)$lrt[2, "Pr(>Chisq)"]
                                }, sem_out = sem_out, par_i = par_i, cl = cl)
          } else {
            pvalues <- parallel::parSapplyLB(cl = cl, thetas, function(x, sem_out, par_i) {
                                  loglike_point(x,
                                                sem_out = sem_out,
                                                par_i = par_i)$lrt[2, "Pr(>Chisq)"]
                                }, sem_out = sem_out, par_i = par_i)
          }
        parallel::stopCluster(cl)
      } else {
        if (requireNamespace("pbapply", quietly = TRUE) &
                    use_pbapply) {
            cat("\n", "Finding p-values for quadratic approximation", "\n",
                sep = "")
            flush.console()
            pvalues <- pbapply::pbsapply(thetas, function(x) {
                                  loglike_point(x,
                                                sem_out = sem_out,
                                                par_i = par_i)$lrt[2, "Pr(>Chisq)"]
                                })
          } else {
            pvalues <- sapply(thetas, function(x) {
                                  loglike_point(x,
                                                sem_out = sem_out,
                                                par_i = par_i)$lrt[2, "Pr(>Chisq)"]
                                })
          }
      }

    out_final <- data.frame(theta = thetas,
                            loglike = out,
                            pvalue = pvalues)
    out_final
  }

#' @return [loglike_quad_point()] returns a single number of the class
#'         `lavaan.vector` because it is the output out
#'         `lavann:;fitMeasures()`. This number is the quadratic
#'         approximation of the log-likelihood when the parameter is
#'         fixed to `theta0`.
#' @describeIn loglikelihood Description of this function
#' @order 4
#' @export


loglike_quad_point <- function(theta0,
                               sem_out,
                               par_i) {
    if (is.character(par_i)) {
        par_i <- syntax_to_i(par_i, sem_out)
        if (length(par_i) != 1) {
            stop("par_i must denote one parameter only.")
          }
      }
    # p_info <- 1 / lavaan::lavInspect(fit, "vcov")[par_i, par_i]
    est <- lavaan::parameterEstimates(sem_out)[par_i, "est"]
    p_info <- 1 / lavaan::parameterEstimates(sem_out)[par_i, "se"]^2
    -.5 * p_info * (theta0 - est) ^ 2 + lavaan::fitMeasures(sem_out, "logl")
  }

#' @return [loglike_compare()] calls [loglike_range()] and
#'          [loglike_quad_range()] and return their results as a list
#'          with these elements:
#'
#' - `quadratic`: The output of [loglike_quad_range()].
#' - `loglikelihood`: The output of [loglike_range()].
#' - `pvalue_quadratic`: The likelihood ratio test *p*-values at the
#'                       quadratic approximation confidence limits.
#' - `pvalue_loglikelihood`: The likelihood ratio test *p*-values at
#'                           the likelihood-based confidence limits.
#' - `est`: The estimate of the parameter in `sem_out`.
#'
#' @describeIn loglikelihood Description of this function
#' @order 5
#' @export

loglike_compare <- function(sem_out,
                            par_i,
                            confidence = .95,
                            n_points = 21,
                            start = "default",
                            parallel = FALSE,
                            ncpus = parallel::detectCores(logical = FALSE) - 1,
                            use_pbapply = TRUE) {
    if (is.character(par_i)) {
        par_i <- syntax_to_i(par_i, sem_out)
        if (length(par_i) != 1) {
            stop("par_i must denote one parameter.")
          }
      }
    par_i_name <- i_to_name(par_i, sem_out)
    ptable <- lavaan::parameterTable(sem_out)
    est <- ptable$est[par_i]
    se <- ptable$se[par_i]
    z <- stats::qnorm(1 - (1 - confidence) / 2)
    zs <- seq(-z, z, length.out = n_points)
    thetas_q <- est + se * zs
    lbci_i <- semlbci(sem_out, pars = par_i, ciperc = confidence)
    lbci_range <- unlist(unname(stats::confint(lbci_i)[1, ]))
    thetas_l <- seq(lbci_range[1], lbci_range[2], length.out = n_points)
    thetas_0 <- sort(c(thetas_q, thetas_l))
    int_q <- thetas_0[which(thetas_0 == min(thetas_q)):which(thetas_0 == max(thetas_q))]
    int_l <- thetas_0[which(thetas_0 == min(thetas_l)):which(thetas_0 == max(thetas_l))]
    ll_q <- loglike_quad_range(sem_out, par_i = par_i,
                               interval = int_q)
    ll <- loglike_range(sem_out, par_i = par_i,
                        interval = int_l,
                        start = start,
                        parallel = parallel,
                        ncpus = ncpus)
    pvalue_q_lb <- loglike_point(ll_q[1, "theta"],
                                 sem_out = sem_out,
                                 par_i = par_i,
                                 start = start)$lrt[2, "Pr(>Chisq)"]
    pvalue_q_ub <- loglike_point(ll_q[nrow(ll_q), "theta"],
                                 sem_out = sem_out,
                                 par_i = par_i,
                                 start = start)$lrt[2, "Pr(>Chisq)"]
    pvalue_l_lb <- loglike_point(ll[1, "theta"],
                                 sem_out = sem_out,
                                 par_i = par_i,
                                 start = start)$lrt[2, "Pr(>Chisq)"]
    pvalue_l_ub <- loglike_point(ll[nrow(ll), "theta"],
                                 sem_out = sem_out,
                                 par_i = par_i,
                                 start = start)$lrt[2, "Pr(>Chisq)"]
    out <- list(quadratic = ll_q,
                loglikelihood = ll,
                pvalue_quadratic = c(pvalue_q_lb, pvalue_q_ub),
                pvalue_loglikelihood = c(pvalue_l_lb, pvalue_l_ub),
                par_name = par_i_name,
                est = est)
    class(out) <- "loglike_compare"
    out
  }

#' @title Plot the Output of 'loglike_compare()'
#'
#' @description Visualize the likelihood of a model with a parameter
#'              fixed to values in a range.
#'
#' @details Given the output of [loglike_compare()], it plots the
#'          log-likelihood based on quadratic approximation and that
#'          based on the "true" log-likelihood. The log-likelihood is
#'          scaled to have a maximum of zero as suggested by Pawitan
#'          (2013).
#'
#' @return Nothing if `type = "default"`, the generated [ggplot2::ggplot()]
#'         graph if `type = "ggplot2"`.
#'
#' @param x The output of [loglike_compare()].
#'
#' @param y Not used.
#'
#' @param type Character. If `"ggplot2"`, will use [ggplot2] to plot
#'             the graph. If `"default"`, will use R base graphics,
#'             The `ggplot2` version prints more information.
#'             Default is `"ggplot2"`.
#'
#' @param add_pvalues If `TRUE`, likelihood ratio test *p*-values will be
#'                    plot for the confidence limits. Only available if
#'                    `type = "ggplot2"`.
#'
#' @param ... Optional arguments. Ignored.
#'
#' @references Pawitan, Y. (2013). *In all likelihood: Statistical
#' modelling and inference using likelihood*. Oxford University Press.
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
#' @export

plot.loglike_compare <- function(x, y,
                                 type = c("ggplot2", "default"),
                                 add_pvalues = FALSE,
                                 ...) {
    if (!is.null(x$par_name)) {
        par_name <- x$par_name
      } else {
        par_name <- "Parameter Value"
      }
    type <- match.arg(type)
    if (type == "ggplot2") {
        dat <- rbind(data.frame(x$quadratic, type = "quadratic"),
                     data.frame(x$loglikelihood, type = "true"))
        dat$loglike <- dat$loglike - max(dat$loglike)
        p <- ggplot2::ggplot() +
                ggplot2::geom_line(data = dat,
                                   ggplot2::aes(x = theta,
                                                y = loglike,
                                                color = type,
                                                linetype = type)) +
                ggplot2::geom_segment(ggplot2::aes(x = x$est,
                                                   y = min(dat$loglike),
                                                   xend = x$est,
                                                   yend = 0),
                                      color = "blue",
                                      linetype = "dashed") +
                ggplot2::geom_point(ggplot2::aes(x = x$est, y = min(dat$loglike)),
                                    color = "blue",
                                    shape = 4) +
                ggplot2::annotate("text", x = x$est, y = min(dat$loglike),
                                  label = formatC(x$est, 3, 4, format = "f"),
                                  color = "blue",
                                  vjust = 1,
                                  size = 4) +
                ggplot2::scale_colour_manual(values = c(quadratic = "red",
                                                        true = "blue")) +
                ggplot2::scale_linetype_manual(values = c(quadratic = "dashed",
                                                          true = "solid")) +
                ggplot2::geom_hline(yintercept = min(dat$loglike)) +
                ggplot2::ylab("Scaled Log (Profile) likelihood") +
                ggplot2::xlab(par_name) +
                ggplot2::labs(title = "Log (Profile) Likelihood") +
                ggplot2::theme(legend.position = "top")
        ll_max <- max(c(x$quadratic$loglike, x$loglike$loglike))
        dat_q <- x$quadratic[c(1, nrow(x$quadratic)), ]
        dat_q$loglike <- dat_q$loglike - ll_max
        dat_q$pvalue <- paste0("p=",formatC(dat_q$pvalue, 3, 4, format = "f"))
        dat_l <- x$loglike[c(1, nrow(x$loglike)), ]
        dat_l$loglike <- dat_l$loglike - ll_max
        dat_l$pvalue <- paste0("p=",formatC(dat_l$pvalue, 3, 4, format = "f"))
        dat_0 <- rbind(data.frame(dat_q, type = "quadratic"),
                        data.frame(dat_l, type = "true"))
        dat_0$theta_str <- formatC(dat_0$theta, 3, 4, "f")
        p <- p + ggrepel::geom_text_repel(data = dat_0,
                                              ggplot2::aes(x = theta,
                                                           y = loglike,
                                                           label = theta_str,
                                                           color = type),
                                              size = 4,
                                              box.padding = .5,
                                              nudge_y = -.5,
                                              show.legend = FALSE) +
                 ggplot2::ylim(-2.1, 0)
        if (add_pvalues) {
            p <- p + ggrepel::geom_text_repel(data = dat_0,
                                              ggplot2::aes(x = theta,
                                                           y = loglike,
                                                           label = pvalue,
                                                           color = type),
                                              size = 4,
                                              box.padding = .5,
                                              nudge_y = .25,
                                              show.legend = FALSE) +
                      ggplot2::geom_point(data = dat_0,
                                          ggplot2::aes(x = theta,
                                                       y = loglike,
                                                       color = type,
                                                       shape = type),
                                          size = 4,
                                          show.legend = c(color = FALSE,
                                                          shape = TRUE)) +
                      ggplot2::guides(color = ggplot2::guide_legend(order = 1),
                                      linetype = ggplot2::guide_legend(order = 1),
                                      shape = ggplot2::guide_legend(order = 2))
          }
        return(p)
      } else {
        theta_range <- range(c(x$quadratic$theta, x$loglikelihood$theta))
        loglik_range <- range(c(x$quadratic$loglike, x$loglikelihood$loglik))
        loglik_max <- max(c(x$quadratic$loglike, x$loglikelihood$loglik))
        loglik_range <- loglik_range - loglik_max
        # Plot quadratice approximated log-likelihood
        plot(x$quadratic$theta,
             x$quadratic$loglike - loglik_max,
             type = "l",
             lty = "dashed",
             col = "red",
             xlim = theta_range,
             ylim = loglik_range,
             main = "Log (Profile) Likelihood",
             xlab = par_name,
             ylab = "Scaled Log (Profile) Likelihood",
             sub = "Blue: Quadratic Approximation; Red: 'True' Log-Likelihood")
        # Plot true loglikelihood
        graphics::points(x$loglikelihood$theta,
               x$loglikelihood$loglike - loglik_max,
               type = "l",
               col = "blue")
        graphics::abline(h = min(x$quadratic$loglike - loglik_max))
      }
    invisible()
  }

#' @noRd

try_more <- function(object, attempts = 5, seed = NULL, rmin = .25, rmax = 1) {
    set.seed(seed)
    ptable <- lavaan::parameterTable(object)
    i_free <- ptable$free > 0
    i_free_p <- i_free & (ptable$op != "~~")
    k <- sum(i_free_p)
    ptable$est <- ptable$start
    x <- replicate(attempts, stats::runif(k, rmin, rmax), simplify = FALSE)
    out0 <- lapply(x, function(x) {
                      ptable_i <- ptable
                      ptable_i[i_free_p, "est"] <- ptable[i_free_p, "est"] * x
                      # Should do something to reject "bad" starting values
                      out <- tryCatch(stats::update(object, start = ptable_i,
                                      check.start = FALSE),
                                      error = function(e) e,
                                      warning = function(w) w)
                      out
                    })
    is_lavaan <- sapply(out0, inherits, what = "lavaan")
    if (all(!is_lavaan)) return(object)
    out0 <- out0[is_lavaan]
    out0 <- c(list(object), out0)
    fit_ok <- sapply(out0, lavaan::lavInspect, what = "post.check")
    if (all(!fit_ok)) return(object)
    out1 <- out0[fit_ok]
    fit_fmin <- sapply(out1, lavaan::fitMeasures, fit.measures = "fmin")
    out2 <- out1[which(fit_fmin == min(fit_fmin))]
    out2[[1]]
  }