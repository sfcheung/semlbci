#' @title Log Profile likelihood of a Parameter
#'
#' @description These functions compute the log profile likelihood of
#'  a parameter when it is fixed to a value or a range of values
#'
#' @details It uses the methods presented in Pawitan (2013) to
#'  compute and visualize the log profile likelihood of a parameter in
#'  a structural equation model when this parameter is fixed to a value or
#'  a range
#'  of values. [loglike_range()] and [loglike_point()] compute the
#'  so-called "true" log profile likelihood, while
#'  [loglike_quad_range()] and [loglike_quad_point()] approximate the log
#'  profile likelihood by a quadratic function.
#'
#' These functions are for creating illustrative examples and learning
#' only, not for research use. Therefore, they are not as versatile as
#' [semlbci()] in the types of models and parameters supported. They
#' can be used for free parameters and user-defined parameters not
#' involved in any constraints. Only a model fitted by maximum
#' likelihood is supported.
#'
#' They will not check whether the computation is appropriate for a
#' model. It is the responsibility of the users to ensure that the
#' computation is appropriate for the model and parameter.
#'
#' @return [loglike_range()] returns a data frame with these columns:
#'
#' - `theta`: The values to which the parameter is fixed to.
#' - `loglike`: The log profile likelihood at `theta`.
#' - `pvalue`: The *p*-values based on the likelihood ratio difference
#'             test between the original model and model with the
#'             parameter fixed to `theta`.
#'
#' @param sem_out The SEM output. Currently the outputs
#'   of [lavaan::lavaan()] or its wrappers, such as [lavaan::sem()]
#'   and [lavaan::cfa()] are supported.
#'
#' @param semlbci_out The output of [semlbci::semlbci()].
#'   If supplied, it will extract the likelihood-based confidence
#'   interval from the output. If not, it will call [semlbci::semlbci()].
#'
#' @param par_i The row number of the parameter in the output of
#'   [lavaan::parameterTable()]. Can also be a [lavaan::model.syntax]
#'   specification for a parameter, e.g., `"y ~ x"` or `ab := `.
#'   It will be converted to the row number by [syntax_to_i()]. Refer to
#'   [syntax_to_i()] for details.
#'
#' @param confidence The level of confidence of the Wald-type
#'   confidence interval. If `interval` is `NULL`, this confidence is
#'   used to form the interval.
#'
#' @param n_points The number of points to be evaluated in the
#'   interval. Default is 21.
#'
#' @param interval A vector of numbers. If provided and has two
#'   elements, this will be used as the end points of the interval. If
#'   it has more than two elements, the elements will be used directly
#'   to form the values in the interval. Default is `NULL`.
#'
#' @param verbose Whether some diagnostic information will be printed.
#'   Default is `FALSE`.
#'
#' @param start How the start values are set in [lavaan::lavaan()].
#'   See [lavaan::lavOptions()] on this argument. Default is
#'   `"default"`. If the plot is too irregular, try setting it to
#'   `"simple"`.
#'
#' @param try_k_more How many more times to try finding the p-values,
#'   by randomizing the starting values. Default is 5. Try increasing
#'   this number if the plot is too irregular.
#'
#' @param parallel If `TRUE`, parallel processing will be used. A
#'   cluster will be created by [parallel::makeCluster()], with the
#'   number of workers equal to `ncpus`. Parallel processing, though
#'   not enabled by default, is recommended because it can speed up
#'   the computation a lot.
#'
#' @param ncpus The number of workers if `parallel` is `TRUE`. Default
#'   is `parallel::detectCores(logical = FALSE) - 1`, the number of
#'   physical cores minus 1.
#'
#' @param use_pbapply If `TRUE` and [pbapply::pbapply] is installed,
#'  [pbapply::pbapply] will be used to display the progress in
#'  computing the log profile likelihood. Default is `TRUE`.
#'
#' @references Pawitan, Y. (2013). *In all likelihood: Statistical
#' modelling and inference using likelihood*. Oxford University Press.
#'
#' @name loglikelihood
NULL

#' @examples
#'
#' ## loglike_range
#'
#' # Usually not to be used directly.
#' # Used by loglike_compare().
#' # 3 points are used just for illustration
#' # ll_1 <- loglike_range(fit, par_i = "y ~ m", n_points = 3)
#' # head(ll_1)
#'
#' @describeIn loglikelihood Find the log profile likelihood for a range of values.
#' @order 2
#' @export


loglike_range <- function(sem_out, par_i,
                          confidence = .95,
                          n_points = 21,
                          interval = NULL,
                          verbose = FALSE,
                          start = "default",
                          try_k_more = 5,
                          parallel = FALSE,
                          ncpus = parallel::detectCores(logical = FALSE) - 1,
                          use_pbapply = TRUE) {
    if (is.character(par_i)) {
        par_i <- syntax_to_i(par_i, sem_out)
        if (length(par_i) != 1) {
            stop("par_i must denote one parameter only.")
          }
      }
    ptable <- lavaan::parameterTable(sem_out)
    if (is.null(interval)) {
        # Use Wald-type CI to determine the interval
        est <- ptable$est[par_i]
        se <- ptable$se[par_i]
        z <- stats::qnorm(1 - (1 - confidence) / 2)
        zs <- seq(-z, z, length.out = n_points)
        thetas <- est + se * zs
      } else {
        if (length(interval) == 2) {
            # Form n_points points in the interval
            thetas <- seq(interval[1], interval[2], length.out = n_points)
          } else {
            # Use the interval as-is
            thetas <- interval
          }
      }
    if (parallel) {
        # Parallel
        cl <- parallel::makeCluster(ncpus)
        # Rebuild the environment
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
        if (requireNamespace("pbapply", quietly = TRUE) &&
                    use_pbapply) {
            # Use pbapply
            cat("\n", "Finding p-values for LR test", "\n",
                sep = "")
            utils::flush.console()
            out <- pbapply::pblapply(thetas,
                                     semlbci::loglike_point,
                                     sem_out = sem_out,
                                     par_i = par_i,
                                     verbose = verbose,
                                     start = start,
                                     try_k_more = try_k_more,
                                     cl = cl)
          } else {
            # No progress bar
            out <- parallel::parLapplyLB(cl = cl,
                                         thetas,
                                         semlbci::loglike_point,
                                         sem_out = sem_out,
                                         par_i = par_i,
                                         verbose = verbose,
                                         start = start,
                                         try_k_more = try_k_more)
          }
        parallel::stopCluster(cl)
      } else {
        # Serial
        if (requireNamespace("pbapply", quietly = TRUE) &&
                    use_pbapply) {
            # Use pbapply
            cat("\n", "Finding p-values for LR test", "\n",
                sep = "")
            utils::flush.console()
            out <- pbapply::pblapply(thetas,
                                     semlbci::loglike_point,
                                     sem_out = sem_out,
                                     par_i = par_i,
                                     verbose = verbose,
                                     start = start,
                                     try_k_more = try_k_more)
          } else {
            # No progress bar
            out <- lapply(thetas, loglike_point, sem_out = sem_out,
                                            par_i = par_i,
                                            verbose = verbose,
                                            start = start,
                                            try_k_more = try_k_more)
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
#' - `loglike`: The log profile likelihood of the parameter when it is
#'              fixed to `theta0`.
#' - `pvalue`: The *p*-values based on the likelihood ratio difference
#'             test between the original model and the model with the
#'             parameter fixed to `theta0`.
#' - `fit`: A [lavaan::lavaan-class] object. The original model with
#'          the parameter fixed to `theta0`.
#' - `lrt`: The output of [lavaan::lavTestLRT()], comparing the
#'          original model to the model with the parameter fixed to
#'          `theta0`.
#' @examples
#'
#' ## loglike_point
#'
#' # Usually not to be used directly.
#' # Used by loglike_compare().
#' # llp_1 <- loglike_point(theta0 = 0.3, sem_out = fit, par_i = "y ~ m")
#' # llp_1$loglike
#' # llp_1$pvalue
#' # llp_1$lrt
#'
#'
#' @describeIn loglikelihood Find the log likelihood at a value.
#' @order 3
#' @export

loglike_point <- function(theta0,
                          sem_out,
                          par_i,
                          verbose = FALSE,
                          start = "default",
                          try_k_more = 5) {
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
    slot_opt3$do.fit <- FALSE
    slot_opt3$se <- "none"
    if (ptable$label[par_i] == "") {
        # par_i has no label
        ptable_i <- ptable
        ptable_i[par_i, "free"] <- 0
        ptable_i[par_i, "start"] <- theta0
        ptable_i[par_i, "est"] <- theta0
        slot_opt3$do.fit <- TRUE
        suppressWarnings(fit_i <- lavaan::lavaan(
                               model = ptable_i,
                               slotOptions = slot_opt3,
                               slotSampleStats = slot_smp2,
                               slotData = slot_dat2))
        suppressWarnings(fit_i <- try_more(fit_i, attempts = try_k_more))
      } else {
        # par_i is labelled
        par_plabel <- ptable$label[par_i]
        ptable_i <- lavaan::lav_partable_merge(ptable,
                                lavaan::lavaanify(paste0(par_plabel,
                                                  " == ",
                                                  theta0)),
                                remove.duplicated = TRUE,
                                warn = FALSE)
        slot_opt3$do.fit <- TRUE
        suppressWarnings(fit_i <- lavaan::lavaan(
                               model = ptable_i,
                               slotOptions = slot_opt3,
                               slotSampleStats = slot_smp2,
                               slotData = slot_dat2))
        suppressWarnings(fit_i <- try_more(fit_i, attempts = try_k_more))
      }
    # Suppress the warning that may occur if theta0 is close to the
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
#' - `loglike`: The log profile likelihood values of the parameter
#'              using quadratic approximation.
#' - `pvalue`: The *p*-values based on the likelihood ratio difference
#'             test between the original model and the model with the
#'             parameter fixed to `theta`.
#'
#' @examples
#'
#' ## loglike_quad_range
#'
#' # Usually not to be used directly.
#' # Used by loglike_compare().
#' # 3 points are used just for illustration
#' # lq_1 <- loglike_quad_range(fit, par_i = "y ~ m", n_points = 3)
#' # head(lq_1)
#'
#'
#' @describeIn loglikelihood Find the approximated log likelihood for a range of values.
#' @order 4
#' @export


loglike_quad_range <- function(sem_out,
                               par_i,
                               confidence = .95,
                               n_points = 21,
                               interval = NULL,
                               parallel = FALSE,
                               ncpus = parallel::detectCores(logical = FALSE) - 1,
                               use_pbapply = TRUE,
                               try_k_more = 5,
                               start = "default") {
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
        # Parallel
        cl <- parallel::makeCluster(ncpus)
        # Rebuild the environment
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
        if (requireNamespace("pbapply", quietly = TRUE) &&
                    use_pbapply) {
            # Use pbapply
            cat("\n", "Finding p-values for quadratic approximation", "\n",
                sep = "")
            utils::flush.console()
            pvalues <- pbapply::pbsapply(thetas,
                                  function(x,
                                           sem_out,
                                           par_i,
                                           start,
                                           try_k_more) {
                                      loglike_point(x,
                                                    sem_out = sem_out,
                                                    par_i = par_i,
                                                    start = start,
                                                    try_k_more = try_k_more)$lrt[2, "Pr(>Chisq)"]
                                    },
                                sem_out = sem_out,
                                par_i = par_i,
                                start = start,
                                try_k_more = try_k_more,
                                cl = cl)
          } else {
            # No progress bar
            pvalues <- parallel::parSapplyLB(cl = cl,
                                  thetas,
                                  function(x,
                                           sem_out,
                                           par_i,
                                           start,
                                           try_k_more) {
                                      loglike_point(x,
                                                    sem_out = sem_out,
                                                    par_i = par_i,
                                                    start = start,
                                                    try_k_more = try_k_more)$lrt[2, "Pr(>Chisq)"]
                                    },
                                sem_out = sem_out,
                                par_i = par_i,
                                start = start,
                                try_k_more = try_k_more)
          }
        parallel::stopCluster(cl)
      } else {
        # Serial
        if (requireNamespace("pbapply", quietly = TRUE) &&
                    use_pbapply) {
            # Use pbapply
            cat("\n", "Finding p-values for quadratic approximation", "\n",
                sep = "")
            utils::flush.console()
            pvalues <- pbapply::pbsapply(thetas, function(x) {
                                  loglike_point(x,
                                                sem_out = sem_out,
                                                par_i = par_i,
                                                start = start,
                                                try_k_more = try_k_more)$lrt[2, "Pr(>Chisq)"]
                                })
          } else {
            # No progress bar
            pvalues <- sapply(thetas, function(x) {
                                  loglike_point(x,
                                                sem_out = sem_out,
                                                par_i = par_i,
                                                start = start,
                                                try_k_more = try_k_more)$lrt[2, "Pr(>Chisq)"]
                                })
          }
      }

    out_final <- data.frame(theta = thetas,
                            loglike = out,
                            pvalue = pvalues)
    out_final
  }

#' @return [loglike_quad_point()] returns a single number of the class
#'         `lavaan.vector` (because it is the output of
#'         [lavaan::fitMeasures()]). This number is the quadratic
#'         approximation of the log profile likelihood when the parameter is
#'         fixed to `theta0`.
#'
#'
#' @examples
#'
#' ## loglike_quad_point
#'
#' # Usually not to be used directly.
#' # Used by loglike_compare().
#' # lqp_1 <- loglike_quad_point(theta0 = 0.3, sem_out = fit, par_i = "y ~ m")
#' # lqp_1
#'
#'
#' @describeIn loglikelihood Find the approximated log likelihood at a value.
#' @order 5
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
    est <- lavaan::parameterEstimates(sem_out)[par_i, "est"]
    p_info <- 1 / lavaan::parameterEstimates(sem_out)[par_i, "se"]^2
    -.5 * p_info * (theta0 - est) ^ 2 + lavaan::fitMeasures(sem_out, "logl")
  }

#' @return [loglike_compare()] calls [loglike_range()] and
#'          [loglike_quad_range()] and returns their results in a
#'          `loglike_compare`-class object, a list
#'          with these elements:
#'
#' - `quadratic`: The output of [loglike_quad_range()].
#' - `loglikelihood`: The output of [loglike_range()].
#' - `pvalue_quadratic`: The likelihood ratio test *p*-values at the
#'                       quadratic approximation confidence bounds.
#' - `pvalue_loglikelihood`: The likelihood ratio test *p*-values at
#'                           the likelihood-based confidence bounds.
#' - `est`: The point estimate of the parameter in `sem_out`.
#'
#' `loglike_compare`-class object has a `plot` method ([plot.loglike_compare()])
#' that can be used to plot the log profile likelihood.
#'
#' @examples
#'
#' ## loglike_compare
#'
#' library(lavaan)
#' data(simple_med)
#' dat <- simple_med
#' mod <-
#' "
#' m ~ a * x
#' y ~ b * m
#' ab := a * b
#' "
#' fit <- lavaan::sem(mod, simple_med, fixed.x = FALSE)
#'
#' # 4 points are used just for illustration
#' # At least 21 points should be used for a smooth plot
#' # Remove try_k_more in real applications. It is set
#' # to run such that this example is not too slow.
#' # use_pbapply can be removed or set to TRUE to show the progress.
#' ll_a <- loglike_compare(fit, par_i = "m ~ x", n_points = 4,
#'                         try_k_more = 0,
#'                         use_pbapply = FALSE)
#' plot(ll_a)
#'
#' # See the vignette "loglike" for an example for the
#' # indirect effect.
#'
#' @seealso [plot.loglike_compare()]
#'
#' @describeIn loglikelihood Generates points for log profile likelihood and
#' quadratic approximation, by calling the helper functions `loglike_range()`
#' and `loglike_quad_range()`.
#'
#' @order 1
#' @export

loglike_compare <- function(sem_out,
                            semlbci_out = NULL,
                            par_i,
                            confidence = .95,
                            n_points = 21,
                            start = "default",
                            try_k_more = 5,
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
    if (is.null(semlbci_out)) {
        lbci_i <- semlbci(sem_out,
                          pars = par_i,
                          ciperc = confidence,
                          parallel = FALSE,
                          use_pbapply = use_pbapply)
      } else {
        lbci_i <- semlbci_out
      }
    lbci_range <- unlist(unname(stats::confint(lbci_i)[1, ]))
    thetas_l <- seq(lbci_range[1], lbci_range[2], length.out = n_points)
    thetas_0 <- sort(c(thetas_q, thetas_l))
    int_q <- thetas_0[which(thetas_0 == min(thetas_q)):which(thetas_0 == max(thetas_q))]
    int_l <- thetas_0[which(thetas_0 == min(thetas_l)):which(thetas_0 == max(thetas_l))]
    ll_q <- loglike_quad_range(sem_out, par_i = par_i,
                               interval = int_q,
                               start = start,
                               try_k_more = try_k_more,
                               parallel = parallel,
                               ncpus = ncpus,
                               use_pbapply = use_pbapply)
    ll <- loglike_range(sem_out, par_i = par_i,
                        interval = int_l,
                        start = start,
                        try_k_more = try_k_more,
                        parallel = parallel,
                        ncpus = ncpus,
                        use_pbapply = use_pbapply)
    pvalue_q_lb <- loglike_point(ll_q[1, "theta"],
                                 sem_out = sem_out,
                                 par_i = par_i,
                                 start = start,
                                 try_k_more = try_k_more)$lrt[2, "Pr(>Chisq)"]
    pvalue_q_ub <- loglike_point(ll_q[nrow(ll_q), "theta"],
                                 sem_out = sem_out,
                                 par_i = par_i,
                                 start = start,
                                 try_k_more = try_k_more)$lrt[2, "Pr(>Chisq)"]
    pvalue_l_lb <- loglike_point(ll[1, "theta"],
                                 sem_out = sem_out,
                                 par_i = par_i,
                                 start = start,
                                 try_k_more = try_k_more)$lrt[2, "Pr(>Chisq)"]
    pvalue_l_ub <- loglike_point(ll[nrow(ll), "theta"],
                                 sem_out = sem_out,
                                 par_i = par_i,
                                 start = start,
                                 try_k_more = try_k_more)$lrt[2, "Pr(>Chisq)"]
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
#' @description Visualize the log profile likelihood of a parameter
#'              fixed to values in a range.
#'
#' @details Given the output of [loglike_compare()], it plots the log
#'   profile likelihood based on quadratic approximation and that
#'   based on the original log-likelihood. The log profile likelihood
#'   is scaled to have a maximum of zero (at the point estimate) as
#'   suggested by Pawitan (2013).
#'
#' @return Nothing if `type = "default"`, the generated [ggplot2::ggplot()]
#'         graph if `type = "ggplot2"`.
#'
#' @param x The output of [loglike_compare()].
#'
#' @param y Not used.
#'
#' @param type Character. If `"ggplot2"`, will use [ggplot2::ggplot()]
#'   to plot the graph. If `"default"`, will use R base graphics, The
#'   `ggplot2` version plots more information. Default is `"ggplot2"`.
#'
#' @param size_label The relative size of the labels for thetas
#'   (and *p*-values, if requested) in the
#'   plot, determined by [ggplot2::rel()]. Default is 4.
#'
#' @param size_point The relative size of the points to be added
#'   if *p*-values are requested in the
#'   plot, determined by [ggplot2::rel()]. Default is 4.
#'
#' @param nd_theta The number of decimal places for the labels
#'   of theta. Default is 3.
#'
#' @param nd_pvalue The number of decimal places for the labels
#'   of *p*-values. Default is 3.
#' @param size_theta Deprecated. No longer used.
#'
#' @param size_pvalue Deprecated. No longer used.
#'
#' @param add_pvalues If `TRUE`, likelihood ratio test *p*-values will
#'    be included for the confidence limits. Only available if `type =
#'    "ggplot2"`.
#'
#' @param ... Optional arguments. Ignored.
#'
#' @references Pawitan, Y. (2013). *In all likelihood: Statistical
#' modelling and inference using likelihood*. Oxford University Press.
#'
#' @examples
#'
#' ## loglike_compare
#'
#' library(lavaan)
#' data(simple_med)
#' dat <- simple_med
#' mod <-
#' "
#' m ~ a * x
#' y ~ b * m
#' ab := a * b
#' "
#' fit <- lavaan::sem(mod, simple_med, fixed.x = FALSE)
#'
#' # Four points are used just for illustration
#' # At least 21 points should be used for a smooth plot
#' # Remove try_k_more in real applications. It is set
#' # to run such that this example is not too slow.
#' # use_pbapply can be removed or set to TRUE to show the progress.
#' ll_a <- loglike_compare(fit, par_i = "m ~ x", n_points = 4,
#'                         try_k_more = 0,
#'                         use_pbapply = FALSE)
#'
#' plot(ll_a)
#' plot(ll_a, add_pvalues = TRUE)
#'
#' # See the vignette "loglike" for an example for the
#' # indirect effect.
#'
#' @importFrom rlang .data
#' @export

plot.loglike_compare <- function(x, y,
                                 type = c("ggplot2", "default"),
                                 size_label = 4,
                                 size_point = 4,
                                 nd_theta = 3,
                                 nd_pvalue = 3,
                                 size_theta = 4,
                                 size_pvalue = 4,
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
                                   ggplot2::aes(x = .data$theta,
                                                y = .data$loglike,
                                                color = .data$type,
                                                linetype = .data$type)) +
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
                                  label = formatC(x$est,
                                                  digits = nd_theta,
                                                  width = nd_theta + 1,
                                                  format = "f"),
                                  color = "blue",
                                  vjust = 1,
                                  size = ggplot2::rel(size_label)) +
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
        # dat_q$pvalue <- paste0("p=",formatC(dat_q$pvalue, 3, 4, format = "f"))
        dat_q$pvalue <- paste0("italic(p) == ",
                                formatC(dat_q$pvalue,
                                        digits = nd_pvalue,
                                        width = nd_pvalue + 1, format = "f"))
        dat_l <- x$loglike[c(1, nrow(x$loglike)), ]
        dat_l$loglike <- dat_l$loglike - ll_max
        # dat_l$pvalue <- paste0("p=",formatC(dat_l$pvalue, 3, 4, format = "f"))
        dat_l$pvalue <- paste0("italic(p) == ",
                                formatC(dat_l$pvalue,
                                        digits = nd_pvalue,
                                        width = nd_pvalue + 1, format = "f"))
        dat_0 <- rbind(data.frame(dat_q, type = "quadratic"),
                        data.frame(dat_l, type = "true"))
        # dat_0$theta_str <- formatC(dat_0$theta, 3, 4, "f")
        dat_0$theta_str <- paste0("theta1 == ",
                                  formatC(dat_0$theta,
                                          digits = nd_theta,
                                          width = nd_theta + 1, "f"))
        dat_1 <- dat_0
        dat_0 <- dat_1[, c("type", "theta", "loglike", "theta_str")]
        colnames(dat_0)[which(colnames(dat_0) == "theta_str")] <- "value"
        dat_0$label_size <- ggplot2::rel(size_label)
        if (add_pvalues) {
            dat_0a <- dat_1[, c("type", "theta", "loglike", "pvalue")]
            colnames(dat_0a)[which(colnames(dat_0a) == "pvalue")] <- "value"
            dat_0a$label_size <- ggplot2::rel(size_label)
            dat_0 <- rbind(dat_0, dat_0a)
          }
        p <- p + ggrepel::geom_text_repel(data = dat_0,
                                          ggplot2::aes(x = .data$theta,
                                                       y = .data$loglike,
                                                       label = .data$value,
                                                       color = .data$type),
                                          size = ggplot2::rel(size_label),
                                          box.padding = .5,
                                          parse = TRUE,
                                          # nudge_y = -.5,
                                          show.legend = FALSE) +
                 ggplot2::ylim(-2.1, 0)
        if (add_pvalues) {
            # p <- p + ggrepel::geom_text_repel(data = dat_0,
            #                                   ggplot2::aes(x = theta,
            #                                                y = loglike,
            #                                                label = pvalue,
            #                                                color = type),
            #                                   size = ggplot2::rel(size_pvalue),
            #                                   box.padding = .5,
            #                                   nudge_y = .25,
            #                                   show.legend = FALSE) +
            p <- p + ggplot2::geom_point(data = dat_1,
                                         ggplot2::aes(x = .data$theta,
                                                      y = .data$loglike,
                                                      color = .data$type,
                                                      shape = .data$type),
                                         size = ggplot2::rel(size_point),
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
    attempts <- as.integer(attempts)
    if (attempts < 2) return(object)
    set.seed(seed)
    ptable <- lavaan::parameterTable(object)
    i_free <- ptable$free > 0
    i_free_p <- i_free & (ptable$op != "~~")
    k <- sum(i_free_p)
    ptable$est <- ptable$start
    x <- replicate(attempts, stats::runif(k, rmin, rmax), simplify = FALSE)
    slot_opt2 <- object@Options
    slot_pat2 <- object@ParTable
    slot_mod2 <- object@Model
    slot_smp2 <- object@SampleStats
    slot_dat2 <- object@Data
    slot_opt3 <- slot_opt2
    slot_opt3$check.start <- FALSE
    out0 <- lapply(x, function(y) {
                      ptable_i <- ptable
                      ptable_i[i_free_p, "est"] <- ptable[i_free_p, "est"] * y
                      ptable_i[i_free_p, "start"] <- ptable[i_free_p, "est"] * y
                      ptable_i[i_free_p, "ustart"] <- ptable[i_free_p, "est"] * y
                      out <- tryCatch(
                              fit2 <- suppressWarnings(lavaan::lavaan(
                                        model = ptable_i,
                                        slotOptions = slot_opt3,
                                        slotSampleStats = slot_smp2,
                                        slotData = slot_dat2,
                                        warn = FALSE)),
                              error = function(e) e
                            )
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