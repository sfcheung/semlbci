#'@title Find the LBCIs for all free parameters in an SEM output
#'
#'@description Find the LBCIs for all free parameters in an SEM output
#'
#'@details
#'
#' Currently supports \code{lavaan} output only.
#'
#'@return
#' The LBCIs for all free parameters
#'
#' @param sem_out The SEM output. Currently \code{lavaan} output only.
#' @param pars Positions of the parameters for which the LBCI to be found.
#'              Use the position as appeared on the parameter tables of the fit object.
#' @param ciperc The proportion of coverage for the confidence interval. Default
#'               is .95.
#' @param ... Arguments to be passed to \code{ci_bound_i}.
#' @param parallel If \code{TRUE}, will use parallel.
#' @param ncpu The number of workers, if parallel is TRUE.
#'
#'@examples
#' library(lavaan)
#' data(cfa_two_factors)
#' mod <-
#' "
#' f1 =~ x1 + x2 + a*x3
#' f2 =~ x4 + a*x5 + equal('f1=~x2')*x6
#' f1 ~~ 0*f2
#' asq := a^2
#' "
#' fit <- sem(mod, cfa_two_factors)
#'@export

semlbci <- function(sem_out,
                    pars = NULL,
                    ciperc = .95,
                    ...,
                    parallel = FALSE,
                    ncpu = 2) {
    if (!inherits(sem_out, "lavaan")) {
        stop("sem_out is not a supported object.")
      }
    ptable <- lavaan::parameterTable(sem_out)
    # Do not check for 
    i <- ptable$free > 0
    i_id <- ptable$id[i]
    # pars must be the position as in the lavaan parameterTable.
    if (!is.null(pars)) {
        i_selected <- i_id[pars]
      } else {
        pars <- seq_len(length(i_id))
        i_selected <- i_id
      }
    npar <- length(i_id)
    environment(set_constraint) <- parent.frame()
    f_constr <- set_constraint(sem_out = sem_out, ciperc = ciperc)
    if (parallel) {
        cl <- parallel::makeCluster(ncpu)
        pkgs <- .packages()
        pkgs <- rev(pkgs)
        parallel::clusterExport(cl, "pkgs", envir = environment())
        parallel::clusterEvalQ(cl, {sapply(pkgs, 
                        function(x) library(x, character.only = TRUE))
                      })
        parallel::clusterExport(cl, "f_constr")
        parallel::clusterExport(cl, "npar")
        out <- parallel::parLapply(cl,
                         pars,
                          function(x, ...) tryCatch(
                            semlbci::ci_i(x,
                              npar = npar,
                              sem_out = sem_out,
                              debug = FALSE,
                              f_constr = f_constr),
                              error = function(e) e
                          ),
                          ...
                         )
        parallel::stopCluster(cl)
        out_error <- sapply(out, inherits, what = "error")
        if (any(out_error)) {
            stop("Error occured in parallel mode. Try setting parallel = FALSE")
          }
      } else {
        out <- lapply(pars, ci_i, 
                      npar = npar,
                      sem_out = sem_out,
                      debug = FALSE,
                      f_constr = f_constr,
                      ...)
      }
    out <- do.call(rbind, out)
    out_p <- ptable[, c("id", "lhs", "op", "rhs")]
    out_p$lbci_lb <- NA
    out_p$est <- ptable[, c("est")]
    out_p$lbci_ub <- NA
    out_p[i_selected, "lbci_lb"] <- out[, 1]
    out_p[i_selected, "lbci_ub"] <- out[, 2]
    out_p
  }