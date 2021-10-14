#' @title Scaling Factor in Satorra-2000 Test (Adjusted for Mean and Variance)
#'
#' @description Finds the scaling factor used in the satorra-2000 test.
#'
#' @return The scaling factor
#'
#' @param sem_out The source fit object.
#'
#' @param i The position of the parameter as appeared in the parameter
#'  table.
#'
#' @param standardized If `TRUE`, the limit to be found is for the
#'  standardized solution. Default is `FALSE`.
#'
#' @param pertubation_factor A factor to modify the original estimate.
#'  Default is .98.
#'
#' @param update_args The list of additional arguments to be passed to
#'  the update method of a lavaan class object. Default is `list()`.
#'
#' @param force_converged Whether the constrained model will be forced
#'   to have converged, without update. Default is `TRUE`.
#'
#' @param std_method The method used to find the standardized solution.
#'  If equal to `"lavaan"``, [lavaan::standardizedSolution()] will be used.
#'  If equal to `"internal"`, an internal function of this package will be used.
#'  The `"lavaan"` method should work in all situations, but the `"internal"`
#'  method can be faster. Default is `"lavaan"` for now, but may be changed to
#'  `"internal"` if it is confirmed to work in all situations tested.
#'
#' @param debug Print debug information. Default is `FALSE`.
#'
#' @noRd

scaling_factor2 <- function(sem_out,
                           i,
                           standardized = FALSE,
                           pertubation_factor = .98,
                           update_args = list(),
                           force_converged = TRUE,
                           std_method = "lavaan",
                           debug = FALSE
                           ) {
    sem_out_name <- deparse(substitute(sem_out))
    # This function will NOT check whether the SEM was done with robust model
    # test. This check should be done before calling this function.
    p_table <- lavaan::parameterTable(sem_out)
    npar <- sum(p_table$free > 0)
    i_op <- p_table[i, "op"]
    i_lor <- get_lhs_op_rhs(i, sem_out, more = TRUE)
    i_labelled <- nchar(p_table[i, "label"]) > 0

    # Get original point estiamte and CI
    if (standardized) {
        p_est <- lavaan::standardizedSolution(sem_out,
                    type = "std.all",
                    se = TRUE,
                    zstat = FALSE,
                    pvalue = FALSE,
                    ci = TRUE,
                    remove.eq = FALSE,
                    remove.ineq = FALSE,
                    remove.def = FALSE,
                    output = "data.frame")
        i_est <- p_est[i, "est.std"]
        i_se <- p_est[i, "se"]
        i_org_ci_lower <- p_est[i, "ci.lower"]
        i_org_ci_upper <- p_est[i, "ci.upper"]
      } else {
        p_est <- lavaan::parameterEstimates(sem_out,
                                            se = TRUE,
                                            zstat = FALSE,
                                            fmi = FALSE,
                                            rsquare = TRUE,
                                            output = "data.frame")
        i_est <- p_est[i, "est"]
        i_se <- p_est[i, "se"]
        i_org_ci_lower <- p_est[i, "ci.lower"]
        i_org_ci_upper <- p_est[i, "ci.upper"]
      }

    p_table_fit <- p_table

    if (standardized) {

        # Standardized. User defined or free does not matter.

        i_label <- gen_unique_name(get_names_from_ptable(p_table_fit))

        if (FALSE) {
        time_start <- Sys.time()
        fit00 <- lavaan::update(sem_out,
                               model = sem_out@ParTable,
                               add = paste0(i_label, " := ",
                                            "semlbci::get_std('",
                                            sem_out_name, "',", i,
                                            ", std_method = ",
                                            sQuote(std_method), 
                                            ")"),
                               do.fit = FALSE,
                               baseline = FALSE,
                               h1 = FALSE,
                               se = "none",
                               test = "satorra.bentler")
        time_spent <- Sys.time() - time_start
        if (debug) {
            cat("\nfit00 update with get_std: \n",
                time_spent, "\n")
          }
        # p_tmp <- lavaan::parameterTable(fit00)
        time_start <- Sys.time()
        fit0 <- lavaan::update(fit00, model = fit00@ParTable,
                               add = paste0(i_label, " == ",
                                            i_est * pertubation_factor),
                               do.fit = FALSE)
        time_spent <- Sys.time() - time_start
        if (debug) {
            cat("\nfit0 update with eq con: \n",
                time_spent, "\n")
          }
        # p_tmp <- lavaan::parameterTable(fit0)
        time_start <- Sys.time()
        fit0 <- lavaan::update(fit0, model = fit0@ParTable,
                               add = "0 < 1",
                               do.fit = FALSE)
        time_spent <- Sys.time() - time_start
        if (debug) {
            cat("\nfit0 update with 0 < 1: \n",
                time_spent, "\n")
          }
        p_table0 <- lavaan::parameterTable(fit0)
        i_std <- which(p_table0$lhs == i_label & p_table0$op == ":=")
        i_constr <- which(p_table0$lhs == i_label & p_table0$op == "==")
        i_extra <- which(p_table0$lhs == "0" & p_table0$rhs == "1")
        # Not sure why we have to manually set this constraint to free
        p_table0[i_std, "free"] <- 0
        p_table0[i_constr, "free"] <- 0
        p_table0[i_extra, "free"] <- 0
        p_table0[p_table0$free > 0, "start"] <-
                                          p_table[p_table0$free > 0, "est"]
        p_table0[p_table0$free > 0, "est"] <-
                                          p_table[p_table0$free > 0, "est"]
        }

        i_syn <- paste0("semlbci::get_std('",
                        sem_out_name, "',", i,
                        ", std_method = ",
                        sQuote(std_method),
                        ")")
        p_table0 <- sem_out@ParTable
        prow <- length(p_table0$id)
        p_table0$id <- c(p_table0$id, prow + 1:3)
        p_table0$block <- c(p_table0$block, rep(0, 3))
        p_table0$user <- c(p_table0$user, rep(1, 3))
        p_table0$free <- c(p_table0$free, rep(0, 3))
        p_table0$ustart <- c(p_table0$ustart, rep(0, 3))
        p_table0$exo <- c(p_table0$exo, rep(0, 3))
        p_table0$label <- c(p_table0$label, i_label, "", "")
        p_table0$plabel <- c(p_table0$plabel, rep("", 3))
        if (!is.null(p_table0$group)) {
            p_table0$group <- c(p_table0$group, rep(0, 3))
          }
        if (!is.null(p_table0$level)) {
            p_table0$level <- c(p_table0$level, rep(0, 3))
          }
        p_table0$start <- c(p_table0$start, rep(0, 3))
        p_table0$est <- c(p_table0$est, rep(0, 3))
        p_table0$se <- c(p_table0$se, rep(NA, 3))
        p_table0$lhs <- c(p_table0$lhs, i_label, i_label, "0")
        p_table0$op <- c(p_table0$op, ":=", "==", "<")
        p_table0$rhs <- c(p_table0$rhs, i_syn,
                                        as.character(i_est * pertubation_factor),
                                        "1")
        p_table0$start[p_table0$free > 0] <- p_table[p_table$free > 0, "est"]
        p_table0$est[p_table0$free > 0] <- p_table[p_table$free > 0, "est"]

        time_start <- Sys.time()
        fit0 <- lavaan::update(sem_out,
                               model = p_table0,
                               do.fit = FALSE,
                               baseline = FALSE,
                               h1 = FALSE,
                               se = "none",
                               test = "satorra.bentler")
        time_spent <- Sys.time() - time_start
        if (debug) {
            cat("\nfit00 update customized ptable: \n",
                time_spent, "\n")
          }

        if (force_converged) {
            time_start <- Sys.time()
            fit1 <- suppressWarnings(
                        lavaan::update(fit0, model = p_table0, start = p_table0, do.fit = FALSE,
                              baseline = FALSE, h1 = FALSE, se = "none",
                              optim.force.converged = TRUE)
                      )
            time_spent <- Sys.time() - time_start
            if (debug) {
                cat("\nfit1 update: \n",
                    time_spent, "\n")
              }
            fit1@test[[1]]$stat <- sem_out@test[[1]]$stat * 10
            fit1@test[[1]]$df <- sem_out@test[[1]]$df + 1
            time_start <- Sys.time()

            fit2 <- fit1
            fit2@test[[1]]$stat <- sem_out@test[[1]]$stat * 25
            fit2@test[[1]]$df <- sem_out@test[[1]]$df + 1
          } else {
            # To-Fix: This block does not work
            update_args0 <- list(object = fit0,
                                model = p_table0,
                                do.fit = TRUE,
                                optim.force.converged = TRUE)
            update_args1 <- utils::modifyList(update_args0,
                                              update_args)
            fit1 <- do.call(lavaan::update, update_args1)
            update_args0b <- list(object = fit0,
                                model = p_table0b,
                                do.fit = TRUE,
                                optim.force.converged = TRUE)
            update_args1b <- utils::modifyList(update_args0b,
                                              update_args)
            fit2 <- do.call(lavaan::update, update_args1b)
          }
      } else {

        # Unstandardized. User defined or not does not matter
        if (!i_labelled) {
            i_label <- gen_unique_name(get_names_from_ptable(p_table_fit))
            p_table_fit[i, "label"] <- i_label
          } else {
            i_label <- p_table_fit[i, "label"]
          }

        fit0 <- lavaan::update(sem_out, model = p_table_fit,
                              add = paste0(i_label, " == ",
                                            i_est * pertubation_factor),
                              do.fit = FALSE,
                              baseline = FALSE,
                              h1 = FALSE,
                              se = "none")
        p_table0 <- lavaan::parameterTable(fit0)
        # # Not sure why we have to manually set this constraint to fixed
        p_table0[p_table0$lhs == i_label, "free"] <- 0

        if (force_converged) {
            fit1 <- suppressWarnings(
                        lavaan::update(fit0, model = p_table0, start = p_table0, do.fit = FALSE,
                              baseline = FALSE, h1 = FALSE, se = "none",
                              optim.force.converged = TRUE)
                      )
            fit1@test[[1]]$stat <- sem_out@test[[1]]$stat * 10
            fit1@test[[1]]$df <- sem_out@test[[1]]$df + 1

            fit2 <- fit1
            fit2@test[[1]]$stat <- sem_out@test[[1]]$stat * 25
            fit2@test[[1]]$df <- sem_out@test[[1]]$df + 1
          } else {
            # To-Fix: This block does not work
            update_args0 <- list(object = fit0,
                                model = p_table0,
                                do.fit = TRUE,
                                optim.force.converged = TRUE)
            update_args1 <- utils::modifyList(update_args0,
                                              update_args)
            fit1 <- do.call(lavaan::update, update_args1)
            update_args0b <- list(object = fit0b,
                                model = p_table0b,
                                do.fit = TRUE,
                                optim.force.converged = TRUE)
            update_args1b <- utils::modifyList(update_args0b,
                                              update_args)
            fit2 <- do.call(lavaan::update, update_args1b)
          }
      }
    # Do the LR test
    lrt_out1 <- lavaan::lavTestLRT(sem_out,
                                  fit1,
                                  method = "satorra.2000",
                                  A.method = "exact")
    lrt_out2 <- lavaan::lavTestLRT(sem_out,
                                  fit2,
                                  method = "satorra.2000",
                                  A.method = "exact")
    chisq_1 <- lrt_out1["fit1", "Chisq"]
    chisq_2 <- lrt_out2["fit2", "Chisq"]
    chisq_0 <- lrt_out1["sem_out", "Chisq"]
    chisq_diff_c_1 <- chisq_1 - chisq_0
    chisq_diff_c_2 <- chisq_2 - chisq_0
    chisq_diff_p_1 <- stats::qchisq(lrt_out1[2, "Pr(>Chisq)"],
                                  1,
                                  lower.tail = FALSE)
    chisq_diff_p_2 <- stats::qchisq(lrt_out2[2, "Pr(>Chisq)"],
                                  1,
                                  lower.tail = FALSE)
    chisq_diff_r_1 <- lrt_out1["fit1", "Chisq diff"]
    chisq_diff_r_2 <- lrt_out2["fit2", "Chisq diff"]
    c_p  <- (chisq_2 - chisq_1) / (chisq_diff_p_2 - chisq_diff_p_1)
    c_pb <- chisq_diff_p_1 - (chisq_1 - chisq_0) / c_p
    c_r  <- (chisq_2 - chisq_1) / (chisq_diff_r_2 - chisq_diff_r_1)
    c_rb <- chisq_diff_r_1 - (chisq_1 - chisq_0) / c_r
    out <-
      data.frame(
        chisq_2 = chisq_2,
        chisq_1 = chisq_1,
        chisq_0 = chisq_0,
        chisq_diff_c_1 = chisq_diff_c_1,
        chisq_diff_c_2 = chisq_diff_c_2,
        chisq_diff_r_1 = chisq_diff_r_1,
        chisq_diff_r_2 = chisq_diff_r_2,
        chisq_diff_p_1 = chisq_diff_p_1,
        chisq_diff_p_2 = chisq_diff_p_2,
        c_p = c_p,
        c_pb = c_pb,
        c_r = c_r,
        c_rb = c_rb)

    return(out)
  }

