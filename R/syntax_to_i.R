#' @title Parameter Positions From lavaan Syntax
#'
#' @description Converts lavaan syntax to positions in the model
#'   parameter table.
#'
#' @details
#'
#' [syntax_to_i()] converts a vector of strings, in lavaan syntax, to the
#' positions in the parameter table of a [lavaan::lavaan-class] fit object.
#'
#' Each element in the vector should have left hand side (`lhs`),
#' operator (`op`), and/or right hand side (`rhs`). For example:all.x
#'
#' - `"m ~ x"` denotes the coefficient of the path from `x` to `m`.
#'
#' - `"y ~~ x"`  denotes the covariance between `y` and `x`.
#'
#' For user-defined parameters, only `lhs` and `op` will be
#' interpreted. For example:
#'
#' - To specify the user parameter `ab`, both `"ab := ..."` and `"ab :="`
#'   will do, `...` the definition of `ab` in the model. The
#'   right-hand side will be ignored.
#'
#' To denote a labelled parameters, such as `"y ~ a*x"`, treat it as a
#' user-defined parameters and use `:=`, e.g., `"a :="` in this example.
#'
#' For multiple-group models, if a parameter is specified as in a
#' single-group models, then this parameter in all groups will be
#' selected. For example:all.x
#'
#' - If a model has three groups, `"y ~ x"` denotes this path parameter
#' in all three groups, and it will be converted to three row numbers.
#'
#' To select the parameter in a specific group, "multiply" the
#' right-hand-side variable by the group number. For example:
#'
#' - `"y ~ 2*x"` denotes the path coefficient from `x` to `y` in Group 2.
#'
#' To denote the parameters in more than one group, multiply the
#' right-hand side variable by a vector of number. For example:all.x
#'
#' - `"f1 =~ c(2,3)*x2"` denotes the factor loading of `x2` on `f1` in Group 2
#' and Group 3.
#'
#' Elements that cannot be converted to a parameter in the parameter table will
#' be ignored.
#'
#' Currently supports [lavaan::lavaan-class] outputs only.
#'
#' @return A numeric vector of positions (row numbers) in
#' the parameter table.
#'
#' @param syntax A vector of parameters, defined as in lavaan.
#'
#' @param sem_out The SEM output. Currently \code{lavaan} output only.
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
#' @export

syntax_to_i <- function(syntax,
                        sem_out) {
    if (!inherits(sem_out, "lavaan")) {
        stop("sem_out is not a supported object.")
      }
    ptable <- lavaan::parameterTable(sem_out)
    ngroups <- lavaan::lavTech(sem_out, "ngroups")
    l_model <- lavaan::lavParseModelString(syntax, as.data.frame = TRUE)
    if (nrow(l_model) > 0) {
        if (ngroups == 1) {
            # Single-sample
            l_model$req <- TRUE
            p_out <- merge(ptable, l_model[, c("lhs", "op", "rhs", "req")],
                          by = c("lhs", "op", "rhs"), all.x = TRUE, sort = FALSE)
            p_out <- p_out[match(ptable$id, p_out$id), ]
            i_par <- which(p_out$req)
          } else {
            # Multi-sample
            l_model$group <- NA
            tmp0 <- list()
            for (i in seq_len(nrow(l_model))) {
                if (l_model[i, "fixed"] == "") {
                    l_model[i, "group"] <- 1
                    tmp <- do.call(rbind, replicate(ngroups - 1,
                                                    l_model[i, ],
                                                    simplify = FALSE))
                    tmp$group <- seq(2, ngroups)
                    tmp0[[i]] <- tmp
                  } else {
                    x_i <- as.numeric(strsplit(l_model[i, "fixed"], ";")[[1]])
                    x_k <- length(x_i)
                    if (x_k == 1) {
                        l_model[i, "group"] <- x_i
                      } else {
                        l_model[i, "group"] <- x_i[1]
                        tmp <- do.call(rbind, replicate(x_k - 1,
                                                        l_model[i, ],
                                                        simplify = FALSE))
                        tmp$group <- x_i[-1]
                        tmp0[[i]] <- tmp
                      }
                  }
              }
            l_model <- rbind(l_model, do.call(rbind, tmp0))
            l_model$req <- TRUE
            p_out <- merge(ptable,
                           l_model[, c("lhs", "op", "rhs", "group", "req")],
                           by = c("lhs", "op", "rhs", "group"),
                           all.x = TRUE,
                           sort = FALSE)
            p_out <- p_out[match(ptable$id, p_out$id), ]
            i_par <- which(p_out$req)
          }
      } else {
        i_par <- NULL
      }
    # User-defined parameter
    syntax_def <- syntax[grepl(":=", syntax)]
    if (length(syntax_def) > 0) {
        l_def <- strsplit(syntax_def, ":=")
        l_def <- sapply(l_def, function(x) trimws(x[1]))
        i_def <- match(l_def, ptable$label)
      } else {
        i_def <- NULL
      }
    sort(c(i_par, i_def))
  }