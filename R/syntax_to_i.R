#' @title Parameter Positions From lavaan Syntax
#'
#' @description Converts lavaan syntax to positions in the fit object
#'  parameter table
#'
#' @details
#'
#' [syntax_to_i()] converts a vector of strings, in lavaan syntax, to the
#' positions in the parameter table of a [lavaan::lavaan-class] fit object.
#'
#' Each element in the vector should have left hand side (`lhs`),
#' operator (`op`), and/or right hand side (`rhs`). For example, "m ~ x"
#' denotes the coefficient of the path from `x` to `m`. "y ~~ x"
#' denotes the covariance between `y` and `x`.
#'
#' For user-defined parameters, only `lhs` and `op` will be
#' interpreted. For example, to specify the user parameter `ab`, "ab
#' := x" will do. The right hand side will be ignored.
#'
#' To denote a labelled parameters, e.g., "y ~ a*x", treat it as a user-defined
#' parameters us use `:=`, e.g., "a :=" in this example.
#'
#' For multiple-group models, if a parameter is specified as in a single-group
#' models, then this parameter in all groups will be selected. For example,
#' if a model has three groups, "y ~ x" denotes this path parameter in all
#' three groups, and it will be converted to three row numbers. To select
#' the parameter in a specific group, label the parameter and select it using
#' `:=` as described above.
#'
#' Elements that cannot be converted to a parameter in the parameter table will
#' be ignored.
#'
#' Currently supports [lavaan::lavaan-class] outputs only.
#'
#' @return A vector of positions in the parameter table.
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
    l_model <- lavaan::lavParseModelString(syntax, as.data.frame = TRUE)
    if (nrow(l_model) > 0) {
        l_model$req <- TRUE
        p_out <- merge(ptable, l_model[, c("lhs", "op", "rhs", "req")], 
                      by = c("lhs", "op", "rhs"), all.x = TRUE, sort = FALSE)
        p_out <- p_out[match(ptable$id, p_out$id), ]
        i_par <- which(p_out$req)
      } else {
        i_par <- NULL
      }
    # User defined parameter
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