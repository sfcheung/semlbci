% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_sem_out.R
\name{check_sem_out}
\alias{check_sem_out}
\title{Pre-analysis Check For 'semlbci'}
\usage{
check_sem_out(
  sem_out,
  robust = c("none", "satorra.2000"),
  multigroup_ok = TRUE
)
}
\arguments{
\item{sem_out}{The output from an SEM analysis. Currently only
supports a \link[lavaan:lavaan-class]{lavaan::lavaan} object.}

\item{robust}{Whether the LBCI based on robust likelihood ratio
test is to be found. Only "satorra.2000" in \code{\link[lavaan:lavTestLRT]{lavaan::lavTestLRT()}}
is supported for now. If \code{"none"}, the default, then likelihood
ratio test based on maximum likelihood estimation will be used.}

\item{multigroup_ok}{If \code{TRUE}, will not check whether the model is a
multiple-group model. Default is \code{TRUE}.}
}
\value{
A numeric vector of one element. If 0, the model and
estimation method are officially supported. If larger than zero,
then the model and method are not officially supported but users
can still try to use \code{\link[=semlbci]{semlbci()}} on it at their own risks. If less
than zero, then the model and/or the method are officially not
supported.

The attributes \code{info} contains the reason for a value other than
zero.
}
\description{
Check the output passed to \code{\link[=semlbci]{semlbci()}}
}
\details{
It checks whether the model and the estimation method in
the \code{sem_out} object passed to \code{\link[=semlbci]{semlbci()}} are supported by the
current version of \code{\link[=semlbci]{semlbci()}}. This function is to be used by
\code{\link[=semlbci]{semlbci()}} but is exported such that the compatibility of an SEM
output can be checked directly.

Estimation methods (\code{estimator} in \code{\link[lavaan:lavaan]{lavaan::lavaan()}}) currently
supported:
\itemize{
\item Maximum likelihood (\code{ML}) and its variants (e.g., \code{MLM}, \code{MLR}).
For methods with robust test statistics (e.g., \code{MLR}),
only robust LBCIs (\code{robust = "satorra.2000"} in calling \code{\link[=semlbci]{semlbci()}})
can be requested.
}

Estimation methods not yet supported:
\itemize{
\item Generalized least squares (\code{GLS}).
\item Weighted least squares (a.k.a. asymptotically distribution
free) (\code{WLS}) and its variants (e.g., \code{WLSMV}).
\item Unweighted least squares (\code{ULS}).
\item Diagonally weighted least squares (\code{DWLS}).
\item Other methods not listed.
}

Models supported:
\itemize{
\item Single-group models with continuous variables.
\item Multiple-group models with continuous variables.
}

Models not tested:
\itemize{
\item Models with categorical variables.
}

Models not yet supported:
\itemize{
\item Models with formative factors.
\item Multilevel models.
}
}
\examples{
library(lavaan)
data(cfa_two_factors)
mod <-
"
f1 =~ x1 + x2 + x3
f2 =~ x4 + x5 + x6
"

fit <- sem(mod, cfa_two_factors)

# Should be 0
check_sem_out(fit)

fit2 <- sem(mod, cfa_two_factors, estimator = "DWLS")

# Should be negative because DWLS is officially not supported
check_sem_out(fit2)

fit3 <- sem(mod, cfa_two_factors, estimator = "MLR")

# Should be negative because MLR is supported only if
# robust is set to "satorra.2000"
check_sem_out(fit3)

# Should be zero because robust is set to "satorra.2000"
check_sem_out(fit3, robust = "satorra.2000")

}
\seealso{
\code{\link[=semlbci]{semlbci()}}, \code{\link[=ci_i_one]{ci_i_one()}}
}
