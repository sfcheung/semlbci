% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data3.R
\docType{data}
\name{mediation_latent_skewed}
\alias{mediation_latent_skewed}
\title{Dataset (SEM, Three Factors, Nine Variables, Mediation, Skewed)}
\format{
A data frame with 150 rows and nine variables:
\describe{
\item{x1}{x1}
\item{x2}{x2}
\item{x3}{x3}
\item{x4}{x4}
\item{x5}{x5}
\item{x6}{x6}
\item{x7}{x7}
\item{x8}{x8}
\item{x9}{x9}
}
}
\usage{
mediation_latent_skewed
}
\description{
Generated from a three-factor model with nine variables, n = 150,
with some observed variables positively skewed.
}
\details{
This model is used for examples like this one:

\if{html}{\out{<div class="sourceCode">}}\preformatted{mod <-
"
fx =~ x1 + x2 + x3
fm =~ x4 + x5 + x6
fy =~ x7 + x8 + x9
fm ~ a*fx
fy ~ b*fm + cp*fx
ab := a*b
"
fit <- lavaan::sem(mod, mediation_latent)
}\if{html}{\out{</div>}}
}
\examples{

print(head(mediation_latent_skewed), digits = 3)
nrow(mediation_latent_skewed)



}
\keyword{datasets}
