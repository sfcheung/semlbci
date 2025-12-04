# semlbci

(Version 0.11.4, updated on 2025-12-04 [release
history](https://sfcheung.github.io/semlbci/news/index.html))

This package includes functions for forming the likelihood-based
confidence intervals (LBCIs) for parameters in structural equation
modeling. It also supports the robust LBCI proposed by [Falk
(2018)](https://doi.org/10.1080/10705511.2017.1367254). It was described
in the following manuscript:

- Cheung, S. F., & Pesigan, I. J. A. (2023). *semlbci*: An R package for
  forming likelihood-based confidence intervals for parameter estimates,
  correlations, indirect effects, and other derived parameters.
  *Structural Equation Modeling: A Multidisciplinary Journal*. *30*(6),
  985–999. <https://doi.org/10.1080/10705511.2023.2183860>

As argued in the article and by others, LBCI is usually better than
Wald-based confidence interval and delta method confidence interval,
which are the default method in most structural equation modeling (SEM)
program. However, there is one technical disadvantage: LBCI cannot be
directly computed but needs to be “found” (searched) by some algorithms.
Wald CIs, on the other hand, can be computed quickly.

In [`semlbci`](https://sfcheung.github.io/semlbci/), we try to address
this disadvantage of LBCI by implementing an efficient method
(illustrated by [Pek & Wu,
2018](https://doi.org/10.1007/s11336-015-9461-1), adapted from [Wu &
Neale, 2012](https://doi.org/10.1007/s10519-012-9560-z)), to help
researchers to form LBCIs for model parameters, including user-defined
parameters, in models fitted by `lavaan`. It can also form LBCIs for the
standardized solution, such as “betas” (standardized regression
coefficients) and correlations, and support multiple-group models. Last,
it supports the robust LBCI proposed by [Falk
(2018)](https://doi.org/10.1080/10705511.2017.1367254) for nonnormal
variables.

More information on this package can be found below:

<https://sfcheung.github.io/semlbci/>

# How To Use It

Illustration with examples can be found in the [*Get Started*
guide](https://sfcheung.github.io/semlbci/articles/semlbci.html)
([`vignette("semlbci", package = "semlbci")`](https://sfcheung.github.io/semlbci/articles/semlbci.md)).

# Installation

The stable CRAN version can be installed by
[`install.packages()`](https://rdrr.io/r/utils/install.packages.html):

``` r
install.packages("semlbci")
```

The latest version at GitHub can be installed by
`remotes::install_github()`:

``` r
remotes::install_github("sfcheung/semlbci")
```

# Implementation

It currently implements the algorithm illustrated by [Pek and Wu
(2018)](https://doi.org/10.1007/s11336-015-9461-1), adapted from [Wu and
Neale (2012)](https://doi.org/10.1007/s10519-012-9560-z) without
adjustment for parameters with attainable bounds. It also supports the
robust LBCI proposed by Falk (2018). More on the implementation can be
found in the [technical
appendices](https://sfcheung.github.io/semlbci/articles/).

# References

Cheung, S. F., & Pesigan, I. J. A. (2023). *semlbci*: An R package for
forming likelihood-based confidence intervals for parameter estimates,
correlations, indirect effects, and other derived parameters.
*Structural Equation Modeling: A Multidisciplinary Journal*. *30*(6),
985–999. <https://doi.org/10.1080/10705511.2023.2183860>

Falk, C. F. (2018). Are robust standard errors the best approach for
interval estimation with nonnormal data in structural equation modeling?
*Structural Equation Modeling: A Multidisciplinary Journal, 25*(2),
244-266. <https://doi.org/10.1080/10705511.2017.1367254>

Pek, J., & Wu, H. (2015). Profile likelihood-based confidence intervals
and regions for structural equation models. *Psychometrika, 80*(4),
1123-1145. <https://doi.org/10.1007/s11336-015-9461-1>

Wu, H., & Neale, M. C. (2012). Adjusted confidence intervals for a
bounded parameter. *Behavior Genetics, 42*(6), 886-898.
<https://doi.org/10.1007/s10519-012-9560-z>

# Issues

If you have any suggestions or found any bugs or limitations, please
feel feel to open a GitHub issue. Thanks.

<https://github.com/sfcheung/semlbci/issues>
