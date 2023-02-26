<!-- badges: start -->
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Code size](https://img.shields.io/github/languages/code-size/sfcheung/semlbci.svg)](https://github.com/sfcheung/semlbci)
[![Last Commit at Main](https://img.shields.io/github/last-commit/sfcheung/semlbci.svg)](https://github.com/sfcheung/semlbci/commits/master)
[![R-CMD-check](https://github.com/sfcheung/semlbci/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sfcheung/semlbci/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

(Version 0.10.0.16, updated on 2023-02-26, [release history](https://sfcheung.github.io/semlbci/news/index.html))

# semlbci

This package includes functions for forming the
likelihood-based confidence intervals (LBCIs) for parameters
in structural equation modeling. It also supports the robust LBCI proposed
by Falk (2018). It was described in the following manuscript:

Cheung, S. F., & Pesigan, I. J. A. (Forthcoming). An R package
for forming likelihood-based confidence intervals for parameter
estimates, correlations, indirect Effects, and other derived
parameters. *Structural Equation Modeling: A Multidisciplinary Journal*.
Accepted for publication.

More information on this package:

https://sfcheung.github.io/semlbci/

# Installation

The latest version can be installed by `remotes::install_github`:

```r
remotes::install_github("sfcheung/semlbci")
```

# Implementation

It currently implements the
algorithm illustrated by Pek and Wu (2018), adapted from Wu
and Neale (2012) without adjustment for parameters with
attainable bounds. It also supports the robust LBCI proposed
by Falk (2018). More on the implementation can be found in
the technical appendices in [the OSF page](https://osf.io/b9a2p/files/osfstorage), in the folder `technical_appendices`.

# References

Falk, C. F. (2018). Are robust standard errors the best approach
for interval estimation with nonnormal data in structural equation
modeling? *Structural Equation Modeling: A Multidisciplinary
Journal, 25*(2), 244-266.
https://doi.org/10.1080/10705511.2017.1367254

Pek, J., & Wu, H. (2015). Profile likelihood-based confidence
intervals and regions for structural equation models.
*Psychometrika, 80*(4), 1123-1145.
https://doi.org/10.1007/s11336-015-9461-1

Wu, H., & Neale, M. C. (2012). Adjusted confidence intervals for a
bounded parameter. *Behavior Genetics, 42*(6), 886-898.
https://doi.org/10.1007/s10519-012-9560-z}
