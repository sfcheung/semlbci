# semlbci 0.0.0.10

- Remove the function for the Neale-Miller-1992 approach. This approach is not
  supported anymore. The default approach is now the Wu-Neale-2012 approach
  (`method = "wn"`).

- Added initial support for likelihood-based confidence interval using 
  robust likelihood ratio test (Falk, XXXX).

- Added tests for confirmatory factor analysis (CFA) models and structural
  models with latent factors.

- Added initial support for using parallel processing to search the limits
  for several parameters.

- Write print generic functions for the class `cibound`, returned by 
  `ci_bound_wn_i()`, and the class `semblci`, returned by `semlbci()`. 

- `ci_bound_wn_i()` now returns a list instead of a one-element numeric vector.

# semlbci 0.0.0.9

- Updated tests due to a change in OpenMx 2.19.x.

# semlbci 0.0.0.8

- Updated the vignette.

# semlbci 0.0.0.7

- Added diagnostic info to the output.

# semlbci 0.0.0.6

* Added functions to implement the Neale-Miller 1992 approach. Preliminary tests passed. Can be invoked by `method = "nm"`.

# semlbci 0.0.0.4

* Can find the LBCI for the free and user-defined parameters in the standardized solution.

* Starting values based on Wald CIs are uesd also for standardized solution.

# semlbci 0.0.0.3

* Can find the LBCI for free parameters and user-defined parameters.

* Work for models with equality constraints.

* Can specify parameters by lavaan model syntax.

* Can use Wald CI to set the starting values. The optimization is usually faster.

# semlbci 0.0.0.1

* A first rough draft of the functions.
