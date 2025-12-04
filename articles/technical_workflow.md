# Internal Workflow of semlbci()

## Introduction

This vignette is a simplified version of the [technical
appendix](https://osf.io/q6e3h) hosted at the [OSF project
site](https://osf.io/b9a2p/) of Cheung & Pesigan (2023), associated with
the package [`semlbci`](https://sfcheung.github.io/semlbci/). It
presents the workflow used by
[`ci_i_one()`](https://sfcheung.github.io/semlbci/reference/ci_i_one.md)
and
[`ci_bound_wn_i()`](https://sfcheung.github.io/semlbci/reference/ci_bound_wn_i.md)
to find a bound of a likelihood-based confidence interval (LBCI) of a
parameter in a fitted `lavaan` model.

[`ci_i_one()`](https://sfcheung.github.io/semlbci/reference/ci_i_one.md)
and
[`ci_bound_wn_i()`](https://sfcheung.github.io/semlbci/reference/ci_bound_wn_i.md)
are internal functions used by
[`semlbci()`](https://sfcheung.github.io/semlbci/reference/semlbci.md).
They are not supposed to be used by users. Nevertheless, developers can
use them directly if so desired.

## Purposes of the Functions

[`ci_i_one()`](https://sfcheung.github.io/semlbci/reference/ci_i_one.md)
is called by
[`semlbci()`](https://sfcheung.github.io/semlbci/reference/semlbci.md)
for each confidence bound (limit), twice for each selected parameter,
once for the lower bound and once for the upper bound. It sets up
necessary arguments and then call the lowest level function responsible
for searching a bound.

Currently, only one such function is available,
[`ci_bound_wn_i()`](https://sfcheung.github.io/semlbci/reference/ci_bound_wn_i.md),
which implements the algorithm proposed by Wu & Neale (2012), adapted by
Pek & Wu (2015), without the part for handling parameters with
attainable bounds. To avoid confusion with the method by Wu & Neale
(2012) for parameters with attainable bound, we denote this method WNPW
(Wu-Neale-Pek-Wu) method in this document.

## Overall Workflow

We first present the overall workflow[¹](#fn1) of
[`semlbci()`](https://sfcheung.github.io/semlbci/reference/semlbci.md).
We then present the workflow of
[`ci_i_one()`](https://sfcheung.github.io/semlbci/reference/ci_i_one.md),
called by
[`semlbci()`](https://sfcheung.github.io/semlbci/reference/semlbci.md)
once for each bound of each parameter requested (twice for each
parameter, once for the lower bound and once for the upper bound). Last,
we present the workflows of the two stages of
[`ci_bound_wn_i()`](https://sfcheung.github.io/semlbci/reference/ci_bound_wn_i.md),
the minimization stage and the checking stage.

Details for the arguments mentioned in this section can be found from
the help pages of
[`semlbci()`](https://sfcheung.github.io/semlbci/reference/semlbci.md),
[`ci_i_one()`](https://sfcheung.github.io/semlbci/reference/ci_i_one.md),
and
[`ci_bound_wn_i()`](https://sfcheung.github.io/semlbci/reference/ci_bound_wn_i.md).

### `semlbci()`

[`semlbci()`](https://sfcheung.github.io/semlbci/reference/semlbci.md)
is the main function used by users. It is used to find the LBCI of
selected parameters. It is also responsible for setting the equality
constraint required by the WNPW method. If robust LBCI proposed by Falk
(2018) is requested, it will also compute the scaling and shift factors
to be used by the robust likelihood ratio test by Satorra (2000).

The general workflow of
[`semlbci()`](https://sfcheung.github.io/semlbci/reference/semlbci.md)
is shown below:

General Workflow of
[`semlbci()`](https://sfcheung.github.io/semlbci/reference/semlbci.md)

We describe below the two functions called by
[`semlbci()`](https://sfcheung.github.io/semlbci/reference/semlbci.md)
in the workflow,
[`set_constraint()`](https://sfcheung.github.io/semlbci/reference/set_constraint.md)
and `:scaling_factor3()`.

#### `set_constraint()`

This function, called by
[`semlbci()`](https://sfcheung.github.io/semlbci/reference/semlbci.md),
is used to set the equality constraint used by the WNPW method.

As presented in Pek & Wu (2015), to find the lower bound, $\theta_{L}$,
of the 100(1-$\alpha$)% LBCI of ${\widehat{\theta}}_{j}$, $\theta_{L}$
is minimized with respect to all parameters,
$\left( \theta_{L},{\mathbf{θ}}_{q} \right)$,
${\mathbf{θ}}_{\mathbf{q}}$ being all parameters other than
$\theta_{L}$, subject to the following constraint:

$$0 = 2nF\left( \theta_{L},{\mathbf{θ}}_{\mathbf{q}} \right) - \left( 2nF\left( \widehat{\theta} \right) + \chi_{(1,1 - \alpha)}^{2} \right)$$

$F\left( \widehat{\mathbf{θ}} \right)$ is the value of the discrepancy
function evaluated at the ML estimate $\widehat{\mathbf{θ}}$.
$\chi_{(1,1 - \alpha)}^{2}$ is the $\chi^{2}$ critical value at $df = 1$
and level of significance = $\alpha$ (about 3.84 with $\alpha = .05$),
and $n$ is the sample size (total sample size in multisample models).

This constraint means that model $\chi^{2}$ difference test between the
model with $\theta_{j}$ fixed to $\theta_{L}$ and the original model
with $\theta_{j}$ freely estimated should have a *p*-value equal to
$\alpha$.

In `lavaan`, $F\left( \theta_{L},{\mathbf{θ}}_{q} \right)$ is `"fmin"`
in the output of
[`lavaan::fitMeasures()`](https://rdrr.io/pkg/lavaan/man/fitMeasures.html).
When the estimator is `ML` and `likelihood = "normal"`, the default,
multiplying `"fmin"` by $2n$ yields the model $\chi^{2}$.

To find the upper bound, $\theta_{U}$, $- \theta_{U}$ is minimized
(equivalently, $\theta_{U}$ is maximized). The final value will be
multiplied by $- 1$ to get the upper bound.

The WNPW method can easily be used when a model has one or more equality
constraints on the parameters. These constraints are simply extracted
from the `lavaan` object and passed to
[`nloptr::nloptr()`](https://astamm.github.io/nloptr/reference/nloptr.html),
the function used to do the minimization, along with the constraint by
the WNPW method when minimizing the objective function.

The function
[`set_constraint()`](https://sfcheung.github.io/semlbci/reference/set_constraint.md)
returns a general version of the constraint:

$$0 = 2nF\left( \theta_{L},{\mathbf{θ}}_{q} \right) - \left( 2nF\left( \widehat{\mathbf{θ}} \right) + \frac{\chi_{(1,1 - \alpha)}^{2} - b}{a} \right)$$

In this general form, $a$ is the scaling factor and $b$ is the shift
factor. When $a = 1$ and $b = 0$, it is the constraint in the WNPW
method. When robust LBCI is requested, the scaling and shift factors are
computed as in the same way by
[`lavaan::lavTestLRT()`](https://rdrr.io/pkg/lavaan/man/lavTestLRT.html)
with `method = satorra.2000` and `A.method = "exact"`.[²](#fn2)

#### `semlbci:::scaling_factors3()`

This internal function, not exported, computes the scaling and shift
factors used by
[`lavaan::lavTestLRT()`](https://rdrr.io/pkg/lavaan/man/lavTestLRT.html)
with `method = satorra.2000` and `A.method = "exact"`. This is the
method proposed by Falk (2018) for robust LBCI. These factors can be
used to do the robust likelihood ratio test proposed by Satorra (2000)
without directly calling
[`lavaan::lavTestLRT()`](https://rdrr.io/pkg/lavaan/man/lavTestLRT.html)
when doing the minimization. Because these factors depend only on the
parameter for which the LBCI is to be searched, they can be computed
once and reused during the minimization.

Since version 0.6-13 of `lavaan`,
[`lavaan::lavTestLRT()`](https://rdrr.io/pkg/lavaan/man/lavTestLRT.html)
stores the scaling and shift factors in the output. Therefore, for
future versions of `semlbci`, it is possible to remove
`semlbci:::scaling_factor3()` and use
[`lavaan::lavTestLRT()`](https://rdrr.io/pkg/lavaan/man/lavTestLRT.html)
to get the scaling and shift factors directly.

### `ci_i_one()`

The workflow of
[`ci_i_one()`](https://sfcheung.github.io/semlbci/reference/ci_i_one.md)
is presented below:

General Workflow of
[`ci_i_one()`](https://sfcheung.github.io/semlbci/reference/ci_i_one.md)

Its job is to call
[`ci_bound_wn_i()`](https://sfcheung.github.io/semlbci/reference/ci_bound_wn_i.md)
once or more than once to find the bound of an LBCI of a parameter. It
does not check the validity or plausibility of the bound. The lowest
level function,
[`ci_bound_wn_i()`](https://sfcheung.github.io/semlbci/reference/ci_bound_wn_i.md),
is responsible for doing the checks. If something is wrong,
[`ci_bound_wn_i()`](https://sfcheung.github.io/semlbci/reference/ci_bound_wn_i.md)
will set `status` to a non-zero value.

The argument `try_k_more_times` is used to determine whether it will try
“harder” for *k* more times if the first attempt failed (e.g.,
`status != 0`, meaning that the bound found fails one or more checks).

### `ci_bound_wn_i()`

The function
[`ci_bound_wn_i()`](https://sfcheung.github.io/semlbci/reference/ci_bound_wn_i.md)
is the lowest level function that is responsible for searching the
bound. It implements an optimization method (the WNPW method in this
case) and takes care of all the technical details. It is not supposed to
be used by users and the interface is not designed to be user-friendly.
Nevertheless, interested users can use it directly to find a bound,
bypassing
[`semlbci()`](https://sfcheung.github.io/semlbci/reference/semlbci.md).
Examples can be found in
[`vignette("technical_searching_one_bound", package = "semlbci")`](https://sfcheung.github.io/semlbci/articles/technical_searching_one_bound.md).

The workflow of
[`ci_bound_wn_i()`](https://sfcheung.github.io/semlbci/reference/ci_bound_wn_i.md)
can be separated into two stages, the *minimization stage* and the
*checking stage*.

#### Minimization Stage

The workflow of the minimization stage is presented below:

Workflow of
[`ci_bound_wn_i()`](https://sfcheung.github.io/semlbci/reference/ci_bound_wn_i.md) -
Minimization Stage

##### Optimization Algorithm

The function used to do the minimization is
[`nloptr::nloptr()`](https://astamm.github.io/nloptr/reference/nloptr.html),
using `NLOPT_LD_SLSQP` as the value for `algorithm`. The parameters
(`x0`) are all free parameters in the model. The objective function,
`eval_f` is set to `lbci_b_f()`, the function that returns the bound in
each iteration. The gradient function, `eval_grad_f`, is
`lbci_b_grad()`. The equality constraint(s), `eval_g_eq`, is the result
of
[`set_constraint()`](https://sfcheung.github.io/semlbci/reference/set_constraint.md),
supplied through the argument `f_constr` from
[`semlbci()`](https://sfcheung.github.io/semlbci/reference/semlbci.md),
along with other equality constraints in the model, if any.

The two functions created by
[`ci_bound_wn_i()`](https://sfcheung.github.io/semlbci/reference/ci_bound_wn_i.md)
in this stage, `lbci_b_f()` and `lbci_b_grad()`, are described below.

###### `lbci_b_f()`

In the WNPW method, if the parameter, $\theta_{j}$, is a free parameter,
and the lower bound is to be searched, this function simply returns the
value of $\theta_{j}$ (all other free parameters other than $\theta_{j}$
are denoted by $\theta_{q}$):

$$f\left( \theta_{j},{\mathbf{θ}}_{q} \right) = \theta_{j}$$

If the upper bound is to be searched, then this function is:

$$f\left( \theta_{j},{\mathbf{θ}}_{q} \right) = - \theta_{j}$$

If the parameter is a function of some free parameters (e.g., an
indirect effect, or a standardized coefficient such as correlation),
denoted by $h({\mathbf{θ}})$, $\mathbf{θ}$ being all free parameters
(though some may not be used in $h$), and the lower bound is to be
searched, then this function is:

$$f({\mathbf{θ}}) = h({\mathbf{θ}})$$

Similarly, if the upper bound is to be searched, this function is:

$$f({\mathbf{θ}}) = - h({\mathbf{θ}})$$

Therefore, in the optimization, minimization is conducted whether the
lower or upper bound is to be searched.

###### `lbci_b_grad()`

The gradient of the object function, `lbci_b_f()`, is either precomputed
(if the parameter for which the bound is being searched is a parameter)
or computed by
[`lavaan::lav_func_gradient_complex()`](https://rdrr.io/pkg/lavaan/man/lav_func.html).

##### Tweaking the Optimization

The first attempt of optimization may fail, especially when the target
parameter is a function of free parameters (e.g., an indirect effect, or
a parameter in the standardized solution). The arguments `xtol_rel`,
`ftol_rel`, and `lb` are adjusted across attempts. This is done by
[`ci_i_one()`](https://sfcheung.github.io/semlbci/reference/ci_i_one.md)
for the second and subsequent calls to
[`ci_bound_wn_i()`](https://sfcheung.github.io/semlbci/reference/ci_bound_wn_i.md).

#### Checking Stage

The workflow of the checking stage is presented below:

Workflow of
[`ci_bound_wn_i()`](https://sfcheung.github.io/semlbci/reference/ci_bound_wn_i.md) -
Checking Stage

It will do the following checks, in this order:

1.  Is the status code of
    [`nloptr::nloptr()`](https://astamm.github.io/nloptr/reference/nloptr.html)
    equal to 0 (“success”)?

    If the status code of
    [`nloptr::nloptr()`](https://astamm.github.io/nloptr/reference/nloptr.html)
    is not equal to 0, then the status code for
    [`ci_bound_wn_i()`](https://sfcheung.github.io/semlbci/reference/ci_bound_wn_i.md)
    will be set to 1.

2.  In final solution, do the values of the free parameters result in an
    admissible solution? (Checked by
    [`lavaan::lavInspect()`](https://rdrr.io/pkg/lavaan/man/lavInspect.html)
    with `what = "post.check"`).

    The values of the parameters in the final solution is used to fit
    the model, and then
    [`lavaan::lavInspect()`](https://rdrr.io/pkg/lavaan/man/lavInspect.html)
    is called to check the solution. Examples of inadmissible solution
    are negative variances and correlations greater than one in
    magnitude.

    If the solution is not admissible, then the status code for
    [`ci_bound_wn_i()`](https://sfcheung.github.io/semlbci/reference/ci_bound_wn_i.md)
    will be set to 1.

3.  When the target parameter (free or derived, i.e., the function of
    parameters, such as an indirect effect or a standardized regression
    coefficient) is fixed to the bound found, is the *p*-value of the
    likelihood ratio test between this constrained model and the
    original model equal to 1 - confidence level (.05 for a 95% LBCI)?

    If not, then the likelihood-based confidence bound is by
    *definition* *invalid*. The status code for
    [`ci_bound_wn_i()`](https://sfcheung.github.io/semlbci/reference/ci_bound_wn_i.md)
    will be set to 1.

    The check is equivalent to using
    [`lavaan::lavTestLRT()`](https://rdrr.io/pkg/lavaan/man/lavTestLRT.html).
    If robust LBCI is requested, then the check is equivalent to using
    [`lavaan::lavTestLRT()`](https://rdrr.io/pkg/lavaan/man/lavTestLRT.html)
    with `method = satorra.2000` and `A.method = "exact"`.

The bound is set to `NA` if the solution fails any of the three checks
presented above, to prevent users from accidentally using a bound that
may be invalid.

In sum, the bound returned, if not `NA`, has the following
characteristics:

- Noted as “success” in the optimization by
  [`nloptr::nloptr()`](https://astamm.github.io/nloptr/reference/nloptr.html).

- The values of the parameters do not yield an inadmissible solution.

- The bound is *by definition* valid.

## Main Arguments

This section presents in details the arguments used by
[`ci_i_one()`](https://sfcheung.github.io/semlbci/reference/ci_i_one.md)
and
[`ci_bound_wn_i()`](https://sfcheung.github.io/semlbci/reference/ci_bound_wn_i.md).

### `ci_i_one()`

[`ci_i_one()`](https://sfcheung.github.io/semlbci/reference/ci_i_one.md)
is an interface between user functions such as
[`semlbci()`](https://sfcheung.github.io/semlbci/reference/semlbci.md)
and low level functions such as
[`ci_bound_wn_i()`](https://sfcheung.github.io/semlbci/reference/ci_bound_wn_i.md).
It is responsible for setting necessary values to be used by
[`ci_bound_wn_i()`](https://sfcheung.github.io/semlbci/reference/ci_bound_wn_i.md).

These are the main arguments of
[`ci_i_one()`](https://sfcheung.github.io/semlbci/reference/ci_i_one.md):

- `i`:

  The position (row number) of the target parameters as appeared in the
  parameter table of the output of
  [`lavaan::lavaan()`](https://rdrr.io/pkg/lavaan/man/lavaan.html) and
  its wrappers, such as
  [`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html) and
  [`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html). This
  uniquely identifies a parameter, which can be free, fixed, or
  user-defined.

- `which`:

  Either `"lbound"` or `"ubound"`, denoting lower bound and upper bound,
  respectively. The confidence bound (limit) to be searched.

- `sem_out`:

  A `lavaan`-class object. the output of
  [`lavaan::lavaan()`](https://rdrr.io/pkg/lavaan/man/lavaan.html) and
  its wrappers, such as
  [`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html) and
  [`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html).

- `method`:

  The method to be used to find a confidence bound. Currently, only the
  modified Wu-Neale method presented by Pek and Wu (2015) is supported
  (`"wn"`). Separating the low level function from this function allows
  for the possibility to develop low level functions for other methods,
  without the need to change the interface implemented in
  [`ci_i_one()`](https://sfcheung.github.io/semlbci/reference/ci_i_one.md).

- `standardized`:

  Logical. Whether the confidence bound of the parameter in the
  standardized solution is to be searched. For example, for a
  covariance, whether the covariance, or the correlation, is to be used
  in searching the bound. Default is `FALSE`.

- `robust`:

  Whether robust likelihood-based confidence bound is to be searched,
  and if yes, the method to be used. Currently only support `"none"`
  (robust method not used) or `"satorra.2000"`, proposed by Falk (2018).

- `sf_full`:

  Used when `robust` is `"satorra.2000"`. If `NA`, the scaling and shift
  factors used in the likelihood ratio test will be computed internally.
  If supplied, it should be a list with two scalar elements, `c_r` and
  `c_rb`, the scaling factor and the shift factors.

- `sf_args`:

  If `robust` is `"satorra.2000"` and `sf_full` is `NA`, this is a named
  list of arguments to be passed to `semlbci:::scaling_factor3()`, an
  internal function for computing the scaling and shift factors proposed
  by Asparouhov & Muthén (2010).

- `try_k_more_times`:

  How many more times to try if the status code is not zero. Default is
  0 but
  [`semlbci()`](https://sfcheung.github.io/semlbci/reference/semlbci.md)
  set this argument to 2 when calling this function. If set to an
  integer greater than zero, it will call the low level function until
  the status code is zero or until this number of additional calls have
  been attempted. In each successive call, some values will be modified
  to do the search using these new settings.

### `ci_bound_wn_i()`

[`ci_bound_wn_i()`](https://sfcheung.github.io/semlbci/reference/ci_bound_wn_i.md)
is the low level function called by
[`ci_i_one()`](https://sfcheung.github.io/semlbci/reference/ci_i_one.md).
This function implements the modified Wu-Neale method presented by Pek &
Wu (2015), named Wu-Neale-Pek-Wu (WNPW) method in this document. This
function is not supposed to be used by users and the interface is not
user-friendly. Interested users can refer to
[`vignette("technical_searching_one_bound", package = "semlbci")`](https://sfcheung.github.io/semlbci/articles/technical_searching_one_bound.md)
to see how to use
[`ci_bound_wn_i()`](https://sfcheung.github.io/semlbci/reference/ci_bound_wn_i.md)
directly.

These are the main arguments of
[`ci_bound_wn_i()`](https://sfcheung.github.io/semlbci/reference/ci_bound_wn_i.md):

- `i`:

  The position (row number) of the target parameters as appeared in the
  parameter table of the output of
  [`lavaan::lavaan()`](https://rdrr.io/pkg/lavaan/man/lavaan.html) and
  its wrappers, such as
  [`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html) and
  [`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html). This id
  uniquely identifies a parameter, which can be free, fixed, or
  user-defined.

- `npar`:

  The number of free parameters in the model, including those
  constrained to be equal. To be supplied by
  [`ci_i_one()`](https://sfcheung.github.io/semlbci/reference/ci_i_one.md).

- `sem_out`:

  A `lavaan`-class object. the output of
  [`lavaan::lavaan()`](https://rdrr.io/pkg/lavaan/man/lavaan.html) and
  its wrappers, such as
  [`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html) and
  [`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html).

- `f_constr`:

  The constraint function generated by
  [`set_constraint()`](https://sfcheung.github.io/semlbci/reference/set_constraint.md).
  Created by
  [`semlbci()`](https://sfcheung.github.io/semlbci/reference/semlbci.md)
  and then passed to it through
  [`ci_i_one()`](https://sfcheung.github.io/semlbci/reference/ci_i_one.md).

- `which`:

  Either `"lbound"` or `"ubound"`, denoting lower bound and upper bound,
  respectively. The confidence bound (limit) to be searched.

- `perturbation_factor`:

  The number by which the parameter estimates in `sem_out` will be
  multiplied, to set the starting values, because using the parameter
  estimates as starting values may lead to errors in the first few
  iterations. Default is .90. This argument is ignored if
  `wald_ci_start`, described below, is `TRUE`.

- `lb_var`:

  The lower bound for free parameters that are variances. If equal to
  `-Inf`, the default, `lb_prop` and `lb_se_k`, described below, will be
  used to set the lower bounds for free variances. If it is a number, it
  will be used to set the lower bounds for all free variances.

- `wald_ci_start`:

  If `TRUE`, there are no equality constraints in the model, and the
  target parameter is not a user-defined parameter, the Wald or delta
  confidence bounds will be used as the starting values.

- `standardized`:

  Logical. Whether the confidence bound of the parameter in the
  standardized solution is to be searched. For example, for a
  covariance, whether the covariance, or the correlation, is to be used
  in searching the bound. Default is `FALSE`.

- `opts`:

  A named list of options to be passed to
  [`nloptr::nloptr()`](https://astamm.github.io/nloptr/reference/nloptr.html),
  the function used for the optimization. Default is
  [`list()`](https://rdrr.io/r/base/list.html). This argument can be
  used to override internal settings used by
  [`ci_bound_wn_i()`](https://sfcheung.github.io/semlbci/reference/ci_bound_wn_i.md).

- `ciperc`:

  The intended coverage probability for the confidence interval. Default
  is .95, and the bound for a 95% likelihood-based confidence interval
  will be sought.

- `ci_limit_ratio_tol`:

  The tolerance for the ratio of `a` to `b`, where `a` is the distance
  between an bound of an LBCI and the point estimate, and the `b` is the
  distance between the original confidence bound (by default the Wald or
  delta CI in
  [`lavaan::lavaan()`](https://rdrr.io/pkg/lavaan/man/lavaan.html)) and
  the point estimate. If the ratio is larger than this value or smaller
  than the reciprocal of this value, a warning is set in the status
  code. Default is 1.5.

- `verbose`:

  If `TRUE`, the function will store more diagnostic information in the
  attribute `diag`. Default is `FALSE`.

- `sf`:

  A scaling factor. Used for robust confidence bounds. Default is 1.
  Precomputed by an internal function called by
  [`semlbci()`](https://sfcheung.github.io/semlbci/reference/semlbci.md)
  or
  [`ci_i_one()`](https://sfcheung.github.io/semlbci/reference/ci_i_one.md)
  when `robust = "satorra.2000"`.

- `sf2`:

  A shift factor. Used for robust confidence bounds. Default is 1.
  Precomputed by an internal function called by
  [`semlbci()`](https://sfcheung.github.io/semlbci/reference/semlbci.md)
  or
  [`ci_i_one()`](https://sfcheung.github.io/semlbci/reference/ci_i_one.md)
  when `robust = "satorra.2000"`.

- `p_tol`:

  The tolerance for checking the achieved level of confidence, that is,
  the *p*-value of the likelihood ratio test between the original model
  and the model with the parameter fixed to the bound found. If the
  absolute difference between the achieved level and `ciperc` is greater
  than this number, a warning is set in the status code and the bound is
  set to `NA`. Default is 5e-4.

- `xtol_rel_factor`:

  Multiply the internal default value of `xtol_rel` for
  [`nloptr::nloptr()`](https://astamm.github.io/nloptr/reference/nloptr.html)
  (1.0e-5) by this number, usually a positive number equal to or less
  than 1, to change the default termination criterion. Default is 1.
  This allows tweaking the settings for optimization without knowing the
  internal default value.

- `ftol_rel_factor`:

  Multiply the internal default value of `ftol_rel` for
  [`nloptr::nloptr()`](https://astamm.github.io/nloptr/reference/nloptr.html)
  (1.0e-5) by this number, usually a positive number equal to or less
  than 1, to change the default termination criterion. Default is 1.
  This allows tweaking the settings for optimization without knowing the
  internal default value.

- `lb_prop`:

  Used by an internal function to set the lower bound for free
  variances. Default is .05, setting the lower bound to (.05)(point
  estimate). Used only if the lower bound set by `lb_se_k` is negative.
  This constraint is used only in the optimization to prevent
  intermediate values too far away from the point estimates. The final
  check done by fitting the model in `lavaan` will not implement this
  constraint.

- `lb_se_k`

  Used by an internal function to set the lower bound for free
  variances. Default is 3, the estimate minus 3 $\times$ standard error.
  If negative, the lower bound is set using `lb_prop`. This constraint
  is used only in the optimization to prevent intermediate values too
  far away from the point estimates. The final check done by fitting the
  model in `lavaan` will not implement this constraint.

Please refer to the help page of
[`ci_bound_wn_i()`](https://sfcheung.github.io/semlbci/reference/ci_bound_wn_i.md)
to learn about other arguments.

## References

Asparouhov, T., & Muthén, B. O. (2010). *Simple second order chi-square
correction*. <https://www.statmodel.com/download/WLSMV_new_chi21.pdf>

Cheung, S. F., & Pesigan, I. J. A. (2023). *Semlbci*: An r package for
forming likelihood-based confidence intervals for parameter estimates,
correlations, indirect effects, and other derived parameters.
*Structural Equation Modeling: A Multidisciplinary Journal*, *30*(6),
985–999. <https://doi.org/10.1080/10705511.2023.2183860>

Falk, C. F. (2018). Are robust standard errors the best approach for
interval estimation with nonnormal data in structural equation modeling?
*Structural Equation Modeling: A Multidisciplinary Journal*, *25*(2),
244–266. <https://doi.org/10.1080/10705511.2017.1367254>

Pek, J., & Wu, H. (2015). Profile likelihood-based confidence intervals
and regions for structural equation models. *Psychometrika*, *80*(4),
1123–1145. <https://doi.org/10.1007/s11336-015-9461-1>

Satorra, A. (2000). Scaled and adjusted restricted tests in multi sample
analysis of moment structures. In R. D. H. Heijmans, D. S. G. Pollock, &
A. Satorra (Eds.), *Innovations in multivariate statistical analysis.
Advanced studies in theoretical and applied econometrics* (Vol. 36).
Springer.

Wu, H., & Neale, M. C. (2012). Adjusted confidence intervals for a
bounded parameter. *Behavior Genetics*, *42*(6), 886–898.
<https://doi.org/10.1007/s10519-012-9560-z>

------------------------------------------------------------------------

1.  Some steps are omitted for readability.

2.  Internally, `sf = a^(-1)` is stored. To be consistent with `T_3`
    presented in Asparouhov & Muthén (2010), we used `a` instead of
    `a^(-1)` in the equation in this document.
