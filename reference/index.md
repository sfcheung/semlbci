# Package index

## Forming LBCIs

Form LBCIs for selected parameters in a model fitted by
\[lavaan::lavaan()\] or related functions such as \[lavaan::sem()\] and
\[lavaan::cfa()\].

- [`semlbci()`](https://sfcheung.github.io/semlbci/reference/semlbci.md)
  : Likelihood-Based Confidence Interval

## Helper Functions

- [`check_sem_out()`](https://sfcheung.github.io/semlbci/reference/check_sem_out.md)
  : Pre-analysis Check For 'semlbci'

- [`get_cibound()`](https://sfcheung.github.io/semlbci/reference/get_cibound.md)
  [`get_cibound_status_not_0()`](https://sfcheung.github.io/semlbci/reference/get_cibound.md)
  : A 'cibound' Output From a 'semlbci' Object

- [`nearby_levels()`](https://sfcheung.github.io/semlbci/reference/nearby_levels.md)
  : LBCI Bounds of Nearby Levels of Confidence

- [`ci_order()`](https://sfcheung.github.io/semlbci/reference/ci_order.md)
  [`print(`*`<ci_order>`*`)`](https://sfcheung.github.io/semlbci/reference/ci_order.md)
  :

  Check The Order of Bounds in a List of `semlbci` Objects

- [`syntax_to_i()`](https://sfcheung.github.io/semlbci/reference/syntax_to_i.md)
  : Parameter Positions From lavaan Syntax

- [`loglike_compare()`](https://sfcheung.github.io/semlbci/reference/loglikelihood.md)
  [`loglike_range()`](https://sfcheung.github.io/semlbci/reference/loglikelihood.md)
  [`loglike_point()`](https://sfcheung.github.io/semlbci/reference/loglikelihood.md)
  [`loglike_quad_range()`](https://sfcheung.github.io/semlbci/reference/loglikelihood.md)
  [`loglike_quad_point()`](https://sfcheung.github.io/semlbci/reference/loglikelihood.md)
  [`loglike_compare_ur()`](https://sfcheung.github.io/semlbci/reference/loglikelihood.md)
  [`loglike_range_ur()`](https://sfcheung.github.io/semlbci/reference/loglikelihood.md)
  [`loglike_point_ur()`](https://sfcheung.github.io/semlbci/reference/loglikelihood.md)
  [`loglike_quad_range_ur()`](https://sfcheung.github.io/semlbci/reference/loglikelihood.md)
  [`loglike_quad_point_ur()`](https://sfcheung.github.io/semlbci/reference/loglikelihood.md)
  : Log Profile likelihood of a Parameter

- [`plot(`*`<loglike_compare>`*`)`](https://sfcheung.github.io/semlbci/reference/plot.loglike_compare.md)
  : Plot the Output of 'loglike_compare()'

## Methods

Methods of `semlbci-class` objects (output of \[semlbci()\]).

- [`confint(`*`<semlbci>`*`)`](https://sfcheung.github.io/semlbci/reference/confint.semlbci.md)
  : Confidence Intervals for a 'smelbci' Object
- [`print(`*`<semlbci>`*`)`](https://sfcheung.github.io/semlbci/reference/print.semlbci.md)
  : Print Method of a 'semlbci' Object

## Advanced Functions

Low level functions for advanced users.

- [`ci_bound_wn_i()`](https://sfcheung.github.io/semlbci/reference/ci_bound_wn_i.md)
  : Likelihood-based Confidence Bound By Wu-Neale-2012
- [`ci_bound_ur_i()`](https://sfcheung.github.io/semlbci/reference/ci_bound_ur_i.md)
  : Likelihood-Based Confidence Bound By Root Finding
- [`ci_bound_ur()`](https://sfcheung.github.io/semlbci/reference/ci_bound_ur.md)
  [`gen_est_i()`](https://sfcheung.github.io/semlbci/reference/ci_bound_ur.md)
  : Find a Likelihood-Based Confidence Bound By Root Finding
- [`ci_i_one()`](https://sfcheung.github.io/semlbci/reference/ci_i_one.md)
  : Likelihood-Based Confidence Bound for One Parameter
- [`set_constraint()`](https://sfcheung.github.io/semlbci/reference/set_constraint.md)
  : Equality Constraint for Finding the LBCI by Wu-Neale-2012
- [`print(`*`<cibound>`*`)`](https://sfcheung.github.io/semlbci/reference/print.cibound.md)
  : Print Method of a 'cibound'-class Object
- [`gen_userp()`](https://sfcheung.github.io/semlbci/reference/gen_userp.md)
  [`gen_sem_out_userp()`](https://sfcheung.github.io/semlbci/reference/gen_userp.md)
  : Create a Wrapper To Be Used in 'lavaan' Models

## Datasets

Datasets used in examples.

- [`cfa_evar_near_zero`](https://sfcheung.github.io/semlbci/reference/cfa_evar_near_zero.md)
  : Dataset (CFA, Two Factors, One Standardized Error Variance Close to
  Zero)
- [`cfa_two_factors`](https://sfcheung.github.io/semlbci/reference/cfa_two_factors.md)
  : Dataset (CFA, Two Factors, Six Variables)
- [`cfa_two_factors_mg`](https://sfcheung.github.io/semlbci/reference/cfa_two_factors_mg.md)
  : Dataset (CFA, Two Factors, Six Variables, Two Groups)
- [`mediation_latent`](https://sfcheung.github.io/semlbci/reference/mediation_latent.md)
  : Dataset (SEM, Three Factors, Nine Variables, Mediation)
- [`mediation_latent_skewed`](https://sfcheung.github.io/semlbci/reference/mediation_latent_skewed.md)
  : Dataset (SEM, Three Factors, Nine Variables, Mediation, Skewed)
- [`reg_cor_near_one`](https://sfcheung.github.io/semlbci/reference/reg_cor_near_one.md)
  : Dataset (Six Variables, One Correlation Close to One)
- [`simple_med`](https://sfcheung.github.io/semlbci/reference/simple_med.md)
  : Dataset (Simple Mediation Model)
- [`simple_med_mg`](https://sfcheung.github.io/semlbci/reference/simple_med_mg.md)
  : Dataset (Simple Mediation Model, Two Groups)
