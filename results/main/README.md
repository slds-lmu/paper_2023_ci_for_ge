**truth.rds**

This file contains the approximated true rirks of the different inducers on the DGPs:

* `size`: The dataset size to which an inducer is applied
* `repl`: The repetition of the experiment (500 of each)
* `learner`: The inducer/learner
* `dgp`: The data generating process
* `loss`: The loss function
* `R`: The Risk
* `task_type`: The task type (regression or classification)
* `ER`: The expected risk

The file is generated via `./analysis/processing/truth_losses.rds`

**ci.rds**

The main (unaggregated) dataset containing the confidence intervals and true values for (expected) Risk:

* `repl`: The repetition of the experiment
* `size`: The dataset size to which the inference method is applied to
* `inducer`: The inducing algorithm
* `dgp`: The data generating process
* `loss`: The loss function
* `method`: The inference method
* `task_type`: The task type (regression or classification)
* `iters`: The total number of resampling iterations needed for the inference method
* `estimate`: The point estimate
* `lower`: The lower bound of the 95% CI
* `upper`: The upper bound of the 95% CI
* `PQ`: The proxy quantity (NA if not applicable / not calculated)
* `R`: The approximated true Risk
* `ER`: The approximated true expected Risk

This file is generated via `./analysis/processing/main.R`

**ci_aggr.rds**

The aggregated version of *ci.rds* (aggregated over the 500 repetitions of each experiment).

* `dgp`: The data generating process
* `inducer`: The inducing algorithm
* `size`: The dataset size to which the inference method is applied
* `method`: The inference method
* `loss`: The loss function
* `cov_R`: The relative coverage frequency w.r.t. the Risk.
* `cov_ER`: The relative coverage frequency w.r.t. the Expected Risk.
* `cov_PQ`: The relative coverage frequency w.r.t. the Proxy Quantity (if calculated / applicable).
* `under_R`: Relative frequence how often the upper boundary of the CI is below the Risk.
* `under_ER`: Relative frequence how often the upper boundary of the CI is below the Expected Risk.
* `under_PQ`: Relative frequence how often the upper boundary of the CI is below the Proxy Quantity (if applicable / calculated).
* `cov_R_se`: The standard error for cov_R
* `cov_ER_se`: The standard error for cov_ER
* `cov_PQ_se`: The standard error for cov_PQ
* `width`: The mean of the widths.
* `width_median`: The median of the widths.
* `width_sd`: The standard deviation of the widths.
* `bias`: The bias of the point estimate
* `ER`: The expected Risk
* `R_sd`: The standard deviation of the Risks
* `estimate_sd`: The standard deviation of the point estimates
* `rmse_ER`: The root-mean-square error of the point estimates with respect to the expected Risk.
* `rmse_R`: The root-mean-square error of the point estimates with respect to the Risk.
* `task_type`: The task type (regression or classification).
* `iters`: The total number of resampling iterations needed for the inference method.

This file is generated via `./analysis/processing/main.R`