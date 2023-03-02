# Experiment

IMPORTANT: If we include to many methods in the study (especially naive and bad ones) 
the plots will become totally unreadable.
Either we avoid stuff that we exclude such methods right from the beginning, or we only exclude them from the plots
(as long as the experiments actually confirm that they are bad).

## Design

The dimension of the study are:

* Repetitions
* Loss Functions
* Statistics of Interest
* Learner
* Tasks
* Resampling Methods
* Inference Methods
* Target of Inference

### Target of Inference

* The target of inference is the prediction error (the expected error of the model trained on the whole training data)
* Compare this with the Bayle paper, where they always use different targets of inference for the different methods (criticize this in our paper?)

### Repetitions

* 1000 would give us 3 digit precision (?)

### Loss Functions

* Just go for 1-0 loss for classification and mse for regression ?

### Statistics of Interest

* Coverage Frequency: 
    * Look at two-sided and one-sided confidence intervals?
    * Under- and over-coverage
    * How to we calculate the CIs? Bayle use the Wilson Interval (Mortality prediction in intensive care units with the 
      Super ICU Learner Algorithm (SICULA): a population-based study) which are more precise than standard CIs
* Average width / distribution of widths
* Computational Efficiency

### Learner

* Classif: Logistische Regression, Random Forest (?)
* Regression: Lineares Modell, Random Forest (?)
* Standardize Features
* In the Bayle paper, the use penalized logreg, ridge, neural network and a random forest
* E.g. the stability of the algorithm might be important. (There is something Austern mentions)
  Also e.g. the Bates method relies on a stability assumption of the empirical loss when calculating the bias 
  adjustment

### Tasks

* Higgs dataset and flights datasets are used in Bayle and they are REALLY large (> 5 Mio each)
  I think this kind of avoids the issues with dependence between samples.
* Some datasets have more than two classes. Why do we restrict ourselves to logistic regression again? 
  Either include a different learner or summarize the classes into a binary classification problem.
* Sample size: In the Bayle paper they set n = 700, 1,000, 1,500, 2,300, 3,400, 5,000, 7,500, 11,000. 
  In general, the dependence of the coverage frequency dependent on the number of observations might be quite interesting to 
  practictioners as this gives guidance which method to pick for which dataset.
  --> Maybe we can come up with a heuristic do determine the method.
* Drop missing values
* Target transformations (e.g. log-trafo for flights data is given in the bayle paper)
* We need to determine how we make use of the 
* Größe der Datensätze ? (Sollte zu klein sein, dass man nicht einfach holdout verwenden kann)
* In the Bayle paper there is information on how to sample from the datasets.

TODO: Add table from overleaf

### Resampling Methods

* LOO
* 10-fold CV (wieso eigentlich immer 10-fold ???)
* Repeated 10 Fold CV?
* BootstrapCCV
* Bootstrap
* Subsampling (wie oft
* Holdout (which split? 70-30, 90-10 ?)


### Inference Methods

From the bayle paper:

We exclude McNemar’s test [40] and the difference-of-proportions test which Dietterich [22] found to
be less powerful than 5 × 2-fold CV and the conservative Z-test which Nadeau and Bengio [43] found less
powerful and more expensive than corrected repeated train-validation splitting.

* 5 x 2-fold CV
* corrected repeated train-validation splitting
* Standard Estimator (LOO)
* Naive Baseline (Holdout, LOO, )
* Naive glmnet (CV, and LOO)
* Bates (CV and LOO): 
* Bayle (Nested CV): Here we should 
* Jiang (BootstrapCCV)
* Jackknife ?

## Bates

## Bayle


* We should use the all-pairs variance, as this is generally appicable.
      



## Careful

* [ ] We should ensure that all the dataset sizes we are using are divisible by the number of folds, as most of the 
      formulas are given also assume that. While they can usually be adapted with some logical thinking, we then use 
      a slightly adjusted version of these methods.


## Visualizations

How will we visualize the experiment results?


## Coverage Frequency vs Time

* x-axis: Time
* y-axis: Coverage Frequency Averaged over all tasks
* color: the method

## Width vs Time

* x-axis: Time
* y-axis: Width, averaged over all Tasks
* color: the method

