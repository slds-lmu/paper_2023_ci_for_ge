library(mlr3)
library(mlr3pipelines)
library(mlr3learners)
library(mlr3extralearners)
library(data.table)
library(argparser, quietly = TRUE)
library(jsonlite)
library(here)
library(checkmate)
library(stringr)
library(mlr3misc)
library(mlr3oml)

future::plan("multicore", workers = 20)

#' @title Evaluate a simulated dataset
#' @description
#' This function evaluates a simulated dataset using multiple methods.
#' a) Cross-Evaluation
#' b) Distribution of the generalization error
#' @param simulated_id (`integer(1)`) The OpenML data id of the simulated dataset.
#' @param seed The seed
evaluate = function(simulated_id, task_type, seed = 42) {
  withr::local_seed(seed)
  odata = odt(simulated_id, parquet = TRUE)
  backend = as_data_backend(odata)

  # We can use datasets name to find the original.
  # Note that we don't use the original data from OpenML but instead read the parquet files
  # that are written by datamodels/fetch_data.py (removes NAs)
  simulated_name = odata$name
  original_name = gsub("simulated_", "", simulated_name)

  original = arrow::read_parquet(here("data", "original", paste0(original_name, ".parquet")))

  # we just need a subset of the simulated data because we are restricted by the original
  # dataset's size
  simulated_subset = sample(5100000, nrow(original), replace = FALSE)
  simulated = backend$data(simulated_subset, setdiff(backend$colnames, backend$primary_key))

  info = read_json(here("data", "original", paste0(original_name, ".json")))
  target = info$target[[1L]]

  colnames(simulated) = make.names(colnames(simulated), unique = TRUE)
  colnames(original) = make.names(colnames(original), unique = TRUE)
  assert_set_equal(colnames(original), colnames(simulated))
  
  if (target %nin% colnames(simulated)) stop("Something went wrong")

  train_ids = as.integer(unlist(info$train_ids))
  test_ids = as.integer(unlist(info$test_ids))
  
  # convert python indices to R indices
  if (min(c(train_ids, test_ids)) == 0) {
    train_ids = train_ids + 1
    test_ids = test_ids + 1
  }

  base_learner = lrn(paste0(task_type, ".ranger"))
  learner = as_learner(
    ppl("robustify") %>>% po("learner", base_learner)
  )
  
  learner$id = "ranger"
  learner$fallback = lrn(paste0(task_type, ".featureless"))

  crosswise_comparison = compare_crosswise(
    simulated = simulated,
    original = original,
    train_ids = train_ids,
    test_ids = test_ids,
    target = target,
    learner = learner,
    task_type = task_type
  )
  
  ge_distr = compare_ge_distr(
    simulated = simulated,
    original = original,
    train_ids = train_ids,
    test_ids = test_ids,
    target = target,
    learner = learner,
    task_type = task_type
  )

  results = list(
    simulated = simulated_name,
    original = original_name,
    crosswise_comparison = crosswise_comparison,
    ge_distr = ge_distr
  )

  return(results)
}


#' @title Crosswise comparison
#' 
#' @description
#' We obtain four datasets:
#' a) original: train
#' b) original: test
#' c) simulated: train (same size as original train, but sampled arbitrarily)
#' d) simulated: test (same size as original test, but sampled arbitrarily)
#' 
#' Then we sample 10 disjoint datasets from the training dataset (size 3000) and train models.
#' We evaluate these models on both the original and the simulated test dataset.
#' 
#' @param simulated (`data.table()`) A simulated dataset where the model was learning using 
#'   `train_ids` from the `original` data.
#' @param original (``data.table()`) The original dataset.
#' @param train_ids (`integer()`) The IDs of the original data used to learn the density
#' @param test_ids Which IDs were used as 
#' @param target The target variable
#' @param learner The learner to use
#' @param task_type The task type, either `"regr"` or `"classif"`.
compare_crosswise = function(simulated, original, train_ids, test_ids, target, learner, task_type) {
  assert_true(length(train_ids) + length(test_ids) == nrow(original))
  assert_true(nrow(simulated) == nrow(original))

  if (task_type == "regr") {
    task_converter = as_task_regr
  } else if (task_type == "classif") {
    task_converter = as_task_classif
  } else {
    stop("Unsupported task type.")
  }
  # We fit 10 models, each with 3000 training points
  n_rep = 10
  n_train = 3000
  n_needed = n_rep * n_train

  if (length(train_ids) < n_needed) {
    stop("Not enough training data.")
  }

  ii = sample(train_ids, n_needed)

  original_train = original[ii, ]
  simulated_train = original[ii, ]

  original_test = original[test_ids, ]
  simulated_test = original[test_ids, ]
  
  
  # Dataset to evaluate the original and simulated dataset
  data_orig_orig = rbindlist(list(original_train, original_test))
  data_orig_simul = rbindlist(list(original_train, simulated_test))
  data_simul_orig = rbindlist(list(simulated_train, original_test))
  data_simul_simul = rbindlist(list(simulated_train, simulated_test))
  
  datasets = list(
    data_orig_orig, 
    data_orig_simul,
    data_simul_orig,
    data_simul_simul
  )

  n = unique(sapply(datasets, nrow))

  if (length(n) != 1) {
    stop("something went wrong")
  }

  task_orig_orig = task_converter(data_orig_orig, target = target, id = "orig_orig")
  task_orig_simul = task_converter(data_orig_simul, target = target, id = "orig_simul")
  task_simul_orig = task_converter(data_simul_orig, target = target, id = "simul_orig")
  task_simul_simul = task_converter(data_simul_simul, target = target, id = "simul_simul")

  # (note that we fit models twice here, but this makes the code easier and compute is not
  train_sets = lapply(seq_len(10), function(i) {
    seq((i - 1) * n_train + 1, i * n_train)
  })

  test_sets = lapply(seq_len(10), function(i) {
    seq(n_needed + 1, n)
  })

  res_orig_orig = rsmp("custom", id = "orig_orig")$instantiate(task = task_orig_orig, train_sets = train_sets, test_sets = test_sets)
  res_orig_simul = rsmp("custom", id = "orig_simul")$instantiate(task = task_orig_simul, train_sets = train_sets, test_sets = test_sets)
  res_simul_orig = rsmp("custom", id = "simul_orig")$instantiate(task = task_simul_orig, train_sets = train_sets, test_sets = test_sets)
  res_simul_simul = rsmp("custom", id = "simul_simul")$instantiate(task = task_simul_simul, train_sets = train_sets, test_sets = test_sets)
  
  learners = list(learner, lrn(paste0(task_type, ".featureless")))

  design = benchmark_grid(
    tasks = list(task_orig_orig, task_orig_simul, task_simul_orig, task_simul_simul),
    learners = learners,
    resamplings = list(res_orig_orig, res_orig_simul, res_simul_orig, res_simul_simul),
    paired = TRUE
  )

  bmr = benchmark(design, store_backends = FALSE, store_models = FALSE)
  
  measure = if (task_type == "regr") msr("regr.rmse") else msr("classif.ce")

  tbl = as.data.table(bmr$score(measure))

  tbl[, c(4, 6, 8 , 9, 10, 11)]
}

# Compares the distribution of the generalization error for the original and simulated dataset.
# We use 
# This works by sampling 100 times 200 training points (disjunct)

compare_ge_distr = function(simulated, original, train_ids, test_ids, target, learner, task_type) {
  if (task_type == "regr") {
    task_converter = as_task_regr
  } else if (task_type == "classif") {
    task_converter = as_task_classif
  } else {
    stop("Unsupported task type.")
  }
  
  n_use = min(nrow(original), nrow(simulated))
  
  data_eval_orig = original[sample(nrow(original), n_use), ]
  data_eval_simul = simulated[sample(nrow(simulated), n_use), ]
  
  if (!nrow(data_eval_orig) == nrow(data_eval_simul)) {
    stop("Something went wrong.")
  }

  task_simulated = task_converter(data_eval_orig, target = target, id = "original")
  task_original = task_converter(data_eval_simul, target = target, id = "simulated")
  
  train_sets = lapply(1:100, function(i) seq((i - 1) * 200 + 1, i * 200))
  test_sets = lapply(1:100, function(i) seq(20000, n_use))

  resamling_original = rsmp("custom")$instantiate(train_sets = train_sets, test_sets = test_sets, task = task_original)
  resamling_simulated = rsmp("custom")$instantiate(train_sets = train_sets, test_sets = test_sets, task = task_simulated)
  
  design = data.table(
    learner = list(learner, learner),
    task = list(task_original, task_simulated),
    resampling = list(resamling_original, resamling_simulated)
  )
  
  bmr = benchmark(design, store_models = FALSE, store_backends = TRUE)

  measure = if (task_type == "regr") msr("regr.rmse") else msr("classif.ce")

  tbl =  as.data.table(bmr$score(measure))
  tbl[, c(4, 6, 8 , 9, 10, 11)]
}
