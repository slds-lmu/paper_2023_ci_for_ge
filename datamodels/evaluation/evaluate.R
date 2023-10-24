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

get_original = function(input_string){
  result = str_extract(input_string, ".*?(?=_[0-9])")
  return(result)
}

# This function evaluates

#' @title Evaluate a simualted dataset
#' @description
#' This function evaluates a simulated dataset using multiple methods.
#' 1. We train two models:
#'    a) On the simulated data (from the model that was trained using the training data from the original data)
#'    b) On the training data from the original data.
#'    We then compare their performance on the test data from the original data.
#' 2. We partition both the simulated and the original datasets into many small training datasets and one
#'    test dataset. Then we estimate and plot the distribution of the PE for both DGPs (Data Generating Processes).
#' 3. We train two models on the original and simulated dataset.
#'    We then cross-evaluate each dataset on the other dataset. (TODO:)
#' @param simulated_name The name of the simulated dataset.
#' @param seed The seed.
main = function(simulated_name, seed) {
  set.seed(seed)
  original_name = get_original(simulated_name)
  simulated = arrow::read_parquet(here("data", "simulated", paste0(simulated_name, ".pq")))
  original = arrow::read_parquet(here("data", "original", paste0(original_name, ".pq")))
  

  assert_set_equal(colnames(original), colnames(simulated))

  info = read_json(here("data", "original", paste0(original_name, ".json")))
  target = info$target[[1L]]


  colnames(simulated) = make.names(colnames(simulated), unique = TRUE)
  colnames(original) = make.names(colnames(original), unique = TRUE)
  
  if (target %nin% colnames(simulated)) stop("target renamed")

  train_ids = as.integer(unlist(info$train_ids))
  test_ids = as.integer(unlist(info$test_ids))
  
  # convert python indices to R indices
  if (min(c(train_ids, test_ids)) == 0) {
    train_ids = train_ids + 1
    test_ids = test_ids + 1
  }
  if (is.numeric(original[[target]])) {
    task_type = "regr"
  } else {
    task_type = "classif"
  }
  # Here we use random forest because of installation trouble of ranger on the GPU server
  base_learner = lrn(paste0(task_type, ".rfsrc"))
  learner = as_learner(
    ppl("robustify") %>>% po("learner", base_learner)
  )
  
  learner$param_set$set_values(
    collapsefactors.target_level_count = 2
  )
  
  learner$param_set$set_values()

  learner$id = "rfsrc"
  learner$fallback = lrn(paste0(task_type, ".featureless"))

  test_perf = compare_test_perf(
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
    test_perf = test_perf,
    ge_distr = ge_distr
  )

  pth = file.path(here("datamodels", "evaluation", "results", original_name))
  print(pth)
  if (!file.exists(pth)) {
    dir.create(pth, recursive = TRUE)
  }
  saveRDS(results, file.path(pth, "result.rds"))
}

compare_test_perf = function(simulated, original, train_ids, test_ids, target, learner, task_type) {
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
  
  if (nrow(simulated) < n_needed) {
    stop("Not enough simulated data.")
  }
  
  original_train = original[train_ids[seq_len(n_needed)], ]
  original_test = original[test_ids, ]
  simulated_train = simulated[seq_len(n_needed), ]
  
  # Dataset to evaluate the original and simulated dataset
  data_eval_orig = rbindlist(list(original_train, original_test))
  data_eval_simul = rbindlist(list(simulated_train, original_test))
  
  if (!(nrow(data_eval_simul) == nrow(data_eval_orig))) {
    stop("Something went wrong.")
  }
  
  cv_10fold = rsmp("cv")$instantiate(task = task_converter(original_train, target = target))

  # We abuse test sets from 10 fold CV to get distinct training sets
  train_sets = lapply(1:10, function(fold) {
    cv_10fold$test_set(fold)
  })
  lapply(train_sets, function(x) if (max(x) > n_needed) stop("Something went wrong."))

  # each time we use the same train set
  test_sets = lapply(1:10, function(fold) seq(n_needed + 1, nrow(data_eval_orig)))
  
  task_orig = task_converter(data_eval_orig, id = "eval_orig", target = target)
  task_simul = task_converter(data_eval_simul, id = "eval_simul", target = target)
  
  #task_orig$set_col_roles(task_orig$feature_types[get("type") %in% c("ordered", "factor"), "id"][[1L]], "stratum")
  #task_simul$set_col_roles(task_simul$feature_types[get("type") %in% c("ordered", "factor"), "id"][[1L]], "stratum")

  rsmp_orig = rsmp("custom")$instantiate(task = task_orig, train_sets = train_sets, test_sets = test_sets)
  rsmp_simul = rsmp("custom")$instantiate(task = task_simul, train_sets = train_sets, test_sets = test_sets)
  learners = list(learner, lrn(paste0(task_type, ".featureless")))

  design = benchmark_grid(
    tasks = list(task_orig, task_simul),
    learners = learners,
    resampling = list(rsmp_orig, rsmp_simul),
    paired = TRUE
  )

  bmr = benchmark(design, store_backends = FALSE, store_models = FALSE)

  return(bmr)
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
  
  #task_simulated$set_col_roles(task_orig$feature_types[get("type") %in% c("ordered", "factor"), "id"][[1L]], "stratum")
  #task_simul$set_col_roles(task_simul$feature_types[get("type") %in% c("ordered", "factor"), "id"][[1L]], "stratum")
  
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
  
  return(bmr)
}

if (!interactive()) {
  p = arg_parser("Round a floating point number")
  p = add_argument(p, "--simulated", help = "The name of the simulated dataset.")
  p = add_argument(p, "--original", help = "The name of the original dataset.")
  p = add_argument(p, "--learner", help = "The learner to use.", default = "lm")
  p = add_argument(p, "--seed", help = "The name of the original dataset.", default = 42)
  argv = parse_args(p)

  main(
    simulated_name = argv$simulated,
    seed = argv$seed
  )
}