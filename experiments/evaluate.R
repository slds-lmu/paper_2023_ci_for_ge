library(batchtools)
library(ggplot2)
library(data.table)
library(mlr3misc)

reg = loadRegistry("/gscratch/sfische6/benchmarks/ci_for_ge/newtest2", writeable = FALSE)

jt = getJobTable()

jt1 = jt[, c("job.id", "prob.pars", "algo.pars")]
jt1$size = map_dbl(jt1$prob.pars, "size")
jt1$task_id = map_chr(jt1$prob.pars, "task_id")
jt1$learner_id = map_chr(jt1$prob.pars, "learner_id")
jt1$learner_params = map(jt1$prob.pars, "learner_params")
jt1$resampling_id = map_chr(jt1$algo.pars, "resampling_id")
jt1$resampling_params = map(jt1$algo.pars, "resampling_params")
jt1$prob.pars = NULL
jt1$algo.pars = NULL

# for objects with the same inference_hash, the same inference methods are applied
jt1$experiment_hash = mlr3misc::map_chr(seq_len(nrow(jt1)), function(i) {
  mlr3misc::calculate_hash(jt1[i, c("resampling_id", "resampling_params", "size", "task_id", "learner_id", "learner_params")])
})

jttmp = jt1

jt1 = jt1[, list(
  ids = list(get("job.id")),
  size = get("size")[[1L]],
  resampling_id = get("resampling_id")[1L],
  task_id = get("task_id")[1L],
  learner_id_id = get("learner_id")[1L],
  resampling_params = get("resampling_params")[1L]),
  by = "experiment_hash"
]

# FIXME: Currently we don't take care of the small and large settings (jiang etc.)
# Also learner_params and learner_id not properly covered


# later the name will be contained as a problem parameter
jt1$resampling_name = map_chr(seq_len(nrow(jt1)), function(i) {
  id = jt1[i, "resampling_id"][[1L]][1L]
  pv = jt1[i, "resampling_params"][[1L]][[1L]]

  if (id == "holdout") {
    if (pv$ratio == 1L) {
      return("prediction_error")
    } else if (pv$ratio == 2 / 3) {
      return("holdout")
    }
  } else if (id == "nested_cv") {
    return("nested_cv")
  } else if (id == "subsampling") {
    if (pv$repeats == 10) {
      return("subsampling_10")
    } else if (pv$repeats == 100) {
      return("subsampling_100")
    }
    return("subsampling")
  } else if (id == "cv") {
    return("cv_10")
  } else if (id == "conservative_z") {
    return("conservative_z")
  } else if (id == "repeated_cv") {
    if (pv$repeats == 5 && pv$folds == 2) {
      return("diettrich")
    } else if (pv$folds == 10 && pv$repeats == 10) {
      return("cv_10_10")
    }
  }
})



# FIXME: This will be easier with resampling_name
#    holdout           = list(id = "holdout", params = list(ratio = 2 / 3)),
#    nested_cv         = list(id = "nested_cv", params = list(folds = 10)),
#    subsampling_10    = list(id = "subsampling", params = list(repeats = 10)),
#    subsampling_100   = list(id = "subsampling", params = list(repeats = 100)),
#    cv_10             = list(id = "cv", params = list(folds = 10)),
#    repeated_cv_10_10 = list(id = "repeated_cv", params = list(folds = 10, repeats = 10)),
#    conservative_z    = list(id = "conservative_z", params = list(J = 10, M = 10, ratio = 0.9)),
#    diettrich         = list(id = "repeated_cv", params = list(repeats = 5, folds = 2)),
#    prediction_error  = list(id = "holdout", params = list(ratio = 1))


get_inference_methods = function(resampling_name) {
 switch(resampling_name,
    holdout = list(holdout = infer_holdout),
    nested_cv = list(bates = infer_bates),
    subsampling_10 = list(corrected_t = infer_corrected_t, simple = infer_simple),
    subsampling_100 = list(corrected_t = infer_corrected_t, simple = infer_simple),
    cv_10 = list(bayle = infer_bayle, simple = infer_simple),
    # FIXME: Bayle also for repeated_cv ?
    cv_10_10 = list(simple_repeated = infer_simple),
    conservative_z = list(conservative_z = infer_conservative_z),
    diettrich = list(diettrich = infer_52cv),
    prediction_error = NULL,
    stop("BUG")
  )
}

# Use normal registry 

REG_EVAL_PATH = "/gscratch/sfische6/benchmarks/ci_for_ge/eval_newtest2-5"

if (file.exists(REG_EVAL_PATH)) {
  reg_eval = loadRegistry(REG_EVAL_PATH, writeable = TRUE)
} else {
  reg_eval = makeRegistry(REG_EVAL_PATH,
    packages = c("data.table", "mlr3misc", "mlr3verse", "inferGE"),
    seed = 1L
  )
  batchExport(list(get_inference_methods = get_inference_methods), reg = reg_eval)
}

reg_eval$default.resources$memory = 32000


if (FALSE) {
  j = 30
  ids = jt1$ids[[j]][1:5]
  resampling_name = jt1$resampling_name[[j]]
  task_id = jt1$task_id[[j]]
  resampling_params = jt1$resampling_params[[j]]
  resampling_id = jt1$resampling_id[[j]]
  learner_id = jt1$learner_id[[j]]
  size = jt1$size[[j]]
}


# FIXME: learner_id shouls also be learner_name I think
fn = function(ids, resampling_name, task_id, resampling_params, resampling_id, learner_id, size) {
  # result1 is just to get the task type
  result1 = readRDS(paste0("/gscratch/sfische6/benchmarks/ci_for_ge/newtest2/results/", ids[1], ".rds"))

  task_type = result1$predictions$test[[1L]]$task_type
  inference_methods = get_inference_methods(resampling_name)

  if (is.null(inference_methods)) {
    # here we evaluate the truth
    prediction_errors = map_dbl(ids, function(id) {
      result = readRDS(paste0("/gscratch/sfische6/benchmarks/ci_for_ge/newtest2/results/", id, ".rds"))
      result$predictions$holdout[[1L]]$score()
    })

    dt = data.table(
      learner_id = learner_id,
      task_id = task_id, 
      size = size,
      prediction_error = prediction_errors,
      replication = seq_along(ids)
    )

    return(list(dt))
  }

  # FIXME: other loss functions

  map(inference_methods, function(inference_method) {
    tmp = map(ids, function(id) {
      result = readRDS(paste0("/gscratch/sfische6/benchmarks/ci_for_ge/newtest2/results/", id, ".rds"))
      tbl = calculate_loss(result$predictions$test)

      task_info = list(
        nrow = size
      )
      
      resampling_info = list(
        id = resampling_id,
        params = resampling_params
      )
      
      # FIXME: remove this, all parameter values must be available here 
      # (should maybe returned by the experiment or something)

      if (resampling_id == "subsampling" && is.null(resampling_info$params$ratio)) {
        resampling_info$params$ratio = 2 / 3
      }

      loss_name = if (task_type == "regr") "se" else "zero_one"

      # higher alpha --> less variance
      inference_method(tbl, alpha = 0.10, task_info = task_info, loss = loss_name, resampling_info = resampling_info)
    })

    dt = rbindlist(tmp)
    dt$learner_id = learner_id
    dt$task_id = task_id
    dt$size = size
    dt$replication = seq_along(ids)
    dt$resampling_name = resampling_name
    dt$resampling_params = list(resampling_params)

    dt
  })
}

if (FALSE) {
  clearRegistry()
  batchMap(fn,
    ids = map(jt1$ids, function(ids) ids),
    resampling_name = jt1$resampling_name,
    task_id = jt1$task_id,
    resampling_params = jt1$resampling_params,
    resampling_id = jt1$resampling_id,
    learner_id = jt1$learner_id,
    size = jt1$size,
    reg = reg_eval
  )
  submitJobs()
  stop()
  waitForJobs()

  getStatus()
  getLog(findErrors()[[1]][1])
  getLog(findExpired()[[1]][1])

}



results = lapply(seq_len(nrow(getJobTable())), loadResult)
results = Reduce(c, results)

is_truth = map_lgl(results, function(x) "prediction_error" %in% colnames(x))
sum(is_truth)

results = imap(results, function(res, nm) {
  res$inference_name = nm
  res
})

truth_table = rbindlist(results[is_truth])

epe = truth_table[, list(epe = mean(get("prediction_error"))), by = c("task_id", "size", "learner_id")]

tbl = rbindlist(results[!is_truth], fill = TRUE)

# first we join the PE
tbl = merge(tbl, truth_table[, c("prediction_error", "task_id", "size", "replication", "learner_id")],
  by = c("task_id", "size", "replication", "learner_id"))
# then the epe
tbl = merge(tbl, epe[, c("task_id", "size", "epe")], by = c("task_id", "size"))

tbl$task_id = as.factor(tbl$task_id)
tbl$learner_id = as.factor(tbl$learner_id)

colnames(tbl)[colnames(tbl) == "prediction_error"] = "pe"
tbl[, contains_pe := ((pe >= lower) & (pe <= upper))]
tbl[, contains_epe := ((epe >= lower) & (epe <= upper))]


tbl[, experiment_id := .GRP, by = c("resampling_name", "task_id", "size", "learner_id", "inference_name")]
tbl$experiment_id = as.factor(tbl$experiment_id)

tbl$inference_name[tbl$inference_name == "corected_t"] = "corrected_t"

tmp = tbl


tbl_aggr = tmp[, list(
  cov_pe = mean(contains_pe),
  cov_pe_se = sd(contains_pe) / sqrt(.N),
  cov_epe = mean(contains_epe),
  cov_epe_se = sd(contains_epe) / sqrt(.N),
  width = mean(upper - lower),
  learner_id = learner_id[1],
  resampling_name = resampling_name[1],
  info = info[1],
  inference_name = inference_name[1],
  task_id = task_id[1],
  size = size[1],
  pe = list(list(pe)),
  epe = epe[1],
  resampling_params = list(list(resampling_params)),
  resampling_id = resampling_id[1L]
  ), by = "experiment_id"]

tbl_aggr = tbl_aggr[inference_name %nin% c("simple", "simple_repeated"),]
tbl_tmp = tmp[inference_name %nin% c("simple", "simple_repeated"),]

saveRDS(tbl_aggr, here::here("results", "test", "aggregated.rds"))
saveRDS(tbl_tmp, here::here("results", "test", "result.rds"))
