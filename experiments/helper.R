make_task = function(data_id, size, repl, resampling) {
  # because insample resampling is used to obtain 'true' PE, we use 100 000 holdout observation
  # all others are only used for proxy quantities, where we aggregate over multiple such estimates
  # only for holdout do we have one iter, so there we also use 100k
  con = dbConnect(duckdb::duckdb(), ":memory:", path = tempfile())
  holdout_ids_path = here::here("data", "splits", data_id, "holdout_100000.parquet")
  use_ids_path = here::here("data", "splits", data_id, paste0(size, ".parquet"))

  DBI::dbExecute(con, paste0("CREATE OR REPLACE VIEW holdout_table AS SELECT * FROM read_parquet('", holdout_ids_path, "')"))

  holdout_preds = inherits(resampling, "ResamplingInsample") | inherits(resampling, "ResamplingCV")
  DBI::dbExecute(con, paste0("CREATE OR REPLACE VIEW use_table AS SELECT * FROM read_parquet('", use_ids_path, "')"))
  use_ids = dbGetQuery(con, sprintf("SELECT row_id FROM use_table WHERE iter = %i", repl))$row_id

  if (holdout_preds) {
    holdout_ids = dbGetQuery(con, paste0("SELECT row_id FROM holdout_table"))$row_id
    ids = c(use_ids, holdout_ids)
  } else {
    ids = use_ids
  }

  DBI::dbDisconnect(con, shutdown = TRUE)

  odata = odt(data_id, parquet = TRUE)
  target = odata$target_names

  backend = as_data_backend(odata)

  # we convert to a data.table for the experiments, because
  # there is a bug: https://github.com/mlr-org/mlr3/issues/961

  tmpdata = backend$data(ids, backend$colnames)
  rm(backend)
  # mlr3 bug in cbind ...
  names(tmpdata)[names(tmpdata) == "mlr3_row_id"] = "..row_id"

  backend = as_data_backend(tmpdata, primary_key = "..row_id")

  task = if (is.factor(tmpdata[[target]])) {
    as_task_classif(backend, target = target)
  } else {
    as_task_regr(backend, target = target)
  }

  task$id = odata$name

  # do not stratify here!

  task$row_roles$use = use_ids
  if (holdout_preds) {
    task$row_roles$holdout = holdout_ids
  }

  return(task)
}

make_resampling = function(resampling_id, resampling_params) {
  resampling = do.call(rsmp, c(list(.key = resampling_id), resampling_params[[1]]))
}

make_learner = function(learner_id, learner_params, learner_name, task, resampling) {
  learner = do.call(lrn,
   args = c(list(.key = learner_id), learner_params)
  )

  graph = ppl("robustify", learner = learner, task = task) %>>%
    learner

  # we need this because bootstrapping is broken with mlr3pipelines
  if (inherits(resampling, "ResamplingBootstrap") || inherits(resampling, "ResamplingNestedBootstrap")) {
    graph = inferGE::PipeOpMetaRobustify$new() %>>% graph
  }

  learner = as_learner(graph)

  if (startsWith(learner_id, "classif")) {
    fallback = lrn("classif.featureless")
    fallback$predict_type = learner$predict_type = "prob"
  } else {
    fallback = lrn("regr.featureless")
  }
  learner$predict_sets = "test"
  learner$encapsulate = c(train = "try", predict = "try")
  learner$fallback = fallback
  learner$id = learner_name
  return(learner)
}

run_resampling = function(instance, resampling_id, resampling_params, job, ...) {
  lgr::get_logger("mlr3")$set_threshold("warn")
  resampling = make_resampling(resampling_id, resampling_params)
  task = make_task(data_id = instance$data_id, size = instance$size, repl = job$repl,
    resampling = resampling)

  # we pass the task to make_learner() so we can skip some robustify steps
  # we pass the resampling to make_learner to know when we need the metarobustify-step (for bootstrap)
  learner = make_learner(
    learner_id = instance$learner_id,
    learner_params = instance$learner_params[[1]],
    learner_name = instance$learner_name,
    task = task,
    resampling = resampling
  )

  # for bootstrap, we also need the train predictions for the location-shifted bootstrap method
  # so we can estimate the quantiles.
  # For two-stage bootstrap, we need it in principle on
  if (inherits(resampling, "ResamplingBootstrap")) {
    learner$predict_sets = union(learner$predict_sets, "train")
  }

  if (length(task$row_roles$holdout)) {
    # use to get the "truth"
    learner$predict_sets = union(learner$predict_sets, "holdout")
  }

  # probably we don't need to take repl and job.pars into account, because the datasets are randomly shuffled anyway,
  # but just to be sure.
  # This ensures that the resampling instances are the same when a resampling method is applied to learner A and B,
  # which reduces variance in the comparison between learners
  resampling_seed = abs(digest::digest2int(digest::sha1(list(job$algo.pars, job$prob.pars[c("data_id", "size")], job$repl))))
  # this allows us to reconstruct the resampling instance later (in case any of the above calls touch the RNG)
  withr::with_seed(seed = resampling_seed,
    resampling$instantiate(task)
  )

  # for good measure we specify the seed here as well
  rr = withr::with_seed(seed = resampling_seed + 1,
    resample(task, learner, resampling, store_models = FALSE, store_backends = FALSE)
  )

  convert_predictions = function(pred) pred$data

  result = list(
    resampling_seed = resampling_seed,
    test_predictions = map(rr$predictions("test"), convert_predictions)
  )

  if ("train" %in% learner$predict_sets) {
    result$train_predictions = map(rr$predictions("train"), convert_predictions)
  }

  if (task$task_type == "regr") {
    measures = msrs(paste0("regr.", c("mse", "mae", "std_mse", "percentual_mse")))
  } else if (task$task_type == "classif") {
    measures = msrs(paste0("classif.", c("acc", "bbrier", "logloss")))
  }

  if ("holdout" %in% learner$predict_sets) {
    result$holdout_scores = map_dtr(rr$predictions("holdout"), function(x) as.data.table(as.list(x$score(measures))))
  }

  return(result)
}

make_resample_result = function(i, jt, reg) {
  job_id = jt[i, "job.id"][[1L]]
  data_id = jt[i, "data_id"][[1L]]
  resampling_id = jt[i, "resampling_id"][[1L]]
  resampling_params = jt[i, "resampling_params"][[1L]][[1L]]
  learner_id = jt[i, "learner_id"][[1L]]
  learner_params = jt[i, "learner_params"][[1L]][[1L]]
  learner_name = jt[i, "learner_name"][[1L]]
  size = jt[i, "size"][[1L]]
  repl = jt[i, "repl"][[1L]]

  result = loadResult(job_id, reg = reg)

  job = makeJob(job_id, reg = reg)
  resampling_seed = result$resampling_seed

  resampling = make_resampling(resampling_id, list(resampling_params))
  task = make_task(data_id, size, repl, resampling = resampling)
  learner = make_learner(learner_id, learner_params, learner_name, task, resampling)
  withr::with_seed(resampling_seed, {
    resampling$instantiate(task)
  })

  predictions = lapply(seq_along(result$test_predictions), function(iter) {
    x = list(test = result$test_predictions[[iter]])
    if (!is.null(result$train_predictions)) {
      x$train = result$train_predictions[[iter]]
    }
    return(x)
  })

  data = as_result_data(
   task = task,
   learners = lapply(seq_len(resampling$iters), function(i) learner),
   predictions = predictions,
   resampling = resampling,
   iterations = seq_len(resampling$iters),
   learner_states = NULL,
   store_backends = TRUE)

  rr = ResampleResult$new(data)
  return(rr)
}


calculate_ci = function(name, inference, x, y, args, learner_id, task_name, size, repl) {

  if (is.na(y)) ids = list(x = x) else ids = list(x = x, y = y)
  # random subset for testing
  rrs = map(ids, function(job_id) {
   jt = EXPERIMENT_TBL[list(job_id), on = "job.id"]
   make_resample_result(i = 1, jt = jt, reg = EXPERIMENT_REG)
  })

  if (length(rrs) == 2) {
    stopifnot(rrs[[1]]$task$id == rrs[[2]]$task$id)
    stopifnot(rrs[[1]]$task$nrow == rrs[[2]]$task$nrow)
    stopifnot(all(rrs[[1]]$task$row_ids == rrs[[2]]$task$row_ids))
    stopifnot(all(rrs[[1]]$task$task_type == rrs[[2]]$task$task_type))
  }

  loss_fns = if (rrs[[1L]]$task$task_type == "classif") {
    list(
      logloss  = inferGE::logloss,
      bbrier   = inferGE::bbrier,
      zero_one = mlr3measures::zero_one
    )
  } else {
    list(
      ae              = mlr3measures::ae,
      se              = mlr3measures::se,
      percentual_se   = inferGE::percentual_se,
      standardized_se = inferGE::standardized_se
    )
  }

  params = c(rrs, list(alpha = 0.05))
  params = mlr3misc::insert_named(params, args)

  dt = map_dtr(seq_along(loss_fns), function(i) {
    tic()
    params = c(params, list(loss_fn = loss_fns[i]))
    ci = try(do.call(inference, args = params), silent = TRUE)
    toc()
    if (inherits(ci, "try-error")) {
      ci = data.table(estimate = NA, lower = NA, upper = NA, info = list(list(error = ci)))
    }
    x = cbind(
      data.table(
        measure = names(loss_fns)[i],
        learner = learner_id,
        task = task_name,
        size = size,
        repl = repl,
        iters = sum(map_int(rrs, "iters"))
      ), ci)

    return(x)
  }, .fill = TRUE)
}
