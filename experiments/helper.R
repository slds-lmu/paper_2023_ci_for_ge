make_task = function(data_id, size, repl) {
  # because insample resampling is used to obtain 'true' PE, we use 100 000 holdout observation
  # all others are only used for proxy quantities, where we aggregate over multiple such estimates
  # only for holdout do we have one iter, so there we also use 100k
  con = dbConnect(duckdb::duckdb(), ":memory:", path = tempfile())
  holdout_ids_path = here::here("data", "splits", data_id, "holdout_100000.parquet")
  use_ids_path = here::here("data", "splits", data_id, paste0(size, ".parquet"))

  DBI::dbExecute(con, paste0("CREATE OR REPLACE VIEW holdout_table AS SELECT * FROM read_parquet('", holdout_ids_path, "')"))
  holdout_ids = dbGetQuery(con, paste0("SELECT row_id FROM holdout_table"))$row_id
  DBI::dbExecute(con, paste0("CREATE OR REPLACE VIEW use_table AS SELECT * FROM read_parquet('", use_ids_path, "')"))
  use_ids = dbGetQuery(con, sprintf("SELECT row_id FROM use_table WHERE iter = %i", repl))$row_id

  DBI::dbDisconnect(con, shutdown = TRUE)

  ids = c(use_ids, holdout_ids)

  odata = odt(data_id, parquet = TRUE)
  target = odata$target_names

  backend = as_data_backend(odata)
  tmpdata = backend$data(ids, backend$colnames)
  rm(backend)
  # mlr3 bug in cbind ...
  names(tmpdata)[names(tmpdata) == "mlr3_row_id"] = "..row_id"

  in_memory_backend = as_data_backend(tmpdata, primary_key = "..row_id")

  task = if (is.factor(tmpdata[[target]])) {
    as_task_classif(in_memory_backend, target = target)
  } else {
    as_task_regr(in_memory_backend, target = target)
  }

  task$id = odata$name

  # do not stratify here!

  task$row_roles$use = use_ids
  task$row_roles$holdout = holdout_ids

  return(task)
}

make_resampling = function(resampling_id, resampling_params) {
  resampling = do.call(rsmp, c(list(.key = resampling_id), resampling_params[[1]]))
}

make_learner = function(learner_id, learner_params, learner_name, task, resampling) {
  learner = do.call(lrn,
    args = c(list(.key = learner_id), learner_params)
  )


  # we need this because bootstrapping is broken with mlr3pipelines
  graph = ppl("robustify", learner = learner, task = task) %>>%
    learner

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
  learner$predict_sets = c("test", "holdout")
  learner$encapsulate = c(train = "try", predict = "try")
  learner$fallback = fallback
  learner$id = learner_name
  return(learner)
}

run_resampling = function(instance, resampling_id, resampling_params, job, ...) {
  mlr3::Task$set("public", "holdout_ratio", NULL, overwrite = TRUE)
  mlr3::Task$set("public", "holdout_rows", NULL, overwrite = TRUE)
  # don't do this at home
  mlr3::Task$set("active", "row_roles", function(rhs) {
    if (missing(rhs)) {
      roro = private$.row_roles
      if (!is.null(self$holdout_ratio)) {
        if (self$holdout_ratio == 1) {
          roro$holdout = self$holdout_rows
        } else {
          roro$holdout = sample(self$holdout_rows, size = self$holdout_ratio * length(self$holdout_rows))
        }
      }

      return(roro)
    }

    assert_has_backend(self)
    assert_list(rhs, .var.name = "row_roles")
    assert_names(names(rhs), "unique", permutation.of = mlr_reflections$task_row_roles, .var.name = "names of row_roles")
    rhs = map(rhs, assert_row_ids, .var.name = "elements of row_roles")

    private$.hash = NULL
    private$.row_roles = rhs
  }, overwrite = TRUE)


  lgr::get_logger("mlr3")$set_threshold("warn")
  task = make_task(data_id = instance$data_id, size = instance$size, repl = job$repl)
  resampling = make_resampling(resampling_id, resampling_params)

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
  # so we can estimate the quantiles
  is_bootstrap = inherits(resampling, "ResamplingBootstrap") || inherits(resampling, "ResamplingNestedBootstrap")
  if (is_bootstrap) {
    learner$predict_sets = union(learner$predict_sets, "train")
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

  holdout = task$row_roles$holdout

  task$holdout_ratio = 1 / resampling$iters
  task$holdout_rows = holdout

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
    measures = msrs(paste0("classif.", c("acc", "bacc", "bbrier", "logloss")))
  }

  result$holdout_scores = map_dtr(rr$predictions("holdout"), function(x) as.data.table(as.list(x$score(measures))))

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

  # FIXME: resampling_seed will be part of the result next time
  job = makeJob(job_id, reg = reg)
  resampling_seed = abs(digest::digest2int(digest::sha1(list(job$algo.pars, job$prob.pars[c("data_id", "size")], job$repl))))
  # resampling_seed = result$resamping_seed

  resampling = make_resampling(resampling_id, list(resampling_params))
  task = make_task(data_id, size, repl)
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
   iterations = resampling$iters,
   learner_states = NULL,
   store_backends = TRUE)


  rr = ResampleResult$new(data)
  browser()
  print(rr)
  return(rr)
}


calculate_ci = function(config) {
  name = config[[1]]
  inference = config[[2]]
  args = config[[3]]

  reg = loadRegistry(EXPERIMENT_PATH, writeable = FALSE, make.default = FALSE)

  job_tables = map(args, function(.resampling_name) {
    jt = unwrap(getJobTable(findDone(reg = reg), reg = reg))[list(.resampling_name), , on = "resampling_name"]
    jt[order(data_id, size, learner_id), ]
  })

  n = nrow(job_tables[[1L]])

  # here we need to reassemble the resample result
  map_dtr(seq_len(n), function(i) {
    rrs = map(job_tables, function(jt) {
      make_resample_result(i, jt, reg = reg)
    })
    names(rrs) = names(args)

    loss_fns = if (rrs[[1L]]$task$task_type == "classif") {
      list(
        logloss  = inferGE::logloss,
        bbrier   = inferGE::bbrier,
        zero_one = mlr3measures::zero_one
      )
    } else {
      list(
        ae              = mlr3measurs::ae,
        se              = mlr3measures::se,
        percentual_se   = inferGE::percentual_se,
        standardized_se = inferGE::standardized_se
      )
    }

    learner_id = job_tables[[1]][i, "learner_id"][[1]]
    task_name = job_tables[[1]][i, "task_name"][[1]]
    size = job_tables[[1]][i, "size"][[1]]

    dt = map_dtr(seq_along(loss_fns), function(i) {
      browser()
      x = cbind(
        data.table(
          measure = names(loss_fns)[i],
          learner = learner_id,
          task = task_name,
          size = size
        ),
        do.call(inference, args = c(rrs, list(alpha = 0.05, loss_fn = loss_fns[i])))
      )

      return(x)
    })

    return(dt)
  })
}
