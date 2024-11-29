make_task_highdim = function(data_id, repl, resampling) {
  needs_holdout = inherits(resampling, "ResamplingInsample")
  dt = nanoparquet::read_parquet(sprintf("/gscratch/sfische6/highdim-data/%s_%d.parquet", data_id, repl))
  if (needs_holdout) {
    if (data_id == "highdim_6400") {
      # for some reason we cannot read the holdout data for 6.4k cols in a single go
      dtho1 = nanoparquet::read_parquet(sprintf("/gscratch/sfische6/highdim-data/%s_holdout_1.parquet", data_id))
      dtho2 = nanoparquet::read_parquet(sprintf("/gscratch/sfische6/highdim-data/%s_holdout_2.parquet", data_id))
      dtho = cbind(dtho1, dtho2)
    } else {
      dtho = nanoparquet::read_parquet(sprintf("/gscratch/sfische6/highdim-data/%s_holdout.parquet", data_id))
    }
    dt = rbind(dt, dtho)
  }
  data.table::setDT(dt)

  task = as_task_classif(id = data_id, x = as_data_backend(dt), target = "outcome")
  if (needs_holdout) {
    task$internal_valid_task = 1001L:nrow(dt)
    task$filter(1:1000)
  }

  return(task)
}

make_task = function(data_id, size, repl, resampling) {
  if (is.character(data_id) && startsWith(data_id, "highdim")) {
    task = make_task_highdim(data_id, repl, resampling)
    return(task)
  }

  con = dbConnect(duckdb::duckdb(), ":memory:", path = tempfile())

  # the ids for the data subset
  use_ids_path = here::here("data", "splits", data_id, paste0(size, ".parquet"))

  holdout_ids_path = here::here("data", "splits", data_id, "holdout_100000.parquet")
  DBI::dbExecute(con, paste0("CREATE OR REPLACE VIEW holdout_table AS SELECT * FROM read_parquet('", holdout_ids_path, "')"))

  # for some resamplings where we also calculate the proxy quantities / the true values,
  # we need a large holdout set to approximate them
  need_holdout = inherits(resampling, "ResamplingInsample") || inherits(resampling, "ResamplingCV") ||
    (inherits(resampling, "ResamplingRepeatedCV") && resampling$param_set$values$folds != 2) || inherits(resampling, "ResamplingHoldout") ||
    inherits(resampling, "ResamplingLOO")

  DBI::dbExecute(con, paste0("CREATE OR REPLACE VIEW use_table AS SELECT * FROM read_parquet('", use_ids_path, "')"))
  use_ids = dbGetQuery(con, sprintf("SELECT row_id FROM use_table WHERE iter = %i", repl))$row_id

  if (need_holdout) {
    holdout_ids = dbGetQuery(con, paste0("SELECT row_id FROM holdout_table"))$row_id
    ids = c(use_ids, holdout_ids)
  } else {
    ids = use_ids
  }

  DBI::dbDisconnect(con, shutdown = TRUE)

  print(list.files("/gscratch/sfische6/mlr3oml_cache/public/data_parquet"))
  odata = odt(data_id, parquet = TRUE)
  odata$.__enclos_env__$private$.parquet_path = sprintf("/gscratch/sfische6/mlr3oml_cache/public/data_parquet/%s.parquet", data_id)
  target = odata$target_names

  backend = as_data_backend(odata)


  # avoid using arff fallback if call to duckdb fails
  setTimeLimit(elapsed = 30)
  tmpdata = backend$data(ids, backend$colnames)
  setTimeLimit(elapsed = Inf)
  rm(backend)
  names(tmpdata)[names(tmpdata) == "mlr3_row_id"] = "..row_id"
  backend = as_data_backend(tmpdata, primary_key = "..row_id")

  # no stratification!
  task = if (is.factor(tmpdata[[target]])) {
    as_task_classif(backend, target = target)
  } else {
    as_task_regr(backend, target = target)
  }

  task$id = odata$name

  if (need_holdout) {
    task$internal_valid_task = holdout_ids
  }
  task$row_roles$use = use_ids

  return(task)
}


make_resampling = function(resampling_id, resampling_params) {
  resampling = do.call(rsmp, c(list(.key = resampling_id), resampling_params[[1]]))
}

time_resampling = function(instance, resampling_id, resampling_params, job, ...) {
  lgr::get_logger("mlr3")$set_threshold("warn")
  lgr::get_logger("mlr3tuning")$set_threshold("warn")
  lgr::get_logger("bbotk")$set_threshold("warn")
  resampling = make_resampling(resampling_id, resampling_params)
  task = make_task(data_id = instance$data_id, size = instance$size, repl = job$repl,
    resampling = resampling)

  task$row_roles$holdout = integer(0)

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
  if (inherits(resampling, "ResamplingBootstrap")) {
    learner$predict_sets = union(learner$predict_sets, "train")
  }

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

  # there is a weird bug in $score()
  rr$aggregate(msr("time_both")) * rr$resampling$iters
}


make_learner = function(learner_id, learner_params, learner_name, task, resampling) {
  # for untuned ridge regression we pre-computed some reasonable lambda	  
  if (grepl("(classif|regr)\\.glmnet", learner_id)) {
    magic_table = readRDS(here("data", "lambdas.rds"))
    magic_lambda = magic_table[name == task$id & size == task$nrow, "lambda"]$lambda
    if (!is.null(learner_params$lambda)) stop("don't overwrite the lambda")
    learner_params$lambda = magic_lambda
  }

  learner = do.call(lrn,
   args = c(list(.key = learner_id), learner_params)
  )

  graph = if (grepl("xgboost", learner_id) | grepl("mlp", learner_id)) {
    res = if (task$nrow <= 1000) {
      rsmp("cv", folds = 3)
    } else {
      rsmp("holdout", ratio = 2/3)
    }

    if (grepl("xgboost", learner_id)) {
      internal_search_space = NULL
      if (task$task_type == "classif") {
        learner$param_set$values$eval_metric = "logloss"
        search_space = ps(
          classif.xgboost.nrounds     = p_int(upper = 500, tags = "internal_tuning", aggr = function(x) as.integer(round(mean(unlist(x))))),
          classif.xgboost.max_depth   = p_int(lower = 2, upper = 12),
          classif.xgboost.alpha       = p_dbl(lower = 1e-8, upper = 1.0, logscale = TRUE),
          classif.xgboost.lambda      = p_dbl(lower = 1e-8, upper = 1.0, logscale = TRUE),
          classif.xgboost.eta         = p_dbl(lower = 0.01, upper = 0.3, logscale = TRUE)
        )
      } else {
        learner$param_set$values$eval_metric = "rmse"
        search_space = ps(
          regr.xgboost.nrounds     = p_int(upper = 500, tags = "internal_tuning", aggr = function(x) as.integer(round(mean(unlist(x))))),
          regr.xgboost.max_depth   = p_int(lower = 2, upper = 12),
          regr.xgboost.alpha       = p_dbl(lower = 1e-8, upper = 1.0, logscale = TRUE),
          regr.xgboost.lambda      = p_dbl(lower = 1e-8, upper = 1.0, logscale = TRUE),
          regr.xgboost.eta         = p_dbl(lower = 0.01, upper = 0.3, logscale = TRUE)
        )
      }
      
    } else if (grepl("mlp", learner_id)) {
      # we use 25 cores with 4 threads each
      learner$param_set$set_values(
        num_threads = 5L
      )
      if (task$task_type == "classif") {
        learner$param_set$set_values(
          measures_valid = msr("classif.logloss")
        )
        internal_search_space = ps(  
          classif.mlp.epochs              = p_int(upper = 500, tags = "internal_tuning", aggr = function(x) as.integer(round(mean(unlist(x)))))
        )
        search_space = ps(
          classif.mlp.p                 = p_dbl(lower = 0.0, upper = 0.5),
          classif.mlp.opt.lr            = p_dbl(lower = 1e-5, upper = 1e-2, logscale = TRUE),
          classif.mlp.opt.weight_decay  = p_dbl(lower = 1e-6, upper = 1e-3, logscale = TRUE, depends = (weight_decay == TRUE)),
          weight_decay                  = p_lgl(),
          n_layers                      = p_int(0, 3),
          latent                        = p_int(1, 256),
          .extra_trafo = function(x, param_set) {
            x$classif.mlp.neurons = rep(x$latent, x$n_layers)
            x$latent = NULL
            x$n_layers = NULL
            x$weight_decay = NULL
            return(x)
          }
        )
      } else {
        learner$param_set$values$measures_valid = msr("regr.rmse")
        internal_search_space = ps(
          regr.mlp.epochs              = p_int(upper = 500, tags = "internal_tuning", aggr = function(x) as.integer(round(mean(unlist(x)))))
        )
        search_space = ps(
          regr.mlp.p                 = p_dbl(lower = 0.0, upper = 0.5),
          regr.mlp.opt.lr            = p_dbl(lower = 1e-5, upper = 1e-2, logscale = TRUE),
          regr.mlp.opt.weight_decay  = p_dbl(lower = 1e-6, upper = 1e-3, logscale = TRUE, depends = (weight_decay == TRUE)),
          weight_decay                  = p_lgl(),
          n_layers                      = p_int(0, 3),
          latent                        = p_int(1, 256, depends = quote(n_layers %in% c(1, 2, 3))),
          .extra_trafo = function(x, param_set) {
            if (x$n_layers > 0) {
              x$regr.mlp.neurons = rep(x$latent, x$n_layers)
            }
            x$latent = NULL
            x$n_layers = NULL
            x$weight_decay = NULL
            return(x)
          }
        )
      }
    }

    graph = ppl("robustify", learner = learner, task = task) %>>% learner

    inner_learner = as_learner(graph)

    if (startsWith(learner_id, "classif")) {
      inner_learner$predict_type = "prob"
    }

    set_validate(inner_learner, validate = "test")

    # we use the validation score for the tuning
    inner_learner$predict_sets = NULL

    at = auto_tuner(
      learner = inner_learner,
      resampling = res,
      measure = msr("internal_valid_score", minimize = TRUE),
      internal_search_space = internal_search_space,
      search_space = search_space,
      terminator = trm("evals", n_evals = 50L),
      store_tuning_instance = TRUE,
      tuner = tnr("internal")
    )
    if (startsWith(learner_id, "classif")) {
      at$predict_type = "prob"
    }
    at
  } else {
    ppl("robustify", learner = learner, task = task) %>>%
      learner
  } 

  
  # we need this because bootstrapping is broken with mlr3pipelines
  if (inherits(resampling, "ResamplingBootstrap") || inherits(resampling, "ResamplingNestedBootstrap")
    || inherits(resampling, "ResamplingBootstrapCCV")) {

    # xgboost and neural networks are not applied to bootstrap as no bootstrap method
    # perfortmed well enough to justify the expensive experiment
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
  learner$encapsulate("try", fallback)
  learner$id = learner_id
  return(learner)
}

run_resampling = function(instance, resampling_id, resampling_params, job, ...) {
  print(torch::cuda_is_available())
  print(torch::cuda_device_count())

  lgr::get_logger("mlr3")$set_threshold("warn")
  lgr::get_logger("mlr3tuning")$set_threshold("warn")
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
  if (inherits(resampling, "ResamplingBootstrap")) {
    learner$predict_sets = union(learner$predict_sets, "train")
  }

  if (!is.null(task$internal_valid_task)) {
    # use to approximate the risk / proxy quantities
    learner$predict_sets = union(learner$predict_sets, "internal_valid")
  }

  # This ensures that the resampling instances are the same when a resampling method is applied to learner A and B,
  # which reduces variance in the comparison between learners
  resampling_seed = abs(digest::digest2int(digest::sha1(list(job$algo.pars, job$prob.pars[c("data_id", "size")], job$repl))))
  # this allows us to reconstruct the resampling instance later (in case any of the above calls touch the RNG)


  withr::with_seed(seed = resampling_seed,
    resampling$instantiate(task)
  )

  # TODO: Activate future parallelization when we are running the mlp.

  if (grepl("mlp", learner$id)) {
    future::plan(future::multisession, workers = 25)
  }

  # for good measure we specify the seed here as well
  rr = withr::with_seed(seed = resampling_seed + 1,
    resample(task, learner, resampling, store_models = FALSE, store_backends = FALSE)
  )

  # we need the resampling seed, so we can re-create them later to obtain the confidence intervals
  # saving them would require too much disk space
  result = list(
    resampling_seed = resampling_seed,
    test_predictions = map(rr$predictions("test"), function(x) x$data)
  )

  if ("train" %in% learner$predict_sets) {
    result$train_predictions = map(rr$predictions("train"), function(x) x$data)
  }

  # for the proxy quantities / risk
  if ("internal_valid" %in% learner$predict_sets) {
    if (task$task_type == "regr") {
      measures = msrs(paste0("regr.", c("mse", "mae", "std_mae", "percentual_mae", "winsorized_mse")))
      measures[[1]]$id = "se"
      measures[[2]]$id = "ae"
      measures[[3]]$id = "standardized_ae"
      measures[[4]]$id = "percentual_ae"
      measures[[5]]$id = "winsorized_se"

    } else if (task$task_type == "classif") {
      measures = msrs(paste0("classif.", c("ce", "bbrier", "logloss")))
      measures[[1]]$id = "zero_one"
      measures[[2]]$id = "bbrier"
      measures[[3]]$id = "logloss"
      if ("prob" %nin% learner$predict_type) {
        measures = measures[1:2]
      }
    }

    holdout_predictions = rr$predictions("internal_valid")
    result$holdout_scores = map_dtr(seq_along(holdout_predictions), function(i) {
      as.data.table(as.list(holdout_predictions[[i]]$score(measures, task = task, train_set = resampling$train_set(i))))
    })
  }

  return(result)
}

# jt is the job table from the resample experiments, i just an index and reg the registry from the resample experiments
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


# x and y represent ids of resample experiments
# Most inference methods need only one resample experiments, but some need two
calculate_ci = function(name, inference, x, y, z, args, learner_name, task_name, size, repl, resampling_name,
  loss_fns_classif = list(
    logloss  = inferGE::logloss,
    bbrier   = inferGE::bbrier,
    zero_one = mlr3measures::zero_one
  ),
  loss_fns_regr = list(
    ae              = mlr3measures::ae,
    se              = mlr3measures::se,
    percentual_ae   = inferGE::percentual_ae,
    standardized_ae = inferGE::standardized_ae,
    winsorized_se   = inferGE::winsorized_se
  )
) {
  ids = list(x = x, y = y, z = z)
  if (is.na(y)) ids$y = NULL
  if (is.na(z)) ids$z = NULL

  rrs = map(ids, function(job_id) {
   jt = EXPERIMENT_TBL[list(job_id), on = "job.id"]
   make_resample_result(i = 1, jt = jt, reg = EXPERIMENT_REG)
  })

  # some sanity checks
  if (length(rrs) == 2) {
    stopifnot(rrs[[1]]$task$id == rrs[[2]]$task$id)
    stopifnot(rrs[[1]]$task$nrow == rrs[[2]]$task$nrow)
    stopifnot(all(rrs[[1]]$task$row_ids == rrs[[2]]$task$row_ids))
    stopifnot(all(rrs[[1]]$task$task_type == rrs[[2]]$task$task_type))
  }

  loss_fns = if (rrs[[1L]]$task$task_type == "classif") {
    loss_fns_classif
  } else {
    loss_fns_regr
  }

  params = c(rrs, list(alpha = 0.05))
  params = mlr3misc::insert_named(params, args)

  dt = map_dtr(seq_along(loss_fns), function(i) {
    params = c(params, list(loss_fn = loss_fns[i]))
    ci = do.call(inference, args = params)
    if (inherits(ci, "try-error")) {
      ci = data.table(estimate = NA, lower = NA, upper = NA, info = list(list(error = ci)))
    }
    x = cbind(
      data.table(
        method = name,
        task_type = rrs[[1]]$task$task_type,
        measure = names(loss_fns)[i],
        learner = learner_name,
        task = task_name,
        size = size,
        repl = repl,
       	resampling = resampling_name,
        iters = sum(map_int(rrs, "iters"))
      ), ci)

    return(x)
  }, .fill = TRUE)
}
