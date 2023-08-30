library(data.table)
library(mlr3)
library(mlr3learners)
library(mlr3pipelines)
library(mlr3db)
library(mlr3oml)
library(mlr3misc)
library(mlr3batchmark)
library(batchtools)

TEST = TRUE

if (is.null(getOption("mlr3oml.cache")) || isFALSE(getOption("mlr3oml.cache"))) {
  stop("Pleasure configure the option mlr3oml.cache to TRUE or a specific path.")
}

if (TEST) {
  REGISTRY_PATH = "/gscratch/sfische6/benchmarks/ci_for_ge/test"
} else {
  REGISTRY_PATH = "/gscratch/sfische6/benchmarks/ci_for_ge/final"
}

SEED = 42

if (!file.exists(REGISTRY_PATH)) {
  makeExperimentRegistry(
    file.dir = REGISTRY_PATH,
    seed = SEED,
    packages = c("mlr3", "mlr3learners", "mlr3pipelines"),
    work.dir = here::here("experiments")
  )
}


# here we have two sublists:
# * both: resamplings are applies to both the small and large setting
# * small: resamplings that are only applied in the small setting
#
# TODO: We need to check that all methods are covered by that:
# There are more methods than entries here (multiple methods for CV e.g.)
# For every resampling we do not only make predictions on the test set, bu also on the holdout set
<<<<<<< Updated upstream
resampling_ids = list(both = list(
  #holdout is the same as subsampling with 1 repetition
  holdout           = list(id = "holdout", params = list()),
  subsampling_10    = list(id = "subsampling", params = list(repeats = 10)),
  subsampling_50    = list(id = "subsampling", params = list(repeats = 30)),
  subsampling_100   = list(id = "subsampling", params = list(repeats = 30)),
  cv_10             = list(id = "cv", params = list(folds = 10)),
  repeated_cv_5_10  = list(id = "repeated_cv", params = list()),
  repeated_cv_10_10 = list(id = "repeated_cv", params = list()),
  nested_cv         = list(id = "nested_cv", params = list("TODO")),
  nadeau            = list("TODO"), # They use subsampling I believe
  # TODO: Nadeau has t2o tests
  diettrich         = list(id = "repeated_cv", params = list(repeats = 5, folds = 2)),
  # gives the true prediction ERROR
  # (use all train for test, only predict on holdout)
  prediction_error  = list(id = "holdout", params = list(ratio = 1))
=======
if (TEST) {
  resamplings_ids = list(both = list(
    holdout           = list(id = "holdout", params = list())
>>>>>>> Stashed changes
  ), small = list(
    loo               = list(id = "loo", params = list())
  ))

} else {
  resampling_ids = list(both = list(
    #holdout is the same as subsampling with 1 repetition
    holdout           = list(id = "holdout", params = list()),
    subsampling_10    = list(id = "subsampling", params = list(repeats = 10)),
    subsampling_50    = list(id = "subsampling", params = list(repeats = 30)),
    subsampling_100   = list(id = "subsampling", params = list(repeats = 30)),
    cv_10             = list(id = "cv", params = list(folds = 10)),
    repeated_cv_5_10  = list(id = "repeated_cv", params = list()),
    repeated_cv_10_10 = list(id = "repeated_cv", params = list()),
    nested_cv         = list(id = "nested_cv", params = list("TODO")),
    nadeau            = list("TODO"), # They use subsampling I believe
    diettrich         = list(id = "repeated_cv", params = list(repeats = 5, folds = 2)),
    # gives the true prediction error
    prediction_error  = list(id = "holdout", params = list(ratio = 1))
    ), small = list(
    loo               = list(id = "loo", params = list()),
    jiang             = list(id = "bootstrap_ccv")
    )
  )
}

if (TEST) {
  learner_ids = list(
    linear = list(
      classif = list(id = "log_reg", params = list()),
      regr    = list(id = "lm", params = list())
    ),
    linear_penalized = list(
      classif = list(id = "cv_glmnet", params = list()),
      regr    = list(id = "cv_glmnet", params = list())
    )
  )

} else {
  # the ids here are generic, i.e. they will be translated to clasisf and regr accordingly
  learner_ids = list(
    ranger_10 = list(
      classif = list(id = "ranger", params = list(num.trees = 10)),
      regr    = list(id = "ranger", params = list(num.trees = 10))
    ),
    ranger_100 = list(
      classif = list(id = "ranger", params = list(num.trees = 100)),
      regr    = list(id = "ranger", params = list(num.trees = 100))
    ),
    linear = list(
      classif = list(id = "log_reg", params = list()),
      regr    = list(id = "lm", params = list())
    ),
    linear_penalized = list(
      classif = list(id = "cv_glmnet", params = list()),
      regr    = list(id = "cv_glmnet", params = list())
    )
  )
}


# @param x An element from the learner_ids list.
# @param name The name from the learner_ids list.
# @param task The task for which to create the learner (classif or regr).
make_learner = function(x, name, task) {
  learner = do.call(lrn, 
    args = c(list(id = paste0(task$task_type, ".", x$id)), x$params)
  )

  if (task$task_type == "prob") {
    learner$predict_type = "prob"
  }


  graph = ppl("robustify", learner = learner)

  learner = as_learner(graph)
  learner$predict_sets = c("test", "holdout")
  learner$id = name

  return(learner)
}

if (TEST) {
  task_ids = list(
    bates_classif_20 = 45654,
    bates_regr_20 = 45655
  ) 
} else {
  # TODO: Update IDs
  # contains all classification tasks and their OpenML IDs
  task_ids = list(
    # -----------
    # classification: 
    # -----------

<<<<<<< Updated upstream
  # simulated with LLM
  adult = ,
  electricity = ,
  bank_marketing = ,
  covertype = ,

  # simplisically simulated
  bates_classif_100 = ,
  bates_classif_20 = ,

  # "real" (simulated with physical simulator)
  higgs = ,

  # simulated with covariance matrix
  prostate = ,
  colon = ,
  breast = ,
=======
    # simulated with LLM
    adult = 45638,
    electricity = 45642,
    bank_marketing = 45639,
    covertype = 45640,

    # simplisically simulated
    bates_classif_100 = 45628,
    bates_classif_20 = 45629,

    # "real" (simulated with physical simulator)
    higgs = 45645,

    # simulated with covariance matrix
    prostate = 45637,
    colon = 45635,
    breast = 45632,
>>>>>>> Stashed changes

    # -----------
    # regression:
    # -----------

<<<<<<< Updated upstream
  # simulated with LLM
  diamonds = ,
  sgemm_gpu_kernel_performance = ,
  physiochemical_protein = ,
  # FIXME
  # video_transcoding = "TODO",

  # simplistically simulated
  bates_regr_100 = ,
  bates_regr_20 = ,
  chen_10 = 
)
=======
    # simulated with LLM
    diamonds = 45641,
    sgemm_gpu_kernel_performance = 45644,
    physiochemical_protein = 45643,
    # FIXME
    # video_transcoding = "TODO",

    # simplistically simulated
    bates_regr_100 = 45630,
    bates_regr_20 = 45631,
    chen_10 = 45634
  )
}

>>>>>>> Stashed changes

dataset_sizes = list(
  small = c(50, 100, 200),
  other = c(500, 1000, 2000, 5000)
)


make_task = function(name, data_id, size, replication) {
  task = tsk("oml", data_id = data_id, parquet = TRUE)
  task$id = paste(name, size, replication, sep = "_")
  task$row_roles$use = seq(1, size) + size * (replication - 1)
  task$row_roles$holdout = 5000001:5100000

  return(task)
}

# We start by creating a data.table
# task_id | learner_id | resampling_id | dataset_size
# ...     | ...        | ...           | ...
# * This table must already take into account which resamplings we want to apply for which dataset sizes
# * The learner_id is not task-dependent here (corresponding learner is created in make_learner)
# * Each row is repreated 1000 times

tasks = imap(unlist(task_ids), function(data_id, name) {
  map(dataset_sizes$small, function(size) {
    if (TEST) {
      replications = 2L
    } else {
      replications = 1000L
    }
    map(seq_len(replications), function(replication) {
      make_task(
        name = name,
        data_id = data_id,
        size = size,
        replication = replication
      )
    })
  })
})

resamplings = map(c("small", "other"), function(category) {
  imap(resampling_ids[[category]], function(x, name) {
    do.call(rsmp, args = c(list(id = x$id, x$params)))
  })
})

tasks = map(c("small", "other"), function(category) {
  imap(task_ids, function(dataset_id, name) {
    map(dataset_sizes[[category]], function(size) {
      map(1:1000, function(replication) {
        make_task(
          name = name,
          dataset_id = dataset_id,
          size = size,
          replication = replication
        )
      })
    })
  })
})

make_design = function(tasks, resamplings) {
  grid = CJ(task = seq_along(tasks), resampling = seq_along(resamplings))

  design = data.table(
    task       = tasks[grid$task],
    resampling = resamplings[grid$task],
  )
 
  # for nicer printer.
  design = mlr3:::set_data_table_class(design)
}

design_small = make_design(tasks$small, resamplings$small)
design_other = make_design(tasks$other, resamplings$other)

design = rbindlist(list(design_small, deisgn_other))

makeExperimentRegistry()