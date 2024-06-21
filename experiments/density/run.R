library(mlr3)
library(mlr3learners)
library(mlr3oml)
library(here)
library(inferGE)
library(mlr3misc)
library(data.table)
library(mlr3pipelines)
library(batchtools)

info = list(
  list(name = "adult", id = 1590),
  list(name = "video_transcoding", id = 44974),
  list(name = "sgemm_gpu", id = 44961),
  list(name = "physiochemical_protein", id = 44963),
  list(name = "diamonds", id = 44974)
  # list(name = "simul_adult", id = 45689),
  # list(name = "simul_video_transcoding", id = 45696),
  # list(name = "simul_sgemm_gpu", id = 45695),
  # list(name = "simul_physiochemical_protein", id = 45694),
  # list(name = "simul_diamonds", id = 45692)
)


f = function(info, size, reps) {
  nm = info$name
  id = info$id
  odata = odt(id, parquet = TRUE)

  ids = if (odata$nrow == 5100000) {
    1:100000
  } else if (nm == "adult") {
    (1:odata$nrow)[complete.cases(odata$data)]
  } else {
    1:odata$nrow
  }
  target = odata$target_names
  backend = as_data_backend(odata)

  tmpdata = backend$data(ids, backend$colnames)
  names(tmpdata)[names(tmpdata) == "mlr3_row_id"] = "..row_id"
  backend = as_data_backend(tmpdata, primary_key = "..row_id")
  task = if (is.factor(tmpdata[[target]])) {
    as_task_classif(backend, target = target)
  } else {
    as_task_regr(backend, target = target)
  }
  task$id = nm

  ho_task = task$clone(deep = TRUE)$filter(sample(task$row_ids, 10000L))
  task$filter(setdiff(task$row_ids, ho_task$row_ids))

  learner = switch(task$task_type,
    regr = lrn("regr.lm"),
    classif = lrn("classif.log_reg")
  )
  learner = as_learner(ppl("robustify") %>>% learner)
  learner$id = "linear"

  tbl = rbindlist(map(seq_len(reps), function(repl) {
    task = task$clone(deep = TRUE)
    task$filter(sample(task$row_ids, size = size))
    rr = resample(task, learner, rsmp("cv"))
    ci = infer_bayle(rr)
    learner$train(task)
    risk = learner$predict(ho_task)$score()
    cbind(ci, data.table(risk = risk, task = task$id, repl = repl, size = size))
  }))

  tbl[, let(
    hit = lower <= risk & upper >= risk,
    width = upper - lower
  )]

  tbl
}

makeRegistry(
  # "/gscratch/sfische6/benchmarks/ci_for_ge/real_vs_simul",
  NA,
  packages = c("mlr3", "mlr3learners", "mlr3pipelines", "mlr3db", "inferGE", "mlr3oml", "mlr3misc", "here", "duckdb", "DBI", "lgr", "data.table"),
)

batchMap(fun = f, info = info, more.args = list(reps = 100, size = 1000))

testJob(1)
