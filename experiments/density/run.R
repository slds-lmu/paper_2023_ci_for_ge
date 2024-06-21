library(mlr3)
library(mlr3learners)
library(mlr3oml)
library(here)
library(inferGE)
library(mlr3misc)
library(data.table)
library(mlr3pipelines)

set.seed(1)

size = 500
reps = 50

id_list = list(
  real = list(
    adult = 1590,
    video_transcoding = 44974,
    sgemm_gpu = 44961,
    physiochemical_protein = 44963,
    diamonds = 44974
  ),
  simulated = list(
    adult = 45689,
    video_transcoding = 45696,
    sgemm_gpu = 45695,
    physiochemical_protein = 45694,
    diamonds = 45692
  )
)

tasks_list = map(id_list, function(ids) {
  imap(ids, function(id, nm) {
    odata = odt(id, parquet = TRUE)

    odata = odt(id, parquet = TRUE)
    ids = if (odata$nrow = 5100000) {
      1:100000
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

    task
  })
})

ho_tasks_list = map(tasks_list, function(tasks) {
  map(tasks, function(task) {
    ho_task = task$clone(deep = TRUE)$filter(sample(task$row_ids, 10000L))
    task$filter(setdiff(task$row_ids, ho_task$row_ids))
    return(ho_task)
  })
})

tbl = rbindlist(pmap(list(tasks = tasks_lists, ho_tasks = ho_tasks_list), function(tasks, ho_tasks) {
  tbl = rbindlist(pmap(list(task = tasks, ho_task = ho_tasks), function(task, ho_task) {
    learner = switch(task$task_type,
      regr = lrn("regr.lm"),
      classif = lrn("classif.log_reg")
    )
    learner = as_learner(ppl("robustify", learner = learner, task = task) %>>% learner)
    learner$id = "linear"
    rbindlist(map(seq_len(reps), function(repl) {
      task = task$clone(deep = TRUE)
      task$filter(sample(task$row_ids, size = 1000L))
      rr = resample(task, learner, rsmp("holdout"))
      ci = infer_holdout(rr)
      learner$train(task)
      risk = learner$predict(ho_task)$score()
      cbind(ci, data.table(risk = risk, task = task$id, repl = repl, size = size))
    }))
  }))
}))


tbl[, let(
  hit = lower <= risk & upper >= risk
)]

tbl = tbl[, list(cov = mean(hit)), by = "task"]
