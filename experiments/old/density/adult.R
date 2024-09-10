library(mlr3)
library(mlr3learners)
library(mlr3oml)
library(here)
library(inferGE)
library(mlr3misc)
library(data.table)
library(mlr3pipelines)

odata = odt(1590)

data = odata$data[complete.cases(odata$data)]

task = as_task_classif(data, id = "adult", target = "class")


learner = lrn("classif.log_reg")

learner = as_learner(
  ppl("robustify", learner = learner, task = task) %>>%
    learner
)

logs = map(1:10, function(i) {
  task = task$clone(deep = TRUE)
  task$filter(sample(task$row_ids, 1000L))
  learner$train(task)
  learner$predict(task)
})

# learner$train(task)


# task = as_task(odata)
# data = 
# chunks = data.table(job.id = ids, chunk = batchtools::chunk(ids, chunk.size = 100))
# learner = lrn("classif.log_reg")

# graph = ppl("robustify", learner = learner, task = task)

# taskout = graph$train(task)[[1L]]
