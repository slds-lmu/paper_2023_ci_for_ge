library(mlr3)
library(mlr3learners)
library(mlr3oml)
library(here)
library(inferGE)
library(mlr3misc)
library(data.table)
library(mlr3pipelines)

odata = odt(1590)

task = as_task(odata)
chunks = data.table(job.id = ids, chunk = batchtools::chunk(ids, chunk.size = 100))
learner = lrn("classif.log_reg")

graph = ppl("robustify", learner = learner, task = task)

taskout = graph$train(task)[[1L]]
