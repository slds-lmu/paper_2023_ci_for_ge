library(mlr3oml)
source(here::here("datamodels", "evaluation", "evaluate.R"))

if (is.null(getOption("mlr3oml.parquet")) || isFALSE(getOption("mlr3oml.parquet"))) {
  stopf("Please configure mlr3oml to use the parquet data format")
}

collection = ocl(410)

data_ids = c(
  45689, # adult
  45704, # covertyp
  45693, # electricity
  45692, # diamonds
  45694, # physiochemical protein
  45695, # sgemm_gpu_kernel_perf
  45696  # video_transcoding
)

task_types = c(rep("classif", 4), rep("regr", 3))

pth = file.path(here("datamodels", "evaluation", "results"))

if (!file.exists(pth)) {
  dir.create(pth, recursive = TRUE)
}

for (i in seq_along(data_ids)) {
  data_id = data_ids[i]
  task_type = task_types[i]
  result = evaluate(data_id, task_type)

  saveRDS(result, file.path(pth, paste0(data_id, ".rds")))
}
