library(mlr3oml)
source(here::here("datamodels", "evaluation", "evaluate.R"))

collection = ocl(396)

data_ids = list_oml_data(data_id = collection$data_ids)[startsWith(name, "simulated_"), ][, data_id]

for (data_id in data_ids) {
  result = evaluate(data_id)

  pth = file.path(here("datamodels", "evaluation", "results"))
  if (!file.exists(pth)) {
    dir.create(pth, recursive = TRUE)
  }
  saveRDS(result, file.path(pth, paste0(data_id, ".rds")))
}
