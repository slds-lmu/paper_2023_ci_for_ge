library(here)
source(here("datamodels/evaluation/evaluate.R"))

simulated_names = list.files(here("data", "simulated"), pattern = ".*\\.pq")

simulated_names = gsub(simulated_names, pattern = ".pq", replacement = "", fixed = TRUE)

for (name in simulated_names) {
  if (!file.exists(here("datamodels", "evaluation", "results", name, "result.rds"))) {
    main(name, 42)
  }
}
