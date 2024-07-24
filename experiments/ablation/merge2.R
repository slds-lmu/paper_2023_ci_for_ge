library(batchtools)
library(devtools)
library(data.table)
library(mlr3)
library(mlr3misc)
library(inferGE)
library(here)


source(here("experiments", "ablation", "helper.R"))

methods = c("ncv", "conz")

reg = makeRegistry("/gscratch/sfische6/benchmarks/ci_for_ge/final_ablation_merge_cheap", packages = c("data.table", "batchtools", "mlr3misc"))
batchExport(list(make_tbl = make_tbl))

merged = batchMap(method = methods, function(method) {
pth = switch(method,
  ncv = "ABLATION_NCV_CHEAP",
  conz = "ABLATION_CONZ_CHEAP"
)

if (method == "ncv") {
	reg_path = "/gscratch/sfische6/benchmarks/ci_for_ge/final_resample_more"
	exp_reg = loadRegistry(reg_path, make.default = FALSE)
	tbl = unwrap(getJobTable(reg = exp_reg))
	tbl = tbl[resampling_name %in% "nested_cv", c("job.id", "repl", "size", "task_name", "learner_id", "data_id", "resampling_name")]
	tbl$reg_path = reg_path
	tbl[, let(group = .GRP), by = c("size", "task_name", "learner_id")]
	setnames(tbl, c("task_name", "learner_id"), c("dgp", "learner"))
} else if (method == "conz") {
	reg_path = "/gscratch/sfische6/benchmarks/ci_for_ge/final_resample_more"
	exp_reg = loadRegistry(reg_path, make.default = FALSE)
	tbl = unwrap(getJobTable(reg = exp_reg))
	tbl = tbl[resampling_name %in% "conservative_z", c("job.id", "repl", "size", "task_name", "learner_id", "data_id", "resampling_name")]
	tbl$reg_path = reg_path
	tbl[, let(group = .GRP), by = c("size", "task_name", "learner_id")]
	setnames(tbl, c("task_name", "learner_id"), c("dgp", "learner"))
} else stop()

if (method == "ncv") tbl = as.data.table(map(tbl, function(col) rep(col, each = 10L)))
if (method == "conz") tbl = as.data.table(map(tbl, function(col) rep(col, each = 9L)))

  reg = loadRegistry(Sys.getenv(pth))

  ids = findDone()[[1L]]

  res = rbindlist(map(ids, loadResult))
  if (nrow(res) != nrow(tbl)) stop("bug")

  res = cbind(res, tbl)
})

submitJobs(resources = list(memory = 512 * 256, clusters = "moran-bigmem"))
