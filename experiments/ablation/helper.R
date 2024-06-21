make_tbl = function(.resampling_names) {
  reg_paths = c(
    "/gscratch/sfische6/benchmarks/ci_for_ge/final_resample",
    "/gscratch/sfische6/benchmarks/ci_for_ge/final_resample_ridge",
    "/gscratch/sfische6/benchmarks/ci_for_ge/final_resample_lm"
  #  "/gscratch/sfische6/benchmarks/ci_for_ge/final_resample_ablation"
  )

  tbl = rbindlist(map(reg_paths, function(reg_path) {
    exp_reg = loadRegistry(reg_path, make.default = FALSE)
    tbl = unwrap(getJobTable(reg = exp_reg))
    tbl = tbl[resampling_name %in% .resampling_names & size == 500, c("job.id", "repl", "size", "task_name", "learner_id", "data_id", "resampling_name")]
    tbl$reg_path = reg_path
    tbl
  }), fill = TRUE)
  tbl[, let(group = .GRP), by = c("size", "task_name", "learner_id")]

  setnames(tbl, c("task_name", "learner_id"), c("dgp", "learner"))

  return(tbl)
 }
