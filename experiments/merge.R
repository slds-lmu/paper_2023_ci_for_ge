library(batchtools)
library(mlr3misc)


reg = makeRegistry(Sys.getenv("MERGE_PATH"), packages = c("batchtools", "data.table"))

ii = ceiling(2808000 / 10000)

batchMap(i = seq_len(ii), fun = function(i) {
  reg_truth = loadRegistry(Sys.getenv("TRUTH_PATH"), make.default = FALSE)
  reg_proxy = loadRegistry(Sys.getenv("PROXY_PATH"), make.default = FALSE)
  reg_ci    = loadRegistry(Sys.getenv("CI_PATH"),    make.default = FALSE)
  
  jt_truth = unwrap(getJobTable(reg = reg_truth))
  jt_proxy = unwrap(getJobTable(reg = reg_proxy))
  #jt_ci    = unwrap(getJobTable(reg = reg_ci))
  
  tbl_truth = map_dtr(seq_len(nrow(jt_truth)), function(i) {
    loadResult(i, reg = reg_truth)
  }, .fill = TRUE)
  
  tbl_proxy = map_dtr(seq_len(nrow(jt_proxy)), function(i) {
    loadResult(i, reg = reg_proxy)
  })
  
  tbl_ci = map_dtr(seq((i - 1) * 10000 + 1, min(i * 10000, 2808000)), function(i) {
    loadResult(i, reg = reg_ci)
  }, .fill = TRUE)
  
  tbl_ci$resampling = map_chr(tbl_ci$method, function(method) {
    switch(method,
      austern_zhou_rep   = "rep_cv_5_5",
      austern_zhou       = "cv_5",
      bayle_5_within     = "cv_5",
      bayle_10_within    = "cv_10",
      bayle_5_all_pairs  = "cv_5",
      bayle_10_all_pairs = "cv_10",
      bayle_loo          = "loo",
      holdout_66         = "holdout_66",
      holdout_90         = "holdout_90", 
      "NOTHING"
    )
  })
  
  # the repeated cv resampling is calculated for all datasets, but
  # we only need the proxy quantity for the small datsets.
  tbl_proxy[size > 500L & resampling == "rep_cv_5_5", PQ := NA]
  
  final = merge(tbl_ci, tbl_proxy, by = c("task", "size", "repl", "learner", "resampling", "measure"), all.x = TRUE)
  final = merge(final, tbl_truth, by = c("task", "size", "repl", "learner", "measure"), all.x = TRUE)
  
  
  names(final)[names(final) == "resampling"] = "proxy_resampling"
  
  if (nrow(final) != nrow(tbl_ci)) warning("something went wrong with the merge, fix this")
  if (length(unique(tbl_proxy$PQ)) != length(unique(final$PQ))) warning("something went wrong with the proxy merge, fix this")
  if (length(unique(tbl_truth$R)) != length(unique(final$R))) warning("something went wrong with the truth, fix this")

  final
})		 


