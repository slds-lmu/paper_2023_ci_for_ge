# contains all classification tasks and their OpenML IDs
task_ids = list(classif = list(
  # simulated with LLM
  adult = 45638,
  electricity = 45642,
  bank_marketing = 45639,
  covertype = 45640,

  # simplisically simulated
  bates_classif_100 = 45628,
  bates_classif_20 = 45629,

  # "real" (simulated with physical simulator)
  higgs = 45645,

  # simulated with covariance matrix
  prostate = 45637,
  colon = 45635,
  breast = 45632,
  ), regr = list(
  # simulated with LLM
  diamonds = 45641,
  sgemm_gpu_kernel_performance = 45644,
  physiochemical_protein = 45643,
  video_transcoding,

  # simplistically simulated
  bates_regr_100 = 45630,
  bates_regr_20 = 45631,
  chen_10 = 45634,
  )
)


make_task = function(x) {
  backend = as_duckdb_backend(file.path(path, paste0(task_id, ".pq")))
}