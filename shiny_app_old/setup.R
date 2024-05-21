ATOM_CHOICES = list(
  learner = c("linear", "ridge", "ridge_tuned", "ranger", "rpart"),
  task = c(
    "higgs",
    "adult",
    "covertype",
    "bates_classif_20",
    "colon",
    "bates_classif_100",
    "breast",
    "prostate",
    "electricity",
    "diamonds",
    "physiochemical_protein",
    "bates_regr_20",
    "friedman1",
    "bates_regr_100",
    "chen_10_null",
    "chen_10",
    "sgemm_gpu",
    "video_transcoding"
  ),
  method = c(
    "holdout_66",
    "holdout_90",
    "corrected_t_10",
    "corrected_t_50",
    "corrected_t_100",
    "bayle_5_within",
    "bayle_5_all_pairs",
    "bayle_10_within",
    "bayle_10_all_pairs",
    "dietterich",
    "oob_10",
    "632plus_10",
    "oob_50",
    "ls_bootstrap_50",
    "632plus_50",
    "oob_100",
    "ls_bootstrap_100",
    "632plus_100",
    "nested_cv",
    "conservative_z",
    "ts_bootstrap",
    "bayle_loo",
    "austern_zhou",
    "austern_zhou_rep",
    "bccv",
    "bccv_bias",
    "oob_500",
    "oob_1000",
    "632plus_500",
    "632plus_1000"
  )
)


capitalize = function(x) {
  paste0(toupper(substring(x, 1, 1)), substring(x, 2))
}

translate_target = function(target) {
  switch(target,
    "Risk" = "R",
    "Expected Risk" = "ER",
    "Proxy Quantity" = "PQ",
    stop("not available")
  )
}
