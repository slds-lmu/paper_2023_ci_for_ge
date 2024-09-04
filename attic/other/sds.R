library(mlr3oml)
library(mlr3misc)

data_names <- list(
  "45570" = "higgs",
  "45689" = "adult",
  "45704" = "covertype",
  "45654" = "bates_classif_20",
  "45665" = "colon",
  "45668" = "bates_classif_100",
  "45669" = "breast",
  "45672" = "prostate",
  "45693" = "electricity",
  "45692" = "diamonds",
  "45694" = "physiochemical_protein",
  "45655" = "bates_regr_20",
  "45666" = "friedman1",
  "45667" = "bates_regr_100",
  "45670" = "chen_10_null",
  "45671" = "chen_10",
  "45695" = "sgemm_gpu",
  "45696" = "video_transcoding"
)

sds = imap(data_names, function(name, id) {
  odata = odt(as.integer(id), parquet = TRUE)
  target = odata$data[[odata$target_names]]
  if (is.numeric(target)) sd(target) else NA
})
