test_that("infer_holdout works", {
  rr = resample(
    tsk("iris"),
    lrn("classif.rpart"),
    rsmp("holdout")
  )

  res = infer_holdout(rr)

})
