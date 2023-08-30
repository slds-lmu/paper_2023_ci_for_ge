test_that("infer_subsampling works", {
  rr = resample(
    tsk("iris"),
    lrn("classif.rpart"),
    rsmp("subsampling", ratio = 0.8, repeats = 10)
  )

  res = infer_subsampling(rr)

})
