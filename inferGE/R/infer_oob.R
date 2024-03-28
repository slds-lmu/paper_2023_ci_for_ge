#' @export
infer_oob = function(x, alpha = 0.05, ...) {
  assert_alpha(alpha)
  UseMethod("infer_oob")
}

#' @export
infer_oob.ResampleResult = function(x, alpha = 0.05, loss_fn = NULL) {
  if (is.null(loss_fn)) loss_fn = default_loss_fn(x$task_type)
  assert_class(x$resampling, "ResamplingBootstrap")
  loss = names(loss_fn)

  n = x$task$nrow
  preds = x$predictions("test")
  loss_table = calculate_loss(preds, loss_fn)

  task_ids = x$task$row_ids
  # IDs that are in at least one test set
  test_ids = unique(unlist(map(seq_len(x$resampling$iters), function(iter) x$resampling$test_set(iter))))

  # useable ids are those that are in the test set at least once
  useable_ids = task_ids[task_ids %in% test_ids]
  Es = loss_table[, list(loss = mean(get(loss))), by = "row_id"]$loss

  # this disregards those that are not useable
  Err1 = mean(Es) # (37)

  # N[i, b] indicates how often `useable_ids[i]` is in the train set of iteration `b`
  N = as.matrix(map_dtc(seq_len(x$resampling$iters), function(b) {
    train_ids = x$resampling$train_set(b)
    map_int(useable_ids, function(id) sum(id == train_ids)) # before (15)
  }))
  # I[i, b] indicates whether `useable_ids[i]` is in the test set of iteration `b`
  I = N == 0 # (15)

  qs = map_dbl(seq_along(preds), function(b) {
    (1 / n) * loss_table[list(b), sum(get(loss)), on = "iter"] # (36)
  })

  Ds = map_dbl(seq_along(useable_ids), function(i) {
    (2 + 1 / (n - 1)) * (Es[i] - Err1) / n + sum((N[i, ] - mean(N[i, ])) * qs) / sum(I[i, ])
  })

  # we re-scale, as the missing terms are completely missing at random and hence we upscale the standard error
  SE_del = sqrt(n / length(useable_ids) * sum(Ds^2)) # (35)

  z = qnorm(1 - alpha / 2)

  data.table(
    estimate = Err1,
    lower = Err1 - z * SE_del,
    upper = Err1 + z * SE_del,
    info = list(list(SE_del = SE_del))
  )
}

