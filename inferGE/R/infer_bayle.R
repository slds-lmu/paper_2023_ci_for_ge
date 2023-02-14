
### test under asymptotic normality Bayles 2020
### resampling: custom (but should be cv or loo)
normality_bayles = function(data_raw) {
}

#' @export
infer_bayle = function(x, alpha = 0.05, loss) {
  UseMethod("infer_bayle")
}

#' @export
infer_bayle.BenchmarkResult = function(x, alpha = 0.05, loss) {
  infer_method_bmr(x, alpha, loss, infer_bayle)
}


#' @export
infer_bayle.ResampleResult = function(x, alpha = 0.05, loss) {
  loss_fn = get_loss_fn(loss, x)
  loss_table = get_loss_table(x, loss_fn)

  n = NROW(loss_table)
  # CONT: Here is where I left
  k = length(levels(data_raw$iteration))
  r_ = setNames(data_raw[, sum(loss_fn), by = .(iteration)], c("iteration", "loss"))

  r_ = data_raw[, sum(loss)] / n
  data_raw[, diff_squared := (loss - r_)^2]
  sigma2_out = setNames(data_raw[, sum(diff_squared), by = .(iteration)], c("iteration", "diff_squared"))
  sigma2_out[, diff_squared := diff_squared * (k / n)]
  sigma2_out = sigma2_out[, sum(diff_squared)] / k
  ci_diff = qnorm(1 - 0.05 / 2) * (sqrt(sigma2_out) / sqrt(n))
  z = r_ / (sqrt(sigma2_out) / sqrt(n))
  list(r_ = r_, sigma2_out = sigma2_out, ci_0.95 = c(r_ - ci_diff, r_ + ci_diff), z = z, p.value = 2 * pnorm(- abs(z)))
}




# get_raw_data = function(benchmark_result, measure_id, return_differences = TRUE, return_is_in_train_set = FALSE) {
#   resample_result1 = benchmark_result$resample_result(1L)
#   resample_result2 = benchmark_result$resample_result(2L)
#   predictions1 = resample_result1$predictions()
#   predictions2 = resample_result2$predictions()
#
#   raw_data1 = data.table(
#     iteration = rep(seq_along(predictions1), map_int(predictions1, .f = function(x) length(x$row_ids))),
#     row_id = unlist(map(predictions1, "row_ids")),
#     loss = unlist(map(predictions1, get_pointwise_loss, measure_id = measure_id))
#   )
#   if (return_is_in_train_set) {
#     for (i in seq_len(resample_result1$resampling$iters)) {
#       raw_data1[, paste0("is_in_train_set", i) := as.integer(raw_data1$row_id %in% resample_result1$resampling$train_set(i))]
#     }
#   }
#
#   raw_data2 = data.table(
#     iteration = rep(seq_along(predictions2), map_int(predictions2, .f = function(x) length(x$row_ids))),
#     row_id = unlist(map(predictions2, "row_ids")),
#     loss = unlist(map(predictions2, get_pointwise_loss, measure_id = measure_id))
#   )
#   if (return_is_in_train_set) {
#     for (i in seq_len(resample_result2$resampling$iters)) {
#       raw_data2[, paste0("is_in_train_set", i) := as.integer(raw_data2$row_id %in% resample_result2$resampling$train_set(i))]
#     }
#   }
#
#   assert_true(identical(raw_data1[, c("iteration", "row_id")], raw_data2[, c("iteration", "row_id")]))
#
#   if (return_differences) {
#     raw_data = raw_data1
#     raw_data$loss = raw_data1$loss - raw_data2$loss
#   } else {
#     raw_data1$learner_id = resample_result1$learner$id
#     raw_data2$learner_id = resample_result2$learner$id
#     raw_data = rbind(raw_data1, raw_data2)
#     #raw_data$learner_id = factor(raw_data$learner_id, levels = benchmark_result$learners$learner_id) FIXME: for some reason this errors sometimes
#     raw_data$learner_id = factor(raw_data$learner_id, levels = benchmark_result$aggregate(measures = list(), conditions = TRUE)[["learner_id"]])
#   }
#   raw_data$iteration = as.factor(raw_data$iteration)
#   raw_data$row_id = as.factor(raw_data$row_id)
#   raw_data
# }
