library(data.table)
library(mlr3)
library(inferGE)
library(mlr3misc)
library(mlr3oml)
library(ggplot2)

secs2years = function(x, n_reps = 1000) {
  sum(x) / 3600 / 24 / 365 * n_reps
}

estimate_iters = function(name, params, n) {
  task = as_task_regr(data.frame(y = rnorm(n), x = rnorm(n)), id = "test", target = "y")
  resampling = invoke(rsmp, .args = insert_named(list(.key = name), params))
  resampling$instantiate(task)$iters
}

jt = readRDS("~/gh/paper_2023_ci_for_ge/job_table.rds")

num_trees = map_int(jt$learner_params, function(x) if (!is.null(x$num.trees)) as.integer(x$num.trees) else 0L)
# we keep RF with 50 trees
jt = jt[num_trees != 100, ]
# we use cv glmnet
jt = jt[learner_id != "regr.glmnet" & learner_id != "classif.glmnet", ]
# was accidentlly included twice
jt = jt[resampling_name != "repeated_nested_cv", ]

# add dataset names from data ids
data_ids = unique(jt$data_id)
data_names = data.table(data_id = data_ids, data_name = map_chr(data_ids, function(data_id) odt(data_id)$name))

# estimate resampling iterations

jt$resampling_hash = map_chr(seq_len(nrow(jt)), function(i) {
  digest::digest(jt[i, c("resampling_id", "resampling_params")])
})
jt = merge(jt, data_names, by = "data_id")

tmp = jt[, .SD[1, ], by = c("resampling_hash", "size")]
tmp$iters = map_int(seq_len(nrow(tmp)), function(i) as.integer(estimate_iters(
  name = tmp[i, "resampling_id"][[1]],
  params = tmp[i, "resampling_params"][[1]][[1]],
  n = tmp[i, "size"][[1]]
)))

jt = merge(jt, tmp[, c("resampling_hash", "iters", "size")], by = c("resampling_hash", "size"))


secs2years(jt$time.running)
# -> 20.4 years

jt[, sum(time.running), by = "resampling_name"]
jt_culprit = jt[, list(total = sum(time.running)), by = "resampling_name"]
jt_culprit = jt_culprit[order(total), ]

sum(as.double(jt_culprit[11:15, ]$total)) / as.double(jt_culprit$total |> sum())

# -> 80% of the total runtime is used for bccv, austern, two-stage, conservative z,


# Question: What happens when we remove ranger for the expensive tasks?

jt_remove_ranger_expensive = jt[
  !((resampling_name %in% c("conservative_z", "austern_zhou", "two_stage", "bootstrap_ccv", "nested_cv")) &
    (learner_id %in% c("regr.ranger", "classif.ranger"))),
]

jt_remove_ranger_expensive$time.running |> secs2years()
jt$time.running |> secs2years()

# --> Not using ranger for the expensive tasks reduces the runtime by 50 %
# But I still think we should not do it


# Now my suggestion:

# 1) Nested CV only for small datasets
suggestion = jt[!(resampling_name == "nested_cv" & size > 100), ]

# 2) But for those we use 40 repeats, the test run used 10
suggestion[resampling_name == "nested_cv", time.running := time.running * 4 ]

# 3) two-stage with 40 reps, the test run used 20
suggestion[resampling_name == "two_stage", time.running := time.running * 2 ]

#) 4) Austern and Zhou with 5 folds, the test run used 10
suggestion[resampling_name == "austern_zhou", time.running := time.running * 0.5 ]

# 5) Bootstrap CCV with 40 repeats, the test run used 20
suggestion[resampling_name == "bootstrap_ccv", time.running := time.running * 2 ]

# 6) Conservative Z with 40 repeats, the test run used 20, but we half number of folds
suggestion[resampling_name == "conservative_z", time.running := time.running * 4 / 2]


p = ggplot(suggestion[, list(total = as.double(secs2years(sum(time.running)))), by = "resampling_name"], aes(x = resampling_name, y = total)) +
  geom_bar(stat = "identity") +
  coord_flip()

p

ggsave("~/Documents/suggestion_runtime_per_resamping_bccv100.png")


# bootstrap ccv is just too expensive for n = 100
suggestion[resampling_name == "bootstrap_ccv" & size > 50, time.running := 0]


p = ggplot(suggestion[, list(total = as.double(secs2years(sum(time.running)))), by = "learner_id"], aes(x = learner_id, y = total)) +
  geom_bar(stat = "identity")

p

ggsave("~/Documents/suggestion_runtime_per_learner.png")


p = ggplot(suggestion[, list(total = as.double(secs2years(sum(time.running)))), by = "resampling_name"], aes(x = resampling_name, y = total)) +
  geom_bar(stat = "identity") +
  coord_flip()

p

ggsave("~/Documents/suggestion_runtime_per_resamping.png")






# --> There is no dataset that uses way more runtime than others

ggplot(data = jt[, list(total_runtime = secs2years(as.double(time.running))), by = "learner_id"]) +
  geom_bar(aes(x = learner_id, y = total_runtime), stat = "identity") +
  labs(y = "Estimated runtime per learner (years)")

ggplot(data = jt[, list(total_runtime = secs2years(as.double(sum(time.running)))), by = "data_id"]) +
  geom_bar(aes(x = data_id, y = total_runtime), stat = "identity") +
  labs(y = "Estimated runtime per learner (years)")
