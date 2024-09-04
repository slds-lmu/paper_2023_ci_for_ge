library(here)
library(ggplot2)
library(data.table)
library(mlr3oml)

theme_set(theme_bw())


files = list.files(here("results", "density-estimate"))
ids = as.integer(gsub(".rds", "", files))
names = map_chr(ids, function(id) odt(id)$name)
names = gsub("simulated_", "", names)

res = pmap(list(file = files, name = names), function(file, name) {
  result = readRDS(here("results", "density-estimate", file))$ge_distr
  result$dgp = name
  if ("classif.ce" %in% names(result)) {
    result$loss = result$classif.ce
    result$classif.ce = NULL
  } else {
    result$loss = result$regr.rmse
    result$regr.rmse = NULL
  }
  result
}) |> rbindlist()

res$dgp = ifelse(res$dgp == "sgemm_gpu_kernel_performance", "sgemm_gpu", res$dgp)

ggplot(res, aes(x = loss, color = task_id)) +
  facet_wrap(vars(dgp), nrow = 2, scales = "free") +
  geom_density() +
  labs(
    y = "Density",
    x = "Risk",
    color = "DGP"
  ) +
  theme(
    legend.position = c(0.9, 0.2),
    text = element_text(size = 8)
  )

ggsave(here("figures", "appendix", "appendix_risk_distribution.png"), dpi = 300, width = 6, height = 2)
