library(data.table)
library(here)
library(ggplot2)

ys = paste0("density_", c("rpart", "linear", "ridge", "rf"), ".rds")
xs = lapply(ys, function(y) readRDS(here("results", y)))

plt = function(x, title) {
  ggplot(x, aes(x = risk, color = simulated)) + 
    geom_density() + 
    facet_wrap(vars(task), scales = "free") + 
    labs(
      title = "rpart"
    )
}

plt(xs[[1]], "rpart")
plt(xs[[2]], "linear")
plt(xs[[3]], "ridge")
plt(xs[[4]], "rf")
