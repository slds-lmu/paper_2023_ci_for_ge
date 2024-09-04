library(here)
library(data.table)
library(ggplot2)
library(mlr3oml)

data = readRDS(here("results", "ci_small.rds"))
data = data[measure %in% c("se", "zero_one")]

methods = unique(data$method)
sizes = unique(data$size)
tasks = unique(data$task)

data = data[,
  .SD[estimate >= quantile(estimate, 0.025) &
      estimate <= quantile(estimate, 0.975) &
      R >= quantile(R, 0.025) & 
      R <= quantile(R, 0.975)],
  by = c("learner", "measure", "task", "size", "method")]
            


plot = function(data, .size, .method, .task) {
  ggplot(data[size == .size & method == .method & task == .task, ],
    aes(x = estimate, y = R), alpha = 0.5) +
    facet_grid(rows = vars(task), cols = vars(learner), scales = "free") + 
    geom_point() + 
    labs(
      title = sprintf("DGP: %s, method; %s n = %s, extreme values excluded",
                .task, .method, .size),
      x = "Point Estimate",
      y = "Risk"
    )
}

plot(data, 500, "bayle_10_all_pairs", "breast")

for (method in methods) {
  for (task in tasks) {
    for (size in sizes) {
      plt = plot(data, size, method, task)
      ggsave(plot = plt, width = 12, height = 6,
        filename = here("point_plots", sprintf("%s_%s_%s.png", method, task, size)))
    }
  }
}



physio = data[
  measure %in% c("se", "zero_one") &
  task == "physiochemical_protein" &
  size > 100 &
  method == "holdout_90"
]

video = data[
  measure %in% c("se", "zero_one") &
  task == "video_transcoding" &
  size > 100 &  
  method == "bayle_10_all_pairs"
]

diamonds = data[
  measure %in% c("se", "zero_one") &
    task == "diamonds" &
    size > 100 &  
    method == "bayle_10_all_pairs"
]



ggplot(physio[size == 10000, ], aes(x = estimate, y = R)) +
  facet_wrap(vars(learner), scales = "free") + 
  geom_point() + 
  labs(
    title = "Physiochemical protein, n = 10 000, extreme values included",
    x = "Holdout 90% estimate",
    y = "Risk"
  )
       

ggplot(physio[size == 10000, ], aes(x = estimate, y = R)) +
  facet_wrap(vars(learner), scales = "free") + 
  xlim(c(0, 50)) +
  geom_point() + 
  labs(
    title = "Physiochemical protein, n = 10 000, extreme values omitted",
    x = "Holdout 90% estimate",
    y = "Risk"
  )


ggplot(video[size == 500, ], aes(x = estimate, y = R)) +
  facet_wrap(vars(learner), scales = "free") + 
  geom_point() + 
  labs(
    title = "Video Transcoding, n = 500, extreme values included",
    x = "CV-10 estimate",
    y = "Risk"
  )


ggplot(video[size == 500, ], aes(x = estimate, y = R)) +
  facet_wrap(vars(learner), scales = "free") + 
  xlim(c(0, 200)) +
  ylim(c(0, 200)) +
  geom_point() + 
  labs(
    title = "Video Transcoding, n = 500, extreme values omitted",
    x = "CV-10 estimate",
    y = "Risk"
  )
       
ggplot(v[size == 500, ], aes(x = estimate, y = R)) +
  facet_wrap(vars(learner), scales = "free") + 
  geom_point() + 
  labs(
    title = "Diamonds, n = 500, extreme values included",
    x = "CV-10 estimate",
    y = "Risk"
  )


