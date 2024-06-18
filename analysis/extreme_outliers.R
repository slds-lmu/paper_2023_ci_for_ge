library(data.table)
library(here)
library(ggplot2)

ci = readRDS(here("results", "ci.rds"))

tmp = ci[task == "video_transcoding" & learner == "linear" & size == 500 & measure == "se" & method == "bayle_10_all_pairs", ]

ggplot(tmp, aes(x = width)) + geom_boxplot()
ggsave(here("figures", "extreme_width_outlier.png"), width = 6, height = 4)
