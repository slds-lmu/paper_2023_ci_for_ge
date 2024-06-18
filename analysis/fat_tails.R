# fat tails?

library(mlr3oml)
library(ggplot2)
library(cowplot)

theme_set(theme_bw(base_size = 15))

options(mlr3oml.cache = TRUE)

# video transcoding

vt = odt(44974)

y_vt = data.table(y = vt$data[[vt$target_names]], dgp = "video_transcoding")

ggplot(y_vt, aes(x = y)) + geom_density()


# sgemm_gpu (regr)

ad = odt(44961)
y_ad = data.table(y = ad$data[[ad$target_names]], dgp = "sgemm_gpu")

ggplot(y_ad, aes(x = y)) + geom_bar()


# physiochemical protein
pp = odt(44963)
y_pp = data.table(y = pp$data[[pp$target_names]], dgp = "physiochemical_protein")
ggplot(y_pp, aes(x = y)) + geom_density()


# diamonds
di = odt(44979)
y_di = data.table(y = di$data[[di$target_names]], dgp = "diamonds")
ggplot(y_di, aes(x = y)) + geom_density()


y = rbindlist(list(y_vt, y_ad, y_pp, y_di))

plots = map(unique(y$dgp), function(.dgp) {
  ggplot(y[dgp == .dgp, ], aes(x = y)) + 
    facet_grid(cols = vars(dgp), scales = "free") + 
    geom_density()
})

plot_grid(plotlist = plots)

ggsave(here("figures", "y_dist_real_regr.png"))
