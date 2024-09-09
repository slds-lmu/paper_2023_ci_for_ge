library(data.table)
library(here)
library(mlr3misc)
library(ggplot2)
library(cowplot)
library(ggh4x)

theme_set(theme_bw())

files = list.files(here("results", "ablation"))
files = files[grepl("aggr", files, fixed = TRUE)]
nms = gsub("_aggr.rds", "", files, fixed = TRUE)

tbls = map(files, function(x) readRDS(here("results", "ablation", x)))
names(tbls) = nms

tbls = map(tbls, function(tbl) {
  tbl$Learner = map_chr(as.character(tbl$inducer), function(l) {
    switch(l,
      lm_or_logreg = "Linear/Logistic\nRegression",
      decision_tree = "Decision Tree",
      random_forest = "Random Forest",
      ridge_lm_or_logreg = "Ridge-penalized\nLinear/Logistic Regression"
    )
  })
  tbl
})


#---- Conservative Z

conz = tbls$conz

f11 = function(conz, .size) {
  conz_classif = conz[task_type == "classif" & size == .size]
  conz_classif = conz_classif[, list(
    average_sd_width = mean(sd_width),
    average_median_width = mean(median_width),
    average_cov_R = mean(cov_R),
    average_cov_ER = mean(cov_ER)
  ), by = c("inner_reps", "outer_reps", "Learner")]

  conz_classif = melt(conz_classif, measure.vars = c("average_sd_width", "average_median_width", "average_cov_R", "average_cov_ER"),
                      value.name = "value", variable.name = "metric")

  conz_classif$metric = map_chr(conz_classif$metric, function(x) {
    switch(as.character(x), average_sd_width = "SD Width",
           average_median_width = "Median Width",
           average_cov_R = "Cov. Risk", "Cov. Exp. Risk")
  })

  learners = c(
    "Linear/Logistic\nRegression",
    "Random Forest",
    "Ridge-penalized\nLinear/Logistic Regression",
    "Decision Tree"
  )

  scales1fill = list(
    scale_fill_continuous(limits = c(0.91, 0.99), breaks = c(0.91, 0.95, 0.99)),
    scale_fill_continuous(limits = c(0.91, 0.99), breaks = c(0.91, 0.95, 0.99)),
    scale_fill_continuous(limits = c(0.08, 0.15)),
    scale_fill_continuous(limits = c(0.01, 0.05), breaks = c(0.01, 0.03, 0.05))
  )

  metrics = c("Cov. Exp. Risk", "Cov. Risk", "Median Width", "SD Width")
  map(learners, function(.learner) {

    map(seq_along(metrics), function(i) {
      .metric = metrics[[i]]
      p3 = ggplot(conz_classif[Learner == .learner & metric == .metric,], aes(x = inner_reps, y = outer_reps, fill = value)) +
        facet_wrap(vars(metric), scales = "free", nrow = 1) +
        scales1fill[[i]] +
        geom_tile() +
        theme(legend.position = "top") +
        labs(
          y = "Outer Repetitions",
          x = "Inner Repetitions",
          fill = "    "
        )
    })
  })
}

linear <- ggplot() +
  geom_text(aes(x = 0.5, y = 0.5, label = "Logistic Regression"), size = 3.5) +
  theme_void() +
  theme(plot.margin = margin(0, 0, -0.2, 0, "cm"))
ridge <- ggplot() +
  geom_text(aes(x = 0.5, y = 0.5, label = "Ridge-penalized Log. Regr."), size = 3.5) +
  theme_void() +
  theme(plot.margin = margin(0, 0, -0.2, 0, "cm"))
rf <- ggplot() +
  geom_text(aes(x = 0.5, y = 0.5, label = "Random Forest"), size = 3.5) +
  theme_void() +
  theme(plot.margin = margin(0, 0, -0.2, 0, "cm"))
dt <- ggplot() +
  geom_text(aes(x = 0.5, y = 0.5, label = "Decision Tree"), size = 3.5) +
  theme_void() +
  theme(plot.margin = margin(0, 0, -0.2, 0, "cm"))
empty <- ggplot() +
  geom_text(aes(x = 0.5, y = 0.5, label = ""), size = 3.5) +
  theme_void() +
  theme(plot.margin = margin(0, 0, 0, 0, "cm"))

ps = f11(conz, 500)

l1 = ggpubr::get_legend(ps[[1]][[1]])
l2 = ggpubr::get_legend(ps[[1]][[2]])
l3 = ggpubr::get_legend(ps[[1]][[3]])
l4 = ggpubr::get_legend(ps[[1]][[4]])
plot_grid(
  l1,
  l2,
  l3,
  l4,
  linear, empty, empty, empty,
  ps[[1]][[1]] + theme(legend.position = "none") + labs(x = NULL),
  ps[[1]][[2]] + theme(legend.position = "none") + labs(x = NULL, y = NULL),
  ps[[1]][[3]] + theme(legend.position = "none") + labs(x = NULL, y = NULL),
  ps[[1]][[4]] + theme(legend.position = "none") + labs(x = NULL, y = NULL),
  rf, empty, empty, empty,
  ps[[2]][[1]] + theme(legend.position = "none", strip.text = element_blank()) + labs(x = NULL),
  ps[[2]][[2]] + theme(legend.position = "none", strip.text = element_blank()) + labs(x = NULL, y = NULL),
  ps[[2]][[3]] + theme(legend.position = "none", strip.text = element_blank()) + labs(x = NULL, y = NULL),
  ps[[2]][[4]] + theme(legend.position = "none", strip.text = element_blank()) + labs(x = NULL, y = NULL),
  ridge, empty, empty, empty,
  ps[[3]][[1]] + theme(legend.position = "none", strip.text = element_blank()) + labs(x = NULL),
  ps[[3]][[2]] + theme(legend.position = "none", strip.text = element_blank()) + labs(x = NULL, y = NULL),
  ps[[3]][[3]] + theme(legend.position = "none", strip.text = element_blank()) + labs(x = NULL, y = NULL),
  ps[[3]][[4]] + theme(legend.position = "none", strip.text = element_blank()) + labs(x = NULL, y = NULL),
  dt, empty, empty, empty,
  ps[[4]][[1]] + theme(legend.position = "none", strip.text = element_blank()),
  ps[[4]][[2]] + theme(legend.position = "none", strip.text = element_blank()) + labs(y = NULL),
  ps[[4]][[3]] + theme(legend.position = "none", strip.text = element_blank()) + labs(y = NULL),
  ps[[4]][[4]] + theme(legend.position = "none", strip.text = element_blank()) + labs(y = NULL),
  byrow = TRUE,
  ncol = 4,
  rel_heights = c(0.2, 0.18, 1, 0.18, 1, 0.18, 1, 0.18, 1)
)

ggsave(here("figures", "appendix", "appendix_ablation_conz_both_500.png"), dpi = 300, height = 7, width = 7)


f1 = function(conz, outer, inner, .size) {
  conz_classif = conz[task_type == "classif" & size == .size]
  conz_classif = conz_classif[, list(
    average_sd_width = mean(sd_width),
    average_median_width = mean(median_width),
    average_cov_R = mean(cov_R),
    average_cov_ER = mean(cov_ER)
  ), by = c("inner_reps", "outer_reps", "Learner")]


  conz_classif1 = conz_classif[outer_reps == outer, ]
  setnames(conz_classif1, "inner_reps", "repetitions")
  conz_classif1$outer_reps = NULL
  conz_classif1$rep_type = "inner"
  conz_classif2 = conz_classif[inner_reps == inner, ]
  conz_classif2$rep_type = "outer"
  setnames(conz_classif2, "outer_reps", "repetitions")
  conz_classif2$inner_reps = NULL
  conz_classif = rbind(conz_classif1, conz_classif2)

  conz_classif = melt(conz_classif, measure.vars = c("average_cov_R", "average_cov_ER", "average_median_width", "average_sd_width"),
                      value.name = "value", variable.name = "metric")
  
  conz_classif$metric = map_chr(conz_classif$metric, function(x) {
    switch(as.character(x), average_sd_width = "SD Width",
            average_median_width = "Median Width",
            average_cov_R = "Cov. Risk", "Cov. Exp. Risk")
  })


  p1 = ggplot(conz_classif[rep_type == "inner", ], aes(x = repetitions, y = value, color = Learner)) +
    facet_wrap(vars(metric), scales = "free", nrow = 1) +
    geom_line() +
    theme(legend.position = "top") +
    labs(
      y = "Value",
      x = "Inner Repetitions",
      color = "Inducer"
    )

  p2 = ggplot(conz_classif[rep_type == "outer", ], aes(x = repetitions, y = value, color = Learner)) +
    facet_wrap(vars(metric), scales = "free", nrow = 1) +
    geom_line() +
    theme(legend.position = "top") +
    labs(
      y = "Value",
      x = "Outer Repetitions",
      color = "Inducer"
    )

  list(p1, p2)
}

scales1 = list(
  scale_y_continuous(limits = c(0.93, 1.0)),
  scale_y_continuous(limits = c(0.93, 1.0)),
  scale_y_continuous(limits = c(0.1, 0.15)),
  scale_y_continuous(limits = c(0.01, 0.04))
)

p1 = f1(tbls$conz, 15, 15, 500)

p11 = p1[[1]] + scale_x_continuous(breaks = c(5, 25, 50)) + facetted_pos_scales(y = scales1)
p12 = p1[[2]] + scale_x_continuous(breaks = c(5, 25, 50)) + facetted_pos_scales(y = scales1)

legend = ggpubr::get_legend(p11)

plot_grid(
  ggdraw() + draw_grob(legend, x = 0.07, y = 0),
  p11 + theme(legend.position = "none"),
  p12 + theme(legend.position = "none"),
  ncol = 1, rel_heights = c(0.2, 1, 1),
  labels = c("", "Size 500", ""), vjust = -0.2, hjust = -0.1,
  label_fontface = "plain",
  label_size = 12
)

ggsave(here("figures", "appendix", "appendix_ablation_conz.png"), dpi = 300, height = 4, width = 7)

scales2 = list(
  scale_y_continuous(limits = c(0.82, 0.97)),
  scale_y_continuous(limits = c(0.82, 0.97)),
  scale_y_continuous(limits = c(0.01, 0.055)),
  scale_y_continuous(limits = c(0.004, 0.026))
)

p2 = f1(tbls$conz_cheap, 12, 10, 10000)
p21 = p2[[1]] + scale_x_continuous(breaks = c(1, 5, 10)) + facetted_pos_scales(y = scales2)
p22 = p2[[2]] + scale_x_continuous(breaks = c(2, 5, 12)) + facetted_pos_scales(y = scales2)

legend = ggpubr::get_legend(p21)

plot_grid(
  ggdraw() + draw_grob(legend, x = 0.07, y = 0),
  p21 + theme(legend.position = "none"),
  p22 + theme(legend.position = "none"),
  ncol = 1, rel_heights = c(0.2, 1, 1),
  labels = c("", "Size 10000", ""), vjust = -0.2, hjust = -0.1,
  label_fontface = "plain",
  label_size = 12
)

ggsave(here("figures", "appendix", "appendix_ablation_conz_cheap.png"), dpi = 300, height = 4, width = 7)

# verify that this also holds for regression, even though we don't include
# it in the plots because it is tricky because of the different scales

ggplot(conz[task_type == "regr" & inner_reps == 15 & size == 500, ], aes(x = outer_reps, y = median_width)) +
  facet_grid(vars(dgp), vars(Learner), scales = "free") +
  geom_line()

# median_width is not really influenced by outer reps

ggplot(conz[task_type == "regr" & inner_reps == 15 & size == 500, ], aes(x = outer_reps, y = sd_width_std_win)) +
  facet_grid(vars(dgp), vars(Learner), scales = "free") +
  geom_line()

# sd decreases with outer reps

ggplot(conz[task_type == "regr" & outer_reps == 15 & size == 500, ], aes(x = inner_reps, y = median_width)) +
  facet_grid(vars(dgp), vars(Learner), scales = "free") +
  geom_line()

# median width decreases with inner reps

ggplot(conz[task_type == "regr" & outer_reps == 15 & size == 500, ], aes(x = inner_reps, y = sd_width_std_win)) +
  facet_grid(vars(dgp), vars(Learner), scales = "free") +
  geom_line()


## cheap

ggplot(tbls$conz_cheap[task_type == "regr" & inner_reps == 10 & size == 10000, ], aes(x = outer_reps, y = median_width)) +
  facet_grid(vars(dgp), vars(Learner), scales = "free") +
  geom_line()

# median_width is not really influenced by outer reps

ggplot(tbls$conz_cheap[task_type == "regr" & inner_reps == 10 & size == 10000, ], aes(x = outer_reps, y = sd_width_std_win)) +
  facet_grid(vars(dgp), vars(learner), scales = "free") +
  geom_line()

# sd decreases with outer reps

ggplot(tbls$conz_cheap[task_type == "regr" & outer_reps == 12 & size == 10000, ], aes(x = inner_reps, y = median_width)) +
  facet_grid(vars(dgp), vars(Learner), scales = "free") +
  geom_line()

# median width decreases with inner reps

ggplot(tbls$conz_cheap[task_type == "regr" & outer_reps == 12 & size == 10000, ], aes(x = inner_reps, y = sd_width_std_win)) +
  facet_grid(vars(dgp), vars(Learner), scales = "free") +
  geom_line()

# sd of width decreases with inner reps


# ------- Nested CV

ncv = tbls$ncv

f2 = function(ncv, .size) {
  ncv = ncv[task_type == "classif" & size == .size, list(
    average_sd_width = mean(sd_width),
    average_median_width = mean(median_width),
    average_cov_R = mean(cov_R),
    average_cov_ER = mean(cov_ER)
  ), by = c("reps_outer", "Learner")]

  ncv = melt(ncv, measure.vars = c("average_cov_R", "average_cov_ER", "average_median_width", "average_sd_width"),
                      value.name = "value", variable.name = "metric")

  ncv$metric = map_chr(ncv$metric, function(x) {
    switch(as.character(x), average_sd_width = "SD Width",
           average_median_width = "Median Width",
           average_cov_R = "Cov. Risk", "Cov. Exp. Risk")
  })

  ncv$repetitions = ncv$reps_outer


  ggplot(ncv, aes(x = repetitions, y = value, color = Learner)) +
    facet_wrap(vars(metric), scales = "free", nrow =1) +
    theme(legend.position = "top") +
    geom_line() +
    labs(
      x = "Repetitions",
      y = "Value",
      color = "Inducer"
    )
}

pncv1 = f2(ncv, 500) + scale_x_continuous(breaks = c(10, 100, 200)) + facetted_pos_scales(
  y = list(
    scale_y_continuous(limits = c(0.966, 0.985)),
    scale_y_continuous(limits = c(0.966, 0.985)),
    scale_y_continuous(limits = c(0.08, 0.12)),
    scale_y_continuous(limits = c(0.007, 0.03))
  )
)


pncv2 = f2(tbls$ncv_cheap, 10000) + scale_x_continuous(breaks = c(1, 5, 10)) + facetted_pos_scales(
  y = list(
    scale_y_continuous(limits = c(0.87, 0.985)),
    scale_y_continuous(limits = c(0.87, 0.985)),
    scale_y_continuous(limits = c(0.0135, 0.024)),
    scale_y_continuous(limits = c(0.0037, 0.0083))
  )
)

legend = ggpubr::get_legend(pncv1)

plot_grid(
  ggdraw() + draw_grob(legend, x = 0.07, y = 0),
  pncv1 + theme(legend.position = "none"),
  pncv2 + theme(legend.position = "none"),
  ncol = 1, rel_heights = c(0.2, 1, 1),
  labels = c("", "Size 500", "Size 10000"), vjust = -0.2, hjust = -0.1,
  label_fontface = "plain",
  label_size = 12
)


ggsave(here("figures", "appendix", "appendix_ablation_ncv.png"), width = 7, height = 4, dpi = 300)

ggplot(ncv[task_type == "regr" & size == 500, ], aes(x = reps_outer, y = median_width)) +
  facet_grid(vars(dgp), vars(Learner), scales = "free") +
  geom_line()

# median width relatively contant

ggplot(ncv[task_type == "regr" & size == 500, ], aes(x = reps_outer, y = sd_width)) +
  facet_grid(vars(dgp), vars(Learner), scales = "free") +
  geom_line()

# sd of width decreases with inner reps

## cheap

ggplot(tbls$ncv_cheap[task_type == "regr" & size == 500, ], aes(x = reps_outer, y = median_width)) +
  facet_grid(vars(dgp), vars(Learner), scales = "free") +
  geom_line()

# median width relatively constant

ggplot(tbls$ncv_cheap[task_type == "regr" & size == 500, ], aes(x = reps_outer, y = sd_width)) +
  facet_grid(vars(dgp), vars(Learner), scales = "free") +
  geom_line()

# sd of width decreases with inner reps


# corrected T

f3 = function(tbl, .size) {
  tbl = tbl[task_type == "classif" & ratio == 0.9 & size == .size, list(
    average_sd_width = mean(sd_width),
    average_median_width = mean(median_width),
    average_cov_R = mean(cov_R),
    average_cov_ER = mean(cov_ER)
  ), by = c("reps", "Learner")]

  tbl = melt(tbl, measure.vars = c("average_cov_R", "average_cov_ER", "average_median_width", "average_sd_width"),
             value.name = "value", variable.name = "metric")

  tbl$metric = map_chr(tbl$metric, function(x) {
    switch(as.character(x), average_sd_width = "SD Width",
           average_median_width = "Median Width",
           average_cov_R = "Cov. Risk", "Cov. Exp. Risk")
  })

  ggplot(tbl, aes(x = reps, y = value, color = Learner)) +
    facet_wrap(vars(metric), scales = "free", nrow = 1) +
    theme(legend.position = "top") +
    geom_line() +
    labs(
      x = "Repetitions",
      y = "Value",
      color = "Inducer"
    )
}

p31 = f3(tbls$cort, 500) + scale_x_continuous(breaks = c(10, 50, 100)) + facetted_pos_scales(
  y = list(
    scale_y_continuous(limits = c(0.88, 0.97)),
    scale_y_continuous(limits = c(0.88, 0.97)),
    scale_y_continuous(limits = c(0.07, 0.165)),
    scale_y_continuous(limits = c(0.005, 0.065))
  )
)

legend = ggpubr::get_legend(p31)

p32 = f3(tbls$cort, 10000) + scale_x_continuous(breaks = c(10, 50, 100)) + facetted_pos_scales(
  y = list(
    scale_y_continuous(limits = c(0.88, 0.97)),
    scale_y_continuous(limits = c(0.88, 0.97)),
    scale_y_continuous(limits = c(0.014, 0.035)),
    scale_y_continuous(limits = c(0.0015, 0.016))
  )
)

p3 = plot_grid(
  ggdraw() + draw_grob(legend, x = 0.07, y = 0),
  p31 + theme(legend.position = "none") + labs(x = ""),
  p32 + theme(legend.position = "none"), ncol = 1, rel_heights = c(0.2, 1, 1),
  labels = c("", "Size 500", "Size 10000"), vjust = -0.5, hjust = -0.25,
  label_fontface = "plain",
  label_size = 12
  )

p3
ggsave(here("figures", "appendix", "appendix_ablation_cort.png"), width = 7, height = 4, dpi = 300)



# more reps smaller width

ggplot(tbls$cort[task_type == "regr" & size == 500 & ratio == 0.9, ], aes(x = reps, y = median_width)) +
  facet_grid(vars(dgp), vars(Learner), scales = "free") +
  geom_line()

ggplot(tbls$cort[task_type == "regr" & size == 10000 & ratio == 0.9, ], aes(x = reps, y = median_width)) +
  facet_grid(vars(dgp), vars(Learner), scales = "free") +
  geom_line()


# more reps lower sd

ggplot(tbls$cort[task_type == "regr" & size == 500 & ratio == 0.9, ], aes(x = reps, y = sd_width)) +
  facet_grid(vars(dgp), vars(Learner), scales = "free") +
  geom_line()

ggplot(tbls$cort[task_type == "regr" & size == 10000 & ratio == 0.9, ], aes(x = reps, y = sd_width)) +
  facet_grid(vars(dgp), vars(Learner), scales = "free") +
  geom_line()



