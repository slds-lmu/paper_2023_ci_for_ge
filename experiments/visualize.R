library(ggplot2)

tbl = readRDS(here::here("results", "test", "aggregated.rds"))
tbl_raw = readRDS(here::here("results", "test", "result.rds"))

tbl$inference_name = as.factor(tbl$inference_name)

#tbl1 = tbl[
#  task_id == "simulated_electricity" &
#]

plt_pe = ggplot(data = tbl, aes(x = size, color = inference_name, y = cov_pe)) + 
  geom_point() + 
  geom_line() + 
  #scale_color_fill() + 
  geom_errorbar(aes(ymin = cov_pe - 1 * cov_pe_se, ymax = cov_pe + 1 * cov_pe_se), width = .5) + 
  theme_bw() +
  facet_grid(cols = vars(task_id)) + 
  labs(
    title = "Coverage of PE"
  )

ggsave("plot_pe.png", plt_pe)

plt_epe = ggplot(data = tbl, aes(x = size, color = inference_name, y = cov_pe)) + 
  geom_point() + 
  geom_line() + 
  #scale_color_fill() + 
  geom_errorbar(aes(ymin = cov_pe - 1 * cov_pe_se, ymax = cov_pe + 1 * cov_pe_se), width = .5) + 
  theme_bw() +
  facet_grid(cols = vars(task_id)) + 
  labs(
    title = "Coverage of EPE"
  )

ggsave("plot_epe.png", plt_epe)


plt_width_pe = ggplot(data = tbl[size == 500, ], aes(x = width, y = cov_pe, color = inference_name)) +
  geom_point() + 
  facet_grid(cols = vars(task_id), scales = "free") + 
  labs(
    title = "Coverage of PE vs. Width (size: 500)"
  )
ggsave("plot_width_pe.png", plt_width_pe)


plt_pe_epe = ggplot(data = tbl, aes(x = cov_epe, y = cov_pe, color = size)) +
  geom_point() + 
  facet_grid(rows = vars(task_id)) + 
  labs(
    title = "Coverage of PE vs EPE"
  )

ggsave("plot_pe_epe.png", plt_pe_epe)

