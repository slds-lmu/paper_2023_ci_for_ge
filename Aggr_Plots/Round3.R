conz_aggr <- readRDS("Aggr_Plots/ablation/conz_aggr.rds")
cort_aggr <- readRDS("Aggr_Plots/ablation/cort_aggr.rds")
cv_aggr <- readRDS("Aggr_Plots/ablation/cv_aggr.rds")
ho_aggr <- readRDS("Aggr_Plots/ablation/ho_aggr.rds")
ncv_aggr <- readRDS("Aggr_Plots/ablation/ncv_aggr.rds")

library(ggeasy)

source("Aggr_Plots/setup.R")

dummy_p <- ggplot(data.frame(x=rep(c(1,2)),y=factor(rep(c(1,2),each=2))),
                  aes(x=x,y=y,linetype=y)) + geom_line(color="grey33") + theme_minimal() +
  scale_linetype_manual(
    values=c("solid","dotted"),
    labels= c("Cutoff for well-performing methods","Adjusted cutoff for re-gression on small data")) +
  labs(linetype="") + 
  theme(legend.position = "top",
        #legend.key.height = unit(3, "line"),
        #legend.key.width = unit(2, "line"),
        legend.text = element_text(size=10))

legend <- ggpubr::get_legend(dummy_p)
################################################################################



CONZ <- aggr_plot_conz(conz_aggr, inducers, DGPs, ylims=c(0.9,1),sds_tbls)

NCV <- aggr_plot_ncv(ncv_aggr, inducers, DGPs, ylims=c(0.9,1),sds_tbls)


plot_grid(legend,merge_plots(CONZ,heights=c(3,10)),
  nrow = 2, rel_heights = c(0.05,1)  # Adjust relative widths as needed
)
ggsave("Aggr_Plots/PNGs/CONZ.png",width=10,height=7)

plot_grid(legend,merge_plots(NCV,heights=c(3,10)),
          nrow = 2, rel_heights = c(0.05,1)  # Adjust relative widths as needed
)

ggsave("Aggr_Plots/PNGs/NCV.png",width=10,height=7)


P_conz_ncv <- ggarrange(NCV[[1]] + theme(legend.position = "bottom",
                                                 legend.box = "vertical",
                                                 plot.margin = margin(t=-10,l=5,r=5)),
                        NCV[[2]] + easy_remove_x_axis() + 
                          theme(legend.position = "top",
                                strip.background = element_blank(),
                                strip.text.x = element_blank(),
                                plot.margin = margin(l=5,r=5)
                          ))





