conz_aggr <- readRDS("Aggr_Plots/ablation/conz_aggr.rds")
cort_aggr <- readRDS("Aggr_Plots/ablation/cort_aggr.rds")
cv_aggr <- readRDS("Aggr_Plots/ablation/cv_aggr.rds")
ho_aggr <- readRDS("Aggr_Plots/ablation/ho_aggr.rds")
ncv_aggr <- readRDS("Aggr_Plots/ablation/ncv_aggr.rds")

library(ggeasy)

source("Aggr_Plots/setup.R")




CONZ <- aggr_plot_conz(conz_aggr, inducers, DGPs, ylims=c(0.9,1),sds_tbls)

NCV <- aggr_plot_ncv(ncv_aggr, inducers, DGPs, ylims=c(0.9,1),sds_tbls)


plot_grid(legend,merge_plots(CONZ,heights=c(3,10)),
  nrow = 2, rel_heights = c(0.05,1)  # Adjust relative widths as needed
)
#ggsave("Aggr_Plots/PNGs/CONZ.png",width=10,height=7)

plot_grid(legend,merge_plots(NCV,heights=c(3,10)),
          nrow = 2, rel_heights = c(0.05,1)  # Adjust relative widths as needed
)

#ggsave("Aggr_Plots/PNGs/NCV.png",width=10,height=7)

bottom_legend <- ggpubr::get_legend(CONZ[[1]] +   theme(legend.position = "right",
                                                        legend.box = "vertical",
                                                        legend.title.position = "left",
                                                        legend.box.just = "left",
                                                        legend.title = element_text(size=12),
                                                        legend.text = element_text(size=10)) 
)

                                    
P_conz_ncv <- ggarrange(NCV[[2]] + easy_remove_x_axis() +
                          theme(legend.position = "none",
                                strip.background.y = element_rect(fill = "white",color="white"),
                                strip.text.y = element_text(color="white"),
                                strip.background.x = element_blank(),
                                strip.text.x = element_blank(),
                                plot.margin = margin(t=20,l=15,r=5,b=-2)
                                ),
                        NCV[[1]] + theme(legend.position = "none",
                                         legend.box = "vertical",
                                         plot.margin = margin(t=-2,l=5,r=5,b=5)
                                         ),
                        CONZ[[2]] + easy_remove_x_axis() + 
                          theme(legend.position = "none",
                                strip.background.y = element_rect(fill = "white",color="white"),
                                strip.text.y = element_text(color="white"),
                                strip.background.x = element_blank(),
                                strip.text.x = element_blank(),
                                plot.margin = margin(l=15,r=5,t=5,b=-2)
                                ),
                        CONZ[[1]] + theme(legend.position = "none",
                                legend.box = "vertical",
                                plot.margin = margin(t=-2,l=5,r=5,b=5),
                                strip.text.x = element_text(margin = margin(1,1,1,1))),
                        bottom_legend  
                        
                          ,nrow=5, heights=c(3,7,2,7,7))

P_conz_ncv

#plot_grid(legend,P_conz_ncv,
#          nrow = 2, rel_heights = c(0.025,1)  # Adjust relative widths as needed
#)
#ggsave("Aggr_Plots/PNGs/Ablation_small.png",width=10,height=9)



CV <- aggr_plot_cv(cv_aggr, inducers, DGPs, ylims=c(0.6,1),sds_tbls)
HO <- aggr_plot_ho(ho_aggr, inducers, DGPs, ylims=c(0.6,1),sds_tbls)
CORT <- aggr_plot_cort(cort_aggr, inducers, DGPs, ylims=c(0.6,1),sds_tbls)


P_cv_ho_cort <- ggarrange(HO[[2]] + easy_remove_x_axis() + 
                          theme(legend.position = "none",
                                strip.background.y = element_rect(fill = "white",color="white"),
                                strip.text.y = element_text(color="white"),
                                strip.background.x = element_blank(),
                                strip.text.x = element_blank(),
                                plot.margin = margin(t=5,l=-5,r=5,b=-2)
                                ),
                        HO[[1]] + theme(legend.position = "none",
                                         legend.box = "vertical",
                                        plot.margin = margin(t=-2,l=5,r=5,b=5)
                                        ),
                        CV[[2]] + easy_remove_x_axis() + 
                          theme(legend.position = "none",
                                strip.background.y = element_rect(fill = "white",color="white"),
                                strip.text.y = element_text(color="white"),
                                strip.background.x = element_blank(),
                                strip.text.x = element_blank(),
                                plot.margin = margin(l=-5,r=5,t=5,b=-2)
                                ),
                        CV[[1]] + theme(legend.position = "none",
                                        legend.box = "vertical",
                                        plot.margin = margin(t=-2,l=5,r=5,b=5)
                                        ),
                        CORT[[2]] + easy_remove_x_axis() + 
                          theme(legend.position = "none",
                                strip.background.y = element_rect(fill = "white",color="white"),
                                strip.text.y = element_text(color="white"),
                                strip.background.x = element_blank(),
                                strip.text.x = element_blank(),
                                plot.margin = margin(l=-5,r=5,t=5,b=-2)
                                ),
                        CORT[[1]] + theme(legend.position = "none",
                                          legend.box = "vertical",
                                          plot.margin = margin(t=-2,l=5,r=5,b=5),
                                          strip.text.x = element_text(margin = margin(1,1,1,1)))
                        
                        ,nrow=6, heights=c(2.5,7,2,7,2,7))

P_cv_ho_cort
#ggsave("Aggr_Plots/PNGs/Ablation_large.png",width=10,height=12)


top_legend <- ggpubr::get_legend(NCV[[2]]+theme(legend.position = "top"))



plot_grid(top_legend,
          plot_grid(P_conz_ncv,P_cv_ho_cort,nrow=1,labels = c("For small data (up to 500)", "(For large data (500 - 10.000)")),
           nrow = 2, rel_heights = c(0.05,1)  # Adjust relative widths as needed
          )





ggsave("Aggr_Plots/PNGs/ThirdRound.png",width=10,height=9)


