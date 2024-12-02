library(here)
source(here("analysis", "figures", "main", "setup.R"))

highdim <- readRDS("~/Repositories/paper_2023_ci_for_ge/analysis/figures/additional/highdim.rds")
highdim_aggr <- highdim[, list(
  ER = ER[[1L]],
  cov_R = mean(lower <= R & upper >= R),
  cov_ER = mean(lower <= ER & upper >= ER),
  p = p[[1]],
  iters = iters[[1]],
  width=median(width)
), by = .(task, loss, method,learner)]


phighdim <- pivot_longer(highdim_aggr,cols=c("cov_ER","cov_R"),values_to = "Coverage",
                         names_to="Cof") %>%
  mutate(learner=recode(learner,"lasso"="Tuned Lasso (10-fold CV)","random_forest"="Random Forest"))
phighdim$Cof <- ifelse(phighdim$Cof=="cov_ER","Expected Risk","Risk")


p2 <- ggplot(phighdim[which(phighdim$loss=="zero_one"),],aes(x=p,y=Coverage,linetype=Cof,color=method))+
  geom_hline(yintercept=0.95,color="black") +
  geom_line() + facet_wrap(learner~., strip.position="bottom") +
  theme_bw() +
  labs(x="Number of features", linetype="",y="Average Coverage") + 
  theme(legend.position = "bottom",plot.margin = margin(t=0,l=15,r=5,b=10)) +
  ylim(0.8,1) 


p1 <- ggplot(phighdim[which(phighdim$loss=="zero_one"),],aes(x=p,y=width,fill=method))+
  geom_col(position="dodge",width=100) + facet_wrap(learner~., strip.position="bottom") +
  theme_bw() +
  labs(y="Median width", linetype="") + theme_classic() + easy_remove_x_axis() +
  theme(legend.position = "none",
        strip.background.y = element_rect(fill = "white",color="white"),
        strip.text.y = element_text(color="white"),
        strip.background.x = element_blank(),
        strip.text.x = element_blank(),
        plot.margin = margin(t=20,l=15,r=5,b=-2)
  )+
  scale_y_continuous(labels=function(x) sprintf("%.2f", x))

ggarrange(p1,p2,nrow=2,heights=c(1,3))


ggsave("figures/additional/highdim.png",width=8,height=5)

################################################################################

highdim_rf <- phighdim[which(phighdim$learner=="random_forest"),]
highdim_lasso <- phighdim[which(phighdim$learner=="lasso"),]

p2_lasso <- ggplot(highdim_lasso,aes(x=p,y=Coverage,linetype=Cof,color=method))+
  geom_hline(yintercept=0.95,color="black") +
  geom_line() + facet_wrap(loss~., strip.position="bottom") +
  theme_bw() +
  labs(x="Number of features", linetype="") + 
  theme(legend.position = "bottom",plot.margin = margin(t=0,l=15,r=5,b=10)) +
  ylim(0.8,1)


p1_lasso <- ggplot(highdim_lasso,aes(x=p,y=width,fill=method))+
  geom_col(position="dodge",width=100) + facet_wrap(loss~., strip.position="bottom") +
  theme_bw() +
  labs(y="Median width", linetype="") + theme_classic() + easy_remove_x_axis() +
  theme(legend.position = "none",
        strip.background.y = element_rect(fill = "white",color="white"),
        strip.text.y = element_text(color="white"),
        strip.background.x = element_blank(),
        strip.text.x = element_blank(),
        plot.margin = margin(t=20,l=15,r=5,b=-2)
  )+
  scale_y_continuous(labels=function(x) sprintf("%.2f", x),limits=c(0,0.15))

p2_rf <- ggplot(highdim_rf,aes(x=p,y=Coverage,linetype=Cof,color=method))+
  geom_hline(yintercept=0.95,color="black") +
  geom_line() + facet_wrap(loss~., strip.position="bottom") +
  theme_bw() +
  labs(x="Number of features", linetype="") + 
  theme(legend.position = "bottom",plot.margin = margin(t=0,l=15,r=5,b=10)) +
  ylim(0.8,1) 


p1_rf <- ggplot(highdim_rf,aes(x=p,y=width,fill=method))+
  geom_col(position="dodge",width=100) + facet_wrap(loss~., strip.position="bottom") +
  theme_bw() +
  labs(y="Median width", linetype="") + theme_classic() + easy_remove_x_axis() +
  theme(legend.position = "none",
        strip.background.y = element_rect(fill = "white",color="white"),
        strip.text.y = element_text(color="white"),
        strip.background.x = element_blank(),
        strip.text.x = element_blank(),
        plot.margin = margin(t=20,l=15,r=5,b=-2)
  )+
  scale_y_continuous(labels=function(x) sprintf("%.2f", x),limits=c(0,0.15))

ggarrange(p1_lasso+ggtitle("Inducer: Tuned Lasso (10-fold CV)"),p2_lasso,p1_rf+ggtitle("Inducer: Random Forest"),p2_rf,
          nrow=4,heights=c(1.5,3,1.5,3), common.legend = TRUE,legend = "bottom")

ggsave("figures/additional/highdim_several_losses.png",width=10,height=9)



