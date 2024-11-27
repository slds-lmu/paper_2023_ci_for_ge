library(here)
source(here("analysis", "figures", "main", "setup.R"))
source(here("analysis", "figures", "main", "NewNames.R"))

ci_aggr_cort = ci_aggr[method=="cort_10",c("dgp","inducer","size","loss","task_type","width")]
names(ci_aggr_cort)[which(names(ci_aggr_cort)=="width")] <- "reference_width"

CW0 <- merge(ci_aggr,ci_aggr_cort,
            c("dgp","inducer","size","loss","task_type"), all = TRUE)
CW0$rel_width <- CW0$width/CW0$reference_width

CW <- CW0[loss %in% translate_losses("Squared", "Zero-One") &
                    as.character(dgp) %nin% susDGPs,
                  list(
                    mean_R = mean(cov_R,na.rm=TRUE),
                    mean_ER = mean(cov_ER,na.rm=TRUE),
                    mean_PQ = mean(cov_PQ,na.rm=TRUE),
                    rel_width=median(rel_width)
                  ),
                  by = c("method")]

MoI <- c("52cv","conz_10_15","cort_10","cort_100","cort_50","cv_10_allpairs",
         "cv_10_within","cv_5_allpairs","cv_5_within","cv_n","holdout_66",
         "holdout_90","ncv_200_5")

CW_lim <- CW[method%in%MoI,]

CW_lim[,suggested := ifelse(grepl("cort|ncv|conz", method), "Recommended", "Other")]

ggplot(CW_lim,aes(x=mean_ER,y=rel_width,label=method,color=suggested)) +
  geom_point() + ggrepel::geom_text_repel(hjust=-0.2, vjust=0,size=3) +
  xlab("Average coverage for expected risk") +
  ylab("Median CI width relative to cort_10") +
  scale_color_manual(values=c("darkslategray","blue"))+
  labs(color="") +
  theme_bw() + xlim(0.85,1)

ggsave("coverageVSwidth.png",width=9,height=5)


ggplot(CW0[method%in%MoI,],aes(x=cov_ER,y=rel_width,color=method)) +
  geom_point(alpha=0.3) + #geom_text(hjust=-0.2, vjust=0,size=1.5,color="blue") +
  xlab("Coverage for expected risk") +
  ylab("CI width relative to cort_10") +
  theme_bw() +
  ylim(0,10)









