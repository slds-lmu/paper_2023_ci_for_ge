library(data.table)
library(ggplot2)
library(here)
library(mlr3misc)
library(ggh4x)
library(dplyr)
library(patchwork)
library(ggforce)


theme_set(theme_bw() + theme(text = element_text(size = 8)))

CI = readRDS(here("results", "clean", "ci.rds"))

dgpsOI = c("higgs","breast")#, "diamonds")
good_methods = c("cv_5","ho_90","cort_25_90","ncv_3_5","conz_10_5")
#ci_aggr = ci_aggr[task %in% problematic & method %in% good_methods, ]
CI = CI[dgp %in% dgpsOI & loss %in% c("zero_one", "se") & method %in% good_methods &
          size == 1000, ]

#12

CI$dgp <- factor(CI$dgp)
CI$inducer <- factor(CI$inducer)


custom_plot <- function(data, limits,ERdat) {
  ggplot(data, aes(x=R,y=estimate,color = inducer)) +
    geom_point(alpha=0.5) +
    xlim(limits$min_limit, limits$max_limit) +
    ylim(limits$min_limit, limits$max_limit) +
    ggtitle(paste(unique(ERdat$dgp), " | ", unique(ERdat$inducer))) +
    geom_vline(xintercept = as.numeric(unique(ERdat$ER)),color="darkblue") +
    theme_bw() + xlab("Risk") +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position="none") +
    facet_wrap(~method,nrow = 1)
}


# Calculate the limits
facet_limits <- function(data){ data %>%
  group_by(dgp, inducer) %>%
  summarize(
    min_limit = min(c(min(R), min(estimate))),
    max_limit = max(c(max(R), max(estimate)))
  ) %>%
  ungroup()}


plots <- list()
for (r in unique(CI$dgp)) {
  for (c in unique(CI$inducer)) {
    Interim <- CI %>% filter(#method == "cv_5" & 
                               dgp == r & inducer == c)
    limits <- facet_limits(Interim)
    p <- custom_plot(CI %>% #filter(method == "cv_5")%>%
                       mutate(
                         R = ifelse(dgp == r & inducer == c, R, NA),
                         estimate = ifelse(dgp == r & inducer == c, estimate, NA)
                         # Add more columns as needed
                       ), limits,Interim)
    plots[[paste(r, c)]] <- p
  }
}



# Combine plots into a grid
wrap_plots(plots,nrow=length(unique(CI$dgp)) * length(unique(CI$inducer)))


ggsave(here("figures", "appendix", "PEtest.png"),width=15,height=15)


