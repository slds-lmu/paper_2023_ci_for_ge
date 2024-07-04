aggr_plot <- function(data, methods, inducers, DGPs, loss_regr, loss_classif, ylims=c(0,1)){
  data = data[as.character(method) %in% methods & as.character(dgp) %in% DGPs  & 
              measure %in% translate_losses(loss_regr, loss_classif),
              list(
                y_R = mean(cov_R),
                y_ER = mean(cov_ER),
                y_PQ = mean(cov_PQ)
              ), by = c("inducer", "method", "size")]
  min_size = min(ylims)
  max_size = max(ylims)
  
  #if(any(is.na(data$y_PQ))){
  #  columns <- c("y_R","y_ER")
  #}else{
   columns <- c("y_R","y_ER","y_PQ")
  #}
  plotdat <- pivot_longer(data, cols = all_of(columns),
                          names_to = "coverage_of", values_to = "value")
  
  output <- ggplot(plotdat,aes(x=size,y=value,color=inducer))+
    geom_hline(yintercept = 0.95,color="red") + 
    geom_line(aes(linetype=coverage_of)) +
    facet_wrap(method ~.,scales="free_x") + ylim(min_size,max_size) +
    theme_bw()
    
  return(output)
}