add_info = function(data, info, common_column, common_column_info = NULL) {
  result = info[data, on = list(dgp = "name_short")]
}

translate_loss <- function(loss) {
  switch(loss,
    "Squared" = "se",
     "Absolute" = "ae",
     "Perc. Sq." = "percentual_se",
     "Std. Sq." = "standardized_se",
     "Zero-One" = "zero_one",
     "Brier" = "bbrier",
     "Log-Loss" = "logloss"
  )
}

translate_losses <- function(...) {
  sapply(list(...), translate_loss)
}


make_baseplot = function(plotdata, y, input_evaluation, input_range, inducers, scales, colors, breaks) {
  plotdata$method = paste0(plotdata$method, " (", plotdata$iters, ")")
  if (input_evaluation == "Coverage Frequency") {
    output = ggplot(plotdata, aes_string(x = y, y = "dgp", shape = "dataset_type", color = "stand_width", label = "numwidth")) +
      geom_vline(xintercept = 0.95, color = "darkred", alpha = 0.6) +
      binned_scale(aesthetics = "color",
        scale_name = "stepsn",
        palette = function(x) colors,
        breaks = breaks,
        limits = c(min(breaks), max(breaks)),
        show.limits = TRUE,
        guide = "colorsteps"
      ) +
      geom_point() + geom_text(hjust = ifelse(plotdata[[y]] < input_range[1] + 0.1, -0.2, 1.2), vjust = 0, size = 2.5) +
      facet_wrap(as.formula(paste("~", "method+inducer")), scales = scales)
  } else {
    output = ggplot(plotdata, aes_string(x = y, y = "dgp", shape = "dataset_type", color = "stand_width", label = "numwidth")) +
      scale_colour_stepsn(colours = colors,
        limits = c(min(breaks), max(breaks)),
        guide = guide_coloursteps(even.steps = FALSE,
          show.limits = TRUE),
        breaks = breaks) +
      geom_point() + geom_text(hjust = ifelse(plotdata[[y]] < input_range[1] + 0.1, -0.2, 1.2), vjust = 0, size = 2.5)
  }
  return(output)
}


over_under <- function(x,coverage = c("over", "under"),alpha) {
  # Ensure 'choice' is one of the allowed values
  coverage <- match.arg(coverage)
  
  # Your function logic here
  if (coverage == "under") {
    x_under <- x[which(x<(1-alpha))]
    if(length(x_under)==0){return(0)
    }else{
    return(sum((1-alpha)-x_under)/((1-alpha)*length(x_under)))
    }
  } else {
    x_over <- x[which(x>(1-alpha))]
    if(length(x_over)==0){return(0)
    }else{
    return(sum(x_over-(1-alpha))/(alpha*length(x_over)))
    }
  }
}

over_under_cutoff <- function(x,coverage = c("over", "under"),alpha,cutoff){
  coverage <- match.arg(coverage)
  x_new <- x[which(x>cutoff)]
  n <- length(x_new)
  value <- ifelse(n>0,over_under(x_new,coverage,alpha),NA)
  return(list(value=value,n=n))
}




find_suspicious_dgps <- function(plotdata,y,cutoff,cutoff_method_check,CI_width){

  lower_than_cutoff <- unique(plotdata[plotdata[[y]]<cutoff & method %in%cutoff_method_check,]$dgp)
  if(length(lower_than_cutoff)>0){
    D1 <- data.frame(name=lower_than_cutoff,
                     reason=paste0("coverage< cutoff (value) for solid methods"),
                     value=cutoff)
  }else{D1 <- data.frame(name="none",reason="none",value="none")}
  
  larger_than_CI_width <- plotdata[,list(mean_width=mean(width)),by="dgp"]
  larger_than_CI_width <- larger_than_CI_width[which(plotdata[,list(mean_width=mean(width)),by="dgp"]$mean_width>CI_width),]
  if(nrow(larger_than_CI_width)>0){
    D2 <- data.frame(name=unique(larger_than_CI_width$dgp),
                     reason=paste0("average width >",CI_width,", specifically see value"),
                     value=round(unique(larger_than_CI_width$mean_width),2))
  }else{D2 <- data.frame(name="none",reason="none",value="none")}
  
  holdout_special <- unique(plotdata[plotdata[[y]]<0.9 & method == "holdout_90" & size == 10000,]$dgp)
  if(length(holdout_special)>0){
  D3 <- data.frame(name=unique(holdout_special),
                   reason=paste0("coverage< cutoff (value) for holdout_90 and size 10000"),
                   value=0.9)
      }else{D3 <- data.frame(name="none",reason="none",value="none")}
  
  return(rbind(D1,D2,D3))
}


initialize_nested_list <- function(lst, keys) {
  for (key in keys) {
    if (is.null(lst[[key]])) {
      lst[[key]] <- list()
    }
    lst <- lst[[key]]
  }
}

flatten_nested_list <- function(lst, parent_names = list(),fun=NULL) {
  do.call(rbind, lapply(names(lst), function(name) {
    current_data <- lst[[name]]
    current_parent_names <- c(parent_names, name)
    
    if (is.data.frame(current_data)) {
      # Create a data frame from parent names
      parent_df <- as.data.frame(t(current_parent_names), stringsAsFactors = FALSE)
      colnames(parent_df) <- paste0("Level", seq_along(current_parent_names))
      
      if(!is.null(fun)){current_data <- fun(current_data)}
      
      # Combine with the current data frame
      combined_df <- cbind(parent_df, current_data)
      return(combined_df)
    } else {
      # Recursively process nested lists
      return(flatten_nested_list(current_data, current_parent_names))
    }
  }))
}





