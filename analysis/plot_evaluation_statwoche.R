library(here)
library(mlr3benchmark)
library(mlr3verse)
library(data.table)
library(ggplot2)


models = list.files(here("datamodels", "evaluation", "results"))

for (model in models) {
    result = readRDS(here("datamodels", "evaluation", "results", model, "result.rds"))
    ge_distr = result$ge_distr
    
    test_perf = result$test_perf
    
    rr1 = ge_distr$resample_result(1)
    rr2 = ge_distr$resample_result(2)
    
    task_type = rr1$task$task_type
    
    measure = switch(task_type, classif = "classif.ce", regr = "regr.mse")
    
    score_simul = as.data.table(rr1$score())
    score_orig = as.data.table(rr2$score())
    
    score = rbindlist(list(score_simul, score_orig))
    score = cbind(score, data.table(set = factor(c(rep("simul", nrow(score_simul)), rep("orig", nrow(score_orig))))))
    
    score = score[, c("iteration", measure, "set"), with = FALSE]
    colnames(score) = c("iteration", "loss", "set")
    
    plt_raw = ggplot(data = score, aes(x = loss, color = set)) + 
      geom_density() + 
      theme_minimal() +
      labs(title = "Raw Generalization Error")
  
    path_raw = here("datamodels", "evaluation", "results", model, "ge.png")
    ggsave(path_raw, plt_raw)
    
    score_shifted = copy(score)
    
    dif_mean = score_shifted[set == "simul", mean(loss)] - score_shifted[set == "orig", mean(loss)]
    
    score_shifted[set == "orig", loss := loss + dif_mean]
    
    plt_shifted = ggplot(data = score_shifted, aes(x = loss, color = set)) + 
      geom_density() + 
      theme_minimal() +
      labs(title = "Shifted Generalization Error")
    
    path_shifted = here("datamodels", "evaluation", "results", model, "ge_shifted.png")
    ggsave(path_shifted, plt_shifted)
    
    score[, var(loss), by = set]

    
    if (FALSE) {
      ggplot(data = score_shifted, aes(x = loss, color = set)) + 
        geom_density() + 
        theme_minimal() +
        labs(title = "Shifted Generalization Error")
      
      ggsave("plot.png")
    }
    
    


}
