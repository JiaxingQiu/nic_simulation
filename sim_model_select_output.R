setwd(dirname(rstudioapi::getSourceEditorContext()$path))
rm(list=ls())
model_size = 10
source("./model_selection_step_loodev.R")# step-wise by loodev
# source("./model_selection_lasso.R")# lasso 
# source("./model_selection_R2.R")# r2 
# source("./model_selection_loodev.R")# step wise by loodev but truth first


library(tidyr)
library(dplyr)
library(ggplot2)
library(ggpubr)

sim_condition = simulation_conditions[which(simulation_conditions$id==1),]
res_df_iter <- list()
best_df_iter <- list()
for(sn in c("lm","lr")){
  res_df_iter[[sn]] <- readRDS(paste0("./res/model_select_",sn,"_",unique(sim_condition$n_ttl_betas),".RDS"))
  best_df_iter[[sn]] <- data.frame()
  for(i in sort(unique(res_df_iter[[sn]]$iter))){
    res_df <- res_df_iter[[sn]] %>% filter(iter==i) %>% as.data.frame()
    res_df$loodev[res_df$loodev > quantile(res_df$loodev,0.9)] <- quantile(res_df$loodev,0.9)
    res_df_iter[[sn]][which(res_df_iter[[sn]]$iter==i), ] <- res_df
    best_df <- data.frame() 
    for(score in c("loodev", "nic", "aic", "bic")){
      best_size <- res_df$model_size[which(res_df[,score]==min(res_df[,score]))][1]
      best_score <- res_df[,score][which(res_df[,score]==min(res_df[,score]))][1]
      score_1se <- sd(res_df[,score])/sqrt(nrow(res_df))
      best_size_1se_min <- min(res_df$model_size[which(abs(res_df[,score]-min(res_df[,score]))<=score_1se)])
      best_size_1se_max <- max(res_df$model_size[which(abs(res_df[,score]-min(res_df[,score]))<=score_1se)])
      best_df <- bind_rows(best_df, data.frame(i,score,best_size, best_score, score_1se, best_size_1se_min,best_size_1se_max))
    }
    best_df_iter[[sn]] <- bind_rows(best_df_iter[[sn]], best_df)
  }
}
source("./model_selection_plot.R")
source("./model_selection_stat.R")
