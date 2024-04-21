setwd(dirname(rstudioapi::getSourceEditorContext()$path))

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
res_df_iter <- run_wrapper_lm(sim_condition)
saveRDS(res_df_iter, paste0("./res/model_select_lm_",unique(sim_condition$n_ttl_betas),".RDS"))
res_df_iter <- run_wrapper_lr(sim_condition)
saveRDS(res_df_iter, paste0("./res/model_select_lr_",unique(sim_condition$n_ttl_betas),".RDS"))

