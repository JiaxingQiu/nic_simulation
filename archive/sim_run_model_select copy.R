setwd(dirname(rstudioapi::getSourceEditorContext()$path))
rm(list=ls())
model_size = 5
cluster_size = 150 # c(5, 150)
sigma_rdm_fix_ratio = 10 #c(0.5,1,5,10)
source("./model_selection_step_loodev.R")# step-wise by loodev
# source("./model_selection_lasso.R")# lasso 
# source("./model_selection_R2.R")# r2 
# source("./model_selection_loodev.R")# step wise by loodev but truth first


library(tidyr)
library(dplyr)
library(ggplot2)
library(ggpubr)

# poly5
if(model_size==5){
  sim_condition = simulation_conditions[which(simulation_conditions$id==1),]
  if(!dir.exists(paste0("./res/",sigma_rdm_fix_ratio))) dir.create(paste0("./res/",sigma_rdm_fix_ratio))
  res_df_iter <- run_wrapper_lm(sim_condition)
  saveRDS(res_df_iter, paste0("./res/",sigma_rdm_fix_ratio,"/model_select_lm_",unique(sim_condition$n_ttl_betas),"_",unique(sim_condition$n_obs_per_cluster),".RDS"))
  res_df_iter <- run_wrapper_lr(sim_condition)
  saveRDS(res_df_iter, paste0("./res/",sigma_rdm_fix_ratio,"/model_select_lr_",unique(sim_condition$n_ttl_betas),"_",unique(sim_condition$n_obs_per_cluster),".RDS"))
}


# # random
# if(model_size==5){
#   sim_condition = simulation_conditions[which(simulation_conditions$id==1),]
#   res_df_iter <- run_wrapper_lm(sim_condition,overfit_type = "random")
#   saveRDS(res_df_iter, paste0("./res/model_select_lm_",unique(sim_condition$n_ttl_betas),"_random.RDS"))
#   res_df_iter <- run_wrapper_lr(sim_condition,overfit_type = "random")
#   saveRDS(res_df_iter, paste0("./res/model_select_lr_",unique(sim_condition$n_ttl_betas),"_random.RDS"))
# }


# if(model_size==10){
#   sim_condition = simulation_conditions[which(simulation_conditions$id==1),]
#    
#   for(batch in c(1:10)){
#     batch_dir <- paste0("./res/model_select_lm_",unique(sim_condition$n_ttl_betas),"_batch") 
#     if(!dir.exists(batch_dir)) dir.create(batch_dir)
#     if(!file.exists(paste0(batch_dir,"/",batch, ".RDS"))){
#       res_df_iter <- run_wrapper_lm(sim_condition)
#       saveRDS(res_df_iter, paste0(batch_dir,"/",batch, ".RDS"))
#     }
#   }
#   for(batch in c(1:10)){
#     batch_dir <- paste0("./res/model_select_lr_",unique(sim_condition$n_ttl_betas),"_batch") 
#     if(!dir.exists(batch_dir)) dir.create(batch_dir)
#     if(!file.exists(paste0(batch_dir,"/",batch, ".RDS"))){
#       res_df_iter <- run_wrapper_lr(sim_condition)
#       saveRDS(res_df_iter, paste0(batch_dir,"/",batch, ".RDS"))
#     }
#   }
# }
