setwd(dirname(rstudioapi::getSourceEditorContext()$path))
rm(list = ls())

library(dplyr)
library(rslurm)
library(ggplot2)
library(tidyr)
library(ggpubr)

lr_output_fname <- "output_run_lr_model_select_5days_2024-04-17 13_23_19.421169.RDS"
lm_output_fname <- "output_run_lm_2days_2024-04-13 22_04_11.920463.RDS"

agg_df_ls <- list()
for(rn in c("lr", "lm")){ 
  if(rn == "lr") output <- readRDS(paste0("./res/", lr_output_fname))
  if(rn == "lm") output <- readRDS(paste0("./res/", lm_output_fname))
  source("./sim_conditions.R")
  n_ttl_betas <- c(15) # fixed number of total effects
  param_grid <- expand.grid(n_cluster = n_cluster,
                            n_obs_per_cluster = n_obs_per_cluster,
                            n_ttl_betas = n_ttl_betas,
                            fix_rdm_ratio = fix_rdm_ratio,
                            sigma_fix = sigma_fix,
                            sigma_rdm_fix_ratio = sigma_rdm_fix_ratio,
                            ar1_phi = ar1_phi,
                            na_rate = na_rate)
  simulation_conditions <- as.data.frame(param_grid)
  simulation_conditions$id <- seq(1:nrow(param_grid))
  simulation_conditions$iter <- 1
  
  
  res_df <- merge(output, simulation_conditions, by="id", all.x=T)
  
  
  res_df$nic_diff <- res_df$nic1 - res_df$loodev1
  res_df$aic_diff <- res_df$aic1 - res_df$loodev1
  res_df$bic_diff <- res_df$bic1 - res_df$loodev1
  lr_res_df_name <- gsub(".RDS",".csv",lr_output_fname)
  lm_res_df_name <- gsub(".RDS",".csv",lm_output_fname)
  if(!file.exists(paste0("./res/", lr_res_df_name))) write.csv(res_df,paste0("./res/", lr_res_df_name),row.names = F)
  if(!file.exists(paste0("./res/", lm_res_df_name))) write.csv(res_df,paste0("./res/", lm_res_df_name),row.names = F)
  agg_df <- res_df %>% 
    group_by(id) %>% 
    summarise(nsim = n(),
              bias0=median(bias0),
              bias1=median(bias1),
              se_ratio0 = median(se_ratio0),
              se_ratio1 = median(se_ratio1),
              nic_diff = median(nic_diff),
              aic_diff = median(aic_diff),
              bic_diff = median(bic_diff),
              loopred0 = median(loopred0),
              loopred1 = median(loopred1),
              loodev0 = median(loodev0),
              loodev1 = median(loodev1) )
  agg_df_ls[[rn]] <- merge(agg_df, simulation_conditions, by="id", all.x=T)
}


# source("./fig_bias.R")
# source("./fig_se.R")
# source("./fig_loo.R")
source("./fig_nic.R")


