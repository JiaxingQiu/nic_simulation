setwd(dirname(rstudioapi::getSourceEditorContext()$path))
rm(list = ls())

library(dplyr)
library(rslurm)
library(ggplot2)
library(tidyr)
library(ggpubr)

lr_output_fname <- "output_run_lr_5days_2024-04-16 09_27_17.56707.RDS"
lm_output_fname <- "output_run_lm_2days_2024-04-13 22_04_11.920463.RDS"

agg_df_ls <- list()
for(rn in c("lr", "lm")){ 
  if(rn == "lr") output <- readRDS(paste0("./res/", lr_output_fname))
  if(rn == "lm") output <- readRDS(paste0("./res/", lm_output_fname))
  source("./sim_conditions.R")
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


source("./fig_bias.R")
source("./fig_se.R")
source("./fig_loo.R")
source("./fig_nic.R")


