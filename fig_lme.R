setwd(dirname(rstudioapi::getSourceEditorContext()$path))
rm(list = ls())

library(dplyr)
library(rslurm)
library(ggplot2)
library(tidyr)
library(ggpubr)

# lr
output <- readRDS(paste0("./res/output_run_3days_2024-04-12 17_25_27.974288.RDS"))
source("./sim_conditions.R")
res_df <- merge(output, simulation_conditions, by="id", all.x=T)
res_df$nic_diff <- res_df$nic - res_df$loodev
res_df$aic_diff <- res_df$aic - res_df$loodev
res_df$bic_diff <- res_df$bic - res_df$loodev
agg_df <- res_df %>% 
  group_by(id) %>% 
  summarise(bias0=median(bias0),
            bias1=median(bias1),
            se_ratio0 = median(se_ratio0),
            se_ratio1 = median(se_ratio1),
            nic_diff = median(nic_diff),
            aic_diff = median(aic_diff),
            bic_diff = median(bic_diff),
            loodev = median(loodev))
agg_df <- merge(agg_df, simulation_conditions, by="id", all.x=T)
agg_df_lr <- agg_df

# lm
output <- readRDS(paste0("./res/output_run_lm_2days_2024-04-12 18_06_55.38065.RDS"))
source("./sim_conditions.R")
res_df <- merge(output, simulation_conditions, by="id", all.x=T)
res_df$nic_diff <- res_df$nic - res_df$loodev
res_df$aic_diff <- res_df$aic - res_df$loodev
res_df$bic_diff <- res_df$bic - res_df$loodev
agg_df <- res_df %>% 
  group_by(id) %>% 
  summarise(bias0=median(bias0),
            bias1=median(bias1),
            se_ratio0 = median(se_ratio0),
            se_ratio1 = median(se_ratio1),
            nic_diff = median(nic_diff),
            aic_diff = median(aic_diff),
            bic_diff = median(bic_diff),
            loodev = median(loodev))
agg_df <- merge(agg_df, simulation_conditions, by="id", all.x=T)
agg_df_lm <- agg_df

