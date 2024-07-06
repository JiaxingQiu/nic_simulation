setwd(dirname(rstudioapi::getSourceEditorContext()$path))
rm(list = ls())

library(dplyr)
library(rslurm)
library(ggplot2)
library(tidyr)
library(ggpubr)

lr_output_fname <- "run_lm_k_fold.RDS"
lm_output_fname <- "run_lr_k_fold.RDS"

agg_df_ls <- list()
for(rn in c("lr", "lm")){ 
  if(rn == "lr") output <- readRDS(paste0("./res/", lr_output_fname))
  if(rn == "lm") output <- readRDS(paste0("./res/", lm_output_fname))
  source("./sim_conditions_k_fold.R")
  output <- as.data.frame(lapply(output, unlist))
  res_df <- as.data.frame(merge(output, simulation_conditions, by="id", all.x=T))
  
  res_df$nicc_diff <- (res_df$nicc1 - res_df$loodev1)/(res_df$n_cluster*res_df$n_obs_per_cluster)
  res_df$nic_diff <- (res_df$nic1 - res_df$loodev1)/(res_df$n_cluster*res_df$n_obs_per_cluster)
  res_df$aic_diff <- (res_df$aic1 - res_df$loodev1)/(res_df$n_cluster*res_df$n_obs_per_cluster)
  res_df$bic_diff <- (res_df$bic1 - res_df$loodev1)/(res_df$n_cluster*res_df$n_obs_per_cluster)
  res_df$k_5_diff <- (res_df$k_5 - res_df$loodev1)/(res_df$n_cluster*res_df$n_obs_per_cluster)
  res_df$k_10_diff <- (res_df$k_10 - res_df$loodev1)/(res_df$n_cluster*res_df$n_obs_per_cluster)
  res_df$k_50_diff <- (res_df$k_50 - res_df$loodev1)/(res_df$n_cluster*res_df$n_obs_per_cluster)
  res_df$k_80_diff <- (res_df$k_80 - res_df$loodev1)/(res_df$n_cluster*res_df$n_obs_per_cluster)
  
  summa <- function(df){
    for(cl in c("nicc_diff", "nic_diff", "aic_diff", "bic_diff",
                # "k_5_diff",
                "k_10_diff", "k_50_diff", "k_80_diff") ){
      df[[paste0(cl,"_mean")]] <- mean(df[[cl]], na.rm=T)
      df[[paste0(cl,"_median")]] <- median(df[[cl]], na.rm=T)
      df[[paste0(cl,"_q25")]] <- quantile(df[[cl]],0.025, na.rm=T)
      df[[paste0(cl,"_q75")]] <- quantile(df[[cl]],0.975, na.rm=T)
      df[[paste0(cl,"_se")]] <- sd(df[[cl]], na.rm=T)/nrow(df)
    }
    return(df)
  }
  by_id <- group_by(res_df, id)
  agg_df <- do(by_id, summa(.))
  agg_df <- distinct(agg_df[,c("id",colnames(agg_df)[endsWith(colnames(agg_df), "_mean") |
                                                endsWith(colnames(agg_df), "_median") |
                                                endsWith(colnames(agg_df), "_q25") |
                                                endsWith(colnames(agg_df), "_q75") |
                                                endsWith(colnames(agg_df), "_se")]) ])
  agg_df <- as.data.frame(agg_df)
  agg_df_ls[[rn]] <- merge(agg_df, simulation_conditions, by="id", all.x=T)
  
}

source("./sim_plot_k_fold.R")
p <- annotate_figure(p, top = text_grob("Approximating K-fold Deviance", size = 14, face = "bold"))

p %>% ggsave(filename=paste0("./res/k_fold.png"), width = 8, height = 7, bg="white")





