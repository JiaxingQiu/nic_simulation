setwd(dirname(rstudioapi::getSourceEditorContext()$path))
rm(list = ls())

library(dplyr)
library(rslurm)
library(ggplot2)
library(tidyr)
library(ggpubr)

lr_output_fname <- "output_run_lr_5days_2024-04-26 09_26_53.793405.RDS"
lm_output_fname <- "output_run_lm_2days_2024-04-26 09_26_50.318184.RDS"

agg_df_ls <- list()
for(rn in c("lr", "lm")){ 
  if(rn == "lr") output <- readRDS(paste0("./res/", lr_output_fname))
  if(rn == "lm") output <- readRDS(paste0("./res/", lm_output_fname))
  source("./sim_conditions.R")
  res_df <- merge(output, simulation_conditions, by="id", all.x=T)
  res_df$nic_diff <- abs(res_df$nic1 - res_df$loodev1)
  res_df$aic_diff <- abs(res_df$aic1 - res_df$loodev1)
  res_df$bic_diff <- abs(res_df$bic1 - res_df$loodev1)
  lr_res_df_name <- gsub(".RDS",".csv",lr_output_fname)
  lm_res_df_name <- gsub(".RDS",".csv",lm_output_fname)
  # if(!file.exists(paste0("./res/", lr_res_df_name))) write.csv(res_df,paste0("./res/", lr_res_df_name),row.names = F)
  # if(!file.exists(paste0("./res/", lm_res_df_name))) write.csv(res_df,paste0("./res/", lm_res_df_name),row.names = F)
  agg_df <- res_df %>%
    group_by(id) %>%
    summarise(niter = n(),
              bias0 = median(bias0), bias0_se = sd(bias0)/sqrt(niter),
              bias1 = median(bias1), bias1_se = sd(bias1)/sqrt(niter),
              se_ratio0 = median(se_ratio0), se_ratio0_se = sd(se_ratio0)/sqrt(niter),
              se_ratio1 = median(se_ratio1), se_ratio1_se = sd(se_ratio1)/sqrt(niter),
              nic_diff = median(nic_diff), nic_diff_se = sd(nic_diff)/sqrt(niter),
              aic_diff = median(aic_diff), aic_diff_se = sd(aic_diff)/sqrt(niter),
              bic_diff = median(bic_diff), bic_diff_se = sd(bic_diff)/sqrt(niter),
              loopred0 = median(loopred0), loopred0_se = sd(loopred0)/sqrt(niter),
              loopred1 = median(loopred1), loopred1_se = sd(loopred1)/sqrt(niter),
              loodev0 = median(loodev0), loodev0_se = sd(loodev0)/sqrt(niter),
              loodev1 = median(loodev1), loodev1_se = sd(loodev1)/sqrt(niter)
              )
  summa <- function(df){
    for(cl in c("nic_diff", "aic_diff", "bic_diff") ){
      df[[paste0(cl,"_mean")]] <- mean(df[[cl]], na.rm=T)
      df[[paste0(cl,"_median")]] <- median(df[[cl]], na.rm=T)
      df[[paste0(cl,"_q25")]] <- quantile(df[[cl]],0.25, na.rm=T)
      df[[paste0(cl,"_q75")]] <- quantile(df[[cl]],0.75, na.rm=T)
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


source("./fig_sim_conditions.R")


