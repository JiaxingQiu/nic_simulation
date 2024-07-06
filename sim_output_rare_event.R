setwd(dirname(rstudioapi::getSourceEditorContext()$path))
rm(list = ls())

library(dplyr)
library(rslurm)
library(ggplot2)
library(tidyr)
library(ggpubr)

lr_output_fname <- "run_lr_rare.RDS"
output <- readRDS(paste0("./res/output_", lr_output_fname))
source("./sim_conditions_rare.R")
output <- as.data.frame(lapply(output, unlist))
res_df <- as.data.frame(merge(output, simulation_conditions, by="id", all.x=T))

res_df$nicc_diff <- (res_df$nicc1 - res_df$loodev1)/(res_df$n_cluster*res_df$n_obs_per_cluster)
res_df$nic_diff <- (res_df$nic1 - res_df$loodev1)/(res_df$n_cluster*res_df$n_obs_per_cluster)
res_df$aic_diff <- (res_df$aic1 - res_df$loodev1)/(res_df$n_cluster*res_df$n_obs_per_cluster)
res_df$bic_diff <- (res_df$bic1 - res_df$loodev1)/(res_df$n_cluster*res_df$n_obs_per_cluster)

summa <- function(df){
  for(cl in c("nicc_diff", "nic_diff", "aic_diff", "bic_diff") ){
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

source("./sim_plot_rare_event.R")
p <- annotate_figure(p, fig.lab = "\nC.", fig.lab.face = "bold", fig.lab.size = 12,
                     top = text_grob("\n     Rare Event", size = 12, face = "bold", x=0,hjust=0))

p %>% ggsave(filename=paste0("./res/rare_event.png"), width = 6, height = 6, bg="white")





