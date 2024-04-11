setwd(dirname(rstudioapi::getSourceEditorContext()$path))
rm(list = ls())

library(dplyr)
library(rslurm)
library(ggplot2)
library(tidyr)
library(ggpubr)

output <- readRDS(paste0("./res/output_2024-04-10 23_01_50.773632.RDS"))

source("./sim_conditions.R")
res_df <- merge(output, simulation_conditions, by="id", all.x=T)
res_df$nic_diff <- res_df$nic - res_df$loodev
res_df$aic_diff <- res_df$aic - res_df$loodev
res_df$bic_diff <- res_df$bic - res_df$loodev
summary(res_df$aic_diff)
summary(res_df$nic_diff)
summary(res_df$bic_diff)

lr_sv_lme_plot <- list()
nic_vs_aic_plot <- list()
for(r in sigma_rdm_fix_ratio){
  agg_df <- res_df %>% 
    filter(sigma_rdm_fix_ratio==r) %>%
    group_by(id) %>% 
    summarise(bias0=median(bias0),
              bias1=median(bias1),
              nic_diff = median(nic_diff),
              aic_diff = median(aic_diff),
              bic_diff = median(bic_diff),
              looauc = median(looauc),
              loodev = median(loodev))
  agg_df <- merge(agg_df, simulation_conditions, by="id", all.x=T)
  plot_df <- pivot_longer(agg_df, 
                          cols = starts_with("bias"), 
                          names_to = "bias_type", 
                          values_to = "bias_value")
  
  plot_df$model <- ifelse(plot_df$bias_type=="bias0","LME", "LR")
  plot_df$n_obs_per_cluster <- paste0(plot_df$n_obs_per_cluster, " observations per cluster")
  plot_df$n_obs_per_cluster <- factor(plot_df$n_obs_per_cluster, levels=c("5 observations per cluster",
                                                                          "10 observations per cluster",
                                                                          "30 observations per cluster",
                                                                          "50 observations per cluster",
                                                                          "80 observations per cluster") )
  plot_df$n_cluster <- paste0(plot_df$n_cluster, " clusters")
  plot_df$n_cluster <- factor(plot_df$n_cluster, levels=c("50 clusters","100 clusters") )
  lr_sv_lme_plot[[paste0("ratio=",r)]] <- ggplot(data = plot_df, aes(x = n_ttl_betas, y = bias_value, color = model)) + 
    geom_point(size=1) +
    # scale_y_sqrt() +
    scale_y_log10() +
    facet_wrap(~n_cluster + n_obs_per_cluster, ncol = 5, nrow=2) + 
    labs(x = "Number of Predictors", y = "Bias in Coefficients", color = "Model") + 
    theme_bw()
  plot_df <- pivot_longer(agg_df, 
                          cols = ends_with("diff"), 
                          names_to = "ic_type", 
                          values_to = "ic_diff")
  
  plot_df$n_obs_per_cluster <- paste0(plot_df$n_obs_per_cluster, " observations per cluster")
  plot_df$n_obs_per_cluster <- factor(plot_df$n_obs_per_cluster, levels=c("5 observations per cluster",
                                                                          "10 observations per cluster",
                                                                          "30 observations per cluster",
                                                                          "50 observations per cluster",
                                                                          "80 observations per cluster") )
  
  plot_df$n_cluster <- paste0(plot_df$n_cluster, " clusters")
  plot_df$n_cluster <- factor(plot_df$n_cluster, levels=c("50 clusters","100 clusters") )
  nic_vs_aic_plot[[paste0("ratio=",r)]] <- ggplot(data = plot_df, aes(x = n_ttl_betas, y = abs(ic_diff), color = ic_type)) + 
    geom_point(size=1) +
    scale_y_sqrt()+
    facet_wrap(~n_cluster + n_obs_per_cluster, ncol=5, nrow=2) + 
    labs(x = "Number of Predictors", y = "|IC - looDeviance|", color = "Evaluate LR") + 
    theme_bw()
  
  
}

p1 <- ggarrange(plotlist = lr_sv_lme_plot, nrow=3, ncol=1, common.legend =T, legend="right")
p2 <- ggarrange(plotlist = nic_vs_aic_plot, nrow=3, ncol=1, common.legend =T, legend="right")


