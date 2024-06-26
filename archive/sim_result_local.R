setwd(dirname(rstudioapi::getSourceEditorContext()$path))
rm(list = ls())

library(dplyr)
library(rslurm)
library(ggplot2)
library(tidyr)
library(ggpubr)

# # lr
# output <- readRDS(paste0("./res/output_run_3days_2024-04-12 17_25_27.974288.RDS"))
# if(!dir.exists("./res/lr")) dir.create("./res/lr")
# res_dir <- "./res/lr"

# lm
output <- readRDS(paste0("./res/output_run_lm_2days_2024-04-12 18_06_55.38065.RDS"))
if(!dir.exists("./res/lm")) dir.create("./res/lm")
res_dir <- "./res/lm"


source("./sim_conditions.R")
res_df <- merge(output, simulation_conditions, by="id", all.x=T)
res_df$nic_diff <- res_df$nic - res_df$loodev
res_df$aic_diff <- res_df$aic - res_df$loodev
res_df$bic_diff <- res_df$bic - res_df$loodev
summary(res_df$aic_diff)
summary(res_df$nic_diff)
summary(res_df$bic_diff)

for(a in unique(res_df$ar1_phi)){
  lr_sv_lme_plot <- list()
  lr_sv_lme_se_plot <- list()
  nic_vs_aic_plot <- list()
  # loop through missingness
  for(r in unique(res_df$na_rate) ){
    agg_df <- res_df %>% 
      filter(ar1_phi==a) %>%
      filter(na_rate==r) %>%
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
    
    # loo prediction performance
    plot_df <- pivot_longer(agg_df, 
                            cols = starts_with("loopred"), 
                            names_to = "bias_type", 
                            values_to = "bias_value")
    plot_df$model <- ifelse(plot_df$bias_type=="bias0","LME", "LM")
    plot_df$n_obs_per_cluster <- paste0(plot_df$n_obs_per_cluster, " observations per cluster")
    plot_df$n_obs_per_cluster <- factor(plot_df$n_obs_per_cluster, levels=c("5 observations per cluster",
                                                                            "10 observations per cluster",
                                                                            "30 observations per cluster",
                                                                            "50 observations per cluster",
                                                                            "80 observations per cluster") )
    plot_df$n_cluster <- paste0(plot_df$n_cluster, " clusters")
    plot_df$n_cluster <- factor(plot_df$n_cluster, levels=c("50 clusters","100 clusters") )
    lr_sv_lme_plot[[paste0("na_rate=",r)]] <- ggplot(data = plot_df, aes(x = n_ttl_betas, y = bias_value, color = model)) + 
      geom_point(size=1) +
      # scale_y_sqrt() +
      # scale_y_log10() +
      # scale_y_continuous(limits = c(0, 8000)) +
      scale_x_continuous(limits = c(3, 15)) +
      coord_trans(y = "sqrt") +
      facet_wrap(~n_cluster + n_obs_per_cluster, ncol = 5, nrow=2) + 
      labs(x = "Number of Predictors", y = "Bias in Coefficients", color = "Model") + 
      theme_bw()
    
    # se ratio
    plot_df <- pivot_longer(agg_df, 
                            cols = starts_with("se_"), 
                            names_to = "se_type", 
                            values_to = "se_value")
    plot_df$model <- ifelse(plot_df$se_type=="se_ratio0","LME", "LM")
    plot_df$n_obs_per_cluster <- paste0(plot_df$n_obs_per_cluster, " observations per cluster")
    plot_df$n_obs_per_cluster <- factor(plot_df$n_obs_per_cluster, levels=c("5 observations per cluster",
                                                                            "10 observations per cluster",
                                                                            "30 observations per cluster",
                                                                            "50 observations per cluster",
                                                                            "80 observations per cluster") )
    plot_df$n_cluster <- paste0(plot_df$n_cluster, " clusters")
    plot_df$n_cluster <- factor(plot_df$n_cluster, levels=c("50 clusters","100 clusters") )
    lr_sv_lme_se_plot[[paste0("na_rate=",r)]] <- ggplot(data = plot_df, aes(x = n_ttl_betas, y = se_value, color = model)) + 
      geom_point(size=1) +
      geom_hline(mapping = aes(yintercept=1)) + 
      # scale_y_sqrt() +
      # scale_y_log10() +
      scale_x_continuous(limits = c(3, 15)) +
      scale_y_continuous(limits = c(0, 20)) +
      coord_trans(y = "sqrt") +
      facet_wrap(~n_cluster + n_obs_per_cluster, ncol = 5, nrow=2) + 
      labs(x = "Number of Predictors", y = "SE accuracy", color = "Model") + 
      theme_bw()
    
    # nic
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
    nic_vs_aic_plot[[paste0("na_rate=",r)]] <- ggplot(data = plot_df, aes(x = n_ttl_betas, y = abs(ic_diff), color = ic_type)) + 
      geom_point(size=1) +
      geom_hline(aes(yintercept=0)) + 
      scale_y_continuous(breaks=c(0,10,25,50,75)) + #limits = c(0, 100), 
      scale_x_continuous(limits = c(3, 15)) +
      coord_trans(y = "sqrt") +
      facet_wrap(~n_cluster + n_obs_per_cluster, ncol=5, nrow=2) + 
      labs(x = "Number of Predictors", y = "|IC - looDeviance|", color = "Evaluate LM") + 
      theme_bw()
    
    
  }
  
  
  for(r in unique(res_df$na_rate) ){
    lr_sv_lme_plot[[paste0("na_rate=",r)]]  <- lr_sv_lme_plot[[paste0("na_rate=",r)]] +
      labs(x=NULL, y=NULL, subtitle = paste0("~", r*100,"% missingness per cluster"),title = NULL)
    nic_vs_aic_plot[[paste0("na_rate=",r)]]  <- nic_vs_aic_plot[[paste0("na_rate=",r)]] +
      labs(x=NULL, y=NULL, subtitle = paste0("~", r*100,"% missingness per cluster"),title = NULL)
    lr_sv_lme_se_plot[[paste0("na_rate=",r)]]  <- lr_sv_lme_se_plot[[paste0("na_rate=",r)]] +
      labs(x=NULL, y=NULL, subtitle = paste0("~", r*100,"% missingness per cluster"),title = NULL)
  }
  
  p1 <- ggarrange(plotlist = lr_sv_lme_plot, nrow=3, ncol=1, common.legend =T, legend="right")
  p1 <- annotate_figure(p1, top="Comparing LM and LME", left = "Bias", bottom = "Total number of effects")
  p1 %>% ggsave(filename=paste0(res_dir,"/lm_vs_lme_ar",a,".png"), width = 10, height = 7, bg="white")
  p2 <- ggarrange(plotlist = nic_vs_aic_plot, nrow=3, ncol=1, common.legend =T, legend="right")
  p2 <- annotate_figure(p2, top="Comparing AIC and NIC of LM", left = "|IC - looDeviance|", bottom = "Total number of effects")
  p2 %>% ggsave(filename=paste0(res_dir,"/nic_vs_aic_ar",a,".png"), width = 10, height = 7, bg="white")
  p3 <- ggarrange(plotlist = lr_sv_lme_se_plot, nrow=3, ncol=1, common.legend =T, legend="right")
  p3 <- annotate_figure(p3, top="Comparing LM and LME SE Accuracy", left = "Accuracy", bottom = "Total number of effects")
  p3 %>% ggsave(filename=paste0(res_dir,"/lm_vs_lme_se_ar",a,".png"), width = 10, height = 7, bg="white")
}
