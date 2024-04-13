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


# lr
plot_df <- pivot_longer(agg_df_lr, 
                        cols = ends_with("diff"), 
                        names_to = "ic_type", 
                        values_to = "ic_diff")
plot_df$n_obs_per_cluster <- paste0(plot_df$n_obs_per_cluster, " obs/cluster")
plot_df$n_obs_per_cluster <- factor(plot_df$n_obs_per_cluster, levels=c("5 obs/cluster",
                                                                        "10 obs/cluster",
                                                                        "30 obs/cluster",
                                                                        "50 obs/cluster",
                                                                        "80 obs/cluster") )

plot_df$ic_type <- stringr::str_to_upper(gsub("_diff","",plot_df$ic_type))
plot_df$ic_type <- factor(plot_df$ic_type, levels=c("NIC","AIC","BIC"))
plot_df$ar1_phi <- paste0("AR1(",plot_df$ar1_phi,")")
plot_df$ar1_phi <- factor(plot_df$ar1_phi, levels = c("AR1(0)", "AR1(0.4)", "AR1(0.8)"))
p_lr <- ggplot(data = plot_df, aes(x = n_ttl_betas, y = abs(ic_diff), color = ic_type)) + 
  geom_point(size=1) +
  geom_line(mapping = aes(linetype=as.factor(na_rate) ))+
  geom_hline(aes(yintercept=0)) + 
  scale_x_continuous(limits = c(5, 15), breaks = c(5,10,15)) +
  # scale_y_continuous(breaks=c(0,10,50,200,400,600,800)) + #limits = c(0, 100),
  coord_trans(y = "sqrt") +
  # scale_y_log10()+
  facet_wrap(~ar1_phi + n_obs_per_cluster, ncol=5, nrow=3, scales="free_x") + 
  labs(title = "Binomial",
       x = "Number of Predictors", 
       y = "Error = |IC - looDeviance|",
       color = "Error",
       linetype="Missing") + 
  theme_bw()

# lm
plot_df <- pivot_longer(agg_df_lm, 
                        cols = ends_with("diff"), 
                        names_to = "ic_type", 
                        values_to = "ic_diff")
plot_df$n_obs_per_cluster <- paste0(plot_df$n_obs_per_cluster, " obs/cluster")
plot_df$n_obs_per_cluster <- factor(plot_df$n_obs_per_cluster, levels=c("5 obs/cluster",
                                                                        "10 obs/cluster",
                                                                        "30 obs/cluster",
                                                                        "50 obs/cluster",
                                                                        "80 obs/cluster") )

plot_df$ic_type <- stringr::str_to_upper(gsub("_diff","",plot_df$ic_type))
plot_df$ic_type <- factor(plot_df$ic_type, levels=c("NIC","AIC","BIC"))
plot_df$ar1_phi <- paste0("AR1(",plot_df$ar1_phi,")")
plot_df$ar1_phi <- factor(plot_df$ar1_phi, levels = c("AR1(0)", "AR1(0.4)", "AR1(0.8)"))
p_lm <- ggplot(data = plot_df, aes(x = n_ttl_betas, y = abs(ic_diff), color = ic_type)) + 
  geom_point(size=1) +
  geom_line(mapping = aes(linetype=as.factor(na_rate) ))+
  geom_hline(aes(yintercept=0)) + 
  scale_x_continuous(limits = c(5, 15), breaks = c(5,10,15)) +
  # scale_y_continuous(breaks=c(0,10,50,200,400,600,800)) + #limits = c(0, 100),
  coord_trans(y = "sqrt") +
  # scale_y_sqrt()+
  facet_wrap(~ar1_phi + n_obs_per_cluster, ncol=5, nrow=3, scales="free_x") + 
  labs(title = "Guassian",
       x = "Number of Predictors", 
       y = "Error = |IC - looDeviance|",
       color = "Criteria",
       linetype="Missing") + 
  theme_bw()

p <- ggarrange(p_lr, p_lm, nrow=1,ncol=2, common.legend = T,legend = "right")
p %>% ggsave(filename=paste0("./res/nic_vs_aic.png"), width = 15, height = 7, bg="white")
