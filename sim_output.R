setwd(dirname(rstudioapi::getSourceEditorContext()$path))
rm(list = ls())

library(dplyr)
library(rslurm)
library(ggplot2)
library(tidyr)
library(ggpubr)

lr_output_fname <- "output_run_lr_5days.RDS"
lm_output_fname <- "output_run_lm_2days.RDS"
lr_output_fname_pair <- "output_run_lr_pair.RDS"
lm_output_fname_pair <- "output_run_lm_pair.RDS"

agg_df_ls <- list()
for(rn in c("lr", "lm")){ 
  
  # 1. original run
  if(rn == "lr") output <- readRDS(paste0("./res/", lr_output_fname))
  if(rn == "lm") output <- readRDS(paste0("./res/", lm_output_fname))
  source("./sim_conditions.R")
  
  
  simulation_conditions <- simulation_conditions[,setdiff(colnames(simulation_conditions),"iter")]
  res_df <- merge(output, simulation_conditions, by="id", all.x=T)
  res_df <- res_df %>% filter(!sigma_rdm_fix_ratio==5) %>%as.data.frame()
  if(!"nicc1" %in%colnames(res_df) ){
    res_df$nicc1 <- res_df$nic1 # comment out remember
    print("error : nic used for nicc ")
  }
  res_df$nicc_diff <- (res_df$nicc1 - res_df$loodev1)/(res_df$n_cluster*res_df$n_obs_per_cluster)#res_df$loodev1 #abs()
  res_df$nic_diff <- (res_df$nic1 - res_df$loodev1)/(res_df$n_cluster*res_df$n_obs_per_cluster)#res_df$loodev1 #abs()
  res_df$aic_diff <- (res_df$aic1 - res_df$loodev1)/(res_df$n_cluster*res_df$n_obs_per_cluster)#res_df$loodev1 #abs()
  res_df$bic_diff <- (res_df$bic1 - res_df$loodev1)/(res_df$n_cluster*res_df$n_obs_per_cluster)#res_df$loodev1 #abs()
  
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
  agg_df <- merge(agg_df, simulation_conditions, by="id", all.x=T)
  agg_df$id <- as.character(agg_df$id)
  agg_df1 <- agg_df
  simulation_conditions1 <- simulation_conditions
  
  
  # 2. additional paired condition
  if(rn == "lr") output2 <- readRDS(paste0("./res/", lr_output_fname_pair))
  if(rn == "lm") output2 <- readRDS(paste0("./res/", lm_output_fname_pair))
  source("./sim_conditions_pair.R")
  simulation_conditions <- simulation_conditions[,setdiff(colnames(simulation_conditions),"iter")]
  res_df <- merge(output, simulation_conditions, by="id", all.x=T)
  res_df <- res_df %>% filter(!sigma_rdm_fix_ratio==5) %>%as.data.frame()
  if(!"nicc1" %in%colnames(res_df) ){
    res_df$nicc1 <- res_df$nic1 # comment out remember
    print("error : nic used for nicc ")
  }
  res_df$nicc_diff <- (res_df$nicc1 - res_df$loodev1)/(res_df$n_cluster*res_df$n_obs_per_cluster)#res_df$loodev1 #abs()
  res_df$nic_diff <- (res_df$nic1 - res_df$loodev1)/(res_df$n_cluster*res_df$n_obs_per_cluster)#res_df$loodev1 #abs()
  res_df$aic_diff <- (res_df$aic1 - res_df$loodev1)/(res_df$n_cluster*res_df$n_obs_per_cluster)#res_df$loodev1 #abs()
  res_df$bic_diff <- (res_df$bic1 - res_df$loodev1)/(res_df$n_cluster*res_df$n_obs_per_cluster)#res_df$loodev1 #abs()
  by_id <- group_by(res_df, id)
  agg_df <- do(by_id, summa(.))
  agg_df <- distinct(agg_df[,c("id",colnames(agg_df)[endsWith(colnames(agg_df), "_mean") |
                                                       endsWith(colnames(agg_df), "_median") |
                                                       endsWith(colnames(agg_df), "_q25") |
                                                       endsWith(colnames(agg_df), "_q75") |
                                                       endsWith(colnames(agg_df), "_se")]) ])
  agg_df <- as.data.frame(agg_df)
  agg_df <- merge(agg_df, simulation_conditions, by="id", all.x=T)
  agg_df$id <- paste0("pair_", agg_df$id)
  agg_df2 <- agg_df
  simulation_conditions2 <- simulation_conditions
  simulation_conditions2$id <- paste0("pair_", simulation_conditions2$id)
  
  
  agg_df_ls[[rn]] <- rbind(agg_df2, agg_df1)
  simulation_conditions <- rbind(simulation_conditions2, simulation_conditions1)
}

source("./sim_plot_N.R")
p_n <- p
p_n <- annotate_figure(p_n, fig.lab = "\nA.", fig.lab.face = "bold", fig.lab.size = 10,
                       top = text_grob("\n Clustering by number of observations per cluster", size = 10, face = "bold", x=0,hjust=0),
                       left = text_grob("Error", size = 8, face = "bold", rot = 90), 
                       bottom = text_grob("Generating model size", size = 8, face = "bold") )
source("./sim_plot_phi.R")
p_p <- p
p_p <- annotate_figure(p_p, fig.lab = "\nB.", fig.lab.face = "bold", fig.lab.size = 10,
                       top = text_grob("\n Clustering in predictors\n(by within-cluster autocorrelation)", size = 10, face = "bold", x=0,hjust=0),
                       left = text_grob("Error", size = 8, face = "bold", rot = 90), 
                       bottom = text_grob("Generating model size", size = 8, face = "bold") )
source("./sim_plot_sigma_ratio.R")
p_r <- p
p_r <- annotate_figure(p_r, fig.lab = "\nC.", fig.lab.face = "bold", fig.lab.size = 10,
                       top = text_grob("\n Clustering in response\n(by strength of random effects)", size = 10, face = "bold", x=0,hjust=0),
                       left = text_grob("Error", size = 8, face = "bold", rot = 90), 
                       bottom = text_grob("Generating model size", size = 8, face = "bold") )

p_bottom <- ggarrange(p_p, p_r, ncol=2, nrow=1, widths = c(3,3) )
p <- ggarrange(p_n, p_bottom, nrow = 2, ncol=1, heights = c(1.2,1))
# r <- rev(c(0.5, 1, 5, 10))[4]
# na <- c(0,1)[1]
# source("./sim_conditions_plot.R")
p <- annotate_figure(p, top = text_grob("Approximating Leave-one-cluster-out Deviance (looDeviance)", size = 14, face = "bold"))
p %>% ggsave(filename=paste0("./res/nic_vs_aic.png"), width = 9, height = 10, bg="white")


# special scenario
source("./sim_plot_unbalance.R")
p_na <- annotate_figure(p, fig.lab = "\nA.", fig.lab.face = "bold", fig.lab.size = 10,
                        top = text_grob("\n Unbalanced cluster sizes", size = 10, face = "bold", x=0,hjust=0),
                        left = text_grob("Error", size = 8, face = "bold", rot = 90), 
                        bottom = text_grob("Generating model size", size = 8, face = "bold") )
# p_na %>% ggsave(filename=paste0("./res/balance_vs_unbalance.png"), width = 7, height = 6, bg="white")
source("./sim_plot_small_cluster.R")
p_sm <- annotate_figure(p, fig.lab = "\nB.", fig.lab.face = "bold", fig.lab.size = 10,
                        top = text_grob("\n Small sample of clusters", size = 10, face = "bold", x=0,hjust=0),
                        left = text_grob("Error", size = 8, face = "bold", rot = 90), 
                        bottom = text_grob("Generating model size", size = 8, face = "bold") )


p_spec <- ggarrange(p_na, p_sm, ncol=2, nrow=1, widths = c(2.5,3) )
p_spec <- annotate_figure(p_spec, top = text_grob("Approximating looDeviance (special scenarios)", size = 14, face = "bold"))

p_spec %>% ggsave(filename=paste0("./res/special_scenario.png"), width = 14, height = 7, bg="white")


