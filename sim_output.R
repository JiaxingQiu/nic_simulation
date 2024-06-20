setwd(dirname(rstudioapi::getSourceEditorContext()$path))
rm(list = ls())

library(dplyr)
library(rslurm)
library(ggplot2)
library(tidyr)
library(ggpubr)

lr_output_fname <- "output_run_lr_5days_2024-05-03 13_05_00.713904.RDS"
lm_output_fname <- "output_run_lm_2days_2024-05-03 15_34_18.063535.RDS"

agg_df_ls <- list()
for(rn in c("lr", "lm")){ 
  if(rn == "lr") output <- readRDS(paste0("./res/", lr_output_fname))
  if(rn == "lm") output <- readRDS(paste0("./res/", lm_output_fname))
  source("./sim_conditions.R")
  res_df <- merge(output, simulation_conditions, by="id", all.x=T)
  res_df <- res_df %>% filter(!sigma_rdm_fix_ratio==5,
                              n_cluster == 50) %>%as.data.frame()
  if(!"nicc1" %in%colnames(res_df) ){
    res_df$nicc1 <- res_df$nic1 # comment out remember
    print("error : nic used for nicc ")
  }
  res_df$nicc_diff <- res_df$nicc1 - res_df$loodev1 #abs()
  res_df$nic_diff <- res_df$nic1 - res_df$loodev1 #abs()
  res_df$aic_diff <- res_df$aic1 - res_df$loodev1 #abs()
  res_df$bic_diff <- res_df$bic1 - res_df$loodev1 #abs()
  # lr_res_df_name <- gsub(".RDS",".csv",lr_output_fname)
  # lm_res_df_name <- gsub(".RDS",".csv",lm_output_fname)
  # if(!file.exists(paste0("./res/", lr_res_df_name))) write.csv(res_df,paste0("./res/", lr_res_df_name),row.names = F)
  # if(!file.exists(paste0("./res/", lm_res_df_name))) write.csv(res_df,paste0("./res/", lm_res_df_name),row.names = F)
  agg_df <- res_df %>%
    group_by(id) %>%
    summarise(niter = n(),
              bias0 = median(bias0), bias0_se = sd(bias0)/sqrt(niter),
              bias1 = median(bias1), bias1_se = sd(bias1)/sqrt(niter),
              se_ratio0 = median(se_ratio0), se_ratio0_se = sd(se_ratio0)/sqrt(niter),
              se_ratio1 = median(se_ratio1), se_ratio1_se = sd(se_ratio1)/sqrt(niter),
              nicc_diff = median(nicc_diff), nicc_diff_se = sd(nicc_diff)/sqrt(niter),
              nic_diff = median(nic_diff), nic_diff_se = sd(nic_diff)/sqrt(niter),
              aic_diff = median(aic_diff), aic_diff_se = sd(aic_diff)/sqrt(niter),
              bic_diff = median(bic_diff), bic_diff_se = sd(bic_diff)/sqrt(niter),
              loopred0 = median(loopred0), loopred0_se = sd(loopred0)/sqrt(niter),
              loopred1 = median(loopred1), loopred1_se = sd(loopred1)/sqrt(niter),
              loodev0 = median(loodev0), loodev0_se = sd(loodev0)/sqrt(niter),
              loodev1 = median(loodev1), loodev1_se = sd(loodev1)/sqrt(niter)
              )
  summa <- function(df){
    for(cl in c("nicc_diff", "nic_diff", "aic_diff", "bic_diff") ){
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

source("./sim_plot_N.R")
p_n <- p
p_n <- annotate_figure(p_n, fig.lab = "\nA.", fig.lab.face = "bold", fig.lab.size = 12,
                       top = text_grob("\n Clustering by number of observations per cluster", size = 12, face = "bold", x=0,hjust=0),
                       left = text_grob("Error", size = 10, face = "bold", rot = 90), 
                       bottom = text_grob("Generating model size", size = 10, face = "bold") )
source("./sim_plot_phi.R")
p_p <- p
p_p <- annotate_figure(p_p, fig.lab = "\nB.", fig.lab.face = "bold", fig.lab.size = 12,
                       top = text_grob("\n Clustering in predictors", size = 12, face = "bold", x=0,hjust=0),
                       left = text_grob("Error", size = 10, face = "bold", rot = 90), 
                       bottom = text_grob("Generating model size", size = 10, face = "bold") )
source("./sim_plot_sigma_ratio.R")
p_r <- p
p_r <- annotate_figure(p_r, fig.lab = "\nC.", fig.lab.face = "bold", fig.lab.size = 12,
                       top = text_grob("\n Clustering in response", size = 12, face = "bold", x=0,hjust=0),
                       left = text_grob("Error", size = 10, face = "bold", rot = 90), 
                       bottom = text_grob("Generating model size", size = 10, face = "bold") )

p_bottom <- ggarrange(p_p, p_r, ncol=2, nrow=1, widths = c(3,3) )
p <- ggarrange(p_n, p_bottom, nrow = 2, ncol=1, heights = c(1,1))
# r <- rev(c(0.5, 1, 5, 10))[4]
# na <- c(0,1)[1]
# source("./sim_conditions_plot.R")
p <- annotate_figure(p, top = text_grob("Out-of-cluster Performance Approximation", size = 14, face = "bold"))
p %>% ggsave(filename=paste0("./res/nic_vs_aic.png"), width = 8, height = 9, bg="white")

source("./sim_plot_unbalance.R")

