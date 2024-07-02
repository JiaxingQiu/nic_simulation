setwd(dirname(rstudioapi::getSourceEditorContext()$path))
rm(list=ls())
model_size = 5
cluster_strength <- c("weak", "moderate", "strong","moderate_strong")#

library(tidyr)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(magick)

p_size_ls <- list()
for(cs in cluster_strength){
  if(cs =="weak"){
    cluster_size = 10
    sigma_rdm_fix_ratio = 0.5
    ar1_phi = 0
  }
  if(cs =="moderate"){
    cluster_size = 50
    sigma_rdm_fix_ratio = 5
    ar1_phi = 0.4
  }
  if(cs == "strong"){
    cluster_size = 150
    sigma_rdm_fix_ratio = 10
    ar1_phi = 0.8
  }
  if(cs == "moderate_strong"){
    cluster_size = 100
    sigma_rdm_fix_ratio = 10
    ar1_phi = 0.4
  }
  source("./model_selection_step_loodev.R")# step-wise by loodev
  en <- "" #"_random" #
  sim_condition = simulation_conditions[which(simulation_conditions$id==1),]
  res_df_iter <- list()
  best_df_iter <- list()
  for(sn in c("lm","lr")){
    res_df_iter[[sn]] <- readRDS(paste0("./res/",sim_condition$sigma_rdm_fix_ratio,"/model_select_",sn,"_",unique(sim_condition$n_ttl_betas),"_",unique(sim_condition$n_obs_per_cluster),en,".RDS"))
    best_df_iter[[sn]] <- data.frame()
    for(i in sort(unique(res_df_iter[[sn]]$iter))){
      res_df <- res_df_iter[[sn]] %>% filter(iter==i) %>% as.data.frame()
      # res_df$loodev[res_df$loodev > quantile(res_df$loodev,0.95)] <- quantile(res_df$loodev,0.95)
      res_df_iter[[sn]][which(res_df_iter[[sn]]$iter==i), ] <- res_df
      best_df <- data.frame() 
      for(score in c("loodev", "nic", "nicc", "aic", "bic")){
        best_size <- res_df$model_size[which(res_df[,score]==min(res_df[,score]))][1]
        best_score <- res_df[,score][which(res_df[,score]==min(res_df[,score]))][1]
        score_1se <- sd(res_df[,score])/sqrt(nrow(res_df))
        best_size_1se_min <- min(res_df$model_size[which(abs(res_df[,score]-min(res_df[,score]))<=score_1se)])
        best_size_1se_max <- max(res_df$model_size[which(abs(res_df[,score]-min(res_df[,score]))<=score_1se)])
        best_df <- bind_rows(best_df, data.frame(i,score,best_size, best_score, score_1se, best_size_1se_min,best_size_1se_max))
      }
      best_df_iter[[sn]] <- bind_rows(best_df_iter[[sn]], best_df)
    }
  }
  source("./model_selection_plot_woagg.R")
  source("./model_selection_stat.R")
  if(cs %in% c("moderate", "weak", "moderate_strong") ){
    p_size_ls[[cs]] <- annotate_figure(p_size, top = text_grob(paste0("\nModel size selection accuracy\n"), size = 14, hjust = -0.15,x=0))
  }
  
  library(grid) 
  p_model_select <- annotate_figure(p_model_select, top = text_grob("Iteration examples", size = 16, face = "bold", hjust = -0.15,x=0), #"Model Selection Process"
                                    fig.lab = "B.", fig.lab.face = "bold", fig.lab.size = 18)
  
  p_size <- annotate_figure(p_size, top = text_grob("Error in 100 iterations", size = 16, face = "bold", hjust = -0.15,x=0),#text_grob("Strong clustering condition", size = 18, face = "bold"),
                            fig.lab = "A.", fig.lab.face = "bold", fig.lab.size = 18)
  p_size <- annotate_figure(p_size, top = text_grob(paste0("Model Size Selection Accuracy (",cs," clustering)\n"), size = 18, face = "bold")) #  (150 obs/cluster)
  
  f <- paste0("./res/",sim_condition$sigma_rdm_fix_ratio,"/model_select_example_",model_size,"_",cluster_size,en,".png")
  p_model_select %>% ggsave(filename=f, width = 10, height = 5.5, bg="white")
  
  f <- paste0("./res/",sim_condition$sigma_rdm_fix_ratio,"/model_select_error_",model_size,"_",cluster_size,en,".png")
  p_size %>% ggsave(filename=f, width = 10, height = 4.5, bg="white")
  
  
  img1 <- image_read(paste0("./res/",sim_condition$sigma_rdm_fix_ratio,"/model_select_error_",model_size,"_",cluster_size,en,".png"))
  img2 <- image_read(paste0("./res/",sim_condition$sigma_rdm_fix_ratio,"/model_select_example_",model_size,"_",cluster_size,en,".png"))
  combined_image <- image_append(c(img1, img2), stack = T)
  image_write(combined_image, paste0("./res/",sim_condition$sigma_rdm_fix_ratio,"/model_select_",cs,".png"))
  
} 

