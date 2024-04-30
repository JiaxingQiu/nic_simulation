rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
set.seed(333)
library(dplyr)
library(tidyr)
library(pROC)
library(foreach)
library(doParallel)
library(ggplot2)


for(u in c("nic", "ass", "stp", "lss", "do")){
  path = paste0("../../nic_simulation/utils/",u,"_utils")
  flst = list.files( path)
  sapply(c(paste(path,flst,sep="/")), source, .GlobalEnv)
}
# ---- load data ----
library(readxl)
data <- read_excel("./data/PAS Challenge Model Data.xlsx")
data_mdl <- assign.dict(data, get.dict(data))
data <- read_excel("./data/PAS Challenge Demographic Data.xlsx")
data_demo <- assign.dict(data, get.dict(data))
# data_demo <- data_demo[,setdiff(colnames(data_demo), c("Apgar5", "Apgar1"))]
data <- read_excel("./data/PAS Challenge Cross-Validation Folds.xlsx")
data_cv <- assign.dict(data, get.dict(data))
data <- read_excel("./data/PAS Challenge Outcome Data.xlsx")
data_outc <- assign.dict(data, get.dict(data))

source("./stepwise_vital.R")
source("./stepwise_combined.R")
source("./stepwise_demo.R")

p <- list()
for(mn in c("clustered_combined", "unclustered_demo")){ # , "clustered_vital"
  res_df <- readRDS(paste0("./res/fwd_",mn,".RDS"))
  # # rescale all the criteria between 0-1
  # for(cr in c("cvdev", "cvpred","nic","bic","aic","dev")){
  #   if(cr == "cvpred") res_df[[cr]] <- -res_df[[cr]]
  #   res_df[[cr]] <- (res_df[[cr]] - min(res_df[[cr]],na.rm=T))/(max(res_df[[cr]],na.rm = T) - min(res_df[[cr]],na.rm=T))
  # }
  # res_df_long <- res_df %>%
  #   pivot_longer(
  #     cols = c(nic, aic, bic, dev, cvdev, cvpred),  # Specify columns to lengthen
  #     names_to = "score",  # New column for the names
  #     values_to = "value"  # New column for the values
  #   )
  # res_df_long$score <- factor(res_df_long$score, levels=c("cvdev","cvpred", "nic", "bic", "aic", "dev"))
  # levels(res_df_long$score) <- c("cvDeviance", "cvAUC", "NICc", "BIC", "AIC", "Deviance")
  # p[[mn]] <- ggplot(res_df_long, aes(x = model_size, y = value, group = score, color = score)) +
  #   geom_line() +
  #   scale_color_manual(values = c("NICc" = "red", "AIC" = "blue", "BIC" = "orange", "cvDeviance" = "black", "Deviance" = "gray", "cvAUC" = "green")) +
  #   theme_minimal() +
  #   # scale_y_continuous(labels = scales::scientific_format()) +
  #   scale_y_continuous(breaks = function(x) {
  #     seq(from = min(x), to = max(x), length.out = 5)
  #   }) +
  #   labs(subtitle= ifelse(mn=="clustered_combined", "Demographic + Vital signs (clustered)", "Demographic (unclustered)" ), 
  #        x = "Model Size", 
  #        y = "Value (e+03)", 
  #        color = "Criterion") +
  #   theme(text = element_text(face = "bold"),
  #         plot.subtitle = element_text(size=12, face="bold"),
  #         axis.title = element_text(size=12),
  #         axis.text = element_text(size=10),
  #         legend.title = element_text(size=12), 
  #         legend.text = element_text(size=10))
  
  res_df_long <- res_df %>%
    dplyr::select(-cvpred) %>%
    pivot_longer(
      cols = c(nic, aic, bic, dev, cvdev),  # Specify columns to lengthen
      names_to = "score",  # New column for the names
      values_to = "value"  # New column for the values
    )
  res_df_long$score <- factor(res_df_long$score, levels=c("cvdev","cvpred", "nic","bic","aic","dev"))
  levels(res_df_long$score) <- c("cvDeviance", "cvAUC", "NICc","BIC","AIC", "Deviance")
  
  best_df <- data.frame() 
  for(score in c("cvdev", "nic", "aic", "bic")){
    best_size <- res_df$model_size[which(res_df[,score]==min(res_df[,score]))][1]
    best_score <- res_df[,score][which(res_df[,score]==min(res_df[,score]))][1]
    score_1se <- sd(res_df[,score])/sqrt(nrow(res_df))
    best_size_1se_min <- min(res_df$model_size[which(abs(res_df[,score]-min(res_df[,score]))<=score_1se)])
    best_size_1se_max <- max(res_df$model_size[which(abs(res_df[,score]-min(res_df[,score]))<=score_1se)])
    best_df <- bind_rows(best_df, data.frame(score,best_size, best_score, score_1se, best_size_1se_min,best_size_1se_max))
  }
  best_df$score <- factor(best_df$score, levels=c("cvdev","cvpred", "nic","bic","aic","dev"))
  levels(best_df$score) <- c("cvDeviance", "cvAUC", "NICc","BIC","AIC", "Deviance")
  
  if(mn=="clustered_combined"){
    title = "Clustered"
    subtitle = "Demographic + Vital signs"
  }
  if(mn=="clustered_vital"){
    title = "Clustered"
    subtitle = "Vital signs"
  }
  if(mn=="unclustered_demo"){
    title = "Non-clustered"
    subtitle = "Demographic"
  }
  p[[mn]] <- ggplot(res_df_long, aes(x = model_size, y = value)) +
    # geom_point(data = res_df_long[which(res_df_long$score=="cvDeviance"),], size = 2) + 
    geom_line(aes(group = score, color = score)) +
    geom_text(data = res_df_long[which(res_df_long$score=="cvDeviance"),], aes(label=x_picked), size = 3, hjust = -0.15, angle = 90) + 
    scale_color_manual(values = c("NICc" = "red", "AIC" = "blue", "BIC" = "darkorange", "cvDeviance" = "black", "Deviance" = "gray")) +
    theme_minimal() +
    geom_errorbar(data = best_df, aes(x = best_size, xmin=best_size_1se_min, xmax=best_size_1se_max, y = best_score, color=score), width=1)+
    geom_point(data = best_df, aes(x = best_size, y = best_score, color=score), size = 1.5)+
    # scale_y_continuous(labels = scales::scientific_format()) +
    scale_y_continuous(
      limits = function(y) { c(min(y)-0.01*(max(y)-min(y)), max(y)+0.1*(max(y)-min(y))) },
      breaks = function(y) { seq(from = min(y), to = max(y), length.out = 5) },
      labels = function(y) sprintf("%.2f", y / 1000) ) +
    labs(title = title,
         subtitle = subtitle, 
         x = "Model Size", 
         y = "Value (e+03)", 
         color = "Criterion") +
    theme(text = element_text(face = "bold"),
          plot.title = element_text(size=16, face="bold"),
          plot.subtitle = element_text(size=12, face="bold"),
          axis.title = element_text(size=12),
          axis.text = element_text(size=10),
          legend.title = element_text(size=12), 
          legend.text = element_text(size=10))
}

p_case <- ggpubr::ggarrange(plotlist = p, common.legend = T, legend = "bottom") 
p_case %>% ggsave(filename="./res/fig_case.png", width = 10, height = 6, bg="white")
