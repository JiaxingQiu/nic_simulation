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

source("./stepwise_combined.R")
source("./stepwise_demo.R")

p <- list()
for(mn in c("clustered_combined", "unclustered_demo")){ # , "clustered_vital"
  res_df <- readRDS(paste0("./res/fwd_",mn,".RDS"))
  
  s_df <- res_df[,colnames(res_df)[!endsWith(colnames(res_df),"_x_picked")]]
  s_df_long <- s_df %>%
    pivot_longer(
      cols = c(nicc, nic, aic, bic, dev, cvdev),  # Specify columns to lengthen
      names_to = "score",  # New column for the names
      values_to = "value"  # New column for the values
    )
  s_df_long$score <- factor(s_df_long$score, levels=c("cvdev","nicc","nic","aic","bic","dev"))
  levels(s_df_long$score) <- c("cvDeviance", "NICc","NIC","AIC","BIC", "Deviance")
  
  x_df <- res_df[,c("model_size", colnames(res_df)[endsWith(colnames(res_df),"_x_picked")])]
  colnames(x_df) <- stringr::str_to_lower( gsub("_x_picked","",gsub("iance","", colnames(x_df))) )
  x_df_long <- x_df %>%
    pivot_longer(
      cols = c(nicc, nic, aic, bic, cvdev),  # Specify columns to lengthen
      names_to = "score",  # New column for the names
      values_to = "xpick"  # New column for the values
    )
  x_df_long$score <- factor(x_df_long$score, levels=c("cvdev","nicc","nic","aic","bic"))
  levels(x_df_long$score) <- c("cvDeviance", "NICc","NIC","AIC","BIC")
  x_df_long <- merge(x_df_long, s_df_long)
  fix_top2 <- function(df){
    maxi <- max(df$value)
    sec_maxi <- max(df$value[df$value<maxi])
    df$value[which(df$value == maxi)] <- maxi-sd(df$value)
    df$value[which(df$value == sec_maxi)] <- sec_maxi-sd(df$value)
    return(df)
  }
  tmp <- group_by(x_df_long, score)
  x_df_long <- do(tmp, fix_top2(.) )
  
  best_df <- data.frame() 
  for(score in c("cvdev", "nicc", "nic", "aic", "bic")){
    best_size <- res_df$model_size[which(res_df[,score]==min(res_df[,score]))][1]
    best_score <- res_df[,score][which(res_df[,score]==min(res_df[,score]))][1]
    score_1se <- sd(res_df[,score])/sqrt(nrow(res_df))
    best_size_1se_min <- min(res_df$model_size[which(abs(res_df[,score]-min(res_df[,score]))<=score_1se)])
    best_size_1se_max <- max(res_df$model_size[which(abs(res_df[,score]-min(res_df[,score]))<=score_1se)])
    best_df <- bind_rows(best_df, data.frame(score,best_size, best_score, score_1se, best_size_1se_min,best_size_1se_max))
  }
  best_df$score <-factor(best_df$score, levels=c("cvdev","nicc","nic","aic","bic","dev"))
  levels(best_df$score) <- c("cvDeviance", "NICc","NIC","AIC","BIC", "Deviance")
  
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
  p_s <- ggplot(s_df_long, aes(x = model_size, y = value)) +
    geom_line(aes(group = score, color = score)) +
    geom_text(data = x_df_long[which(x_df_long$score=="cvDeviance"),], aes(label=xpick), size = 3, hjust = -0.15, angle = 90) + 
    scale_color_manual(values = c("NICc" = "red", "NIC"="lightblue3", "AIC" = "blue", "BIC" = "darkorange", "cvDeviance" = "black", "Deviance" = "gray")) +
    theme_minimal() +
    geom_errorbar(data = best_df, aes(x = best_size, xmin=best_size_1se_min, xmax=best_size_1se_max, y = best_score, color=score), width=1)+
    geom_point(data = best_df, aes(x = best_size, y = best_score, color=score), size = 1.5)+
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
          legend.text = element_text(size=10),
          legend.position = "bottom")
  
  p_x <- ggplot(s_df_long[which(!s_df_long$score%in%c("cvDeviance","Deviance") ),], aes(x = model_size, y = value)) +
    # geom_point(data = s_df_long[which(s_df_long$score=="cvDeviance"),], size = 2) + 
    geom_line(aes(group = score, color = score)) +
    geom_text(data = x_df_long[which(!x_df_long$score%in%c("cvDeviance","Deviance") ),], aes(label=xpick, color=score), size = 3, hjust = -0.15, angle = 90) + 
    scale_color_manual(values = c("NICc" = "red", "NIC"="lightblue3", "AIC" = "blue", "BIC" = "darkorange", "cvDeviance" = "black", "Deviance" = "gray")) +
    theme_minimal() +
    geom_errorbar(data = best_df[which(!best_df$score%in%c("cvDeviance","Deviance") ),], aes(x = best_size, xmin=best_size_1se_min, xmax=best_size_1se_max, y = best_score, color=score), width=1)+
    geom_point(data = best_df[which(!best_df$score%in%c("cvDeviance","Deviance") ),], aes(x = best_size, y = best_score, color=score), size = 1.5)+
    facet_wrap(~score) + 
    scale_y_continuous(
      limits = function(y) { c(min(y)-0.01*(max(y)-min(y)), max(y)+0.1*(max(y)-min(y))) },
      breaks = function(y) { seq(from = min(y), to = max(y), length.out = 5) },
      labels = function(y) sprintf("%.2f", y / 1000) ) +
    labs(x = "Model Size", 
         y = "Value (e+03)", 
         color = "Criterion") +
    theme(text = element_text(face = "bold"),
          plot.title = element_text(size=16, face="bold"),
          plot.subtitle = element_text(size=12, face="bold"),
          strip.text = element_text(size=16, face="bold"),
          axis.title = element_text(size=12),
          axis.text = element_text(size=10),
          legend.title = element_text(size=12), 
          legend.text = element_text(size=10))
  p[[mn]] <- ggarrange(p_s, p_x, nrow=2, ncol=1, heights = c(1,1), common.legend = T, legend = "none")
  leg <- get_legend(p_s)
  

}

p_case <- ggpubr::ggarrange(plotlist = p, common.legend = T, legend.grob = leg, legend = "bottom") 
p_case %>% ggsave(filename="./res/fig_case.png", width = 12, height = 12, bg="white")
