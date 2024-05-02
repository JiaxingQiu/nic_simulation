# setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# rm(list = ls())

library(dplyr)
library(rslurm)
library(Matrix)
library(lme4)
library(MASS)
library(pROC)
library(foreach)
library(doParallel)

source("./sim_functions.R")
for(u in c("nic", "ass", "stp", "lss", "do")){
  path = paste0("./utils/",u,"_utils")
  flst = list.files( path)
  sapply(c(paste(path,flst,sep="/")), source, .GlobalEnv)
}
source("./sim_conditions_model_selection.R")
sim_condition = simulation_conditions[which(simulation_conditions$id==1),]


run_wrapper_lr <- function(sim_condition) {
  results_list = list()
  for(i in 1:sim_condition$iter){
    tryCatch({
      res <- generate_data(sim_condition$n_cluster,
                           sim_condition$n_obs_per_cluster,
                           sim_condition$n_ttl_betas, 
                           sim_condition$fix_rdm_ratio,
                           sim_condition$sigma_fix,
                           sim_condition$sigma_rdm_fix_ratio,
                           sim_condition$ar1_phi,
                           sim_condition$na_rate)
      res <- generate_overfit(res)
      
      # step wise loodev in ground truth model
      df <- as.data.frame(res$data_org)
      df$y <- res$y
      df$cl <- res$c
      fix_vars <- grep("^fix\\d+$", colnames(res$data_org), value = TRUE)
      rdm_vars <- grep("^rdm\\d+$", colnames(res$data_org), value = TRUE)
      m0_sl <- modified_stepwise_glm_parallel(df = df,
                                        y = "y",
                                        x = c(fix_vars, rdm_vars),
                                        c = "cl", 
                                        maxstep = min(30,length(c(fix_vars, rdm_vars))),
                                        eval_ls=c("Deviance", "AIC", "BIC", "NIC", "looDeviance"),
                                        eval_by="looDeviance",
                                        family = "binomial",
                                        forward = T,
                                        free_cores = 2)
      res_df_0 <- format_forward(m0_sl)
       
      # rank most likely to overfit vars step wise loodev
      df <- as.data.frame(res$data)
      df$y <- res$y
      df$cl <- res$c
      fix_vars <- grep("^fix\\d+$", colnames(res$data_org), value = TRUE)
      rdm_vars <- setdiff(grep("^rdm\\d+$", colnames(res$data),value = TRUE), grep("^rdm\\d+$", colnames(res$data_org), value = TRUE))
      mo_sl <- modified_stepwise_glm_parallel(df = df,
                                              y = "y",
                                              x = c(fix_vars, rdm_vars),
                                              c = "cl", 
                                              maxstep = min(30,length(c(fix_vars, rdm_vars))),
                                              eval_ls=c("Deviance", "AIC", "BIC", "NIC", "looDeviance"),
                                              eval_by="looDeviance",
                                              family = "binomial",
                                              forward = T,
                                              free_cores = 2)
      res_df_o <- format_forward(mo_sl)
      
      
      # stepwise from least to most to overfit
      df <- as.data.frame(res$data)
      df$y <- res$y
      df$cl <- res$c
      x_left <- c(rev(res_df_0$x_picked), res_df_o$x_picked)
      x_picked <- c()
      res_df <- data.frame()
      for(x in x_left){
        x_picked <- c(x_picked, x)
        x_left <- setdiff(x_left, x)
        res_ls <- all_subset_glm_parallel(df, y="y", x, c="cl", 
                                          size=length(x),
                                          eval_ls=c("Deviance", "AIC", "BIC", "NIC","loopred", "looDeviance"),
                                          family = "binomial",
                                          free_cores = 2)
        res_df <- bind_rows (res_df, data.frame(x_picked = x,
                                                model_size = length(x_picked),
                                                dev = res_ls$s1[[1]]$Deviance,
                                                aic = res_ls$s1[[1]]$AIC,
                                                bic = res_ls$s1[[1]]$BIC,
                                                nic = res_ls$s1[[1]]$NIC,
                                                loopred = res_ls$s1[[1]]$loopred,
                                                loodev = res_ls$s1[[1]]$looDeviance ) )
      }
      
      res_df$id <- sim_condition$id
      res_df$N = nrow(res$data)
      results_list[[i]] = res_df
      
    }, error = function(e){
      print(e)
      print(paste0("skip iteration ",i))
    })
  }
  results_list <- Filter(function(x) !is.null(x), results_list)
  toReturn = do.call("rbind", results_list)
  return(toReturn)
}

run_wrapper_lm <- function(sim_condition) {
  results_list = list()
  for(i in 1:sim_condition$iter){
    tryCatch({
      res <- generate_data(sim_condition$n_cluster,
                           sim_condition$n_obs_per_cluster,
                           sim_condition$n_ttl_betas, 
                           sim_condition$fix_rdm_ratio,
                           sim_condition$sigma_fix,
                           sim_condition$sigma_rdm_fix_ratio,
                           sim_condition$ar1_phi,
                           sim_condition$na_rate)
      res <- generate_overfit(res)
      
      # step wise loodev in ground truth model
      df <- as.data.frame(res$data_org)
      df$y <- res$y
      df$cl <- res$c
      fix_vars <- grep("^fix\\d+$", colnames(res$data_org), value = TRUE)
      rdm_vars <- grep("^rdm\\d+$", colnames(res$data_org), value = TRUE)
      m0_sl <- modified_stepwise_glm_parallel(df = df,
                                              y = "y",
                                              x = c(fix_vars, rdm_vars),
                                              c = "cl", 
                                              maxstep = min(30,length(c(fix_vars, rdm_vars))),
                                              eval_ls=c("Deviance", "AIC", "BIC", "NIC", "looDeviance"),
                                              eval_by="looDeviance",
                                              family = "gaussian",
                                              forward = T,
                                              free_cores = 2)
      res_df_0 <- format_forward(m0_sl)
      
      # rank most likely to overfit vars step wise loodev
      df <- as.data.frame(res$data)
      df$y <- res$y
      df$cl <- res$c
      fix_vars <- grep("^fix\\d+$", colnames(res$data_org), value = TRUE)
      rdm_vars <- setdiff(grep("^rdm\\d+$", colnames(res$data),value = TRUE), grep("^rdm\\d+$", colnames(res$data_org), value = TRUE))
      mo_sl <- modified_stepwise_glm_parallel(df = df,
                                              y = "y",
                                              x = c(fix_vars, rdm_vars),
                                              c = "cl", 
                                              maxstep = min(30,length(c(fix_vars, rdm_vars))),
                                              eval_ls=c("Deviance", "AIC", "BIC", "NIC", "looDeviance"),
                                              eval_by="looDeviance",
                                              family = "gaussian",
                                              forward = T,
                                              free_cores = 2)
      res_df_o <- format_forward(mo_sl)
      
      
      # stepwise from least to most to overfit
      df <- as.data.frame(res$data)
      df$y <- res$y
      df$cl <- res$c
      x_left <- c(rev(res_df_0$x_picked), res_df_o$x_picked)
      x_picked <- c()
      res_df <- data.frame()
      for(x in x_left){
        x_picked <- c(x_picked, x)
        x_left <- setdiff(x_left, x)
        res_ls <- all_subset_glm_parallel(df, y="y", x, c="cl", 
                                          size=length(x),
                                          eval_ls=c("Deviance", "AIC", "BIC", "NIC","loopred", "looDeviance"),
                                          family = "gaussian",
                                          free_cores = 2)
        res_df <- bind_rows (res_df, data.frame(x_picked = x,
                                                model_size = length(x_picked),
                                                dev = res_ls$s1[[1]]$Deviance,
                                                aic = res_ls$s1[[1]]$AIC,
                                                bic = res_ls$s1[[1]]$BIC,
                                                nic = res_ls$s1[[1]]$NIC,
                                                loopred = res_ls$s1[[1]]$loopred,
                                                loodev = res_ls$s1[[1]]$looDeviance ) )
      }
      
      res_df$id <- sim_condition$id
      res_df$N = nrow(res$data)
      results_list[[i]] = res_df
      
    }, error = function(e){
      print(e)
      print(paste0("skip iteration ",i))
    })
  }
  results_list <- Filter(function(x) !is.null(x), results_list)
  toReturn = do.call("rbind", results_list)
  return(toReturn)
}
