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

run_wrapper_lr <- function(sim_condition, overfit_type = c("poly","rcs", "random")[1]) {
  results_list = list()
  for(i in 1:sim_condition$iter){
    results_list[[i]] <- "null"
    while(T){
      tryCatch({
        res <- generate_data(sim_condition$n_cluster,
                             sim_condition$n_obs_per_cluster,
                             sim_condition$n_ttl_betas, 
                             sim_condition$fix_rdm_ratio,
                             sim_condition$sigma_fix,
                             sim_condition$sigma_rdm_fix_ratio,
                             sim_condition$ar1_phi,
                             sim_condition$na_rate)
        
        # add overfit predictors with high order poly
        res <- generate_overfit(res, type = overfit_type)
        
        # lr model selection
        df <- as.data.frame(res$data)
        df$y <- res$y
        df$cl <- res$c
        fix_vars <- grep("^fix\\d+$", colnames(res$data), value = TRUE)
        rdm_vars <- grep("^rdm\\d+$", colnames(res$data), value = TRUE)
        
        # step-wise forward 
        res_df <- NULL
        for(cr in c("AIC", "BIC", "NIC", "NICc", "looDeviance") ){
          m1_sl <- modified_stepwise_glm_parallel(df = df,
                                                  y = "y",
                                                  x = c(fix_vars, rdm_vars),
                                                  c = "cl", 
                                                  maxstep = min(30,length(c(fix_vars, rdm_vars))),
                                                  eval_ls=c(cr,"Deviance"),
                                                  eval_by=cr,
                                                  family = "binomial",
                                                  forward = T,
                                                  free_cores = 2)
          
          res_df_cr <- format_forward(m1_sl)
          colnames(res_df_cr)[which(colnames(res_df_cr)=="x_picked")] <- paste0(cr,"_x_picked")
          x_picked_cn <- paste0(cr,"_x_picked")
          if(cr == "looDeviance"){
            cr = c(gsub("iance","",cr),"dev")
          }
          cr <- stringr::str_to_lower(cr)
          if(is.null(res_df)) {
            res_df <- res_df_cr[,c("model_size",x_picked_cn, cr)]
          }else{
            res_df <- merge(res_df, res_df_cr[,c("model_size",x_picked_cn,cr)])
          }
        }
        
        # # fix tails in last 2 steps of loodev
        # res_df$loodev[res_df$model_size>(max(res_df$model_size)-1)] <- res_df$loodev[res_df$model_size==(max(res_df$model_size)-1)]
        stopifnot(!detect_mal(res_df, sim_condition))
        
        res_df$id <- sim_condition$id
        res_df$iter <- i
        res_df$N = nrow(res$data)
        results_list[[i]] = res_df
        
      }, error = function(e){
        print(e)
        print(paste0("skip iteration ",i))
      })
      if(!paste0(as.character(results_list[[i]]),collapse = ";") == "null"){
        break
      }else{
        print(paste0("re-run iteration ",i))
      }
    }
  }
  results_list <- Filter(function(x) !is.null(x), results_list)
  toReturn = do.call("rbind", results_list)
  return(toReturn)
}

run_wrapper_lm <- function(sim_condition, overfit_type = c("poly","rcs", "random")[1]) {
  results_list = list()
  for(i in 1:sim_condition$iter){
    results_list[[i]] <- "null"
    while(T){
      tryCatch({
        res <- generate_data(sim_condition$n_cluster,
                             sim_condition$n_obs_per_cluster,
                             sim_condition$n_ttl_betas, 
                             sim_condition$fix_rdm_ratio,
                             sim_condition$sigma_fix,
                             sim_condition$sigma_rdm_fix_ratio,
                             sim_condition$ar1_phi,
                             sim_condition$na_rate)
        
        # add overfit predictors with high order poly
        res <- generate_overfit(res, type = overfit_type)
        
        # lr model selection
        df <- as.data.frame(res$data)
        df$y <- res$y
        df$cl <- res$c
        fix_vars <- grep("^fix\\d+$", colnames(res$data), value = TRUE)
        rdm_vars <- grep("^rdm\\d+$", colnames(res$data), value = TRUE)
        
        # step-wise forward 
        res_df <- NULL
        for(cr in c("AIC", "BIC", "NIC", "NICc", "looDeviance") ){
          m1_sl <- modified_stepwise_glm_parallel(df = df,
                                                  y = "y",
                                                  x = c(fix_vars, rdm_vars),
                                                  c = "cl", 
                                                  maxstep = min(30,length(c(fix_vars, rdm_vars))),
                                                  eval_ls=c(cr,"Deviance"),
                                                  eval_by=cr,
                                                  family = "gaussian",
                                                  forward = T,
                                                  free_cores = 2)
          
          res_df_cr <- format_forward(m1_sl)
          colnames(res_df_cr)[which(colnames(res_df_cr)=="x_picked")] <- paste0(cr,"_x_picked")
          x_picked_cn <- paste0(cr,"_x_picked")
          if(cr == "looDeviance"){
            cr = c(gsub("iance","",cr),"dev")
          }
          cr <- stringr::str_to_lower(cr)
          if(is.null(res_df)) {
            res_df <- res_df_cr[,c("model_size",x_picked_cn, cr)]
          }else{
            res_df <- merge(res_df, res_df_cr[,c("model_size",x_picked_cn,cr)])
          }
        }
        # # fix tails in last 2 steps of loodev
        # res_df$loodev[res_df$model_size>(max(res_df$model_size)-1)] <- res_df$loodev[res_df$model_size==(max(res_df$model_size)-1)]
        stopifnot(!detect_mal(res_df, sim_condition))
        res_df$id <- sim_condition$id
        res_df$iter <- i
        res_df$N = nrow(res$data)
        results_list[[i]] = res_df
        
      }, error = function(e){
        print(e)
        print(paste0("skip iteration ",i))
      })
      if(!paste0(as.character(results_list[[i]]),collapse = ";") == "null"){
        break
      }else{
        print(paste0("re-run iteration ",i))
      }
    }
  }
  results_list <- Filter(function(x) !is.null(x), results_list)
  toReturn = do.call("rbind", results_list)
  return(toReturn)
}

