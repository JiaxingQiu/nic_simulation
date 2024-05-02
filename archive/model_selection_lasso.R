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
                           sim_condition$na_rate,
                           family = "binomial")
      res <- generate_overfit(res)
      
      df <- as.data.frame(res$data)
      df$y <- res$y
      df$cl <- res$c
      fix_vars <- grep("^fix\\d+$", colnames(res$data), value = TRUE)
      rdm_vars <- grep("^rdm\\d+$", colnames(res$data), value = TRUE)
      dict_df <- get.dict(df)
      dict_df$type[which(dict_df$varname=="cl")] <- "key"
      dict_df$unique_per_sbj[which(dict_df$varname=="cl")]  <- TRUE
      rownames(dict_df) <- NULL
      x_labels_linear = c(fix_vars,rdm_vars)
      y_label = "y"
      cluster_label = "cl"
      lasso_cv_select <- front_lasso_select(data=df,
                                            dict_data = dict_df,
                                            y_label = y_label,
                                            cluster_label = cluster_label,
                                            x_labels_linear = x_labels_linear,
                                            # x_labels_nonlin_rcs5 = rdm_vars,
                                            y_map_func="probability",
                                            y_map_max=1,
                                            aggregate_per = "row",
                                            return_performance = F,
                                            lasso_by = "group",
                                            tune_by = "misclass",
                                            lambda="min",
                                            lambda_value = c())
      
      # lambda per zero-coef trace
      lasso_trace <- lasso_cv_select$mdl_obj$x_select_mdls_grouped$lasso_trace
      lasso_coef <- as.data.frame( lasso_trace$beta )
      lasso_coef <- as.data.frame( t(lasso_coef) )
      lasso_coef$s <- as.numeric( gsub("s","", rownames(lasso_coef)))
      lmd_s <- c()
      var_s <- c()
      for (var in setdiff(colnames(lasso_coef),"s") ){
        lmd_s <- c(lmd_s, max(lasso_coef$s[which(lasso_coef[,var]==0)]) )
        var_s <- c(var_s, var)
      }
      lambda_zero_coef <- data.frame(varname = var_s, 
                                     lambda = lasso_trace$lambda[lmd_s+1],
                                     log_lambda = log(lasso_trace$lambda[lmd_s+1]))
      lambda_zero_coef <- lambda_zero_coef[complete.cases(lambda_zero_coef),]
      lambda_zero_coef <- lambda_zero_coef[order(lambda_zero_coef$log_lambda),]
      rownames( lambda_zero_coef ) <- NULL
      
      res_df <- data.frame()
      for(l in sort(lambda_zero_coef$lambda[!is.na(lambda_zero_coef$lambda)]) ){
        
        # # retrain lasso model
        # mdl <- gglasso::gglasso(x = as.matrix(df[,c(x_labels_linear)]),
        #                         y = ifelse(df[,c(y_label)]==1, 1, -1),
        #                         group = NULL,
        #                         loss = "logit",
        #                         lambda = l)
        # rmvd_vars <- rownames(coef(mdl))[coef(mdl)==0]
        # kept_vars <- setdiff(rownames(coef(mdl))[!coef(mdl)==0],"(Intercept)")
        # stopifnot(mdl$df==length(kept_vars))
        rmvd_vars <- lambda_zero_coef$varname[which(lambda_zero_coef$lambda <= l)]
        kept_vars <- setdiff(x_labels_linear, rmvd_vars)
        
        
        if(length(kept_vars)>1){
          m1 <- fit_eval_glm(y=df[,"y"], c=df[,"cl"], data=res$data[,kept_vars], family="binomial")
          res_df <- bind_rows(res_df,data.frame(aic = m1$aic,
                                                bic = m1$bic,
                                                nic = m1$nic,
                                                dev = m1$deviance,
                                                loopred = m1$loopred,
                                                loodev = m1$looDeviance,
                                                model_size = length(kept_vars),
                                                x_rmvd = paste0(rmvd_vars, collapse = ";")))
        }
      }
      
      res_df$id <- sim_condition$id
      res_df$iter <- i
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
                           sim_condition$na_rate,
                           family = "guassian")
      res <- generate_overfit(res)
      
      df <- as.data.frame(res$data)
      df$y <- res$y
      df$cl <- res$c
      fix_vars <- grep("^fix\\d+$", colnames(res$data), value = TRUE)
      rdm_vars <- grep("^rdm\\d+$", colnames(res$data), value = TRUE)
      dict_df <- get.dict(df)
      dict_df$type[which(dict_df$varname=="cl")] <- "key"
      dict_df$unique_per_sbj[which(dict_df$varname=="cl")]  <- TRUE
      rownames(dict_df) <- NULL
      x_labels_linear = c(fix_vars,rdm_vars)
      y_label = "y"
      cluster_label = "cl"
      lasso_cv_select <- front_lasso_select(data=df,
                                            dict_data = dict_df,
                                            y_label = y_label,
                                            cluster_label = cluster_label,
                                            x_labels_linear = x_labels_linear,
                                            aggregate_per = "row",
                                            return_performance = F,
                                            lasso_by = "group",
                                            tune_by = "logloss",
                                            lambda="min",
                                            lambda_value = c())
      
      # lambda per zero-coef trace
      lasso_trace <- lasso_cv_select$mdl_obj$x_select_mdls_grouped$lasso_trace
      lasso_coef <- as.data.frame( lasso_trace$beta )
      lasso_coef <- as.data.frame( t(lasso_coef) )
      lasso_coef$s <- as.numeric( gsub("s","", rownames(lasso_coef)))
      lmd_s <- c()
      var_s <- c()
      for (var in setdiff(colnames(lasso_coef),"s") ){
        lmd_s <- c(lmd_s, max(lasso_coef$s[which(lasso_coef[,var]==0)]) )
        var_s <- c(var_s, var)
      }
      lambda_zero_coef <- data.frame(varname = var_s, 
                                     lambda = lasso_trace$lambda[lmd_s+1],
                                     log_lambda = log(lasso_trace$lambda[lmd_s+1]))
      lambda_zero_coef <- lambda_zero_coef[complete.cases(lambda_zero_coef),]
      lambda_zero_coef <- lambda_zero_coef[order(lambda_zero_coef$log_lambda),]
      rownames( lambda_zero_coef ) <- NULL
      
      res_df <- data.frame()
      for(l in sort(lambda_zero_coef$lambda[!is.na(lambda_zero_coef$lambda)]) ){
        
        # # retrain lasso model
        # mdl <- gglasso::gglasso(x = as.matrix(df[,c(x_labels_linear)]),
        #                         y = df[,c(y_label)],
        #                         group = NULL,
        #                         lambda = l)
        # rmvd_vars <- rownames(coef(mdl))[coef(mdl)==0]
        # kept_vars <- setdiff(rownames(coef(mdl))[!coef(mdl)==0],"(Intercept)")
        rmvd_vars <- lambda_zero_coef$varname[which(lambda_zero_coef$lambda <= l)]
        kept_vars <- setdiff(x_labels_linear, rmvd_vars)
        if(length(kept_vars)>1){
          m1 <- fit_eval_glm(y=df[,"y"], c=df[,"cl"], data=res$data[,kept_vars], family="gaussian")
          res_df <- bind_rows(res_df,data.frame(aic = m1$aic,
                                                bic = m1$bic,
                                                nic = m1$nic,
                                                dev = m1$deviance,
                                                loopred = m1$loopred,
                                                loodev = m1$looDeviance,
                                                model_size = length(kept_vars),
                                                x_rmvd = paste0(rmvd_vars, collapse = ";")))
        }
      }
      
      res_df$id <- sim_condition$id
      res_df$iter <- i
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
# 
# sjob = slurm_map(
#   split(simulation_conditions, simulation_conditions$id),
#   run_wrapper_lm,
#   nodes=nrow(simulation_conditions),
#   cpus_per_node = 1,
#   submit = TRUE,
#   preschedule_cores = F,
#   slurm_options =
#     c(account = "netlab", partition = "standard", time = "1-00:00:00"), # standard
#   global_objects = lsf.str()
# )
# save(sjob, file = paste0("nic_simulation_run_lr_model_select_1day.Rdata"))
# 

