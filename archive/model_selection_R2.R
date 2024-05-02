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
library(pscl)  # For pseudo-R^2 calculations

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
                           sim_condition$na_rate)
      res <- generate_overfit(res)
      
      # rank ground truth vars by partial R2 in ground truth model
      df <- as.data.frame(res$data_org)
      df$y <- res$y
      df$cl <- res$c
      fix_vars <- grep("^fix\\d+$", colnames(res$data_org), value = TRUE)
      rdm_vars <- grep("^rdm\\d+$", colnames(res$data_org), value = TRUE)
      mdl0 <- glm(formula(paste0("y~",paste0(c(fix_vars,rdm_vars),collapse = "+"))), data = df, family = "binomial")
      # partial_r2 <- c()
      # for (var in c(fix_vars, rdm_vars) ) {
      #   # Model without the specific predictor
      #   reduced_formula <- formula(paste0("y~",paste0(setdiff(c(fix_vars,rdm_vars),var),collapse = "+")))
      #   reduced_model <- update(mdl0, formula = reduced_formula)
      #   # Conduct ANOVA likelihood ratio test
      #   anova_result <- anova(reduced_model, mdl0, test = "Chisq")
      #   chi_sq_stat <- anova_result[2, "Deviance"]
      #   # p_value <- anova_result[2, "Pr(>Chi)"]
      #   partial_r2 <- c(partial_r2, chi_sq_stat)
      # }
      # names(partial_r2) <- c(fix_vars, rdm_vars)
      partial_r2_0 <- summary(mdl0)$coefficients[, "z value"]^2 # close enough
      
      # rank most likely to overfit vars
      df <- as.data.frame(res$data)
      df$y <- res$y
      df$cl <- res$c
      fix_vars <- grep("^fix\\d+$", colnames(res$data_org), value = TRUE)
      rdm_vars <- setdiff(grep("^rdm\\d+$", colnames(res$data),value = TRUE), grep("^rdm\\d+$", colnames(res$data_org), value = TRUE))
      mdlo <- glm(formula(paste0("y~",paste0(c(rdm_vars),collapse = "+"))), data = df, family = "binomial")
      partial_r2_o <- summary(mdlo)$coefficients[, "z value"]^2 # close enough
      
      
      # stepwise from least to most to overfit
      df <- as.data.frame(res$data)
      df$y <- res$y
      df$cl <- res$c
      x_left <- c(setdiff(names(sort(partial_r2_0)),"(Intercept)"), setdiff(names(sort(-partial_r2_o)),"(Intercept)"))
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
                           sim_condition$na_rate)
      res <- generate_overfit(res)
      
      # rank ground truth vars by partial R2 in ground truth model
      df <- as.data.frame(res$data_org)
      df$y <- res$y
      df$cl <- res$c
      fix_vars <- grep("^fix\\d+$", colnames(res$data_org), value = TRUE)
      rdm_vars <- grep("^rdm\\d+$", colnames(res$data_org), value = TRUE)
      mdl0 <- glm(formula(paste0("y~",paste0(c(fix_vars,rdm_vars),collapse = "+"))), data = df, family = "gaussian")
      partial_r2 <- c()
      for (var in c(fix_vars, rdm_vars) ) {
        # Model without the specific predictor
        reduced_formula <- formula(paste0("y~",paste0(setdiff(c(fix_vars,rdm_vars),var),collapse = "+")))
        reduced_model <- update(mdl0, formula = reduced_formula)
        # Conduct ANOVA likelihood ratio test
        anova_result <- anova(reduced_model, mdl0, test = "Chisq")
        chi_sq_stat <- anova_result[2, "Deviance"]
        # p_value <- anova_result[2, "Pr(>Chi)"]
        partial_r2 <- c(partial_r2, chi_sq_stat)
      }
      names(partial_r2) <- c(fix_vars, rdm_vars)
      partial_r2_0 <- partial_r2
      # partial_r2_0 <- summary(mdl0)$coefficients[, "z value"]^2 # close enough
      
      # rank most likely to overfit vars
      df <- as.data.frame(res$data)
      df$y <- res$y
      df$cl <- res$c
      fix_vars <- grep("^fix\\d+$", colnames(res$data_org), value = TRUE)
      rdm_vars <- setdiff(grep("^rdm\\d+$", colnames(res$data),value = TRUE), grep("^rdm\\d+$", colnames(res$data_org), value = TRUE))
      mdlo <- glm(formula(paste0("y~",paste0(c(rdm_vars),collapse = "+"))), data = df, family = "gaussian")
      partial_r2 <- c()
      for (var in c(fix_vars, rdm_vars) ) {
        # Model without the specific predictor
        reduced_formula <- formula(paste0("y~",paste0(setdiff(c(fix_vars,rdm_vars),var),collapse = "+")))
        reduced_model <- update(mdl0, formula = reduced_formula)
        # Conduct ANOVA likelihood ratio test
        anova_result <- anova(reduced_model, mdl0, test = "Chisq")
        chi_sq_stat <- anova_result[2, "Deviance"]
        # p_value <- anova_result[2, "Pr(>Chi)"]
        partial_r2 <- c(partial_r2, chi_sq_stat)
      }
      names(partial_r2) <- c(fix_vars, rdm_vars)
      partial_r2_o <- partial_r2
      # partial_r2_o <- summary(mdlo)$coefficients[, "z value"]^2 # close enough
      
      
      # stepwise from least to most to overfit
      df <- as.data.frame(res$data)
      df$y <- res$y
      df$cl <- res$c
      x_left <- c(setdiff(names(sort(partial_r2_0)),"(Intercept)"), setdiff(names(sort(-partial_r2_o)),"(Intercept)"))
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
#   run_wrapper_lr,
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
# save(sjob, file = paste0("nic_simulation_run_lm_model_select_1day.Rdata"))
# 
# 
