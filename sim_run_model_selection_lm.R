# setwd(dirname(rstudioapi::getSourceEditorContext()$path))

rm(list = ls())

library(dplyr)
library(rslurm)
library(Matrix)
library(lme4)
library(MASS)
library(pROC)
library(foreach)
library(doParallel)

list.of.packages <- c("dplyr",
                      "rslurm",
                      "MASS",
                      "lme4",
                      "Matrix",
                      "pROC",
                      "foreach",
                      "doParallel")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, lib = "/sfs/qumulo/qhome/jq2uw/R/goolf/4.3")





source("./sim_functions.R")
path = paste0("./utils/nic_utils")
flst = list.files( path)
sapply(c(paste(path,flst,sep="/")), source, .GlobalEnv)
path = paste0("./utils/ass_utils")
flst = list.files( path)
sapply(c(paste(path,flst,sep="/")), source, .GlobalEnv)
path = paste0("./utils/stp_utils")
flst = list.files( path)
sapply(c(paste(path,flst,sep="/")), source, .GlobalEnv)
source("./sim_conditions.R")
n_ttl_betas <- c(15) # fixed number of total effects
param_grid <- expand.grid(n_cluster = n_cluster,
                          n_obs_per_cluster = n_obs_per_cluster,
                          n_ttl_betas = n_ttl_betas,
                          fix_rdm_ratio = fix_rdm_ratio,
                          sigma_fix = sigma_fix,
                          sigma_rdm_fix_ratio = sigma_rdm_fix_ratio,
                          ar1_phi = ar1_phi,
                          na_rate = na_rate)
simulation_conditions <- as.data.frame(param_grid)
simulation_conditions$id <- seq(1:nrow(param_grid))
simulation_conditions$iter <- 1




#This function runs each condition (i.e. each row in the simulation condition data.frame)
# for test: 
sim_condition = simulation_conditions[which(simulation_conditions$id==1),]
# saveRDS(res, "./doug_lr_sim_data.RDS")

run_wrapper <- function(sim_condition) {
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
      
      # add overfit predictors with high order poly
      res <- generate_overfit(res)
      
      # lr model selection
      df <- as.data.frame(res$data)
      df$y <- res$y
      df$cl <- res$c
      fix_vars <- grep("^fix\\d+$", colnames(res$data), value = TRUE)
      rdm_vars <- grep("^rdm\\d+$", colnames(res$data), value = TRUE)
      
      # step-wise forward 
      m1_sl <- modified_stepwise_glm_parallel(df = df,
                                     y = "y",
                                     x = c(fix_vars, rdm_vars),
                                     c = "cl", 
                                     maxstep = 30,
                                     eval_ls=c("Deviance", "AIC", "BIC", "NIC", "looDeviance"),
                                     eval_by="looDeviance",
                                     family = "gaussian",
                                     forward = T,
                                     free_cores = 1)
      res_df <- format_forward(m1_sl)
      # library(ggplot2)
      # ggplot(res_df, aes(x=model_size))+
      #   geom_line(aes(y=nic), color="red")+
      #   geom_line(aes(y=aic), color="blue")+
      #   # geom_line(aes(y=bic), color="orange")+
      #   geom_line(aes(y=dev), color="gray") +
      #   geom_line(aes(y=loodev), color="black", linetype="dotted") +
      #   geom_point(aes(y=loodev), color="black") +
      #   geom_text(aes(label=x_picked, y=loodev, vjust=-1), parse = T) +
      #   labs(x="model size", y="criteria")
      
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

sjob = slurm_map(
  split(simulation_conditions, simulation_conditions$id),
  run_wrapper,
  nodes=nrow(simulation_conditions),
  cpus_per_node = 1,
  submit = TRUE,
  preschedule_cores = F,
  slurm_options =
    c(account = "netlab", partition = "standard", time = "5-00:00:00"), # standard
  global_objects = lsf.str()
)
save(sjob, file = paste0("nic_simulation_run_lm_model_select_5days.Rdata"))


