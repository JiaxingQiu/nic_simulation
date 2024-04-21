# setwd(dirname(rstudioapi::getSourceEditorContext()$path))

rm(list = ls())

library(dplyr)
library(rslurm)
library(Matrix)
library(lme4)
library(MASS)
library(pROC)

list.of.packages <- c("dplyr",
                      "rslurm",
                      "MASS",
                      "lme4",
                      "Matrix",
                      "pROC")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, lib = "/sfs/qumulo/qhome/jq2uw/R/goolf/4.3")



source("./sim_functions.R")
for(u in c("nic", "ass", "stp", "do")){
  path = paste0("./utils/",u,"_utils")
  flst = list.files( path)
  sapply(c(paste(path,flst,sep="/")), source, .GlobalEnv)
}
source("./sim_conditions.R")



#This function runs each condition (i.e. each row in the simulation condition data.frame)
# for test: 
# sim_condition = simulation_conditions[which(simulation_conditions$id==56),]
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
      # ground truth mixed effect model
      m0 <- fit_eval_glmer(y = res$y,
                           c = res$c,
                           data = res$data)
      # lr model evaluation matrices
      m1 <- fit_eval_glm(y = res$y,
                         c = res$c,
                         data = res$data)
      stopifnot(!is.na(m1$aic))
      
      # measure bias
      bias <- calculate_bias(res, m0, m1)
      
      # measure se estimate 
      se_ratio <- calculate_se_accuracy(res, m0, m1)
      
      results_list[[i]] = list(id = sim_condition$id, 
                               iter = i, 
                               N = nrow(res$data),
                               bias0 = bias$bias0,
                               bias1 = bias$bias1,
                               se_ratio0 = se_ratio$se_ratio0,
                               se_ratio1 = se_ratio$se_ratio1,
                               aic0 = m0$aic,
                               aic1 = m1$aic,
                               bic0 = m0$bic,
                               bic1 = m1$bic,
                               nic1 = m1$nic,
                               dev0 = m0$deviance,
                               dev1 = m1$deviance,
                               loopred0 = m0$loopred,
                               loopred1 = m1$loopred,
                               loodev0 = m0$looDeviance,
                               loodev1 = m1$looDeviance)
      
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
save(sjob, file = "nic_simulation_run_lr_5days.Rdata")

