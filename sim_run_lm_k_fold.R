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
source("./sim_conditions_k_fold.R")


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
                           family = "gaussian")
      # maintain the loo 
      m1 <- fit_eval_glm(y = res$y,
                         c = res$c,
                         data = res$data,
                         family = "gaussian")
      stopifnot(!is.na(m1$aic))
      results_list[[i]] = list(id = sim_condition$id, 
                               iter = i, 
                               N = nrow(res$data),
                               aic1 = m1$aic,
                               bic1 = m1$bic,
                               nic1 = m1$nic,
                               nicc1 = m1$nicc,
                               dev1 = m1$deviance,
                               loopred1 = m1$loopred,
                               loodev1 = m1$looDeviance)
      # assign unique(res$c) into k folds
      for(k in c(5, 10, 50, 80)) {
        folds <- cut(seq_along(unique(res$c)), breaks=k, labels=FALSE)
        res$fold <- folds[match(res$c, unique(res$c))]
        m2 <- fit_eval_glm(y = res$y,
                           c = res$fold,
                           data = res$data,
                           family = "gaussian")
        results_list[[i]][[paste0("k_", k)]] <- m2$looDeviance
      }
      
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
  run_wrapper_lm,
  nodes=nrow(simulation_conditions),
  cpus_per_node = 1,
  submit = TRUE,
  preschedule_cores = F,
  slurm_options =
    c(account = "netlab", partition = "standard", time = "2-00:00:00"), # standard
  global_objects = lsf.str()
)
save(sjob, file = "nic_simulation_run_lm_k_fold.Rdata")
