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
source("./sim_conditions_rare.R")



#This function runs each condition (i.e. each row in the simulation condition data.frame)
# for test: 
sim_condition = simulation_conditions[which(simulation_conditions$id=="rare_18"),]


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
                           sim_condition$na_rate,
                           family = "binomial")
      
      
      # rare case scenario
      if("case" %in% colnames(sim_condition)){
        if(sim_condition$case=="rare"){
          # control roughly the prevalence
          while(T){
            res <- generate_data(sim_condition$n_cluster,
                                 sim_condition$n_obs_per_cluster,
                                 sim_condition$n_ttl_betas, 
                                 sim_condition$fix_rdm_ratio,
                                 sim_condition$sigma_fix,
                                 sim_condition$sigma_rdm_fix_ratio,
                                 sim_condition$ar1_phi,
                                 sim_condition$na_rate,
                                 family = "binomial")
            res$p <- res$p*0.015
            res$y <- rbinom(length(res$p), 1, res$p)
            prev <- length(unique(res$c[which(res$y==1)])) / length(unique(res$c))
            print(prev)
            if(prev > 0.1 & prev < 0.2) break
          }
        }
      }
      
      
      # lr model evaluation matrices
      m1 <- fit_eval_glm(y = res$y,
                         c = res$c,
                         data = res$data)
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
    c(account = "netlab", partition = "standard", time = "2-00:00:00"), # standard
  global_objects = lsf.str()
)
save(sjob, file = "nic_simulation_run_lr_rare.Rdata")

