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
path = paste0("./nic_utils")
flst = list.files( path)
sapply(c(paste(path,flst,sep="/")), source, .GlobalEnv)




# Parameters
n_cluster <- c(50, 100) # number of clusters
n_obs_per_cluster <- c(5, 10, 30, 50, 100) # number of observations per cluster
n_ttl_betas <- seq(3,15) # number of total effects
fix_rdm_ratio <- c(0.2, 0.5, 0.8) # proportion of fix effects
# residual_error <- c(0.5, 1)#, 3) # residual error

# Define the simulation conditions
param_grid <- expand.grid(n_cluster = n_cluster, 
                          n_obs_per_cluster = n_obs_per_cluster, 
                          n_ttl_betas = n_ttl_betas,
                          fix_rdm_ratio = fix_rdm_ratio)


simulation_conditions <- as.data.frame(param_grid)
simulation_conditions$id <- seq(1:nrow(param_grid))
simulation_conditions$iter <- 100

#This function runs each condition (i.e. each row in the simulation condition data.frame)
# for test: sim_condition = simulation_conditions[which(simulation_conditions$id==1),]

run_wrapper <- function(sim_condition) {
  results_list = list()
  for(i in 1:sim_condition$iter){
    tryCatch({
      res <- generate_data(sim_condition$n_cluster,
                           sim_condition$n_obs_per_cluster,
                           sim_condition$n_ttl_betas, 
                           sim_condition$fix_rdm_ratio)
      # ground truth mixed effect model
      m0 <- fit_glmer(y = res$y,
                      c = res$c,
                      data = res$data)
      # lr model evaluation matrices
      m1 <- eval_glm(y = res$y,
                    c = res$c,
                    data = res$data)
      stopifnot(!is.na(m1$aic))
      
      # measure bias
      bias <- calculate_bias(res, m0, m1)
      results_list[[i]] = list(id = sim_condition$id, 
                               iter = i, 
                               bias0 = bias$bias0,
                               bias1 = bias$bias1,
                               aic = m1$aic,
                               bic = m1$bic,
                               nic = m1$nic,
                               dev = m1$deviance,
                               looauc = m1$looAUC,
                               loodev = m1$looDeviance)
      
      
    }, error = function(e){
      print(e)
      print(paste0("skip iteration ",i))
    })
  }
  results_list <- Filter(function(x) !is.null(x), results_list)
  toReturn = do.call("rbind", results_list)
  return(toReturn)
}

#This function actually runs the whole simulation study by deploying it to the SLURM cluster
sjob = slurm_map(
  #The use of split here breaks the simulation conditions into a list of rows
  #so it can be used by slurm_map
  split(simulation_conditions, simulation_conditions$id),
  run_wrapper,
  ###From here to the next comment are control parameters, you will likely not change these
  nodes=nrow(simulation_conditions),
  cpus_per_node = 1,
  submit = TRUE,
  preschedule_cores = F,
  #The slurm options is where you specify the time, as well as our lab account
  #The partition should be usually set to standard.
  slurm_options =
    c(account = "netlab", partition = "standard", time = "2:00:00"), # standard
  #This line is vitally important: It imports all functions you have in your environment
  #Because you've sourced your SimFunction file, you should have all necessary functions
  #In the environment.
  global_objects = lsf.str()
)


#This saves the sjob object, if you don't save it, you can't easily pull out the results
save(sjob, file = "nic_simulation_run.Rdata")


#You run these lines after your simulation is complete.
load("nic_simulation_run.Rdata")
output = get_slurm_out(sjob, outtype = "table")



