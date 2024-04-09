setwd(dirname(rstudioapi::getSourceEditorContext()$path))
rm(list = ls())

library(dplyr)
library(Matrix)
library(lme4)
library(MASS)
library(foreach)
library(doParallel)


source("./sim_functions.R")
path = paste0("./nic_utils")
flst = list.files( path)
sapply(c(paste(path,flst,sep="/")), source, .GlobalEnv)


# Parameters
n_cluster <- c(50, 100) # number of clusters
n_obs_per_cluster <- c(5, 10, 50) # number of observations per cluster
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
# for test:sim_condition = simulation_conditions[which(simulation_conditions$id==1),]

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

# Detect the number of cores
numCores <- detectCores() - 2  # Leave two cores free
registerDoParallel(cores=numCores)
# Pre-calculate the row indices for each batch of 10
row_indices <- split(c(1:nrow(simulation_conditions)), ceiling(seq_along(1:nrow(simulation_conditions))/10))
# Iterate through each batch of indices
for (bn in names(row_indices) ) {
  if(!file.exists(paste0("./res/result_batch",bn,".RDS"))){
    batch_indices <- row_indices[[bn]]
    # Use foreach to run simulations in parallel for the current batch of rows
    results <- foreach(i = batch_indices, .packages = c("pROC", "dplyr", "lme4", "MASS", "Matrix")) %dopar% {
      run_wrapper(simulation_conditions[i,])
    }
    saveRDS(results, paste0("./res/result_batch",bn,".RDS"))
  }
}



