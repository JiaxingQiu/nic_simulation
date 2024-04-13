# # Parameters for output_2024-04-11 22_23_13.206088.RDS
# n_cluster <- c(50, 100) # number of clusters
# n_obs_per_cluster <- c(5, 10, 30, 50, 80) # number of observations per cluster
# n_ttl_betas <- seq(3, 15) # number of total effects
# fix_rdm_ratio <- c(0.2, 0.5, 0.8) # proportion of fix effects
# sigma_fix <- c(5, 10) # fix effect beta variance
# sigma_rdm_fix_ratio <- c(0.2, 0.5, 0.8) # random effect beta variance proportional to fixed effects
# param_grid <- expand.grid(n_cluster = n_cluster, 
#                           n_obs_per_cluster = n_obs_per_cluster, 
#                           n_ttl_betas = n_ttl_betas,
#                           fix_rdm_ratio = fix_rdm_ratio,
#                           sigma_fix = sigma_fix,
#                           sigma_rdm_fix_ratio = sigma_rdm_fix_ratio)
# simulation_conditions <- as.data.frame(param_grid)
# simulation_conditions$id <- seq(1:nrow(param_grid))
# simulation_conditions$iter <- 100


# Parameters
n_cluster <- c(50) #c(50, 100) # number of clusters
n_obs_per_cluster <- c(5, 10, 30, 50, 80) # number of observations per cluster
n_ttl_betas <- seq(5, 15) # number of total effects
fix_rdm_ratio <- c(0.5) # c(0.2, 0.5, 0.8) # proportion of fix effects
sigma_fix <- c(5) # c(5, 10) # fix effect beta variance
sigma_rdm_fix_ratio <- c(0.5) #c(0.2, 0.5, 0.8) # random effect beta variance proportional to fixed effects
ar1_phi <- c(0, 0.4, 0.8) # runif min, max = min + 0.2, 0-0.2 means low within-cluster correlation, 0.4-0.6 median correlation, 0.8-1 high correlation
na_rate <- c(0, 0.3, 0.7, 1) # cluster wise missingness, rnorm centers, 0.3 means low level missingness, 0.7 means high level missingness, 0 means no missing
# 1 means unbalanced missingness, ranging from 0-1

# Define the simulation conditions
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
simulation_conditions$iter <- 100


