# Parameters
n_cluster <- c(50, 100) # number of clusters
n_obs_per_cluster <- c(5, 10, 30, 50, 80) # number of observations per cluster
n_ttl_betas <- seq(3, 15) # number of total effects
fix_rdm_ratio <- c(0.2, 0.5, 0.8) # proportion of fix effects
sigma_fix <- c(5, 10)
sigma_rdm_fix_ratio <- c(0.2, 0.5, 0.8)

# Define the simulation conditions
param_grid <- expand.grid(n_cluster = n_cluster, 
                          n_obs_per_cluster = n_obs_per_cluster, 
                          n_ttl_betas = n_ttl_betas,
                          fix_rdm_ratio = fix_rdm_ratio,
                          sigma_fix = sigma_fix,
                          sigma_rdm_fix_ratio = sigma_rdm_fix_ratio )
simulation_conditions <- as.data.frame(param_grid)
simulation_conditions$id <- seq(1:nrow(param_grid))
simulation_conditions$iter <- 100