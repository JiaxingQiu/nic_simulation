# weak clustering
n_cluster <- 50 # number of clusters
n_obs_per_cluster <- 10 # number of observations per cluster
n_ttl_betas <- seq(5,10) # number of total effects
fix_rdm_ratio <- c(0.2) # proportion of fix effects
sigma_fix <- c(5) # fix effect beta variance 
sigma_rdm_fix_ratio <- 0.5 # random effect beta variance proportional to fixed effects
ar1_phi <- 0 # runif min, max = min + 0.2, 0-0.2 means low within-cluster correlation, 0.4-0.6 median correlation, 0.8-1 high correlation
na_rate <- 0
param_grid <- expand.grid(n_cluster = n_cluster,
                          n_obs_per_cluster = n_obs_per_cluster,
                          n_ttl_betas = n_ttl_betas,
                          fix_rdm_ratio = fix_rdm_ratio,
                          sigma_fix = sigma_fix,
                          sigma_rdm_fix_ratio = sigma_rdm_fix_ratio,
                          ar1_phi = ar1_phi,
                          na_rate = na_rate)
simulation_conditions1 <- as.data.frame(param_grid)
simulation_conditions1$clustering_strength <- "weak"

# median clustering
n_cluster <- 50 # number of clusters
n_obs_per_cluster <- 50 # number of observations per cluster
n_ttl_betas <- seq(5,10) # number of total effects
fix_rdm_ratio <- c(0.2) # proportion of fix effects
sigma_fix <- c(5) # fix effect beta variance 
sigma_rdm_fix_ratio <- 1 # random effect beta variance proportional to fixed effects
ar1_phi <- 0.4 # runif min, max = min + 0.2, 0-0.2 means low within-cluster correlation, 0.4-0.6 median correlation, 0.8-1 high correlation
na_rate <- 0
param_grid <- expand.grid(n_cluster = n_cluster,
                          n_obs_per_cluster = n_obs_per_cluster,
                          n_ttl_betas = n_ttl_betas,
                          fix_rdm_ratio = fix_rdm_ratio,
                          sigma_fix = sigma_fix,
                          sigma_rdm_fix_ratio = sigma_rdm_fix_ratio,
                          ar1_phi = ar1_phi,
                          na_rate = na_rate)
simulation_conditions2 <- as.data.frame(param_grid)
simulation_conditions2$clustering_strength <- "median"


# strong clustering
n_cluster <- 50 # number of clusters
n_obs_per_cluster <- 100 # number of observations per cluster
n_ttl_betas <- seq(5,10) # number of total effects
fix_rdm_ratio <- c(0.2) # proportion of fix effects
sigma_fix <- c(5) # fix effect beta variance 
sigma_rdm_fix_ratio <- 10 # random effect beta variance proportional to fixed effects
ar1_phi <- 0.8 # runif min, max = min + 0.2, 0-0.2 means low within-cluster correlation, 0.4-0.6 median correlation, 0.8-1 high correlation
na_rate <- 0
param_grid <- expand.grid(n_cluster = n_cluster,
                          n_obs_per_cluster = n_obs_per_cluster,
                          n_ttl_betas = n_ttl_betas,
                          fix_rdm_ratio = fix_rdm_ratio,
                          sigma_fix = sigma_fix,
                          sigma_rdm_fix_ratio = sigma_rdm_fix_ratio,
                          ar1_phi = ar1_phi,
                          na_rate = na_rate)
simulation_conditions3 <- as.data.frame(param_grid)
simulation_conditions3$clustering_strength <- "strong"


simulation_conditions <- rbind(simulation_conditions1, simulation_conditions2, simulation_conditions3)
simulation_conditions$case <- "rare"
simulation_conditions0 <- simulation_conditions
simulation_conditions0$case <- "raw"
simulation_conditions <- rbind(simulation_conditions0, simulation_conditions)
rm(simulation_conditions0, simulation_conditions1, simulation_conditions2, simulation_conditions3)
simulation_conditions$id <- seq(1:nrow(simulation_conditions))
simulation_conditions$iter <- 100


