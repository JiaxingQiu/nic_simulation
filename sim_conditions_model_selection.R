# 5 effects
if(model_size == 5 ){ 
  n_ttl_betas <- c(5) # fixed number of total effects
  n_cluster <- c(50) ## number of clusters
  n_obs_per_cluster <- cluster_size # number of observations per cluster
  fix_rdm_ratio <- c(0.2) # c(0.2, 0.5, 0.8) # proportion of fix effects
  sigma_fix <- c(5) # fix effect beta variance
  sigma_rdm_fix_ratio <- sigma_rdm_fix_ratio  # random effect beta variance proportional to fixed effects 0.5, 1, 5,
  ar1_phi <- ar1_phi # runif min, max = min + 0.2, 0-0.2 means low within-cluster correlation, 0.4-0.6 median correlation, 0.8-1 high correlation 0, 0.4,
  na_rate <- c(0) # cluster wise missingness, rnorm centers, 0.3 means low level missingness, 0.7 means high level missingness, 0 means no missing
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
  print(simulation_conditions)
}