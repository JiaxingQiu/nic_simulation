# ---- test ----
# sim_condition = simulation_conditions[which(simulation_conditions$id==14),]
# n_cluster = sim_condition$n_cluster
# n_obs_per_cluster= sim_condition$n_obs_per_cluster
# n_ttl_betas= sim_condition$n_ttl_betas
# fix_rdm_ratio= sim_condition$fix_rdm_ratio

sim_condition = simulation_conditions[which(simulation_conditions$id==14),]
res <- generate_data(sim_condition$n_cluster,
                     sim_condition$n_obs_per_cluster,
                     sim_condition$n_ttl_betas,
                     sim_condition$fix_rdm_ratio)
eff <- as.data.frame(res$data * res$betas)
par(mfrow=c(4,4))
for(v in colnames(eff)){
  plot(res$data[,v], eff[,v])
  plot(res$data[,v], res$p)
}
plot(res$p, res$y)
betas <- as.data.frame(res$betas)
print(unique(betas$beta_fix_itc))
print(unique(betas$beta_fix1))
print(unique(betas$beta_fix_rdm1))
print(unique(betas$beta_fix_rdm2))
# mean(unique(betas$beta_rdm1)) # the variation in the slope of x1 across the individuals.
# mean(unique(betas$beta_rdm2)) # the variation in the slope of x1 across the individuals.
# mean(unique(betas$beta_rdm_itc)) # the variation in the slope of x1 across the individuals.
var(eff$rdm1)
var(eff$rdm2)
var(eff$rdm_itc)




library(Matrix)
library(lme4)
data <- res$data
y <- res$y
c <- res$c
colnames(data)
df_mdl <- as.data.frame(res$data)
df_mdl$y <- res$y
df_mdl$c <- as.factor(res$c)
# model <- lmer(y ~ fix1 + rdm1 + rdm2 + (rdm1 + rdm2 + 1|c), data = df_mdl)
model <- glmer(y ~ fix1 + rdm1 + rdm2 + (0+rdm1|c) + (0+rdm2|c) + (1|c), data = df_mdl, family = binomial)
summary(model)

mdl <- glm(y ~ fix1 + rdm1 + rdm2, data=df_mdl, family = "binomial")
summary(mdl)



n_cluster = sim_condition$n_cluster
n_obs_per_cluster= sim_condition$n_obs_per_cluster
n_ttl_betas= sim_condition$n_ttl_betas
fix_rdm_ratio= sim_condition$fix_rdm_ratio
sigma_fix = sim_condition$sigma_fix
sigma_rdm_fix_ratio = sim_condition$sigma_rdm_fix_ratio
ar1_phi_min = sim_condition$ar1_phi_min
na_rate = sim_condition$na_rate


