plot_ls <- list()
for(rn in c("lr", "lm")){ 
  plot_df <- pivot_longer(agg_df_ls[[rn]], 
                          cols = starts_with("se_ratio"), 
                          names_to = "se_type", 
                          values_to = "se_value")
  plot_df$n_obs_per_cluster <- paste0(plot_df$n_obs_per_cluster, " obs/cluster")
  plot_df$n_obs_per_cluster <- factor(plot_df$n_obs_per_cluster, levels=c("5 obs/cluster",
                                                                          "10 obs/cluster",
                                                                          "30 obs/cluster",
                                                                          "50 obs/cluster",
                                                                          "80 obs/cluster") )
  plot_df$model <- ifelse(plot_df$se_type=="se_ratio0","GLME", "GLM")
  plot_df$ar1_phi <- paste0("AR1(",plot_df$ar1_phi,")")
  plot_df$ar1_phi <- factor(plot_df$ar1_phi, levels = c("AR1(0)", "AR1(0.4)", "AR1(0.8)"))
  plot_df$na_rate_factor <- factor(plot_df$na_rate, levels = c(0, 0.3, 0.7, 1))
  levels(plot_df$na_rate_factor) <- c("None", "Low", "High", "Random")
  plot_ls[[rn]] <- ggplot(data = plot_df, aes(x = n_ttl_betas, y = se_value, color = model)) + 
    geom_point(size=1) +
    geom_line(mapping = aes(linetype=na_rate_factor))+
    scale_linetype_manual(values = c("solid", "dashed", "dotdash", "dotted")) +
    geom_hline(aes(yintercept=1)) + 
    scale_x_continuous(limits = c(5, 15), breaks = c(5,10,15)) +
    # coord_trans(y = "srqt") +
    scale_y_log10() + 
    facet_wrap(~ar1_phi + n_obs_per_cluster, ncol=5, nrow=3, scales="free_x") + 
    labs(title = ifelse(rn=="lr", "Binomial", "Gaussian"),
         x = "Number of Predictors", 
         y = "SE ratio",
         color = "Model",
         linetype="Missing") + 
    theme_bw()
}

p <- ggarrange(plotlist = plot_ls, nrow=1,ncol=2, common.legend = T,legend = "right")
p %>% ggsave(filename=paste0("./res/se_lm_vs_lme.png"), width = 15, height = 7, bg="white")

