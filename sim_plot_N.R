plot_ls <- list()
for(rn in c("lm","lr")){
  agg_df <- agg_df_ls[[rn]]
  agg_df <- agg_df %>% filter(na_rate == 0,
                              !n_obs_per_cluster == 75, 
                              sigma_rdm_fix_ratio == 1,
                              ar1_phi == 0.4) %>% 
    as.data.frame()
  plot_df <- NULL
  for(en in c("_median","_q25", "_q75")){
    tmpdf <- pivot_longer(agg_df[,c("id", paste0(c("aic_diff", "bic_diff", "nic_diff", "nicc_diff"),en))], 
                          cols = ends_with(en), 
                          names_to = "ic_type", 
                          values_to = paste0("ic_diff",en))
    tmpdf$ic_type <- gsub(en,"", tmpdf$ic_type)
    if(is.null(plot_df)){plot_df <- tmpdf}
    else{
      plot_df <- merge(plot_df, tmpdf)
    }
  }
  plot_df$ic_diff <-  plot_df$ic_diff_median
  plot_df$ic_diff_l <- plot_df$ic_diff_q25
  plot_df$ic_diff_u <- plot_df$ic_diff_q75
  plot_df <- merge(plot_df,simulation_conditions, by="id", all.x=T) %>% as.data.frame()
  plot_df$n_obs_per_cluster <- paste0(plot_df$n_obs_per_cluster, " obs/cluster")
  plot_df$n_obs_per_cluster <- factor(plot_df$n_obs_per_cluster, levels=c("5 obs/cluster",
                                                                          "10 obs/cluster",
                                                                          "50 obs/cluster",
                                                                          "100 obs/cluster",
                                                                          "150 obs/cluster") )
  plot_df$ic_type <- stringr::str_to_upper(gsub("_diff","",plot_df$ic_type))
  plot_df$ic_type <- factor(plot_df$ic_type, levels=c("NICC", "NIC","AIC","BIC"))
  levels(plot_df$ic_type) <- c("NICc","NIC","AIC","BIC")
  plot_ls[[rn]] <- ggplot(data = plot_df, aes(x = n_ttl_betas, y = ic_diff, color = ic_type)) + 
    geom_point(size=1.5) +
    geom_line() + 
    geom_errorbar(aes(ymin = ic_diff_l, ymax = ic_diff_u),width=0.3) + 
    geom_hline(aes(yintercept=0)) + 
    scale_x_continuous(limits = c(min(plot_df$n_ttl_betas), max(plot_df$n_ttl_betas)), breaks = seq(min(plot_df$n_ttl_betas), max(plot_df$n_ttl_betas), 1)) +
    # coord_trans(y = "sqrt") +
    facet_wrap(~ n_obs_per_cluster, ncol=5, nrow=1, scales="free_x") + 
    labs(subtitle = ifelse(rn=="lr", "Binomial", "Gaussian"),
         x = NULL, 
         y = NULL,
         color = "Criterion") + 
    theme_bw()+
    scale_color_manual(values = c("NICc" = "red", "NIC" = "lightblue3", "AIC" = "blue", "BIC" = "darkorange", "looDeviance" = "black", "Deviance" = "gray")) +
    theme(text = element_text(face = "bold"),
          plot.subtitle = element_text(size=10, face="bold"),
          axis.text = element_text(size=8),
          legend.title = element_text(size=10), 
          legend.text = element_text(size=8))
}

p <- ggarrange(plotlist = plot_ls, nrow=2,ncol=1, common.legend = T,legend = "right")



