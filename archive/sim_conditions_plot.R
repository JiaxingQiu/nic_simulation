plot_ls <- list()
for(rn in c("lm","lr")){ 
  agg_df <- agg_df_ls[[rn]]
  # # use mean and se
  # plot_df <- NULL
  # for(en in c("_mean","_se")){
  #   tmpdf <- pivot_longer(agg_df[,c("id", paste0(c("aic_diff", "bic_diff", "nic_diff"),en))], 
  #                         cols = ends_with(en), 
  #                         names_to = "ic_type", 
  #                         values_to = paste0("ic_diff",en))
  #   tmpdf$ic_type <- gsub(en,"", tmpdf$ic_type)
  #   if(is.null(plot_df)){plot_df <- tmpdf}
  #   else{
  #     plot_df <- merge(plot_df, tmpdf)
  #   }
  # }
  # plot_df$ic_diff <-  plot_df$ic_diff_mean
  # plot_df$ic_diff_l <- plot_df$ic_diff - plot_df$ic_diff_se
  # plot_df$ic_diff_u <- plot_df$ic_diff + plot_df$ic_diff_se
  # use IQR
  plot_df <- NULL
  for(en in c("_median","_q25", "_q75")){
    tmpdf <- pivot_longer(agg_df[,c("id", paste0(c("aic_diff", "bic_diff", "nic_diff"),en))], 
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
  plot_df <- plot_df %>% filter(sigma_rdm_fix_ratio == r, na_rate == na) %>% as.data.frame()
  plot_df$n_obs_per_cluster <- paste0(plot_df$n_obs_per_cluster, " obs/cluster")
  plot_df$n_obs_per_cluster <- factor(plot_df$n_obs_per_cluster, levels=c("5 obs/cluster",
                                                                          "10 obs/cluster",
                                                                          "50 obs/cluster",
                                                                          "100 obs/cluster",
                                                                          "150 obs/cluster") )
  
  plot_df$ic_type <- stringr::str_to_upper(gsub("_diff","",plot_df$ic_type))
  plot_df$ic_type <- factor(plot_df$ic_type, levels=c("NIC","AIC","BIC"))
  levels(plot_df$ic_type) <- c("NICc","AIC","BIC")
  plot_df$ar1_phi <- paste0("AR1(",plot_df$ar1_phi,")")
  plot_df$ar1_phi <- factor(plot_df$ar1_phi, levels = c("AR1(0)", "AR1(0.4)", "AR1(0.8)"))
  # plot_df$na_rate_factor <- factor(plot_df$na_rate, levels = c(0, 1))
  # levels(plot_df$na_rate_factor) <- c("Balanced", "Unbalanced")
  plot_ls[[rn]] <- ggplot(data = plot_df, aes(x = n_ttl_betas, y = ic_diff, color = ic_type)) + 
    geom_point(size=2) +
    geom_line() + 
    # geom_line(mapping = aes(linetype=na_rate_factor))+
    geom_errorbar(aes(ymin = ic_diff_l, ymax = ic_diff_u),width=0.5) + 
    scale_linetype_manual(values = c("solid", "dashed", "dotdash", "dotted")) +
    geom_hline(aes(yintercept=0)) + 
    scale_x_continuous(limits = c(min(plot_df$n_ttl_betas), max(plot_df$n_ttl_betas)), breaks = seq(min(plot_df$n_ttl_betas), max(plot_df$n_ttl_betas), 1)) +
    coord_trans(y = "sqrt") +
    facet_wrap(~ar1_phi + n_obs_per_cluster, ncol=5, nrow=3, scales="free_x") + 
    labs(subtitle = ifelse(rn=="lr", "Binomial", "Gaussian"),
         x = "Generating Model Size", 
         y = "Error = |IC - looDeviance|",
         color = "Criterion",
         linetype="Cluster") + 
    theme_bw()+
    scale_color_manual(values = c("NICc" = "red", "AIC" = "blue", "BIC" = "darkorange", "looDeviance" = "black", "Deviance" = "gray")) +
    theme(text = element_text(face = "bold"),
          plot.subtitle = element_text(size=12, face="bold"),
          axis.title = element_text(size=12),
          axis.text = element_text(size=10),
          legend.title = element_text(size=12), 
          legend.text = element_text(size=10))
}


p <- ggarrange(plotlist = plot_ls, nrow=2,ncol=1, common.legend = T,legend = "right")
p <- annotate_figure(p, top = text_grob("Error in approximating out-of-cluster performance", size = 14, face = "bold"))

