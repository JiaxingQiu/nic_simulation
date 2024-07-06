plot_ls <- list()
for(rn in c("lm","lr")){
  agg_df_org <- agg_df_ls[[rn]] %>% filter(n_cluster == 50)
  
  
  # group conditions to weak, median, high clustering
  # 1
  agg_df <- agg_df_org[which(agg_df_org$na_rate==1),]
  agg_df$cluster_strength <- NA
  agg_df$cluster_strength[which(agg_df$n_obs_per_cluster==10 &
                                  agg_df$sigma_rdm_fix_ratio==0.5 &
                                  agg_df$ar1_phi==0 )] <- "weak"
  
  agg_df$cluster_strength[which(agg_df$n_obs_per_cluster==50 &
                                  agg_df$sigma_rdm_fix_ratio==1 &
                                  agg_df$ar1_phi==0.4 )] <- "moderate"
  
  agg_df$cluster_strength[which(agg_df$n_obs_per_cluster==100 &
                                  agg_df$sigma_rdm_fix_ratio==10 &
                                  agg_df$ar1_phi==0.8 )] <- "strong"
  agg_df1 <- agg_df
  # 0
  agg_df <- agg_df_org[which(agg_df_org$na_rate==0),]
  agg_df$cluster_strength <- NA
  agg_df$cluster_strength[which(agg_df$n_obs_per_cluster==5 &
                                  agg_df$sigma_rdm_fix_ratio==0.5 &
                                  agg_df$ar1_phi==0)] <- "weak"
  
  agg_df$cluster_strength[which(agg_df$n_obs_per_cluster==25 &
                                  agg_df$sigma_rdm_fix_ratio==1 &
                                  agg_df$ar1_phi==0.4 )] <- "moderate"
  
  agg_df$cluster_strength[which(agg_df$n_obs_per_cluster==50 &
                                  agg_df$sigma_rdm_fix_ratio==10 &
                                  agg_df$ar1_phi==0.8 )] <- "strong"
  agg_df0 <- agg_df
  
  agg_df <- bind_rows(agg_df0, agg_df1)
  
  agg_df <- agg_df %>% filter(!is.na(cluster_strength)) %>% as.data.frame()
  
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
  cs <- distinct(agg_df[,c("id","cluster_strength")])
  plot_df <- merge(plot_df, cs, by="id", all.x=T) %>% as.data.frame()
  
  
  plot_df$cluster_strength <- factor(plot_df$cluster_strength, levels=c("weak","moderate", "strong"))
  plot_df$na_rate <- factor(plot_df$na_rate, levels=c(0,1))
  levels(plot_df$na_rate) <- c("unbalanced", "balanced")
  plot_df$ic_type <- stringr::str_to_upper(gsub("_diff","",plot_df$ic_type))
  plot_df$ic_type <- factor(plot_df$ic_type, levels=c("NICC", "NIC","AIC","BIC"))
  levels(plot_df$ic_type) <- c("NICc","NIC","AIC","BIC")
  
  plot_ls[[rn]] <- ggplot(data = plot_df, aes(x = n_ttl_betas, y = ic_diff, color = ic_type)) + 
    geom_point(size=0.9) +
    geom_line(linewidth=0.3) + 
    geom_errorbar(aes(ymin = ic_diff_l, ymax = ic_diff_u),width=0.2) + 
    geom_hline(aes(yintercept=0)) + 
    scale_x_continuous(limits = c(min(plot_df$n_ttl_betas), max(plot_df$n_ttl_betas)), breaks = seq(min(plot_df$n_ttl_betas), max(plot_df$n_ttl_betas), 1)) +
    facet_wrap(cluster_strength ~ na_rate , ncol=2, nrow=3, scales="free_x") + 
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
          legend.text = element_text(size=8),
          strip.text.y = element_text(size=10, face="bold", angle=0))
  
  
  
}

p <- ggarrange(plotlist = plot_ls, nrow=1,ncol=2, common.legend = T, legend = "none")
