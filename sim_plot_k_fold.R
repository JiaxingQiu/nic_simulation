plot_ls <- list()
for(rn in c("lm","lr")){
  agg_df <- agg_df_ls[[rn]]
  
  plot_df <- NULL
  for(en in c("_median","_q25", "_q75")){
    tmpdf <- pivot_longer(agg_df[,c("id", paste0(c("aic_diff", "bic_diff", "nic_diff", "nicc_diff",
                                                   # "k_5_diff", 
                                                   "k_10_diff", "k_50_diff", "k_80_diff"),en))], 
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
  
  
  plot_df$cluster_strength <- factor(plot_df$cluster_strength, levels=c("weak", "moderate", "strong"))
  levels(plot_df$cluster_strength) <- paste0(levels(plot_df$cluster_strength), " clustering")
  plot_df$ic_type <- stringr::str_to_upper(gsub("_diff","",plot_df$ic_type))
  plot_df$ic_type <- factor(plot_df$ic_type, levels=c("NICC", "NIC","AIC","BIC",
                                                      # "K_5",
                                                      "K_10","K_50","K_80"))
  levels(plot_df$ic_type) <- c("NICc","NIC","AIC","BIC",
                               # "5-fold",
                               "10-fold","50-fold","80-fold")
  
  plot_df <- plot_df %>%
    mutate(x_adjusted = case_when(
      # ic_type %in% c("5-fold") ~ n_ttl_betas + 0.6,
      ic_type %in% c("10-fold") ~ n_ttl_betas + 0.45,
      ic_type %in% c("50-fold") ~ n_ttl_betas + 0.3,
      ic_type %in% c("80-fold") ~ n_ttl_betas + 0.15,
      ic_type %in% c("AIC") ~ n_ttl_betas - 0.15,
      ic_type %in% c("NIC") ~ n_ttl_betas - 0.3,
      TRUE ~ n_ttl_betas
    ))
  plot_ls[[rn]] <- ggplot(data = plot_df, aes(x = x_adjusted, y = ic_diff, color = ic_type)) + 
    geom_point(size=1) +
    geom_line(linewidth=0.2) + 
    geom_errorbar(aes(ymin = ic_diff_l, ymax = ic_diff_u),width=0.2) + 
    geom_hline(aes(yintercept=0)) + 
    scale_x_continuous(limits = c(5, 11), breaks = seq(5, 11, 1)) +
    # coord_trans(y = "sqrt") +
    facet_wrap(~ cluster_strength, ncol=3, nrow=1, scales="free_x") + 
    labs(subtitle = ifelse(rn=="lr", "Binomial", "Gaussian"),
         x = "Generating model size", 
         y = "Error",
         color = "Criterion") + 
    theme_bw()+
    scale_color_manual(values = c(
      "NICc" = "red", 
      "NIC" = "lightblue3", 
      "AIC" = "blue", 
      "BIC" = "darkorange", 
      "looDeviance" = "black", 
      # "5-fold" = "darkgrey", 
      "10-fold" = "green4", 
      "50-fold" = "purple", 
      "80-fold" = "brown"
    )) +
    theme(text = element_text(face = "bold"),
          plot.subtitle = element_text(size=12, face="bold"),
          axis.text = element_text(size=8),
          legend.title = element_text(size=10), 
          legend.text = element_text(size=8)) 
}

p <- ggarrange(plotlist = plot_ls, nrow=2,ncol=1, common.legend = T,legend = "right")


