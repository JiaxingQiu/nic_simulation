
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


plot_df$cluster_strength <- factor(plot_df$clustering_strength, levels=c("weak", "median", "strong"))
levels(plot_df$cluster_strength) <- c("weak", "moderate", "strong")

plot_df$ic_type <- stringr::str_to_upper(gsub("_diff","",plot_df$ic_type))
plot_df$ic_type <- factor(plot_df$ic_type, levels=c("NICC", "NIC","AIC","BIC"))
levels(plot_df$ic_type) <- c("NICc","NIC","AIC","BIC")

plot_df$case <- factor(plot_df$case, levels=c("raw", "rare"))
levels(plot_df$case) <- c("Original","Rare Event")

plot_df <- plot_df %>%
  mutate(n_ttl_betas = case_when(
    ic_type %in% c("AIC") ~ n_ttl_betas - 0.15,
    ic_type %in% c("NIC") ~ n_ttl_betas - 0.3,
    TRUE ~ n_ttl_betas
  ))
p <- ggplot(data = plot_df, aes(x = n_ttl_betas, y = ic_diff, color = ic_type)) + 
  geom_point(size=1) +
  geom_line(linewidth=0.2) + 
  geom_errorbar(aes(ymin = ic_diff_l, ymax = ic_diff_u),width=0.2) + 
  geom_hline(aes(yintercept=0)) + 
  scale_x_continuous(limits = c(4.55, 10.5), breaks = seq(5, 10, 1)) +
  # coord_trans(y = "sqrt") +
  facet_wrap(~ case + cluster_strength, ncol=3, nrow=2, scales="free_x") + 
  labs(subtitle="Binomial",
       x = "Generating model size", 
       y = "Error",
       color = "Criterion") + 
  theme_bw()+
  scale_color_manual(values = c(
    "NICc" = "red", 
    "NIC" = "lightblue3", 
    "AIC" = "blue", 
    "BIC" = "darkorange", 
    "looDeviance" = "black"
  )) +
  theme(text = element_text(face = "bold"),
        plot.subtitle = element_text(size=12, face="bold"),
        axis.text = element_text(size=8),
        legend.title = element_text(size=10), 
        legend.text = element_text(size=8)) 

