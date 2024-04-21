
# aggregation plot
p_agg <- list()
for(sn in c("lm","lr") ){
  # Plot using stat_summary and stat_summary_ribbon
  res_df_iter_long <- res_df_iter[[sn]] %>%
    dplyr::select(model_size, nic, aic, bic, loodev, iter) %>%
    pivot_longer(
      cols = c(nic, aic, bic, loodev),  # Specify columns to lengthen
      names_to = "score",  # New column for the names
      values_to = "value"  # New column for the values
    )
  # Function to calculate mean and standard error
  mean_se <- function(x) {
    n <- length(x)
    se <- sd(x) / sqrt(n)
    return(c(y = mean(x), ymin = mean(x) - se, ymax = mean(x) + se))
  }
  res_df_iter_long$score <- factor(res_df_iter_long$score, levels=c("loodev", "nic","bic","aic"))
  levels(res_df_iter_long$score) <- c("looDeviance", "rNIC","BIC","AIC")
  p_agg[[sn]] <- ggplot(res_df_iter_long, aes(x = model_size, y = value, color=score, fill=score)) +
    stat_summary(fun = mean, geom = "line", linewidth = 1) +
    stat_summary(fun.data = mean_se, geom = "ribbon", alpha = 0.2, color=NA) +
    geom_text(aes(x=0, y=8000, label="Mean +/- SE" ),hjust = 0, vjust = 0, size = 4, color="black") + 
    # geom_vline(aes(xintercept = sim_condition$n_ttl_betas),color="black")+
    labs(title = ifelse(sn=="lm", "Gaussian", "Binomial"),
         x = "Model size",
         y = "Value (e+03)",
         color="Score",
         fill="Score") +
    scale_color_manual(values = c("rNIC" = "red", "AIC" = "blue", "BIC" = "orange", "looDeviance" = "darkgray")) +
    scale_fill_manual(values = c("rNIC" = "red", "AIC" = "blue", "BIC" = "orange", "looDeviance" = "darkgray")) +
    scale_y_continuous(breaks = function(x) {
      seq(from = min(x), to = max(x), length.out = 5)
    },labels = function(y) sprintf("%.2f", y / 1000) ) +
    theme_minimal()+
    theme(text = element_text(face = "bold"))
}
p_aggregate <- ggarrange(plotlist = p_agg, nrow=2,ncol=1, common.legend = T, legend = "none")


pie <- list()
for(sn in c("lm", "lr")){
  pl <- list()
  for(i in sort(unique(res_df_iter[[sn]]$iter))){
    # res_df <- run_wrapper_lr(sim_condition)
    res_df <- res_df_iter[[sn]][which(res_df_iter[[sn]]$iter==i),]
    res_df_long <- res_df %>%
      pivot_longer(
        cols = c(nic, aic, bic, dev, loodev),  # Specify columns to lengthen
        names_to = "score",  # New column for the names
        values_to = "value"  # New column for the values
      )
    best_df <- best_df_iter[[sn]][which(best_df_iter[[sn]]$i==i),]
    # ymin <- res_df_long$value[which(res_df_long$value == min(res_df_long$value[res_df_long$score=="dev"]) )][1]
    # ymax <- res_df_long$value[res_df_long$score=="loodev"&res_df_long$model_size==1]
    res_df_long$score <- factor(res_df_long$score, levels=c("loodev", "nic","bic","aic","dev"))
    levels(res_df_long$score) <- c("looDeviance", "rNIC","BIC","AIC", "Deviance")
    best_df$score <- factor(best_df$score, levels=c("loodev", "nic","bic","aic","dev"))
    levels(best_df$score) <- c("looDeviance", "rNIC","BIC","AIC", "Deviance")
    
    pl[[i]] <- ggplot(res_df_long, aes(x = model_size, y = value, group = score, color = score)) +
      geom_line() +
      geom_vline(xintercept = 5, color = "grey") +
      scale_color_manual(values = c("rNIC" = "red", "AIC" = "blue", "BIC" = "orange", "looDeviance" = "black", "Deviance" = "gray")) +
      theme_minimal() +
      # scale_y_continuous(labels = scales::scientific_format()) +
      scale_y_continuous(breaks = function(x) {
        seq(from = min(x), to = max(x), length.out = 5)
      },labels = function(y) sprintf("%.2f", y / 1000) ) +
      geom_errorbar(data = best_df, aes(x = best_size, xmin=best_size_1se_min, xmax=best_size_1se_max, y = best_score, color=score))+
      geom_point(data = best_df, aes(x = best_size, y = best_score, color=score), size = 1.5)+
      labs(title = paste0("iter = ",i), x = "Model Size", y = "Value", color = "Score") +
      theme(text = element_text(face = "bold"))
    
    if(detect_mal(res_df, sim_condition)){
      print(i)
    }
  }
  
  ggarrange(plotlist = pl[1:20],nrow=4, ncol=5,common.legend = T, legend = "right")
  ggarrange(plotlist = pl[21:40],nrow=4, ncol=5,common.legend = T, legend = "right")
  ggarrange(plotlist = pl[41:60],nrow=4, ncol=5,common.legend = T, legend = "right")
  ggarrange(plotlist = pl[61:80],nrow=4, ncol=5,common.legend = T, legend = "right")
  ggarrange(plotlist = pl[81:100],nrow=4, ncol=5,common.legend = T, legend = "right")
  
  # find 5 examples
  if(model_size==5){
    if(sn == "lm") ie <- c(76, 54, 26, 32) #20
    if(sn == "lr") ie <- c(16, 33, 46, 51) #27
  }
  if(model_size==10){
    if(sn == "lm") ie <- c(76, 54, 26, 32) #, 20
    if(sn == "lr") ie <- c(16, 33, 46, 51) #27
  }
  
  pl_ie <- lapply(pl[ie], function(p) return(p+labs(title=NULL, x=NULL, y=NULL)))
  pie[[sn]] <- ggarrange(plotlist = pl_ie,nrow=1, ncol=4,common.legend = T, legend = "right")
  
  pie[[sn]] <- annotate_figure(pie[[sn]], top=text_grob("Examples from 100 iterations", size = 13, face = "bold", hjust = 0, x = 0), 
                         # left = text_grob("Value*e+03", size = 10, face = "bold", rot = 90), 
                         bottom = text_grob("Model Size", size = 10, face = "bold"))
}

p_example <- ggarrange(plotlist = pie, nrow=2, ncol=1)

p_model_select <- ggarrange(p_aggregate, p_example, ncol=2, nrow=1, widths = c(1,4))
p_model_select %>% ggsave(filename=paste0("./res/model_select_example_",model_size,".png"), width = 13, height = 5, bg="white")
