
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
  levels(res_df_iter_long$score) <- c("looDeviance\n(baseline)", "rNIC","BIC","AIC")
  p_agg[[sn]] <- ggplot(res_df_iter_long, aes(x = model_size, y = value, color=score, fill=score)) +
    stat_summary(fun = mean, geom = "line", linewidth = 1) +
    stat_summary(fun.data = mean_se, geom = "ribbon", alpha = 0.2, color=NA) +
    geom_vline(xintercept = 5, color = "grey") +
    # geom_text(aes(x=0, y=8000, label="Mean +/- SE" ),hjust = 0, vjust = 0, size = 4, color="black") + 
    # geom_vline(aes(xintercept = sim_condition$n_ttl_betas),color="black")+
    labs(#title = ifelse(sn=="lm", "Gaussian", "Binomial"),
         subtitle = "Mean +/- SE",
         x = "Model size",
         y = "Value (e+03)",
         color="Criterion",
         fill="Criterion") +
    scale_color_manual(values = c("rNIC" = "red", "AIC" = "blue", "BIC" = "orange", "looDeviance\n(baseline)" = "black")) +
    scale_fill_manual(values = c("rNIC" = "red", "AIC" = "blue", "BIC" = "orange", "looDeviance\n(baseline)" = "darkgray")) +
    scale_y_continuous(breaks = function(x) {
      seq(from = min(x), to = max(x), length.out = 5)
    },labels = function(y) sprintf("%.2f", y / 1000) ) +
    theme_minimal()+
    theme(text = element_text(face = "bold"),
          plot.subtitle = element_text(size=12, face="bold"),
          axis.title = element_text(size=12),
          axis.text = element_text(size=10),
          legend.title = element_text(size=12), 
          legend.text = element_text(size=10))
}

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
    levels(res_df_long$score) <- c("looDeviance\n(baseline)", "rNIC","BIC","AIC", "Deviance")
    best_df$score <- factor(best_df$score, levels=c("loodev", "nic","bic","aic","dev"))
    levels(best_df$score) <- c("looDeviance\n(baseline)", "rNIC","BIC","AIC", "Deviance")
    
    pl[[i]] <- ggplot(res_df_long, aes(x = model_size, y = value, group = score, color = score)) +
      geom_line() +
      geom_vline(xintercept = 5, color = "grey") +
      scale_color_manual(values = c("rNIC" = "red", "AIC" = "blue", "BIC" = "orange", "looDeviance\n(baseline)" = "black", "Deviance" = "gray")) +
      theme_minimal() +
      # scale_y_continuous(labels = scales::scientific_format()) +
      scale_y_continuous(breaks = function(x) {
        seq(from = min(x), to = max(x), length.out = 5)
      },labels = function(y) sprintf("%.2f", y / 1000) ) +
      geom_errorbar(data = best_df, aes(x = best_size, xmin=best_size_1se_min, xmax=best_size_1se_max, y = best_score, color=score))+
      geom_point(data = best_df, aes(x = best_size, y = best_score, color=score), size = 1.5)+
      labs(title = paste0("iter = ",i), x = "Model Size", y = "Value", color = "Criterion") +
      theme(text = element_text(face = "bold"),
            plot.subtitle = element_text(size=12, face="bold"),
            axis.title = element_text(size=12),
            axis.text = element_text(size=10),
            legend.title = element_text(size=12), 
            legend.text = element_text(size=10))
    if(detect_mal(res_df, sim_condition)){
      print(i)
    }
  }
  
  # ggarrange(plotlist = pl[1:20],nrow=4, ncol=5,common.legend = T, legend = "right")
  # ggarrange(plotlist = pl[21:40],nrow=4, ncol=5,common.legend = T, legend = "right")
  # ggarrange(plotlist = pl[41:60],nrow=4, ncol=5,common.legend = T, legend = "right")
  # ggarrange(plotlist = pl[61:80],nrow=4, ncol=5,common.legend = T, legend = "right")
  # ggarrange(plotlist = pl[81:100],nrow=4, ncol=5,common.legend = T, legend = "right")
  
  # find 5 examples
  if(model_size==5){
    if(sn == "lm") ie <- c(76, 54, 26, 32) #20
    if(sn == "lr") ie <- c(16, 33, 46, 51) #27
  }
  if(model_size==10){
    if(sn == "lm") ie <- c(76, 54, 26, 32) #, 20
    if(sn == "lr") ie <- c(16, 33, 46, 51) #27
  }
  
  pl_ie <- lapply(pl[ie], function(p) return(p+labs(title=NULL, x=NULL, y=NULL) + theme(legend.position = "top")))
  pie[[sn]] <- ggarrange(plotlist = pl_ie, nrow=2, ncol=2, common.legend = T, legend = "none") # ifelse(sn=="lm", "none", "bottom")
  
  pie[[sn]] <- annotate_figure(pie[[sn]], top=text_grob("Iteration examples", size = 12, face = "bold", hjust = -0.35, x = 0), 
                         left = text_grob("Value (e+03)", size = 12, face = "bold", rot = 90), 
                         bottom = text_grob("Model Size", size = 12, face = "bold"))
}

# title_text <- "Model selection trajectory"
p_lm <- ggarrange(p_agg[["lm"]],pie[["lm"]],nrow=2,ncol=1, common.legend = T, legend = "none")
p_lm <- annotate_figure(p_lm, top = text_grob("Gaussian", size = 14, face = "bold"))
p_lr <- ggarrange(p_agg[["lr"]],pie[["lr"]],nrow=2,ncol=1, common.legend = T, legend = "none")
p_lr <- annotate_figure(p_lr, top = text_grob("Binomial", size = 14, face = "bold"))
p_model_select <- ggarrange(p_lm, p_lr, nrow = 1, ncol = 2, common.legend = TRUE, legend.grob = get_legend(pl_ie[[1]]), legend = "bottom")



# p_aggregate <- ggarrange(plotlist = p_agg, nrow=1,ncol=2, common.legend = T, legend = "none")
# 
# 
# p_example <- ggarrange(plotlist = pie, nrow=1, ncol=2)
# p_model_select <- ggarrange(p_aggregate, p_example, ncol=1, nrow=2, heights = c(1,1),#c(1.2,4), 
#                             legend.grob = get_legend(pl_ie[[1]]), legend = "none")
# # title_text <- ifelse(cluster_size == 150,
# #                      paste0("Strong clustering condition: ", cluster_size," obs/cluster, AR1(0.8)"),
# #                      paste0("Weak clustering condition: ", cluster_size," obs/cluster, AR1(0)"))
# title_text <- "Criteria Value Trajectory"
# p_model_select <- annotate_figure(p_model_select, top = text_grob(title_text, size = 16, face = "bold")) # , hjust = 0,x=0
