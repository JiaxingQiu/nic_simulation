

pie <- list()
for(sn in c("lm", "lr")){
  pl <- list()
  for(i in sort(unique(res_df_iter[[sn]]$iter))){
    # res_df <- run_wrapper_lr(sim_condition)
    res_df <- res_df_iter[[sn]][which(res_df_iter[[sn]]$iter==i),]
    res_df_long <- res_df %>%
      pivot_longer(
        cols = c(nic, aic, nicc, bic, dev, loodev),  # Specify columns to lengthen
        names_to = "score",  # New column for the names
        values_to = "value"  # New column for the values
      )
    best_df <- best_df_iter[[sn]][which(best_df_iter[[sn]]$i==i),]
    # ymin <- res_df_long$value[which(res_df_long$value == min(res_df_long$value[res_df_long$score=="dev"]) )][1]
    # ymax <- res_df_long$value[res_df_long$score=="loodev"&res_df_long$model_size==1]
    res_df_long$score <- factor(res_df_long$score, levels=c("loodev","nicc", "nic","aic","bic","dev"))
    levels(res_df_long$score) <- c("looDeviance\n(baseline)","NICc", "NIC", "AIC", "BIC", "Deviance")
    best_df$score <- factor(best_df$score, levels=c("loodev","nicc", "nic","aic","bic","dev"))
    levels(best_df$score) <- c("looDeviance\n(baseline)","NICc", "NIC", "AIC", "BIC", "Deviance")
    
    ymax <- res_df_long$value[which(res_df_long$score=="looDeviance\n(baseline)" & res_df_long$model_size==1)] + 300
    ymin <- res_df_long$value[which(res_df_long$score=="NIC" & res_df_long$model_size==max(res_df_long$model_size))] - 50
    
    # res_df_long <- res_df_long %>% filter(model_size<=24)
    # best_df$best_size[which(best_df$best_size>24)] <- 24
    # best_df$best_size_1se_max[which(best_df$best_size_1se_max>24)] <- 24
    # best_df$best_size_1se_min[which(best_df$best_size_1se_min>24)] <- 24
    
    pl[[i]] <- ggplot(res_df_long, aes(x = model_size, y = value, group = score, color = score)) +
      geom_line() +
      #geom_vline(xintercept = 5, color = "grey") +
      scale_color_manual(values = c("NICc" = "red", "AIC" = "blue","NIC" = "lightblue3", "BIC" = "orange", "looDeviance\n(baseline)" = "black", "Deviance" = "gray")) +
      theme_minimal() +
      xlim(0,24)+
      scale_y_continuous(
        limits = c(ymin,ymax),
        breaks = function(x) {
        seq(from = min(x), to = max(x), length.out = 5)
      },labels = function(y) sprintf("%.2f", y / 1000) ) +
      geom_errorbar(data = best_df, aes(x = best_size, xmin=best_size_1se_min, xmax=best_size_1se_max, y = best_score, color=score))+
      geom_point(data = best_df, aes(x = best_size, y = best_score, color=score), size = 1.5)+
      labs(title = paste0("iter = ",i), x = "Model size", y = "Value", color = "Criterion") +
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

  # find 4 examples
  if(model_size==5 & cluster_size==150){
    if(sn == "lm") ie <- c(1, 34, 42, 66) 
    if(sn == "lr") ie <- c(16, 30, 56, 79)
  }
  if(model_size==5 & cluster_size==5){
    if(sn == "lm") ie <- c(2, 33, 80, 100) #50
    if(sn == "lr") ie <- c(3, 31, 59, 100) 
  }
  if(model_size==10){
    if(sn == "lm") ie <- c(76, 54, 26, 32) #, 20
    if(sn == "lr") ie <- c(16, 33, 46, 51) #27
  }
  
  pl_ie <- lapply(pl[ie], function(p) return(p+labs(title=NULL, x=NULL, y=NULL) + theme(legend.position = "top")))
  pie[[sn]] <- ggarrange(plotlist = pl_ie, nrow=2, ncol=2, common.legend = T, legend = "none") # ifelse(sn=="lm", "none", "bottom")
  # ggarrange(plotlist = pl_ie, nrow=2, ncol=5, common.legend = T, legend = "none") # ifelse(sn=="lm", "none", "bottom")
  
  pie[[sn]] <- annotate_figure(pie[[sn]], #top=text_grob("Iteration examples", size = 12, face = "bold", hjust = -0.35, x = 0), 
                         left = text_grob("Value (e+03)", size = 12, face = "bold", rot = 90), 
                         bottom = text_grob("Model size", size = 12, face = "bold"))
}

# title_text <- "Model selection trajectory"
p_lm <- ggarrange(pie[["lm"]],nrow=1,ncol=1, common.legend = T, legend = "none")
p_lm <- annotate_figure(p_lm, top = text_grob("Gaussian", size = 14, face = "bold"))
p_lr <- ggarrange(pie[["lr"]],nrow=1,ncol=1, common.legend = T, legend = "none")
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
