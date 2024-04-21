
pl <- list()
pl_1se <- list()
for(sn in c("lm","lr")){
  diff_model_size <- data.frame()
  diff_model_size_1se <- data.frame()
  for(i in sort(unique(best_df_iter[[sn]]$i))){
    # calculate the difference in 1se min between loodev and ic
    best_df <- best_df_iter[[sn]][which(best_df_iter[[sn]]$i==i),]
    diff_model_size <- bind_rows(diff_model_size,
                                     data.frame(iter = i,
                                                nic = best_df$best_size[which(best_df$score=="nic")] -  best_df$best_size[which(best_df$score=="loodev")],
                                                aic = best_df$best_size[which(best_df$score=="aic")] -  best_df$best_size[which(best_df$score=="loodev")],
                                                bic = best_df$best_size[which(best_df$score=="bic")] -  best_df$best_size[which(best_df$score=="loodev")]))
    diff_model_size_1se <- bind_rows(diff_model_size_1se,
                                     data.frame(iter = i,
                                                nic = best_df$best_size_1se_min[which(best_df$score=="nic")] -  best_df$best_size_1se_min[which(best_df$score=="loodev")],
                                                aic = best_df$best_size_1se_min[which(best_df$score=="aic")] -  best_df$best_size_1se_min[which(best_df$score=="loodev")],
                                                bic = best_df$best_size_1se_min[which(best_df$score=="bic")] -  best_df$best_size_1se_min[which(best_df$score=="loodev")]))
    
  }
  # best model by min
  diff_model_size_long <- diff_model_size %>%
    pivot_longer(
      cols = c(nic, aic, bic),  # Specify columns to lengthen
      names_to = "score",  # New column for the names
      values_to = "value"  # New column for the values
    )
  diff_model_size_long$score <- factor(diff_model_size_long$score, levels = c("nic","bic","aic"))
  levels(diff_model_size_long$score) <- c("rNIC", "BIC","AIC")
  pl[[sn]] <- ggplot(diff_model_size_long, aes(x=value, group=score, fill=score))+
    geom_histogram(binwidth=1) +
    geom_vline(aes(xintercept = 0),color="black")+
    facet_wrap(~score)+
    scale_fill_manual(values = c("rNIC" = "red", "AIC" = "blue", "BIC" = "orange")) +
    theme_minimal() +
    labs(subtitle = ifelse(sn=="lm", "Gaussian", "Binomial"), 
         x = paste0("Error in model selection by Minimum"), y = "Count", fill = "Criteria")+
    theme(text = element_text(face = "bold"))  
  # best model by 1se
  diff_model_size_long <- diff_model_size_1se %>%
    pivot_longer(
      cols = c(nic, aic, bic),  # Specify columns to lengthen
      names_to = "score",  # New column for the names
      values_to = "value"  # New column for the values
    )
  diff_model_size_long$score <- factor(diff_model_size_long$score, levels = c("nic","bic","aic"))
  levels(diff_model_size_long$score) <- c("rNIC", "BIC","AIC")
  pl_1se[[sn]] <- ggplot(diff_model_size_long, aes(x=value, group=score, fill=score))+
    geom_histogram(binwidth=1) +
    geom_vline(aes(xintercept = 0),color="black")+
    facet_wrap(~score)+
    scale_fill_manual(values = c("rNIC" = "red", "AIC" = "blue", "BIC" = "orange")) +
    theme_minimal() +
    labs(subtitle = ifelse(sn=="lm", "Gaussian", "Binomial"), 
         x = paste0("Error in model selection by 1SE"), y = "Count", fill = "Criteria") +
    theme(text = element_text(face = "bold"))  
  
}

# (generating model size = ",model_size,")

p <- ggarrange(plotlist=pl,ncol=2,nrow=1,common.legend = T, legend = "none")
p1 <- ggarrange(plotlist=pl_1se,ncol=2,nrow=1,common.legend = T, legend = "none")
p_size <- ggarrange(p1,p,nrow=2,ncol=1)
title_text <- paste0("Generating model size = ", model_size, 
                     "; Number of clusters = ", sim_condition$n_cluster)
p_size <- annotate_figure(p_size, top = text_grob(title_text, size = 14, face = "bold"))
p_size %>% ggsave(filename=paste0("./res/model_select_error_",model_size,".png"), width = 10, height = 5, bg="white")


