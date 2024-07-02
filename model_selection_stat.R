
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
                                                nicc = best_df$best_size[which(best_df$score=="nicc")] -  best_df$best_size[which(best_df$score=="loodev")],
                                                aic = best_df$best_size[which(best_df$score=="aic")] -  best_df$best_size[which(best_df$score=="loodev")],
                                                bic = best_df$best_size[which(best_df$score=="bic")] -  best_df$best_size[which(best_df$score=="loodev")]))
    diff_model_size_1se <- bind_rows(diff_model_size_1se,
                                     data.frame(iter = i,
                                                nic = best_df$best_size_1se_min[which(best_df$score=="nic")] -  best_df$best_size_1se_min[which(best_df$score=="loodev")],
                                                nicc = best_df$best_size_1se_min[which(best_df$score=="nicc")] -  best_df$best_size_1se_min[which(best_df$score=="loodev")],
                                                aic = best_df$best_size_1se_min[which(best_df$score=="aic")] -  best_df$best_size_1se_min[which(best_df$score=="loodev")],
                                                bic = best_df$best_size_1se_min[which(best_df$score=="bic")] -  best_df$best_size_1se_min[which(best_df$score=="loodev")]))
    
  }
  # best model by min
  diff_model_size_long <- diff_model_size %>%
    pivot_longer(
      cols = c(nicc, nic, aic, bic),  # Specify columns to lengthen
      names_to = "score",  # New column for the names
      values_to = "value"  # New column for the values
    )
  diff_model_size_long$score <- factor(diff_model_size_long$score, levels = c("nicc","nic","aic","bic"))
  levels(diff_model_size_long$score) <- c("NICc","NIC","AIC", "BIC")
  pl[[sn]] <- ggplot(diff_model_size_long, aes(x=value, group=score, fill=score))+
    geom_histogram(binwidth=2) +
    geom_vline(aes(xintercept = 0),color="black")+
    facet_wrap(~score, nrow=1,ncol=4)+
    scale_fill_manual(values = c("NICc" = "red", "NIC" = "lightblue3", "AIC"="blue", "BIC" = "orange")) +
    theme_minimal() +
    xlim(ifelse(cs=="strong",-5,-10),ifelse(cs=="strong",20,10))+
    labs(#subtitle = ifelse(sn=="lm", "Gaussian", "Binomial"), 
         x = paste0("Error in selected size by minimal"), y = "Count", fill = "Criteria")+
    theme(text = element_text(face = "bold"),
          strip.text = element_text(size=12, face="bold"),
          axis.title = element_text(size=12),
          axis.text = element_text(size=10),
          legend.title = element_text(size=12), 
          legend.text = element_text(size=10))  
  # best model by 1se
  diff_model_size_long <- diff_model_size_1se %>%
    pivot_longer(
      cols = c(nicc, nic, aic, bic),  # Specify columns to lengthen
      names_to = "score",  # New column for the names
      values_to = "value"  # New column for the values
    )
  diff_model_size_long$score <- factor(diff_model_size_long$score, levels = c("nicc","nic","aic","bic"))
  levels(diff_model_size_long$score) <- c("NICc","NIC","AIC", "BIC")
  pl_1se[[sn]] <- ggplot(diff_model_size_long, aes(x=value, group=score, fill=score))+
    geom_histogram(binwidth=2) +
    geom_vline(aes(xintercept = 0),color="black")+
    facet_wrap(~score, nrow=1,ncol=4)+
    scale_fill_manual(values = c("NICc" = "red", "NIC" = "lightblue3", "AIC"="blue", "BIC" = "orange")) +
    theme_minimal() +
    xlim(ifelse(cs=="strong",-5,-10),ifelse(cs=="strong",20,10))+
    labs(#subtitle = ifelse(sn=="lm", "Gaussian", "Binomial"),
         x = paste0("Error in selected size by 1SE"), y = "Count", fill = "Criteria") +
    theme(text = element_text(face = "bold"),
          strip.text = element_text(size=12, face="bold"),
          axis.title = element_text(size=12),
          axis.text = element_text(size=10),
          legend.title = element_text(size=12), 
          legend.text = element_text(size=10))  
  
}



p_lm <- ggarrange(pl[["lm"]],pl_1se[["lm"]],nrow=2,ncol=1, common.legend = T, legend = "none")
p_lm <- annotate_figure(p_lm, top = text_grob("Gaussian", size = 14, face = "bold"))
p_lr <- ggarrange(pl[["lr"]],pl_1se[["lr"]],nrow=2,ncol=1, common.legend = T, legend = "none")
p_lr <- annotate_figure(p_lr, top = text_grob("Binomial", size = 14, face = "bold"))
p_size <- ggarrange(p_lm, p_lr, nrow = 1, ncol = 2, common.legend = TRUE)

# p <- ggarrange(plotlist=pl,ncol=2,nrow=1,common.legend = T, legend = "none")
# p1 <- ggarrange(plotlist=pl_1se,ncol=2,nrow=1,common.legend = T, legend = "none")
# p_size <- ggarrange(p1,p,nrow=2,ncol=1)
# # title_text <- paste0("Model selection error in 100 interations ", ifelse(cluster_size==150,"(strong clustering)", "(weak clustering)") )
# title_text <- "Error in 100 interations"
# p_size <- annotate_figure(p_size, top = text_grob(title_text, size = 14, face = "bold", hjust=0, x=0))
# p_size %>% ggsave(filename=paste0("./res/model_select_error_",model_size,"_",cluster_size,en,".png"), width = 10, height = 5, bg="white")


