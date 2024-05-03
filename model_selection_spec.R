
pl <- list()
pl_1se <- list()
pl_box <- list()
pl_1se_box <- list()
for(sn in c("lm","lr")){
  model_misspec <- data.frame()
  model_misspec_1se <- data.frame()
  for(i in sort(unique(best_df_iter[[sn]]$i))){
    best_df <- best_df_iter[[sn]][which(best_df_iter[[sn]]$i==i),]
    res_df <- res_df_iter[[sn]][which(res_df_iter[[sn]]$iter==i),]
    colnames(res_df) <- gsub("iance","", stringr::str_to_lower(colnames(res_df)))
    
    # find set of predictors
    opt_model_size_base <- best_df$best_size[best_df$score=="loodev"]
    set_cr_base <- res_df[which(res_df$model_size<=opt_model_size_base), paste0("loodev_x_picked")]
    jaccard <- list()
    for(cr in c("nic","nicc","aic","bic")){
      opt_model_size <- best_df$best_size[best_df$score==cr]
      set_cr <- res_df[which(res_df$model_size<=opt_model_size), paste0(cr,"_x_picked")]
      jaccard[[cr]] <- length(intersect(set_cr_base, set_cr))/length(union(set_cr_base, set_cr))
    }
    jdf <- as.data.frame(jaccard)
    jdf$iter <- i
    model_misspec <- bind_rows(model_misspec, jdf)
    
    # find set of predictors at 1se
    opt_model_size_base <- best_df$best_size_1se_min[best_df$score=="loodev"]
    set_cr_base <- res_df[which(res_df$model_size<=opt_model_size_base), paste0("loodev_x_picked")]
    jaccard <- list()
    for(cr in c("nic","nicc","aic","bic")){
      opt_model_size <- best_df$best_size_1se_min[best_df$score==cr]
      set_cr <- res_df[which(res_df$model_size<=opt_model_size), paste0(cr,"_x_picked")]
      jaccard[[cr]] <- length(intersect(set_cr_base, set_cr))/length(union(set_cr_base, set_cr))
    }
    jdf <- as.data.frame(jaccard)
    jdf$iter <- i
    model_misspec_1se <- bind_rows(model_misspec_1se, jdf)
  }
  
  # best model by min
  model_misspec_long <- model_misspec %>%
    pivot_longer(
      cols = c(nicc, nic, aic, bic),  # Specify columns to lengthen
      names_to = "score",  # New column for the names
      values_to = "value"  # New column for the values
    )
  model_misspec_long$score <- factor(model_misspec_long$score, levels = c("nicc","nic","aic","bic"))
  levels(model_misspec_long$score) <- c("NICc","NIC","AIC", "BIC")
  pl[[sn]] <- ggplot(model_misspec_long, aes(x=value, group=score, fill=score))+
    geom_histogram(binwidth=0.1) +
    geom_vline(aes(xintercept = 1),color="black")+
    facet_wrap(~score, nrow=1,ncol=4)+
    scale_fill_manual(values = c("NICc" = "red", "NIC" = "lightblue3", "AIC"="blue", "BIC" = "orange")) +
    theme_minimal() +
    labs(#subtitle = ifelse(sn=="lm", "Gaussian", "Binomial"), 
         x = paste0("Accuracy at the Minimal"), y = "Count", fill = "Criteria")+
    theme(text = element_text(face = "bold"),
          strip.text = element_text(size=12, face="bold"),
          axis.title = element_text(size=12),
          axis.text = element_text(size=10),
          legend.title = element_text(size=12), 
          legend.text = element_text(size=10))  
  
  model_misspec_long$score <- factor(model_misspec_long$score, levels=rev(c("NICc","NIC","AIC", "BIC")))
  pl_box[[sn]] <- ggplot(model_misspec_long, aes(x=value, group=score, fill=score))+
    geom_boxplot(aes(y=score)) + 
    scale_fill_manual(values = c("NICc" = "red", "NIC" = "lightblue3", "AIC"="blue", "BIC" = "orange")) +
    theme_minimal() +
    labs(x = paste0("Accuracy at the Minimal"), y = "Count", fill = "Criteria")+
    theme(text = element_text(face = "bold"),
          strip.text = element_text(size=12, face="bold"),
          axis.title = element_text(size=12),
          axis.text = element_text(size=10),
          legend.title = element_text(size=12), 
          legend.text = element_text(size=10))  
  
  
  # best model by 1se
  model_misspec_1se_long <- model_misspec_1se %>%
    pivot_longer(
      cols = c(nicc, nic, aic, bic),  # Specify columns to lengthen
      names_to = "score",  # New column for the names
      values_to = "value"  # New column for the values
    )
  model_misspec_1se_long$score <- factor(model_misspec_1se_long$score, levels = c("nicc","nic","aic","bic"))
  levels(model_misspec_1se_long$score) <- c("NICc","NIC","AIC", "BIC")
  pl_1se[[sn]] <- ggplot(model_misspec_1se_long, aes(x=value, group=score, fill=score))+
    geom_histogram(binwidth=0.1) +
    geom_vline(aes(xintercept = 1),color="black")+
    facet_wrap(~score, nrow=1,ncol=4)+
    scale_fill_manual(values = c("NICc" = "red", "NIC" = "lightblue3", "AIC"="blue", "BIC" = "orange")) +
    theme_minimal() +
    labs(x = paste0("Accuracy at 1SE"), y = "Count", fill = "Criteria") +
    theme(text = element_text(face = "bold"),
          strip.text = element_text(size=12, face="bold"),
          axis.title = element_text(size=12),
          axis.text = element_text(size=10),
          legend.title = element_text(size=12), 
          legend.text = element_text(size=10))  
  
  model_misspec_1se_long$score <- factor(model_misspec_1se_long$score, levels = rev(c("NICc","NIC","AIC", "BIC")) )
  pl_1se_box[[sn]] <- ggplot(model_misspec_1se_long, aes(x=value, group=score, fill=score))+
    geom_boxplot(aes(y=score)) + 
    scale_fill_manual(values = c("NICc" = "red", "NIC" = "lightblue3", "AIC"="blue", "BIC" = "orange")) +
    theme_minimal() +
    labs(x = paste0("Accuracy at 1SE"), y = "Count", fill = "Criteria") +
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
p_spec <- ggarrange(p_lm, p_lr, nrow = 1, ncol = 2, common.legend = TRUE)



p_lm <- ggarrange(pl_box[["lm"]],pl_1se_box[["lm"]],nrow=2,ncol=1, common.legend = T, legend = "none")
p_lm <- annotate_figure(p_lm, top = text_grob("Gaussian", size = 14, face = "bold"))
p_lr <- ggarrange(pl_box[["lr"]],pl_1se_box[["lr"]],nrow=2,ncol=1, common.legend = T, legend = "none")
p_lr <- annotate_figure(p_lr, top = text_grob("Binomial", size = 14, face = "bold"))
p_spec_box <- ggarrange(p_lm, p_lr, nrow = 1, ncol = 2, common.legend = TRUE)
