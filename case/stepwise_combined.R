
# merge data in one
data <- merge(data_mdl, data_outc, all=TRUE)
data <- merge(data, data_cv, all=TRUE)
data <- merge(data[,setdiff(colnames(data),"EGA")], data_demo, all=TRUE)
# must be data.frame type object before use dictionary functions
data <- as.data.frame(data)
# Create dictionary for data
data <- remove.dict(data)
data <- assign.dict(data, get.dict(data)) 
dict_data <- get.dict(data) # get a dictionary for this data
dict_data$type[which(dict_data$varname=="VitalID")] <- "key"
dict_data$unique_per_sbj[which(dict_data$varname=="VitalID")] <- TRUE
rownames(dict_data) <- NULL

# ---- engineering ----
c_col <- "VitalID"
y_col <- "Event"
x_cols <- setdiff(colnames(data_mdl),c_col)
x_cols <- c(x_cols[9:16],setdiff(colnames(data_demo),c(c_col,"Fold")))
df_mdl <- distinct(as.data.frame(data[,c("Fold", c_col, y_col, x_cols)]))
dict_mdl <- get.dict(df_mdl)
dict_mdl$type[which(dict_mdl$varname==c_col)] <- "key"
dict_mdl$unique_per_sbj <- TRUE
rownames(dict_mdl) <- NULL
c_label = dict_mdl$label[which(dict_mdl$varname==c_col)] 
y_label = dict_mdl$label[which(dict_mdl$varname==y_col)] 
x_labels_linear = dict_mdl$label[which(dict_mdl$type=="num")] 
x_labels_linear = setdiff(x_labels_linear, c(y_label, c_label, "Fold"))
x_labels_tag = dict_mdl$label[which(dict_mdl$type=="fct"&dict_mdl$unit=="tag01")] 
x_labels_tag = setdiff(x_labels_tag, c(y_label, c_label))
standardize_df <- data.frame(varname = x_labels_linear, 
                             center=apply(df_mdl[,x_labels_linear],2,mean,na.rm=TRUE),
                             scale=apply(df_mdl[,x_labels_linear],2,sd,na.rm=TRUE))
standardize_df$center <- round(standardize_df$center, 4)
standardize_df$scale <- round(standardize_df$scale, 4)
rownames( standardize_df ) <- NULL
df_mdl <- engineer(data = df_mdl,
                   num_cols = x_labels_linear,
                   fct_cols = c(x_labels_tag,y_label,"Fold"),
                   cluster_col = c_label,
                   imputation = "Median",
                   standardize_df = standardize_df)
df_mdl <- df_mdl[complete.cases(df_mdl),]

# ---- 100 fold ----
if(!file.exists("./res/fwd_clustered_combined.RDS")){
  fwd <- modified_stepwise_glm_parallel(df = df_mdl,
                                 y = y_col,
                                 x = x_cols,
                                 c = c_col, 
                                 maxstep = length(x_cols),
                                 eval_ls=c("Deviance", "AIC", "BIC", "NIC", "cvpred", "cvDeviance"),
                                 eval_by="cvDeviance",
                                 nfold = 50,
                                 family = "binomial",
                                 forward = T,
                                 free_cores = 2)
  res_df <- format_forward(fwd)
  saveRDS(res_df, file="./res/fwd_clustered_combined.RDS")
}


# # ---- loo subset 100 ----
# topn=500
# df_sub <- df_mdl%>%
#   group_by(Event, VitalID) %>%
#   summarise(n = n()) %>%
#   arrange(desc(n)) %>%
#   group_by(Event) %>%
#   slice_max(n, n = topn)
# df_sub <- df_mdl[which(df_mdl$VitalID %in%df_sub$VitalID), ]
# rm(fwd)
# fwd <- modified_stepwise_glm_parallel(df = df_sub,
#                                       y = y_col,
#                                       x = x_cols,
#                                       c = c_col, 
#                                       maxstep = length(x_cols),
#                                       eval_ls=c("Deviance", "AIC", "BIC", "NIC", "cvpred", "cvDeviance"),
#                                       eval_by="cvDeviance",
#                                       nfold = 50,
#                                       family = "binomial",
#                                       forward = T,
#                                       free_cores = 2)
# res_df <- format_forward(fwd)
# res_df_long <- res_df %>%
#   dplyr::select(-cvpred) %>%
#   pivot_longer(
#     cols = c(nic, aic, bic, dev, cvdev),  # Specify columns to lengthen
#     names_to = "score",  # New column for the names
#     values_to = "value"  # New column for the values
#   )
# res_df_long$score <- factor(res_df_long$score, levels=c("cvdev","cvpred", "nic","bic","aic","dev"))
# levels(res_df_long$score) <- c("cvDeviance", "cvAUC", "rNIC","BIC","AIC", "Deviance")
# 
# best_df <- data.frame() 
# for(score in c("cvdev", "nic", "aic", "bic")){
#   best_size <- res_df$model_size[which(res_df[,score]==min(res_df[,score]))][1]
#   best_score <- res_df[,score][which(res_df[,score]==min(res_df[,score]))][1]
#   score_1se <- sd(res_df[,score])/sqrt(nrow(res_df))
#   best_size_1se_min <- min(res_df$model_size[which(abs(res_df[,score]-min(res_df[,score]))<=score_1se)])
#   best_size_1se_max <- max(res_df$model_size[which(abs(res_df[,score]-min(res_df[,score]))<=score_1se)])
#   best_df <- bind_rows(best_df, data.frame(score,best_size, best_score, score_1se, best_size_1se_min,best_size_1se_max))
# }
# best_df$score <- factor(best_df$score, levels=c("cvdev","cvpred", "nic","bic","aic","dev"))
# levels(best_df$score) <- c("cvDeviance", "cvAUC", "rNIC","BIC","AIC", "Deviance")
# ggplot(res_df_long, aes(x = model_size, y = value)) +
#   # geom_point(data = res_df_long[which(res_df_long$score=="cvDeviance"),], size = 2) + 
#   geom_line(aes(group = score, color = score)) +
#   geom_text(data = res_df_long[which(res_df_long$score=="cvDeviance"),], aes(label=x_picked), size = 3, hjust = -0.15, angle = 90) + 
#   scale_color_manual(values = c("rNIC" = "red", "AIC" = "blue", "BIC" = "darkorange", "cvDeviance" = "black", "Deviance" = "gray")) +
#   theme_minimal() +
#   geom_errorbar(data = best_df, aes(x = best_size, xmin=best_size_1se_min, xmax=best_size_1se_max, y = best_score, color=score), width=1)+
#   geom_point(data = best_df, aes(x = best_size, y = best_score, color=score), size = 1.5)+
#   # scale_y_continuous(labels = scales::scientific_format()) +
#   scale_y_continuous(
#     limits = function(y) { c(min(y)-0.01*(max(y)-min(y)), max(y)+0.1*(max(y)-min(y))) },
#     breaks = function(y) { seq(from = min(y), to = max(y), length.out = 5) },
#     labels = function(y) sprintf("%.2f", y / 1000) ) +
#   labs(title = "combined",
#        subtitle = paste0("top ", 2*topn), 
#        x = "Model Size", 
#        y = "Value (e+03)", 
#        color = "Criterion") +
#   theme(text = element_text(face = "bold"),
#         plot.title = element_text(size=16, face="bold"),
#         plot.subtitle = element_text(size=12, face="bold"),
#         axis.title = element_text(size=12),
#         axis.text = element_text(size=10),
#         legend.title = element_text(size=12), 
#         legend.text = element_text(size=10))
# 
# 

