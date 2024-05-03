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
x_cols <- setdiff(colnames(data_demo),c(c_col,"Fold"))
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

# ---- k fold ----
if(!file.exists("./res/fwd_unclustered_demo.RDS")){
  # fwd <- modified_stepwise_glm_parallel(df = df_mdl,
  #                                       y = y_col,
  #                                       x = x_cols,
  #                                       c = c_col, 
  #                                       maxstep = length(x_cols),
  #                                       eval_ls=c("Deviance", "AIC", "BIC", "NIC", "cvpred", "cvDeviance"),
  #                                       eval_by="cvDeviance",
  #                                       nfold = length(unique(df_mdl[,c_col])),
  #                                       family = "binomial",
  #                                       forward = T,
  #                                       free_cores = 2)
  # res_df <- format_forward(fwd)
  # step-wise forward 
  res_df <- NULL
  for(cr in c("AIC", "BIC", "NIC", "NICc", "cvDeviance") ){
    m1_sl <- modified_stepwise_glm_parallel(df = df_mdl,
                                            y = y_col,
                                            x = x_cols,
                                            c = c_col, 
                                            maxstep = length(x_cols),
                                            eval_ls=c(cr,"Deviance"),
                                            eval_by=cr,
                                            nfold = 100,
                                            family = "binomial",
                                            forward = T,
                                            free_cores = 1)
    
    res_df_cr <- format_forward(m1_sl)
    colnames(res_df_cr)[which(colnames(res_df_cr)=="x_picked")] <- paste0(cr,"_x_picked")
    x_picked_cn <- paste0(cr,"_x_picked")
    if(cr == "cvDeviance"){
      cr = c(gsub("iance","",cr),"dev")
    }
    cr <- stringr::str_to_lower(cr)
    if(is.null(res_df)) {
      res_df <- res_df_cr[,c("model_size",x_picked_cn, cr)]
    }else{
      res_df <- merge(res_df, res_df_cr[,c("model_size",x_picked_cn,cr)])
    }
  }
  saveRDS(res_df, file="./res/fwd_unclustered_demo.RDS")
}
