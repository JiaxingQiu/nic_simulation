rm(list=ls())
setwd("/Users/joyqiu/Documents/MediDSToolbox")
source("/Users/joyqiu/Documents/MediDSToolbox/shiny.R") # load tools from Joy's 
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
set.seed(333)
library(dplyr)
library(tidyr)
library(pROC)
library(foreach)
library(doParallel)
library(ggplot2)


for(u in c("nic", "ass", "stp", "lss", "do")){
  path = paste0("../../nic_simulation/utils/",u,"_utils")
  flst = list.files( path)
  sapply(c(paste(path,flst,sep="/")), source, .GlobalEnv)
}
# ---- load data ----
library(readxl)
data <- read_excel("./data/PAS Challenge Model Data.xlsx")
data_mdl <- assign.dict(data, get.dict(data))
data <- read_excel("./data/PAS Challenge Demographic Data.xlsx")
data_demo <- assign.dict(data, get.dict(data))
# data_demo <- data_demo[,setdiff(colnames(data_demo), c("Apgar5", "Apgar1"))]
data <- read_excel("./data/PAS Challenge Cross-Validation Folds.xlsx")
data_cv <- assign.dict(data, get.dict(data))
data <- read_excel("./data/PAS Challenge Outcome Data.xlsx")
data_outc <- assign.dict(data, get.dict(data))
data <- read.csv("./data/train_hr_uu.csv")[,c("VitalID", "VitalTime", "hr_increases")]
data_hruu <- assign.dict(data, get.dict(data))

# merge data in one
data <- merge(data_mdl, data_outc, all=TRUE)
data <- merge(data, data_cv, all=TRUE)
data <- merge(data, data_hruu, all=TRUE)
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
demo_vars <- c("EGA",
               "BWT",
               "Male", 
               "Multiple", 
               "Hispanic", 
               "Black",
               "White" )
demo_tbl <- distinct( data[,c(demo_vars, "Died", "VitalID")] )
demo_tbl$Ethnicity <- factor(ifelse(demo_tbl$Hispanic==1,"Hispanic","not"),levels=c("not","Hispanic" ))
demo_tbl$Race <- ifelse(demo_tbl$Black==1,"Black","Other")
demo_tbl$Race[which(demo_tbl$White==1)] <- "Caucasian"
demo_tbl$Race <- factor(demo_tbl$Race, levels = c("Caucasian", "Black", "Other") ) 
demo_tbl$Gender <- factor(ifelse(demo_tbl$Male==1,"Male","Female"),levels=c("Female","Male" ))
demo_tbl$Multiple <- factor(ifelse(demo_tbl$Multiple==1,"Yes","No"),levels=c("No","Yes" ))
demo_tbl[, "Gestational age in weeks"] <- demo_tbl$EGA
demo_tbl[, "Birth weight in grams"] <- demo_tbl$BWT
demo_tbl$Died <- factor(demo_tbl$Died , levels=c(0,1))
levels(demo_tbl$Died) <- c("Survived", "Died")
demo_tbl <- demo_tbl[,c("Gender", "Race", "Ethnicity", "Multiple", 
                        "Gestational age in weeks", "Birth weight in grams","Died","VitalID")]
# summary table
library(knitr)
library(kableExtra)

table_obj <- tableone::CreateTableOne(vars = c("Race", "Gender", "Ethnicity", "Multiple", 
                                               "Gestational age in weeks", "Birth weight in grams"),
                         strata = c("Died"),
                         data=demo_tbl,
                         test = F,
                         includeNA = F,
                         addOverall=T)
p <- print(table_obj)
kable(p, format = "latex", 
      caption = "Empirical Example: Patient Demographics",
      booktabs = T,
      align="l")%>% 
  kable_minimal(full_width = F,  html_font = "Source Sans Pro")

# Variable description 
df <- readxl::read_excel("./data/description.xlsx")
df$Description <- gsub("HR", "Heart rate", df$Description)
df %>% as.data.frame() %>%
  kable(caption="Empirical Example: Variable Description",
        format = "latex", 
        booktabs = T,
        align="l")


