setwd(dirname(rstudioapi::getSourceEditorContext()$path))
rm(list=ls())
source("./sim_model_size_output.R")
source("./sim_model_misspecification_output.R")

p_weak <- ggarrange(plotlist = list(p_size_ls$weak,p_miss_ls$weak), nrow=2,ncol=1, heights = c(1,1.3))
p_mode <- ggarrange(plotlist = list(p_size_ls$moderate,p_miss_ls$moderate), nrow=2,ncol=1, heights = c(1,1.3))

p_weak <- annotate_figure(p_weak, top=text_grob("Weak Clustering", size = 16, face = "bold"))
p_mode <- annotate_figure(p_mode, top=text_grob("Moderate Clustering", size = 16, face = "bold"))

p_not_strong <- ggarrange(p_weak, p_mode, nrow=1, ncol=2)
p_not_strong %>% ggsave(filename="./res/model_select_not_strong_clustering.png", width = 14, height = 10, bg="white")

p_mode_strong <- ggarrange(plotlist = list(p_size_ls$moderate_strong,p_miss_ls$moderate_strong), nrow=2,ncol=1, heights = c(1,1.3))
p_mode_strong <- annotate_figure(p_mode_strong, top=text_grob("Moderate Strong Clustering", size = 16, face = "bold"))
p_mode_strong %>% ggsave(filename="./res/model_select_moderate_strong_clustering.png", width = 7, height = 10, bg="white")

