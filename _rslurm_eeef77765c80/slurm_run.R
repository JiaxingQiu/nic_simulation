library(base, quietly = TRUE)
library(methods, quietly = TRUE)
library(datasets, quietly = TRUE)
library(utils, quietly = TRUE)
library(grDevices, quietly = TRUE)
library(graphics, quietly = TRUE)
library(stats, quietly = TRUE)
library(dplyr, quietly = TRUE)
library(rslurm, quietly = TRUE)
library(Matrix, quietly = TRUE)
library(lme4, quietly = TRUE)
library(MASS, quietly = TRUE)
library(pROC, quietly = TRUE)
library(foreach, quietly = TRUE)
library(iterators, quietly = TRUE)
library(parallel, quietly = TRUE)
library(doParallel, quietly = TRUE)
library(ggplot2, quietly = TRUE)
library(ggpubr, quietly = TRUE)
library(tidyr, quietly = TRUE)

load('add_objects.RData')

.rslurm_func <- readRDS('f.RDS')
.rslurm_x <- readRDS('x.RDS')
.rslurm_more_args <- readRDS('more_args.RDS')
.rslurm_id <- as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID'))
.rslurm_istart <- .rslurm_id * 1 + 1
.rslurm_iend <- min((.rslurm_id + 1) * 1, length(.rslurm_x))
.rslurm_result <- do.call(parallel::mclapply, c(list(
    X = .rslurm_x[.rslurm_istart:.rslurm_iend],
    FUN = .rslurm_func),
    .rslurm_more_args,
    mc.cores = 1,
    mc.preschedule = FALSE
    ))

saveRDS(.rslurm_result, file = paste0('results_', .rslurm_id, '.RDS'))
