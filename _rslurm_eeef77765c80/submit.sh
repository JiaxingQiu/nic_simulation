#!/bin/bash
#
#SBATCH --array=0-1079
#SBATCH --cpus-per-task=1
#SBATCH --job-name=eeef77765c80
#SBATCH --output=slurm_%a.out
#SBATCH --account=netlab
#SBATCH --partition=standard
#SBATCH --time=2-00:00:00
/Library/Frameworks/R.framework/Resources/bin/Rscript --vanilla slurm_run.R
