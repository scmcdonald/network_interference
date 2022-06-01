#!/bin/bash
#
#SBATCH --job-name=run_bigvar
#
#SBATCH --output=out/run_bigvar_%j.out
#SBATCH --time=480
#SBATCH --mem=16G
#SBATCH --mail-type=all
#SBATCH --mail-user=scmcd@stanford.edu

ml R
Rscript big_var_yens.R
