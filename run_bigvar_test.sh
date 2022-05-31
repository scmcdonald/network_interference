#!/bin/bash
#
#SBATCH --job-name=run_bigvar
#
#SBATCH --output=out/run_bigvar_%j.out
#SBATCH --time=60
#SBATCH --mem=8G
#SBATCH --mail-type=all
#SBATCH --mail-user=scmcd@stanford.edu

ml R
Rscript bigvar_test.R
