#PBS -l walltime=48:00:00
#PBS -l select=1:ncpus=5:mem=800gb
#PBS -N cov_processing

cd /rds/general/user/ft824/home/Term3-project/Script

eval "$(~/anaconda3/bin/conda shell.bash hook)"
source activate r442

Rscript covariates_era5.R