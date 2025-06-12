#PBS -l walltime=72:00:00
#PBS -l select=1:ncpus=4:mem=600gb
#PBS -N 4_cov_processing

cd /rds/general/user/ft824/home/Term3-project/Script/data_processing

eval "$(~/anaconda3/bin/conda shell.bash hook)"
source activate r442

Rscript covariates_era5_v2.R