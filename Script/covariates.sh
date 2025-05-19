#PBS -l walltime=10:00:00
#PBS -l select=1:ncpus=4:mem=400gb
#PBS -N covariates_processing

cd /rds/general/user/ft824/home/Term3-project/Script

eval "$(~/anaconda3/bin/conda shell.bash hook)"
source activate r442

Rscript covariates.R