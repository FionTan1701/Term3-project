#PBS -l walltime=36:00:00
#PBS -l select=1:ncpus=4:mem=200gb
#PBS -N lsoa_prediction

cd /rds/general/user/ft824/home/Term3-project/MLgeospatialpaper/scripts/prediction

eval "$(~/anaconda3/bin/conda shell.bash hook)"
source activate r442

Rscript inla_lsoa_pred.R