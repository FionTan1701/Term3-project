#PBS -l walltime=24:00:00
#PBS -l select=1:ncpus=4:mem=100gb
#PBS -N rf_model_cv

cd /rds/general/user/ft824/home/Term3-project/MLgeospatialpaper/scripts/ml_model

eval "$(~/anaconda3/bin/conda shell.bash hook)"
source activate r442

Rscript RF_model_cv.R