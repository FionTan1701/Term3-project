#PBS -l walltime=12:00:00
#PBS -l select=1:ncpus=2:mem=200gb
#PBS -N lasso_model_cv

cd /rds/general/user/ft824/home/Term3-project/Script

eval "$(~/anaconda3/bin/conda shell.bash hook)"
source activate r442

Rscript lasso_model_cv.R