#PBS -l walltime=36:00:00
#PBS -l select=1:ncpus=4:mem=600gb
#PBS -N inla_cv_reduced

cd /rds/general/user/ft824/home/Term3-project/MLgeospatialpaper/scripts/scripts

eval "$(~/anaconda3/bin/conda shell.bash hook)"
source activate r442

Rscript INLA_cv.R