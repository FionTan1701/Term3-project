#PBS -l walltime=6:00:00
#PBS -l select=1:ncpus=1:mem=200gb
#PBS -N lsoa_prediction_plot

cd /rds/general/user/ft824/home/Term3-project/Script/prediction

eval "$(~/anaconda3/bin/conda shell.bash hook)"
source activate r442

Rscript lsoa_prediction_plot.R