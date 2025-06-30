#PBS -l walltime=12:00:00
#PBS -l select=1:ncpus=4:mem=100gb
#PBS -N prediction_plot

cd /rds/general/user/ft824/home/Term3-project/Script

eval "$(~/anaconda3/bin/conda shell.bash hook)"
source activate r442

Rscript predict_whole.R