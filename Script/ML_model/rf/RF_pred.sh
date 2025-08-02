#PBS -l walltime=24:00:00
#PBS -l select=1:ncpus=2:mem=100gb
#PBS -N RF_lsoa_prediction

cd /rds/general/user/ft824/home/Term3-project/Script/ML_model/rf

eval "$(~/anaconda3/bin/conda shell.bash hook)"
source activate r442

Rscript RF_prediction.R