#PBS -l walltime=24:00:00
#PBS -l select=1:ncpus=4:mem=200gb
#PBS -N processed_model6

cd /rds/general/user/ft824/home/Term3-project/Script

eval "$(~/anaconda3/bin/conda shell.bash hook)"
source activate r442

Rscript processed_model6.R