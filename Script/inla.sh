#PBS -l walltime=5:00:00
#PBS -l select=1:ncpus=1:mem=50gb
#PBS -N inla


cd /rds/general/user/ft824/home/Term3-project/Script

eval "$(~/anaconda3/bin/conda shell.bash hook)"
source activate r442

Rscript inla_try.R