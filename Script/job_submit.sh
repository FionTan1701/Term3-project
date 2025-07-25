#PBS -l walltime=24:00:00
#PBS -l select=1:ncpus=2:mem=50gb
#PBS -N INLA_runtime

cd /rds/general/user/ft824/home/Term3-project/Script

eval "$(~/anaconda3/bin/conda shell.bash hook)"
source activate r442

Rscript INLA_runtime.R