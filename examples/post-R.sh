#!/bin/bash --login
#---------------------------- special -------------------------------
#PBS -N job.postR
#PBS -A 
#PBS -l walltime=12:00:00
#PBS -q regular
### Select 4 nodes with 16 CPUs each for a total of 64 MPI processes
#PBS -l select=1:ncpus=18:mpiprocs=18:mem=109GB

dir=wrf_output_folder

cd /glade/scratch/${USER}

conda activate rspatial

echo $dir

date

# METAR 5
Rscript extract_metar.R $dir T2  3d &
Rscript extract_metar.R $dir P      &
Rscript extract_metar.R $dir Q2  3d &
Rscript extract_metar.R $dir U10 3d &
Rscript extract_metar.R $dir V10 3d &

wait

echo $dir

date

tar -cvf Rds_${dir}.tar *.Rds
rm metar.d0*
