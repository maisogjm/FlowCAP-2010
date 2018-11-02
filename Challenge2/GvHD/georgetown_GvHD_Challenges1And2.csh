#!/bin/csh
# The above line indicates run the C shell.

# This C-shell script will invoke the common Bourne shell script
# that in turn invokes the common R script.
# The curvHDR method automatically determines the number of clusters,
set dataSet   = GvHD
set inputDir  = /home/jmm97/flowCAP/csv/$dataSet/CSV
set outputDir = /home/jmm97/flowCAP/csv/$dataSet/CSV/Output_curvHDR_062910_nndsvd

# Invoke georgetown_Challenge1.sh in the background with nohup and nice.
# This allows you to log off, and the job will keep running.
nohup nice georgetown_Challenge1.sh $inputDir $outputDir &
