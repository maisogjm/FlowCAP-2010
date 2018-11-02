# Run R script with input and output directories as command line arguments.
#
# Argument #1: input folder/directory containing input CSV files
# Argument #2: output folder/directory where results will be written to
# Argument #3: CSV file containing the number of expected populations
# Argument #4: alpha value
# Argument #5: number of runs/iterations
# Argument #6: Ksteps value

Rscript georgetown_Challenge3_tcluster_v4_072110.r $1 $2 $3 $4 $5 $6

# Something like this might work, too:
#
#     nohup R CMD BATCH georgetown_script.r $1 $2 $3 $4 logfile.out
#
