# Run R script with input and output directories as command line arguments.
#
# Argument #1: input folder/directory containing input CSV files
# Argument #2: output folder/directory where results will be written to
# Argument #3: CSV file containing the number of expected populations
# Argument #4: number of runs/iterations

Rscript georgetown_Challenge3_SNMFL_WithVarNorm_v2_070910.r $1 $2 $3 $4

# Something like this might work, too:
#
#     nohup R CMD BATCH georgetown_script.r $1 $2 $3 $4 logfile.out
#
