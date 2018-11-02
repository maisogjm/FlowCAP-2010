########################################################################
## Function to find zero rows.
########################################################################
FindZeroRow <- function(theRow) {
    return(all(theRow==0))
}

########################################################################
## Script execution begins below.
########################################################################

## Loop over all data sets.
dataSets = c("CFSE","GvHD","Lymph","NDD","StemCell")

for (j in 1:length(dataSets) ) {
    # Make list of CSV files for this data set.
    print("------------------------------------")
    print(sprintf("%s",dataSets[j]))
    inputFolder  = file.path("H:/DATA/Documents and Settings/Joe/Papers/GeneralStatistics/FlowCap/csv",
        dataSets[j],"CSV")
    fileList = sort(Sys.glob(file.path(inputFolder,"*.csv")))
    numFiles = length(fileList)
    
    # Loop over CSV files.
    for (i in 1:numFiles) {
        # Load data.
        fileName    = fileList[i]
        flowCytData = read.csv(fileName)

        ##----------------------------------------------------------
        ## Remove the FS and SS variables (first two columns),
        ## because the manual analysis didn't use these variables.

        nCols       = ncol(flowCytData)
        flowCytData = flowCytData[,3:nCols]
        allZeros    = t(apply(flowCytData, 1, FindZeroRow))

        ##----------------------------------------------------------
        ## If any rows that are all zeros are detected, show info.
        
        if ( sum(allZeros) > 0 ) {
            print(c(sprintf("    %s",basename(fileName)),which(allZeros>0)))
        }
    }
}
