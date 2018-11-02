##----------------------------------------------------------
## Load input flow cytometry data.

dataSet     = "GvHD"
topInputDir = "H:/DATA/Documents and Settings/Joe/Papers/GeneralStatistics/FlowCap/csv"
inputFolder = file.path(topInputDir,dataSet,"CSV")
fileList    = sort(Sys.glob(file.path(inputFolder,"*.csv")))
i           = 7
flowCytData = read.csv(fileList[i])

##----------------------------------------------------------
## Remove the FS and SS variables (first two columns),
## because the manual analysis didn't use these variables.

nCols       = ncol(flowCytData)
flowCytData = flowCytData[,3:nCols]

##------------------------------------------------------------------
## Load the file containing the number of populations/classes in
## each sample file.
## Grab only the 2nd column of the output of read.csv.

nccDir       = "H:/DATA/Documents and Settings/Joe/Papers/GeneralStatistics/FlowCap/NCC"
numPopFile   = file.path(nccDir,"GvHD.csv")
numClass     = read.csv(numPopFile)[,2]
k            = numClass[i]

##------------------------------------------------------------------
## Try kmeans.  The result will be in km$cluster.

km = kmeans(flowCytData,k)

##------------------------------------------------------------------
## Try sparcl.

library(sparcl)
kmsc = KMeansSparseCluster(as.matrix(flowCytData), k,
    wbounds = NULL, nstart = 20, silent = FALSE, maxiter=6)

inputData = read.csv("H:/DATA/Documents and Settings/Joe/Papers/GeneralStatistics/FlowCap/csv/GvHD/CSV/007.csv")
kmsc      = KMeansSparseCluster(as.matrix(inputData), 5, wbounds = 1, nstart = 20, silent = FALSE, maxiter=6)
kmsc      = KMeansSparseCluster(as.matrix(inputData), 5, wbounds = 1)
