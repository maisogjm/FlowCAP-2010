library(tclust)

dataSets = c("CFSE","GvHD","Lymph","NDD","StemCell")

# Select GvHD Data
j = 2
inputFolder  = file.path("H:/DATA/Documents and Settings/Joe/Papers/GeneralStatistics/FlowCap/csv",
        dataSets[j],"CSV")
outputFolder = "H:/DATA/Documents and Settings/Joe/Papers/GeneralStatistics/FlowCap/csv/Challenge3"

fileList = sort(Sys.glob(file.path(inputFolder,"*.csv")))
numFiles = length(fileList)

# Load in 7th sample
i = 7
csvFileName = basename(fileList[i])
flowCytData = read.csv(fileList[i])
dataDim     = dim(flowCytData)
nCols       = dataDim[2]

# Narrow down to only SSC and FL2
inputData = flowCytData[,c(2,4)]

## # Scatter plot of raw data.
## title = sprintf("Scatter Plot of Raw Data for Sample #%d",i)
## pairs(inputData ,main=title,
##                     lower.panel=function(...) {par(new=TRUE);smoothScatter(...)},
##                     upper.panel=panel.cor)
## png1Name = sprintf("Raw.TwoColumnsOnly.%s.%03d",dataSets[j],i)
## savepng(png1Name,dir=outputFolder,width=640,asp=1)

# Now do tCLUST classification.  # of groups = 5
tclustResult = tclust(flowCytData, k=5)
plot(tclustResult)
ps1Name = sprintf("tclust_TwoColumnsOnly_%s_%03d.eps",dataSets[j],i)
ps1Name = file.path(outputFolder,ps1Name)
savePlot(ps1Name,type="eps")
## savepng(png2Name, dir=outputFolder,width=640,asp=1)
