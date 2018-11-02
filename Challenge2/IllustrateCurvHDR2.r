#library(geneplotter)
#library(NMF)
#library(RColorBrewer)
library(curvHDR)

dataSets = c("CFSE","GvHD","Lymph","NDD","StemCell")

# Select GvHD Data
j = 2
inputFolder  = file.path("H:/DATA/Documents and Settings/Joe/Papers/GeneralStatistics/FlowCap/csv",
        dataSets[j],"CSV")
outputFolder = file.path("H:/DATA/Documents and Settings/Joe/Papers/GeneralStatistics/FlowCap/csv/Challenge2",
        dataSets[j])

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

# Now do curvHDR classification, HDR Level = 0.1
hdrLevel = 0.1
polygongate1 = curvHDRfilter(inputData, HDRlevel = hdrLevel)
numGates     = length(polygongate1$polys)
plot(polygongate1,xlab="SSC.H",ylab="FL2.H",xlim=c(0,500))
ps1Name = sprintf("curvHDR1_TwoColumnsOnly_%s_%03d.eps",dataSets[j],i)
ps1Name = file.path(outputFolder,ps1Name)
savePlot(ps1Name,type="eps")
## savepng(png2Name, dir=outputFolder,width=640,asp=1)

# Now do curvHDR classification, HDR Level = 0.2
hdrLevel = 0.2
polygongate2 = curvHDRfilter(inputData, HDRlevel = hdrLevel)
numGates     = length(polygongate2$polys)
plot(polygongate2 ,xlab="SSC.H",ylab="FL2.H",xlim=c(0,500))
ps2Name = sprintf("curvHDR2_TwoColumnsOnly_%s_%03d.eps",dataSets[j],i)
ps2Name = file.path(outputFolder,ps2Name)
savePlot(ps2Name,type="eps")
## savepng(png3Name , dir=outputFolder,width=640,asp=1)



