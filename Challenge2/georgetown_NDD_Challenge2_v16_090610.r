#########################################################
## Function to perform classification within one row.
## This handles only rows in which there is no overlap.
#########################################################
ClassifyNonoverlapRow <- function(aRow,curvHdrObj,clusterMeans) {
    ##----------------------------------------------------------
    ## If this row has no TRUE entry, return 0.

    if ( all( ! aRow ) ) {
        return(0)
    }

    ##----------------------------------------------------------
    ## If this row has only one TRUE entry, return the index for
    ## that entry.

    if ( sum(as.numeric(aRow)) == 1 ) {
        return(which(aRow==TRUE))
    }

    ##----------------------------------------------------------
    ## This row has more than one TRUE entry, return -1 to
    ## indicate this.

    return(-1)
}

#########################################################
## Function to resolve overlaps in a row.
#########################################################
ResolveOverlaps <- function(aDataFrameRow,clusterMeans) {
    ##----------------------------------------------------------
    ## If the classification is NOT -1, return -- this
    ## row did not have an overlap problem.

    if ( aDataFrameRow[3] != -1 ) {
        return(0)
    }

    ##----------------------------------------------------------
    ## Compute squared distance of this point from the centroid
    ## of each of the gates.  (No need to take the square root.)

    numGates =  nrow(clusterMeans)
    sqrDist  = rep(0,numGates)
    for (i in 1:numGates) {
        delta      = aDataFrameRow[1:2] - clusterMeans[i,]
        sqrDist[i] = sum(delta * delta)
    }

    ##----------------------------------------------------------
    ## Return the index where the minimum (squared) distance
    ## occurs.

    return(which.min(sqrDist))
}

#########################################################
## Function to perform curvHDR classification.
## The input 'inputData' must have exactly two columns.
#########################################################
ClassifyUsing2DCurvHDR <- function(inputData, hdrLevel = 0.1,
        nmfMethod="snmf/l", numRuns = 5, nmfSeed = 123456,i) {
        
    ##------------------------------------------------------
    ## Take the absolute value so that the data are non-negative.

    inputData = abs(inputData);
    ##------------------------------------------------------
    ## Next, normalize variance of data columns.

    nCols   = ncol(inputData)
    dataSD  = sd(inputData)
    for (j in 1:nCols) {
        inputData[,j] = inputData[,j] / dataSD[j]
    }
    ##----------------------------------------------------------
    ## Compute NMF.  We need to reduce the rank to 2, because
    ## we are using the 2D version of curvHDR.

    nmfResults = nmf(as.matrix(inputData), rank = 2,
        method=nmfMethod, nrun=numRuns, seed=nmfSeed)

    ##----------------------------------------------------------
    ## Obtain the W matrix.

    W = basis(nmfResults)
    rm(nmfResults)

    ##------------------------------------------------------
    ## Perform asinh transformation.

    W = asinh(W)
    ##----------------------------------------------------------
    ## Compute curvHDR polygon gates.

    polygongate = curvHDRfilter(W, HDRlevel = hdrLevel)
    numGates    = length(polygongate$polys)

    ##----------------------------------------------------------
    ## Loop over each gate, determine whether each data point
    ## is inside or outside each gate.
    ## The number of possible classes is equal to the number of
    ## gates plus one, to account for the points not falling
    ## within any of the gates.

    numClasses = numGates + 1
    nRows      = nrow(inputData)
    tempClass  = matrix(rep(FALSE,numGates*nRows),nrow=nRows)
    meanXY     = matrix(rep(0,numGates*2),ncol=2)
    for (whichGate in 1:numGates) {
        ##------------------------------------------------------
        # Get the polygon represented by the coordinates of the vertices

        polygonData <- cbind(polygongate$polys[[whichGate]]$x,
                                polygongate$polys[[whichGate]]$y)

        ##------------------------------------------------------
        # Call the function to see which data points are inside each
        # polygonal gate/cluster

        tempClass[,whichGate] <-
            as.logical(flowCore:::inpolygon(W,polygonData))

        ##------------------------------------------------------
        ## Compute the mean XY coordinate for the current gate.
        ## We'll use this to resolve cases where a data point
        ## falls within more than one gate.

        meanXY[whichGate,1] = mean(polygongate$polys[[whichGate]]$x)
        meanXY[whichGate,2] = mean(polygongate$polys[[whichGate]]$y)
    }
    rm(polygonData,W)

    ##----------------------------------------------------------
    ## At this point, tempClass[i,j] contains TRUE if the i-th
    ## data point fell within the j-th gate, FALSE otherwise.
    ## Perform "hard classification": determine classification
    ## for each data point. If a data point did not fall within
    ## any gate, it is assigned a classification of '0'.
    ## If a data point falls within more than one gate,
    ## it will be assigned a class of -1.

    singleClass = apply(tempClass, 1,
                        ClassifyNonoverlapRow, curvHdrObj = polygongate)
    rm(polygongate)

    ##----------------------------------------------------------
    ## Resolve classifications where the data point fell within
    ## more than one gate.

    myDataFrame  = data.frame(inputData, theClass=singleClass)
    rm(inputData)
    resolveClass = apply(myDataFrame, 1,
                        ResolveOverlaps , clusterMeans = meanXY)
    rm(myDataFrame,meanXY)

    ##----------------------------------------------------------
    ## Set values in 'singleClass' where there were overlaps to 0.
    ## Summing the result with 'resolveClass' gives the final
    ## classification.

    whereMultiClass              = ( singleClass == -1 )
    singleClass[whereMultiClass] = 0
    finalClass                   = singleClass + resolveClass
    rm(whereMultiClass,singleClass,resolveClass)
    return(finalClass)
}

#########################################################
## Function to put (absolute) correlations on the upper
## panels of the scatter plot matrix, with size proportional
## to the correlations.
#########################################################
panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y))
    txt <- format(c(r, 0.123456789), digits=digits)[1]
    txt <- paste(prefix, txt, sep="")
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex.cor * r)
}

#########################################################
## Function to make a small empty file so we can tell
## how far the job is (for noninteractive job, especially
## after logging off having used nohup).
#########################################################
makeEmptyFile <- function(dirName,fileName,noProgressFiles=TRUE)
{
    if ( ! noProgressFiles ) {
        filePath = file.path(dirName,fileName)
        sysCMD   = sprintf("touch %s",filePath)
        system(sysCMD)
    }
}

#########################################################
## Function to compute unsupervised clustering using curvHDR.
##
## This function returns TRUE if it gets to the end with no APPARENT
## problems, otherwise it returns FALSE.  For each sample,
## the NMF result, rank, and hard classifications will be stored
## in a .Rdata file.
##
## inputFolder is the input folder containing the input files.
##
## outputFolder is the folder where output will be written to.
##
## nmfMethod is the NMF algorithm to use.  The default is
## the H-version of Kim and Park's Alternating Least Squares
## method.
##
## numRuns is the number of runs for the actual NMF algorithm,
## passed to the NMF function as argument 'nrun'.
## Typically set to 100 to 200.
## See p. 14 of the NMF vignette PDF file,
## http://cran.r-project.org/web/packages/NMF/vignettes/NMF-vignette.pdf
##
## nmfSeed is passed to the NMF function as argument 'seed'.
## By default, this is set to the arbitrary number '123456',
## but could be set to, e.g., 'ica' or 'nndsvd'.
## See section 2.3 of the NMF vignette PDF file,
## http://cran.r-project.org/web/packages/NMF/vignettes/NMF-vignette.pdf
##
## showScatterPlots controls whether or not scatter plots are generated.
## If you want to run in non-interactive batch mode using nohup and then
## log off, it might be a good idea to turn off scatter plots.
## Otherwise the job seems to crash, I think when R tries to generate
## plots but the interactive windowing session is gone.
## By default, scatter plots are turned OFF.
##
###########################################################

processFlowCAPDataSet <- function(inputFolder, outputFolder, hdrLevel = 0.1,
    nmfMethod="snmf/l", numRuns = 5, nmfSeed = 123456,
    showScatterPlots = FALSE) {

    ##------------------------------------------------------------------
    ## If outputFolder == inputFolder, exit with FALSE result.
    ## Otherwise, we'd overwrite the input data.

    if ( outputFolder == inputFolder ) {
        return(FALSE)
    }

    ##------------------------------------------------------------------
    ## Make list of data files, given the main input folder.
    ## Not sure how Sys.glob will sort the list of files; this may vary
    ## from system to system.  So, explicitly sort the list alphabetically.
    ## (If the file list is not sorted alphabetically, it'll screw up the
    ## clustering labels!)

    fileList = sort(Sys.glob(file.path(inputFolder,"*.csv")))
    numFiles = length(fileList)

    ##------------------------------------------------------------------
    ## If the output folder does not exist, try to create it.

    if ( ! file.exists(outputFolder) ) {

        ##--------------------------------------------------------------
        ## Try to create the output folder.
        ## If we can't, exit the function -- we cannot proceed!

        if ( ! dir.create(outputFolder,recursive=TRUE) ) {
            return(FALSE)
        }

    }; ## End IF file.exists(outputFolder)

    ##------------------------------------------------------------------
    ## Loop over all numFiles CSV data sets, produce scatter plot of raw
    ## data, compute NMF, perform hard clustering using W matrix from NMF,
    ## then produce scatter plot of clustering results from NMF.

    for (i in 1:numFiles) {

        ##--------------------------------------------------------------
        ## Load CSV data.

        csvFileName = basename(fileList[i])
        flowCytData = read.csv(fileList[i])
        dataDim     = dim(flowCytData)
        nCols       = dataDim[2]

        ##--------------------------------------------------------------
        ## Show scatter plot of raw data.
        ## This produces a smoothed color density representation of the
        ## scatterplot, obtained through a kernel density estimate.
        ## Also, this produces scatter plot matrix. If separate 2D scatter plots
        ## are preferred, we can modify this.
        ## If the PNG file already exists, we can skip this.

        if ( showScatterPlots ) {

            png1Name = sprintf("%03d.png",i)
            png1Path = file.path(outputFolder,png1Name)

            if ( ! file.exists(png1Path) ) {

                title = sprintf("Scatter Plot of Raw Data for Sample #%d",i)
                pairs(flowCytData,main=title,
                    lower.panel=function(...) {par(new=TRUE);smoothScatter(...)},
                    upper.panel=panel.cor)
                png1Root = sprintf("%03d",i)
                savepng(png1Root,dir=outputFolder,width=640,asp=1)

            }; ## End IF file.exists(png1Path)

        }; ## End IF showScatterPlots

        ##--------------------------------------------------------------
        ## If a CSV file containing load pre-computed classifications
        ## for the i-th sample does not exist, load it into R.
        ## Otherwise, process the i-th sample.
        
        hardClassFileName = sprintf("%03d.csv",i)
        hardClassFilePath = file.path(outputFolder,hardClassFileName)

        if ( file.exists(hardClassFilePath) ) {

            ##----------------------------------------------------------
            ## File exists, so load it.

            hardClass = read.csv(hardClassFilePath)

            ##----------------------------------------------------------
            ## Convert from data frame to vector.
            ## Otherwise, the pairs() function below complains.

            hardClass = hardClass[1,]

        } else {

            ##----------------------------------------------------------
            ## File doesn't exist, so perform curvHDR classification.

            hardClass = ClassifyUsing2DCurvHDR(flowCytData)

            ##----------------------------------------------------------
            ## Save the hard classifications in CSV format
            ## (following the flowCAP example).
            ## Note: write.csv ignores "col.names=FALSE", so this is omitted.

            write.csv(hardClass,row.names=FALSE,file=hardClassFilePath)

        }; ## End IF file.exists(hardClassFilePath)

        ##--------------------------------------------------------------
        ## Show scatter plot of classifications.
        ## This produces scatter plot matrix with Lowess curve and
        ## plotting symbols showing the cluster each data point belongs to.
        ## If separate 2D scatter plots are preferred, we can modify this.
        ## If the PNG file already exists, we can skip this.

        if ( showScatterPlots ) {

            png2Name = sprintf("%03d_nmf.png",i)
            png2Path = file.path(outputFolder,png2Name)

            if ( ! file.exists(png2Path) ) {

                title = sprintf("Classifications for Sample #%d",i)
                mycols <- brewer.pal(8,"Accent")[c(6,1,5)]
                pairs(flowCytData,main=title,
                    col=mycols[hardClass],
                    pch=as.character(hardClass),
                    lower.panel=panel.smooth,upper.panel=panel.cor)
                png2Root = paste(sprintf("%03d",i),"nmf",sep="_")
                savepng(png2Root,dir=outputFolder,width=640,asp=1)

            }; ## End IF file.exists(png2Path)

        }; ## End IF showScatterPlots
        rm(flowCytData,hardClass)

    }; ## End FOR i loop over input files

    return(TRUE)

}; ## End FUNCTION definition of computeNmfClusters

#########################################################
#########################################################
## Functions were defined above.  Actual execution is below.
#########################################################
#########################################################

##----------------------------------------------------------------------
## Accept input parameters from the shell script.

if ( TRUE ) {
    args = ( commandArgs(TRUE) )
    print(args)

    if ( ( length(args) < 2 ) || ( length(args) > 3 ) ) {

        stop("Need input and output folders, and optionally the desired number of clusters.")

    } else {

        ##------------------------------------------------------------------
        ## Obtain input and output folders from the command line arguments.

        InputFolder  = args[[1]]
        OutputFolder = args[[2]]

        ##------------------------------------------------------------------
        ## Check whether desired number of clusters was given.

        if ( length(args) < 3 ) {

            ##--------------------------------------------------------------
            ## Desired number of clusters was NOT given.

            print("Number of populations NOT pre-identified, will perform internal estimates")
            n = -1

        } else {

            ##--------------------------------------------------------------
            ## Desired number of clusters was given.

            n = as.numeric(args[[3]])

        }; ## End IF length(args)

    }; ## End IF length(args)
}

##----------------------------------------------------------------------
## Load libraries: geneplotter, NMF, and RColorBrewer.

# .libPaths("C:/Users/Preset/Documents/georgetown/year1/sra2")
library(geneplotter)
library(NMF)
library(RColorBrewer)
library(curvHDR)

# libLIST = list("geneplotter","NMF","RColorBrewer","mvtnorm","ks","rpanel",
#     "akima","locfit","ash","hdrcde","geometry","feature","robustbase",
#     "pcaPP","rrcov","flowCore","curvHDR")
# libLOC = "/home/jmm97/lib/lib64/R"
# for (i in 1:length(libLIST)) {
#     theLib = libLIST[[i]]
#     library(package=theLib,lib.loc=libLOC)
# }

##----------------------------------------------------------------------
## Invoke NMF clustering function.
##
## Ideally, we'd set numRuns to 100 - 200 (section 2.4 of the vignette)
## http://cran.r-project.org/web/packages/NMF/vignettes/NMF-vignette.pdf)
## But that can take a long time; for now, set numRuns to a lower number.
##
## nmfSeed: nndsvd, 12345, 1234, 3756, 3237, 1498
# processFlowCAPDataSet(InputFolder, OutputFolder,
#     nmfMethod = "brunet", numRuns = 1, nmfSeed = "nndsvd",
#     showScatterPlots = FALSE)

dataSets = c("CFSE","GvHD","Lymph","NDD","StemCell")
for (i in 1:length(dataSets)) {
    InputFolder  = file.path("H:/DATA/Documents and Settings/Joe/Papers/GeneralStatistics/FlowCap/csv",
        dataSets[i],"CSV")
    OutputFolder = file.path("H:/DATA/Documents and Settings/Joe/Papers/GeneralStatistics/FlowCap/csv/Challenge2",
        dataSets[i])
    processFlowCAPDataSet(InputFolder, OutputFolder,
        nmfMethod = "brunet", numRuns = 1, nmfSeed = "nndsvd",
        showScatterPlots = TRUE)
}

