########################################################################
## Function to put (absolute) correlations on the upper
## panels of the scatter plot matrix, with size proportional
## to the correlations.
########################################################################
panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...) {
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y))
    txt <- format(c(r, 0.123456789), digits=digits)[1]
    txt <- paste(prefix, txt, sep="")
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex.cor * r)
}

########################################################################
## Function to make a small empty file so we can tell
## how far the job is (for noninteractive job, especially
## after logging off having used nohup).
########################################################################
makeEmptyFile <- function(dirName,fileName,noProgressFiles=TRUE) {
    if ( ! noProgressFiles ) {
        filePath = file.path(dirName,fileName)
        sysCMD   = sprintf("touch %s",filePath)
        system(sysCMD)
    }
}

########################################################################
## Function to compute unsupervised clustering using NMF.
##
## This function returns TRUE if it gets to the end with no APPARENT
## problems, otherwise it returns FALSE.
##
## inputFolder is the input folder containing the input files.
##
## outputFolder is the folder where output will be written to.
##
## numClassFile is a CSV file containing the expected number of
## populations within each input CSV sample file.  The number of
## entries in this file MUST be the same as the number of input
## CSV files in the inputFolder.
##
## nmfMethod is the NMF algorithm to use, e.g. "brunet" or "snmf/l".
##
## numRuns is the number of runs for the actual NMF algorithm,
## passed to the NMF function as argument 'nrun'.
## Typically set to 100 to 200.
## See p. 14 of the NMF vignette PDF file,
## http://cran.r-project.org/web/packages/NMF/vignettes/NMF-vignette.pdf
##
## normalizeHcolumns controls whether or not the variance of the columns
## of H should be accounted for when performing classification using
## the rows of W.  It may be desirable to normalize the columns of H
## to some common value (e.g. 1.0) before using W to perform classification.
##
## nmfSeed is passed to the NMF function as argument 'seed'.
## This could be set to an  arbitrary number such as '123456',
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
########################################################################
processFlowCAPDataSet <- function(inputFolder, outputFolder,
    numClassFile, alphaVal = 0.05, numRuns = 50, kSteps = 10,
    showScatterPlots=FALSE) {

    ##------------------------------------------------------------------
    ## If outputFolder == inputFolder, exit with FALSE result.
    ## Otherwise, we'd overwrite the input data.

    if ( outputFolder == inputFolder ) {
        print("Input and output folders cannot be the same!")
        return(FALSE)
    }

    ##------------------------------------------------------------------
    ## Load the file containing the number of populations/classes in
    ## each sample file.
    ## Grab only the 2nd column of the output of read.csv.

    numClass = read.csv(numClassFile)[,2]

    ##------------------------------------------------------------------
    ## Make list of data files, given the main input folder.
    ## Not sure how Sys.glob will sort the list of files; this may vary
    ## from system to system.  So, explicitly sort the list alphabetically.
    ## (If the file list is not sorted alphabetically, it'll screw up the
    ## clustering labels!)
    ## Make sure that the number of files is the same as the number of
    ## values in numClass.

    fileList = sort(Sys.glob(file.path(inputFolder,"*.csv")))
    numFiles = length(fileList)

    if ( numFiles != length(numClass) ) {
        print(sprintf("Number of rows in %s not equal to number of files in %s!",
            numClassFile,inputFolder))
        return(FALSE)
    }

    ##------------------------------------------------------------------
    ## If the output folder does not exist, try to create it.

    if ( ! file.exists(outputFolder) ) {

        ##--------------------------------------------------------------
        ## Try to create the output folder.
        ## If we can't, exit the function -- we cannot proceed!

        if ( ! dir.create(outputFolder,recursive=TRUE) ) {
            print(sprintf("Cannot create %s!",outputFolder))
            return(FALSE)
        }

    }; ## End IF file.exists(outputFolder)

    ##------------------------------------------------------------------
    ## Loop over all numFiles CSV data sets, process them if necessary.

    for (i in 1:numFiles) {

        ##--------------------------------------------------------------
        ## If a CSV file containing load output classifications for the
        ## i-th sample already exists, load it into R, and make a
        ## scatterplot if desired.
        ## Otherwise, process the i-th sample and save the result to a
        ## CSV file.

        hardClassFileName = sprintf("%03d.csv",i)
        hardClassFilePath = file.path(outputFolder,hardClassFileName)

        if ( file.exists(hardClassFilePath) ) {

            ##----------------------------------------------------------
            ## Show scatter plot of classifications if desired.

            if ( showScatterPlots ) {

                ##------------------------------------------------------
                ## Make scatter plot if it doesn't exist on disk already.

                png2Name = sprintf("%03d_nmf.png",i)
                png2Path = file.path(outputFolder,png2Name)

                if ( ! file.exists(png2Path) ) {

                    ##--------------------------------------------------
                    ## Load pre-existing output file.

                    hardClass = read.csv(hardClassFilePath)

                    ##--------------------------------------------------
                    ## Convert from data frame to vector.
                    ## Otherwise, the pairs() function below complains.

                    hardClass = hardClass[1,]

                    ##------------------------------------------------------
                    ## Load plotting libraries.

                    library(geneplotter)
                    library(RColorBrewer)

                    ##--------------------------------------------------
                    ## Make scatter plot, save to disk.
                    ## This produces scatter plot matrix with Lowess curve and
                    ## plotting symbols showing the cluster each data point belongs to.
                    ## If separate 2D scatter plots are preferred, we can modify this.

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

        ##--------------------------------------------------------------
        ## Otherwise, if the output CSV file for the i-th input CSV file
        ## doesn't exist yet, process the i-th sample, compute
        ## classifications, and save the result to an output CSV file.

        } else {

            ##----------------------------------------------------------
            ## Load input flow cytometry data.

            flowCytData = read.csv(fileList[i])

            ##----------------------------------------------------------
            ## Remove the FS and SS variables (first two columns),
            ## because the manual analysis didn't use these variables.

            nCols = ncol(flowCytData)
            flowCytData = flowCytData[,3:nCols]

            ##----------------------------------------------------------
            ## Make scatter plot of raw data if desired.

            if ( showScatterPlots ) {

                png1Name = sprintf("%03d.png",i)
                png1Path = file.path(outputFolder,png1Name)

                ##----------------------------------------------------------
                ## Make scatter plot if it doesn't exist on disk already.
                ## This produces a smoothed color density representation of the
                ## scatterplot, obtained through a kernel density estimate.
                ## Also, this produces scatter plot matrix.
                ## If separate 2D scatter plots are preferred, we can modify this.

                if ( ! file.exists(png1Path) ) {

                    ##------------------------------------------------------
                    ## Load plotting libraries, then plot.

                    library(geneplotter)
                    library(RColorBrewer)

                    title = sprintf("Scatter Plot of Raw Data for Sample #%d",i)
                    pairs(flowCytData,main=title,
                        lower.panel=function(...) {par(new=TRUE);smoothScatter(...)},
                        upper.panel=panel.cor)
                    png1Root = sprintf("%03d",i)
                    savepng(png1Root,dir=outputFolder,width=640,asp=1)

                }; ## End IF file.exists(png1Path)

            }; ## End IF showScatterPlots

            ##----------------------------------------------------------
            ## Perform NMF classification.
            ## Remove intermediate results to conserve memory.

            tclustResult = tclust(flowCytData, k=numClass[i],
                                    alphaVal, numRuns, kSteps, restr = "sigma")
            rm(flowCytData)
            hardClass = tclustResult$assign
            rm(tclustResult)

            ##----------------------------------------------------------
            ## Save the hard classifications in CSV format
            ## (following the flowCAP example).
            ## Note: write.csv ignores "col.names=FALSE", so this is omitted.
            ## Remove intermediate results to conserve memory.

            write.csv(hardClass,row.names=FALSE,file=hardClassFilePath)
            rm(hardClass)

        }; ## End IF file.exists(hardClassFilePath)

    }; ## End FOR i loop over input files

    return(TRUE)

}; ## End FUNCTION definition of processFlowCAPDataSet 

########################################################################
########################################################################
## Function definitions were above.  Actual execution is below.
########################################################################
########################################################################

##----------------------------------------------------------------------
## Load tclust library.

# .libPaths("C:/Users/Preset/Documents/georgetown/year1/sra2")
library(tclust)

##----------------------------------------------------------------------
## The following IF statement selects between an interacive R session and
## non-interactive BATCH mode (perhaps invoked via a Bourne shell script).
##
## If you want to run this script in an INTERACTIVE R session,
## set runInBatchModeWithShellScripts to FALSE.
##
## If you want to run this script in NON-INTERACTIVE BATCH MODE,
## set runInBatchModeWithShellScripts to TRUE.

runInBatchModeWithShellScripts = TRUE

if ( runInBatchModeWithShellScripts ) {
    ## IF clause for batch (non-interactive) behavior

    args = ( commandArgs(TRUE) )

    if ( length(args) != 6 ) {

        print("Need input and output folders, CSV file containing number of expected populations, alpha value,")
        print("number of iterations, and Ksteps value.")
        stop("Execution halting...")

    }; ## End IF length(args)

    ##------------------------------------------------------------------
    ## Obtain parameters from the command line arguments.
    ## 'varianceNorm' will control whether or not the variance of the
    ## columns of H will in effect be normalized to 1.0.

    inputFolder  = args[[1]]
    outputFolder = args[[2]]
    numPopFile   = args[[3]]
    alphaVal     = as.numeric(args[[4]])
    nRuns        = as.integer(args[[5]])
    kSteps       = as.integer(args[[6]])

    ##------------------------------------------------------------------
    ## Process the flowCAP data set.

    processFlowCAPDataSet(
        inputFolder,
        outputFolder,
        numClassFile      = numPopFile,
        alphaVal          = alphaVal,
        numRuns           = nRuns,
        kSteps            = kSteps,
        showScatterPlots  = FALSE
    )

} else {
    ## ELSE clause for interactive behavior

    ##------------------------------------------------------------------
    ## Set up some parameters.
    ## 'varianceNorm' will control whether or not the variance of the
    ## columns of H will in effect be normalized to 1.0.

    showPlots     = FALSE

    aList = c(0.01, 0.05, 0.1)
    rList = c( 50, 100, 200 )
    kList = c( 10, 20 )

    topInputDir  = "H:/DATA/Documents and Settings/Joe/Papers/GeneralStatistics/FlowCap/csv"
    topOutputDir = "H:/DATA/Documents and Settings/Joe/Papers/GeneralStatistics/FlowCap/csv/Challenge3"
    nccDir       = "H:/DATA/Documents and Settings/Joe/Papers/GeneralStatistics/FlowCap/NCC"

    for (u in 1:3) {
        alphaVal = aList[u]
        for (v in 1:3) {
            nRuns = rList[v]
            for (w in 1:2) {
                kSteps = kList[w]
                    outSubDirName = sprintf("tclust_alpha_%g_Runs_%d_kSteps_%d",
                            alphaVal, nRuns, kSteps)

                ##-----------------------------------------
                ## GvHD data set

                dataSet      = "GvHD"
                numPopFile   = file.path(nccDir,"GvHD.csv")
                inputFolder  = file.path(topInputDir,dataSet,"CSV")
                outputFolder = file.path(topOutputDir,outSubDirName,dataSet)

                processFlowCAPDataSet(
                    inputFolder,
                    outputFolder,
                    numClassFile      = numPopFile,
                    alphaVal          = alphaVal,
                    numRuns           = nRuns,
                    kSteps            = kSteps,
                    showScatterPlots  = showPlots
                )

                ##-----------------------------------------
                ## Lymph/DLBCL data set

                dataSet      = "Lymph"
                numPopFile   = file.path(nccDir,"DLBCL.csv")
                inputFolder  = file.path(topInputDir,dataSet,"CSV")
                outputFolder = file.path(topOutputDir,outSubDirName,dataSet)

                processFlowCAPDataSet(
                    inputFolder,
                    outputFolder,
                    numClassFile      = numPopFile,
                    alphaVal          = alphaVal,
                    numRuns           = nRuns,
                    kSteps            = kSteps,
                    showScatterPlots  = showPlots
                )

                ##-----------------------------------------
                ## StemCell data set

                dataSet      = "StemCell"
                numPopFile   = file.path(nccDir,"StemCell.csv")
                inputFolder  = file.path(topInputDir,dataSet,"CSV")
                outputFolder = file.path(topOutputDir,outSubDirName,dataSet)

                processFlowCAPDataSet(
                    inputFolder,
                    outputFolder,
                    numClassFile      = numPopFile,
                    alphaVal          = alphaVal,
                    numRuns           = nRuns,
                    kSteps            = kSteps,
                    showScatterPlots  = showPlots
                )
            }
        }
    }

}; ## End IF runInBatchModeWithShellScripts (to toggle on/off batch behavior)
