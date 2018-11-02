########################################################################
## Function to process Challenge 4 data.
##
## INPUT:
##
## predictorDir - directory containing *.csv files containing predictor
##                variables, e.g. FL1.LOG
##
## responseDir - directory containing *.csv files containing manual
##               classifications, to be used for algorithm tuning
##               (for Challenge 4).
##
## outputDir - directory for output.  Cannot be the same as predictorDir
##             (otherwise results would overwrite input files).
##
## writeIntermediateOutputFiles - Optional Boolean indicating whether you want
##             intermediate output files to be written.  Default = FALSE.
##
## OUTPUT:
##
## This function returns FALSE if there was an error, TRUE otherwise.
##
########################################################################
ProcessChallenge4Data <- function( predictorDir, responseDir, outputDir,
    writeIntermediateOutputFiles = FALSE ) {

#print(sprintf("predictorDir = %s",predictorDir))
#print(sprintf("responseDir  = %s",responseDir))
#print(sprintf("outputDir    = %s",outputDir))
    ##------------------------------------------------------------------
    ## If outputFolder == inputFolder, exit with FALSE result.
    ## Otherwise, we'd overwrite the input data.

    if ( outputDir == predictorDir ) {
        print("Input and output folders cannot be the same!")
        return(FALSE)
    }

    ##------------------------------------------------------------------
    ## If the output folder does not exist, try to create it.

    if ( ! file.exists(outputDir) ) {

        ##--------------------------------------------------------------
        ## Try to create the output folder.
        ## If we can't, exit the function -- we cannot proceed!

        if ( ! dir.create(outputDir,recursive=TRUE) ) {
            print(sprintf("Cannot create %s!",outputDir))
            return(FALSE)
        }
        
    }; ## End IF file.exists(outputDir)

    ##------------------------------------------------------------------
    ## Compose two lists, one of response variable files, the other of
    ## predictor variable files.  I'm not sure whether Sys.glob sorts
    ## the results alphabetically (in some scripting languages the glob
    ## function does NOT sort), so use the sort function to make sure
    ## that results are sorted alphabetically.  The order is important.

    classFileList   = sort(Sys.glob(file.path(responseDir,"*.csv")))
    numClassFiles   = length(classFileList)

    predVarFileList = sort(Sys.glob(file.path(predictorDir,"*.csv")))
    numPredVarFiles = length(predVarFileList)

    ##------------------------------------------------------------------
    ## Loop over the response variable files, load in the data for the
    ## corresponding predictor variables.  This is training/tuning data.
    ## Remove intermediate results to conserve memory.

    for (i in 1:numClassFiles) {
#print(sprintf("Gold Standard Response Variable File #%d\n",i))
        ##--------------------------------------------------------------
        ## Load gold standard classification data.

        responseData = read.csv(classFileList[i])

        ##--------------------------------------------------------------
        ## Load predictor data -- same name, different directory.

        fileName      = basename(classFileList[i])
        predictorPath = file.path(predictorDir,fileName)
        predictorData = read.csv(predictorPath)

        ##--------------------------------------------------------------
        ## Cumulate response variable and predictor variables.
        ## Remove intermediate results to conserve memory.

        if ( i == 1 ) {
            predictorVars = predictorData
            responseVar   = responseData[[1]]
        } else {
            predictorVars = rbind(predictorVars,predictorData)
            responseVar   = c(responseVar,responseData[[1]])
        }
        rm(predictorData)
        rm(responseData)
        
    }; # End FOR i in 1:numClassFiles

    responseVar = factor(responseVar)
    rm(classFileList,numClassFiles)

    ##------------------------------------------------------------------
    ## Randomly split the training data into two.
    ## The first half will be used to TUNE the parameter 'mtry'.
    ## The second half will be used to TRAIN the tree ensemble.
    ## Remove intermediate results to conserve memory.

    numVar         = length(responseVar)
    randomIndices  = sample(numVar)
    lastIndex      = floor(numVar/2)
    halfIndices    = randomIndices[1:lastIndex]
    
    tuneMtryPred   = predictorVars[halfIndices,]
    tuneMtryResp   = responseVar[halfIndices]
    
    trainPredData  = predictorVars[-halfIndices,]
    trainRespData  = responseVar[-halfIndices]

    rm(responseVar,predictorVars,randomIndices,halfIndices,numVar,lastIndex)
    
    ##------------------------------------------------------------------
    ## Tune randomForest for the optimal mtry parameter.
    ## Remove intermediate results to conserve memory.

    tuneResultPath = file.path(outputDir ,"tuneResult.rda")
#print(sprintf("tuneResultPath = %s",tuneResultPath))
    if ( file.exists(tuneResultPath) ) {
        load(tuneResultPath)
    } else {
#print("Optimizing mtry")
        tuneResult     = tuneRF( x = tuneMtryPred, y = tuneMtryResp, trace = FALSE)
        if ( writeIntermediateOutputFiles ) {
            save(tuneResult, file = tuneResultPath)
        }
    }
    mtryOptimIndex =  max.col(-t(tuneResult[,2]))
    mtryOptim      = tuneResult[mtryOptimIndex,1]
    rm(tuneMtryPred,tuneMtryResp)
    rm(mtryOptimIndex,tuneResult)

    ##------------------------------------------------------------------
    ## Now run Breiman's random forest algorithm with the optimized mtry
    ## value.  Use 'try' to catch errors -- randomForest can crash if
    ## there's not enough memory.
    ## Remove intermediate results to conserve memory.

    rfResultPath = file.path(outputDir ,"rfResult.rda")
#print(sprintf("rfResultPath = %s",rfResultPath))
    if ( file.exists(rfResultPath) ) {
        load(rfResultPath)
        rm(predictorVars,responseVar,mtryOptim)
    } else {
#print("Running randomForest")
        rfResult = randomForest(x = trainPredData, y = trainRespData,
                                        ntree = 1000, mtry = mtryOptim)
                                        
        if ( writeIntermediateOutputFiles ) {
            save(rfResult, file = rfResultPath)
        }
    }
    rm(trainPredData,trainRespData,rfResultPath,mtryOptim)

    ##------------------------------------------------------------------
    ## Loop over the predictor variable files, perform classification.
    ## Note that some of these test data sets were used for training.
    ## We'll generate TWO output files for each input sample file.
    ## One will be the hard classifications, stored in files with the
    ## same names as the original input files, e.g. 001.csv.
    ## The other will be probabilities of belonging to a given class,
    ## with names such as Probabilities.001.csv.
    ## Remove intermediate results to conserve memory.

    for (i in 1:numPredVarFiles) {
#print(sprintf("Predictor File #%d\n",i))

        ##--------------------------------------------------------------
        ## Determine output file names.

        outputProbFile = sprintf("Probabilities.%03d.csv",i)
        outputProbPath = file.path(outputDir,outputProbFile)

        outputClassFile = basename(predVarFileList[i])
        outputClassPath = file.path(outputDir,outputClassFile)

        ##--------------------------------------------------------------
        ## If the output files don't exist, create them.

        if ( ! file.exists(outputProbPath) ) {
#print(sprintf("outputProbPath = %s",outputProbPath))

            ##----------------------------------------------------------
            ## Load predictor variables.

            predictorData = read.csv(predVarFileList[i])

            ##----------------------------------------------------------
            ## Perform classification.  With 'type' set to "votes", the
            ## proportion of trees selecting a given classification can be
            ## interpreted as a probability (probability as a sort of
            ## measure of belief, cf. Bayesian stats).
            ## Remove intermediate results to conserve memory.

            predictResult = predict(rfResult,
                                    newdata = predictorData, type = "vote")
            write.csv(predictResult, file = outputProbPath)
            rm(predictorData)

            ##----------------------------------------------------------
            ## For each sample, use the votes to perform "hard"
            ## classification -- majority vote wins.  The name of the output
            ## file will be the same as the original input file of predictor
            ## variables.
            ## Remove intermediate results to conserve memory.

            classResult = max.col(predictResult)
            write.csv(classResult, file = outputClassPath)
            rm(predictResult,classResult)

        }; # End IF ! file.exists(outputProbPath)

    }; # End FOR i in 1:numPredVarFiles

    ##------------------------------------------------------------------
    ## Remove intermediate results to conserve memory.

    rm(outputProbFile,outputProbPath,outputClassFile,outputClassPath)
    rm(rfResult,predVarFileList,numPredVarFiles)
    return(TRUE)

}; # End definition of ProcessChallenge4Data

########################################################################
########################################################################
## Function definitions were above.  Actual execution is below.
########################################################################
########################################################################

##----------------------------------------------------------------------
## Load randomForest library.

.libPaths("/home/jmm97/lib/lib64/R")
library(randomForest)

##----------------------------------------------------------------------
## The following IF statement selects between an interactive R session and
## non-interactive BATCH mode (perhaps invoked via a Bourne shell script).

if ( length(commandArgs(TRUE)) == 0 ) {

    ##------------------------------------------------------------------
    ## Interactive R session.  Define main directories and data sets.

    options(show.error.messages=F,warn=-1)
    mainPredictorDir = "/home/jmm97/flowCAP/csv"
    mainResponseDir  = "/home/jmm97/flowCAP/Challenge4/CH4"
    mainOutputDir    = "/home/jmm97/flowCAP/Challenge4/Results080410"
    
    dataSetList      = c("NDD","CFSE","GvHD","Lymph","StemCell")
    numDataSets      = length(dataSetList)

    ##------------------------------------------------------------------
    ## Loop over the five data sets.

    for (i in 1:numDataSets) {

        ##--------------------------------------------------------------
        ## Splice together the three directories for the data set.

        predictorDir = file.path(mainPredictorDir,dataSetList[i],"CSV")
        outputDir    = file.path(mainOutputDir,   dataSetList[i])
        if ( dataSetList[i] == "Lymph" ) {
            responseDir = file.path(mainResponseDir, "DLBCL")
        } else {
            responseDir = file.path(mainResponseDir, dataSetList[i])
        }

        ##--------------------------------------------------------------
        ## Process the flowCAP data set.

        pc4dResult = ProcessChallenge4Data(predictorDir,responseDir,outputDir,FALSE)
        if ( ! pc4dResult ) {
            stop("Error running ProcessChallenge4Data in interactive mode!")
        }
        
    }; # End FOR i in 1:numDataSets

} else {

    ##------------------------------------------------------------------
    ## Non-interactive Batch mode.
    
    if ( length(args) != 3 ) {

        print("Need input folder containing .csv files of predictor variables,")
        print("input folder containing .csv files of response variables,")
        print("and desired name for output folder.")
        stop("Execution halting...")

    }; ## End IF length(args)

    ##------------------------------------------------------------------
    ## Obtain parameters from the command line arguments.

    predictorDir = args[[1]]
    responseDir  = args[[2]]
    outputDir    = args[[3]]

    ##------------------------------------------------------------------
    ## In batch mode, suppress warning and error messages.

    options(show.error.messages=F,warn=-1)

    ##------------------------------------------------------------------
    ## Process the flowCAP data set.

    pc4dResult = ProcessChallenge4Data(predictorDir,responseDir,outputDir)
    if ( ! pc4dResult ) {
        stop("Error running ProcessChallenge4Data in batch mode!")
    }
}; ## End IF length(commandArgs(TRUE))

#pc4dResult   = ProcessChallenge4Data(predictorDir,responseDir,outputDir )
