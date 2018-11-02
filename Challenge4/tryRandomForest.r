########################################################################
## Load library.
library(randomForest)

########################################################################
## Input directory containing the .csv files we used for Challenges 2 & 3.
## Here, we're pointing to the one for StemCell.
inputDataDir = "H:/DATA/Documents and Settings/Joe/Papers/GeneralStatistics/FlowCap/csv/StemCell/CSV"

########################################################################
## Input directory containing the new .csv files for Challenge 4.
## Here, we're pointing to the one for StemCell.
goldStandardClassificationDir = "H:/DATA/Documents and Settings/Joe/Papers/GeneralStatistics/FlowCap/csv/Challenge4/CH4/StemCell"

########################################################################
## Output directory, where to write some intermediate results.
## The creation times of files written to this directory can help
## indicate how long certain steps took to run.
outputDir = "H:/DATA/Documents and Settings/Joe/Papers/GeneralStatistics/FlowCap/csv/Challenge4"

##------------------------------------------------------------------
## Compose list of input gold standard files.

classFileList = sort(Sys.glob(file.path(goldStandardClassificationDir ,"*.csv")))
numClassFiles = length(classFileList)

##------------------------------------------------------------------
## Loop over input gold standard files, load corresponding input
## data files.  Cumulate into one response vector and one matrix of
## predictor variables.  After the loop, force 'responseVar' to be
## of type factor.

for (i in 1:numClassFiles) {

    ##--------------------------------------------------------------
    ## Load gold standard classification data.

    classData = read.csv(classFileList[i])

    ##--------------------------------------------------------------
    ## Load input data.

    fileName      = basename(classFileList[i])
    inputFilePath = file.path(inputDataDir,fileName)
    inputData     = read.csv(inputFilePath )

    ##--------------------------------------------------------------
    ## Cumulate response variable and predictor variables.
    ## Remove intermediate results to conserve memory.

    if ( i == 1 ) {
        predictorVars = inputData
        responseVar   = classData[[1]]
    } else {
        predictorVars = rbind(predictorVars,inputData)
        responseVar   = c(responseVar,classData)
    }
    rm(inputData)
    rm(classData)

}; # FOR i in 1:numClassFiles

responseVar = factor(responseVar)

##------------------------------------------------------------------
## Tune randomForest for the optimal mtry parameter.
## If you set doBest to TRUE, tuneRF will run a forest too, but
## in this exploratory study I want to examine tuneRF's output a
## bit more closely.

tuneResult <- tuneRF( x = predictorVars, y = responseVar, plot = FALSE)
mtryOptimIndex =  max.col(-t(tuneResult [,2]))
mtryOptim = tuneResult[mtryOptimIndex,1]
filePath = file.path(outputDir ,"tuneResult.rda")
save(tuneResult , file = filePath)

##------------------------------------------------------------------
## Now run Breiman's random forest algorithm with the optimized mtry
## value.  Again, it is possible to have this function also perform
## classification on a test data set once tuning is finished, but
## in this exploratory study I want to do it in a separate step
## so I can see what's going on.

rfResult = randomForest(x = predictorVars, y = responseVar,
    ntree = 500, mtry = mtryOptim)
filePath = file.path(outputDir ,"rfResult .rda")
save(rfResult, file = filePath)
