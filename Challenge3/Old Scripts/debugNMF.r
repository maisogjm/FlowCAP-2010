# Load NMF library and data.
library(NMF)
inputData = read.csv('D:/Users/Joe/flowCAP/GvHD/CSV/007.csv')
nCols     = ncol(inputData)
inputData = inputData[,3:nCols]

# Normalize variance.
inputSD   = sd(inputData)
Dinv      = diag( 1 / inputSD )
inputData = as.matrix(inputData) %*% Dinv
rm(inputSD,Dinv)

    ##----------------------------------------------------------
    ## Some rows of the data may be all zero, which NMF can't handle.
    ## Add a little bit of noise.
    ## Remove intermediate results to conserve memory.

    nCols     = ncol(inputData)
    nRows     = nrow(inputData)
    totNum    = nCols * nRows
    noise     = matrix(runif(totNum),nrow=nRows)
    inputData = inputData + ( 0.0001 * noise )
    rm(nCols,nRows,totNum,noise)

# Compute NMF.
# 001.csva : numClass = 4
# 007.csva : numClass = 5
numClass  = 5
nmfMethod = "brunet"
nmfSeed   = 12345
numRuns   = 1

nmfResults = nmf(inputData, rank = numClass,
        method=nmfMethod, nrun=numRuns, seed=nmfSeed)
