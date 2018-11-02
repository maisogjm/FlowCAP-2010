########################################################################
## Function to fix a zero row.
########################################################################
FixZeroRow <- function(theRow,numVals,noiseMag) {
    ##-----------------------------------------
    ## If all entries are zero, then fix the row.

    if ( all(theRow==0) ) {
        fixedRow = noiseMag*runif(numVals)
        return(fixedRow)
    } else {
        return(theRow)
    }
}

q     = matrix(runif(20),nrow=5)
q[3,] = rep(0,4)
q     = t(apply(q, 1, FixZeroRow , numVals = 4, noiseMag = 0.0001))
