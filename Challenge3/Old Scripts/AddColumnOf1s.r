########################################################################
## Function to add a column of 1's to a matrix.
########################################################################
AddColumnOf1s <- function(x) {
    nRows          = nrow(x)
    nCols          = ncol(x)
    totN1          = nRows * ( nCols + 1 )
    newX           = matrix(rep(1,totN1),nrow=nRows,ncol=nCols+1)
    newX[,1:nCols] = x
    return(newX)
}
