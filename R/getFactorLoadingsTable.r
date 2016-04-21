
getScreePlot <- function(data) {

    #--- Determine Number of Factors to Extract
    ev = eigen(cor(data)) # get eigenvalues

    VarianceTable = as.data.frame(ev$values)
    names(VarianceTable) = c("EigenValue")
    VarianceTable$Variance = VarianceTable$EigenValue/sum(VarianceTable$EigenValue)
    VarianceTable$CumVar = cumsum(VarianceTable$Variance)

    # copy table to clipboard...
    write.table(VarianceTable,"clipboard",sep="\t",col.names=NA)
    print("Table written to clipboard")

    #disply scree plot...
    par(pch=20, col="black")
    plot(VarianceTable$EigenValue,type="o")
    abline(a=1,b=0,col = "lightgray",lty="dashed")
}


getFactorLoadingsTable <- function(FAResult){
    # extract loadings
    fTable = as.data.frame(FAResult$loadings[,])

    # get maximum loading factor for each row
    maxFac = apply(abs(fTable),1,which.max)
    maxValue = apply(abs(fTable),1,max) # also store max value

    # sort table by maximum factor...
    fTable = fTable[order(maxFac,1-maxValue),]

    # add uniqueness
    fTable$Uniqueness = 1 - apply(fTable^2,1,sum)

    # and write to clipboard...
    write.table(fTable,"clipboard",sep="\t",col.names=NA)
    print("Factor Loadings table written to clipboad")
}
