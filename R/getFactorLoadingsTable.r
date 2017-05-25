
getScreePlot <- function(data, toClipboard = TRUE) {

    # Determine Number of Factors to Extract
    eigen_values <- eigen(cor(data)) # get eigenvalues

    variance_table <- as.data.frame(eigen_values$values)
    names(variance_table) <- c("EigenValue")
    variance_table$Variance <- variance_table$EigenValue/sum(variance_table$EigenValue)
    variance_table$CumulativeVariance <- cumsum(variance_table$Variance)

    if (toClipboard == TRUE) {
        # copy table to clipboard...
        write.table(variance_table, "clipboard", sep = "\t", col.names = NA)
        print("Table written to clipboard")
    } else {
        print(variance_table)
    }

    # disply scree plot...
    par(pch = 20, col = "black")
    plot(variance_table$EigenValue, type = "o")
    abline(a = 1, b = 0, col = "lightgray",lty = "dashed")
}


getFactorLoadingsTable <- function(FAResult, toClipboard = TRUE){
    # extract loadings
    factor_loadings <- as.data.frame(FAResult$loadings[,])

    # get maximum loading factor for each row
    max_loading_factor <- apply(abs(factor_loadings),1,which.max)
    max_value <- apply(abs(factor_loadings),1,max) # also store max value

    # sort table by maximum factor...
    factor_loadings <- factor_loadings[order(max_loading_factor,1-max_value),]

    # add uniqueness
    factor_loadings$Uniqueness <- FAResult$uniquenesses

    if (toClipboard == TRUE) {
        # and write to clipboard...
        write.table(factor_loadings, "clipboard", sep = "\t", col.names = NA)
        print("Factor Loadings table written to clipboad")
    } else {
        print(factor_loadings)
    }

}
