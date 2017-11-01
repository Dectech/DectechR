unpackUserVariable <- function(rawText, maxCols = NULL, emptyReplacement = -99, numeric = FALSE, delimiter = "*"){
    #######################################################
    ### Function to extract data from user defined      ###
    ###  ...variables from questback custom questions   ###
    #######################################################

    # Make sure that input object is 1 dimentional...
    if (is.null(dim(rawText)) == FALSE) {
        if (min(dim(rawText)) > 1) {
            stop("object has more than one column. Please enter a single column of data.")
        }
    }

    # For each item...
    thisMatrix <- t(sapply(rawText, FUN = function(x) {
        # split the elements...
        y <- strsplit(x, delimiter, fixed = TRUE)[[1]]
        # if a max number of columns defined, then fill the rest with emptyReplacement...
        if (is.null(maxCols) == FALSE){
            excess = maxCols - length(y)
            y <- c(y, matrix(emptyReplacement, nrow = 1, ncol = excess))
        }
        # if specified, convert to numeric
        if (numeric == TRUE){
            y <- as.numeric(y)
        }
        return(y)
    }))
    # clean the row names...
    rownames(thisMatrix) <- 1:nrow(thisMatrix)

    # check that all rows have same number of items
    rowLengths <- sapply(thisMatrix, length)
    if (length(unique(rowLengths)) > 1) {
        # ...if not give a warning about how to fix it...
        warning(paste0("Not all rows have same number of items. Longest row has ", max(rowLengths) ," items. Try setting maxCols = ", max(rowLengths)))
    }

    return(thisMatrix)
}
