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

    # split the elements...
    split_strings = lapply(rawText, strsplit, split = delimiter, fixed = TRUE)
    # ...strsplit returns a unnecessarily nested list, so need to unlist:
    split_strings = lapply(split_strings, "[[", 1)


    # if maxCols not given, then guess it...
    if (is.null(maxCols) == TRUE) {
        maxCols = max(lengths(split_strings))
        print(paste0("...the max number of items per row is: ", maxCols))
    }


    # For each item...
    thisMatrix <- t(sapply(split_strings, FUN = function(y) {
        # get the number of items, and pad out if necessary...
        excess = maxCols - length(y)
        y <- c(y, matrix(emptyReplacement, nrow = 1, ncol = excess))

        # if specified, convert to numeric
        if (numeric == TRUE) {
            y <- as.numeric(y)
        }
        return(y)
    }))

    # clean the row names...
    rownames(thisMatrix) <- 1:nrow(thisMatrix)

    return(thisMatrix)
}
