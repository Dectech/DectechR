cc <- function(data, destination = NA, includeRowNames = FALSE, nestedOrderOutToIn = TRUE) {
    ####################################################
    ### Fucntion to write an object to the clipboard ###
    ###  ...making allowance for nested tables       ###
    ####################################################

    #--(1) If file name given, write to file, otherwise write to clipboard...
    if (!is.na(destination)) {

        # if file name given then write to file...
        write.table(data, destination,sep = "\t", row.names = FALSE)

    } else {
        #---(2) will use the "writeClipboard" function, however...
        # ...this function only takes strings, so need to format data

        # if desired, save the row names...
        if (includeRowNames == TRUE) {
            originalRowNames <- rownames(data)
        }


        #---(3) we will treat nested tables differently, so check status..
        isNestedTable <- FALSE
        if (class(data) == "table") {
            # only care if there is more than one dimension
            if (length(dim(data)) > 1) {
                isNestedTable <- TRUE
            }
        }

        #---(4) convert all objects to dataframes if not already
        if (class(data) != "data.frame") {
            data <- as.data.frame(data)
        }

        #---(5) for multi dimensional nested tables...
        #  ...want to reshape into usable format
        if (isNestedTable) {
            nvars <- length(data)

            # create an extra header row, so show name of variable that will span columns
            extraRow <- paste(array("\t",nvars - 2), collapse = "")
            extraRow <- paste0(extraRow, names(data)[nvars - 1])


            #-- reshape so that last variable is split into columns...
            # first get some details about current shape...
            numCols <- ncol(data)
            myValues <- names(data)[numCols]
            myColumnVar <- names(data)[numCols - 1]
            myColumnVarNames <- as.character(unique(data[, myColumnVar]))
            myOtherVars <- names(data)[-c(numCols - 1,numCols)]

            # now reshape so "myColumnVar" is switched to wide format...
            data <- reshape(data, v.names = myValues,timevar = myColumnVar,
                         idvar = myOtherVars, direction = "wide")

            if (nestedOrderOutToIn == TRUE) {
                for (v in myOtherVars[length(myOtherVars):1]) {
                    data = data[order(data[,v]),]
                }

            }
            # ...and use orginal levels for column headings
            names(data) <- c(myOtherVars, myColumnVarNames)

        }

        #---(6) get column headings for table...

        colHeadings <- paste(names(data),collapse = "\t")
        if (isNestedTable) {
            colHeadings <- append(extraRow,colHeadings)
        }

        #---(7) Go through every row, make it a string sep. by tabs..
        outputVector <- apply(data, 1, paste, collapse = "\t")
        outputVector <- append(colHeadings, outputVector)

        #--- if desired, include original row names....
        if ((includeRowNames == TRUE) & (isNestedTable == FALSE)) {
            NextraRows <- length(outputVector) - length(originalRowNames)
            originalRowNames <- append(rep("", NextraRows), originalRowNames)
            outputVector <- paste0(originalRowNames, "\t", outputVector)
        }



        #---(8) then write to clipboard
        writeClipboard(outputVector)
    }
}

cc2 <- cc
