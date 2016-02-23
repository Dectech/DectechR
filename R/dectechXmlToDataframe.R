dectechXmlToDataframe <- function(filePath, removeIncompletes = TRUE, saveLabels = TRUE, dropTimeStamps = TRUE) {
    ################################################################################################
    ##  Function to convert an XML file exported from GlobalPark/Questback into an R data frame   ##
    ################################################################################################

    startTime <- proc.time()
    # make sure file exists, before starting...
    if (file.exists(filePath) == FALSE) {
        stop("-- Couldn't find that file! Make sure you have the correct path and file name")
    }


    # ----- (1) output for user   -----------------------------------
    print("------- GlobalPark XML to dataframe ----------------")
    print("-   With the following options:                    -")
    if (removeIncompletes) {
        print("-       Remove incompetes: Yes                     -")
    } else {
        print("-       Remove incompetes: No                      -")
    }

    if (saveLabels) {
        print("-       Save labels as an attribute: Yes           -")
    } else {
        print("-       Save labels as an attribute: No            -")
    }

    if (dropTimeStamps) {
        print("-       Drop GP time stamp variables: Yes          -")
    } else {
        print("-       Drop GP time stamp variables: No           -")
    }
    print("----------------------------------------------------")



    # ----- (2) load xml file and get variable names/labels/type  --------

    data <- XML::xmlTreeParse(filePath, useInternalNodes = TRUE,
                              options = (XML::HUGE) | (XML::RECOVER), encoding = "UTF-8")


    print("---> Extracting variable labels...")
    vars <- XML::getNodeSet(data, "//variable[@name]")
    varNames <- sapply(vars, function(x) XML::xmlGetAttr(x, "name"))
    varType <- sapply(vars, function(x) XML::xmlGetAttr(x, "type"))
    varLabels <- XML::xpathSApply(data,"//variable[@name]/label[text()]",XML::xmlValue)


    # some nodes also have a "code" that define the levels of the variable...
    varsWithCode <- XML::getNodeSet(data,"//variable/codes/parent::*")
    varNamesWithCode <- sapply(varsWithCode, function(x) XML::xmlGetAttr(x, "name"))


    # the fastest way to get variable labels is to extract all of them first
    # ...however this will be one long undifferentiated vector of names

    rawKeyList <- XML::xpathSApply(data,"//variable[@name]/codes/code",XML::xmlGetAttr,"key")
    rawLabelList <- XML::xpathSApply(data,"//variable[@name]/codes/code",XML::xmlValue)
    # ...therefore we will also need an index to link labesl back to their variable...
    # ...this will be vector of the appropriate break points in rawKeyList and rawLabelList
    rawLabelListIndex  <- c(0,cumsum(sapply(varsWithCode, function(x) {
        length(XML::xmlChildren(XML::xmlChildren(x)$codes))
    })))


    #--- (3) convert to data frame....

    if (removeIncompletes == TRUE) {
        print("---> Dropping incompletes...")
        # questback completes are numbered 31 and 32
        mainXMLData <- XML::getNodeSet(data,"//row/dispcode[text()=31 or text()=32]/parent::*")
    } else {
        mainXMLData <- XML::getNodeSet(data,"//row")
    }


    print("---> Getting raw data (this bit can take a few mins!)")

    mainData <- XML::xmlToDataFrame(mainXMLData,homogeneous = T,nodes = varNames,collectNames = F)
    print("  -->time so far (minutes):")
    print((proc.time() - startTime)/60)


    # ----- (4) go through each variable....  ---------------------
    # -----   (a) if it has a code, make it into an R factor...
    # -----   (b) if not, convert to the appropriate type...

    print("---> Matching labels to values...")

    for (v in varNames) {
        if (v %in% varNamesWithCode) { #--- (a) if has a code....


            #--- get the keys and labels for this variable....

            vi <- which(varNamesWithCode == v)
            thisKey <- rawKeyList[(rawLabelListIndex[vi] + 1):rawLabelListIndex[vi + 1]]
            thisLabels <- rawLabelList[(rawLabelListIndex[vi] + 1):rawLabelListIndex[vi + 1]]

            #--- sometimes levels will not be in original order...
            mainData[,v] <- factor(mainData[,v],
                                  levels = sort(as.numeric(levels(mainData[,v]))))

            #--- map labels onto values....
            if (sum(table(thisLabels) > 1) > 0) {
                thisLabels <- paste(thisKey,thisLabels)
            }
            mainData[,v] <- factor(mainData[,v],levels = thisKey,
                                  labels = thisLabels)


        } else {#--- (b) if does not have a code....
            #...then doesn't need to be factor...
            thisVarType <- varType[which(varNames == v)]
            if (thisVarType %in% c("integer","decimal")) {
                mainData[,v] <- as.numeric(as.character(mainData[,v]))
            }else if (thisVarType %in% c("character","blob")) {
                mainData[,v] <- as.character(mainData[,v])
            }
        }
    }

    print("  -->time so far (minutes):")
    print((proc.time() - startTime)/60)


    if (dropTimeStamps == TRUE) {
        print("---> Removing timestamps...")
        mainData <- mainData[,(varType != "relative timestamp")]
        varLabels <- varLabels[(varType != "relative timestamp")]
    }


    print("---> Finished...")

    if (saveLabels == TRUE) {
        attr(mainData,"labels") <- varLabels
        print("   ...to get GP labels type something like: attr(df,'labels')")
    }

    return(mainData)
}
