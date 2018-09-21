
runUnivariate <- function(mod, ...) UseMethod("runUnivariate")

runUnivariate.lm <- function(mod, returnIntercept = FALSE) { #mod = m1
    # run univariate regression for each input variable...
    # get all IV's in original call...
    IVList <- attr(mod$terms , "term.labels")
    DVname <- (mod$terms[[2]])

    if (returnIntercept == TRUE) {
        intercept_terms <- "(Intercept)"
    } else {
        intercept_terms <- NULL
    }

    #---- loop through each variable...----------------
    resultMatrix <- NULL
    for (var in IVList) { # var <- IVList[2]
        # construct a formula for each variable and run model...
        thisFormula <- as.formula(paste0(DVname," ~ `", var,"`")) # NB: include [`] to deal with variables declared as "factor(var)" etc.

        thisMod <- lm(thisFormula, data = mod$model)

        # extract the relevant information (beta, p-value)...
        coeffTable <- summary(thisMod)$coefficients

        # if it was a factor variable, create a row for each level...
        # ...only include the intercept once (as a way of indicating that it is a factor)
        # ...but perhaps there is a better way of doing this??

        thisResult <- NULL

        thisIntercept <- coeffTable[1,"Estimate"]
        for (i in 2:nrow(coeffTable)) {
            if (returnIntercept == TRUE) {
                thisSubResult <- c(rownames(coeffTable)[i], thisIntercept,coeffTable[i,"Estimate"],coeffTable[i,"Pr(>|t|)"])
            } else {
                thisSubResult <- c(rownames(coeffTable)[i], coeffTable[i,"Estimate"],coeffTable[i,"Pr(>|t|)"])
            }
            thisResult <- rbind(thisResult,thisSubResult)
            thisIntercept <- NA
        }


        resultMatrix <- rbind(resultMatrix, thisResult)
    }

    #---- format output -------------------
    rownames(resultMatrix) <- 1:nrow(resultMatrix)
    resultMatrix <- as.data.frame(resultMatrix,stringsAsFactors = FALSE)
    names(resultMatrix) <- c("IV",intercept_terms,"beta","p-value")

    resultMatrix[,-1] <- data.matrix(resultMatrix[,-1])

    resultMatrix[,1] <- gsub("`","",resultMatrix[,1])

    return(resultMatrix)
}

runUnivariate.glm <- function(mod, returnIntercept = FALSE) {
    # run univariate regression for each input variable...

    # get all IV's in original call...
    IVList <- attr(mod$terms , "term.labels")
    DVname <- (mod$terms[[2]])
    model_family <- as.character(mod$call["family"])

    if (returnIntercept == TRUE) {
        intercept_terms <- "(Intercept)"
    } else {
        intercept_terms <- NULL
    }

    #---- loop through each variable...----------------
    resultMatrix <- NULL
    for (var in IVList) { # var <- IVList[1]
        # construct a formula for each variable and run model...
        thisFormula <- as.formula(paste0(DVname," ~ `", var,"`")) # NB: include [`] to deal with variables declared as "factor(var)" etc.

        thisMod <- glm(thisFormula, data = mod$model, family = model_family)


        # extract the relevant information (beta, p-value)...
        coeffTable <- summary(thisMod)$coefficients

        # if it was a factor variable, create a row for each level...
        # ...only include the intercept once (as a way of indicating that it is a factor)
        # ...but perhaps there is a better way of doing this??

        thisResult <- NULL
        thisIntercept <- coeffTable[1,"Estimate"]
        for (i in 2:nrow(coeffTable)) {
            if (returnIntercept == TRUE) {
                thisSubResult <- c(rownames(coeffTable)[i], thisIntercept,coeffTable[i,"Estimate"],coeffTable[i,"Pr(>|z|)"])
            } else {
                thisSubResult <- c(rownames(coeffTable)[i], coeffTable[i,"Estimate"],coeffTable[i,"Pr(>|z|)"])
            }
            thisResult <- rbind(thisResult,thisSubResult)
            thisIntercept <- NA
        }



        resultMatrix <- rbind(resultMatrix, thisResult)
    }

    #---- format output -------------------
    rownames(resultMatrix) <- 1:nrow(resultMatrix)
    resultMatrix <- as.data.frame(resultMatrix,stringsAsFactors = FALSE)
    names(resultMatrix) <- c("IV",intercept_terms,"beta","p-value")

    resultMatrix[,-1] <- data.matrix(resultMatrix[,-1])

    resultMatrix[,1] <- gsub("`","",resultMatrix[,1])

    return(resultMatrix)
}

runUnivariate.polr <- function(mod, returnIntercept = FALSE) { # mod = ol1
    # run univariate regression for each input variable...

    # get all IV's in original call...
    IVList <- attr(mod$terms , "term.labels")
    DVname <- (mod$terms[[2]])
    DV_levels <- levels(mod$model[,which(colnames(mod$model) == DVname)])
    n_levels <- length(DV_levels)


    if (returnIntercept == TRUE) {
        intercept_terms <- names(mod$zeta)
    } else {
        intercept_terms <- NULL
    }

    #---- loop through each variable...----------------
    resultMatrix <- NULL
    for (var in IVList) { # var <- IVList[1]

        # construct a formula for each variable and run model...
        thisFormula <- as.formula(paste0(DVname," ~ `", var,"`")) # NB: include [`] to deal with variables declared as "factor(var)" etc.

        thisMod <- polr(thisFormula, data = mod$model,Hess = T)


        # extract the relevant information (beta, p-value)...
        coeffTable <- DectechR::getOutputTable(thisMod)
        coeffTable <- coeffTable[["out_table"]]


        n_iv_levels <- nrow(coeffTable) - (n_levels - 1)



        # if it was a factor variable, create a row for each level...
        # ...only include the intercept once (as a way of indicating that it is a factor)
        # ...but perhaps there is a better way of doing this??

        thisResult <- NULL
        thisIntercept <- coeffTable[-(1:n_iv_levels),"Value"]

        for (i in 1:n_iv_levels) {
            if (returnIntercept == TRUE) {
                thisSubResult <- c(rownames(coeffTable)[i], thisIntercept,coeffTable[i,"Value"],coeffTable[i,"p value"])
            } else {
                thisSubResult <- c(rownames(coeffTable)[i], coeffTable[i,"Value"],coeffTable[i,"p value"])
            }
            thisResult <- rbind(thisResult,thisSubResult)
            thisIntercept <- rep(NA,(n_levels - 1))
        }



        resultMatrix <- rbind(resultMatrix, thisResult)
    }
    rownames(resultMatrix) <- 1:nrow(resultMatrix)
    resultMatrix <- as.data.frame(resultMatrix,stringsAsFactors = FALSE)
    names(resultMatrix) <- c("IV",intercept_terms,"beta","p-value")

    resultMatrix[,-1] <- data.matrix(resultMatrix[,-1])

    resultMatrix[,1] <- gsub("`","",resultMatrix[,1])

    return(resultMatrix)
}
