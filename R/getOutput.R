
getOutputTable <- function(mod,...) UseMethod("getOutputTable")

getOutputTable.lm <- function(mod,tolerance = TRUE,...){
    # (1) get output table...

    outTable <- coef(summary(mod))
    outTable <- as.data.frame(outTable)

    #-- add standardised betas
    betas <- outTable[-1, 1] # get unstandardised betas
    betaNames <- row.names(outTable)[-1]
    modelData <- model.matrix(mod)

    # get standard deviations of all variables
    sx <- apply(modelData,2,sd,na.rm=T)
    sy <- sapply(mod$model[1],sd) # sd of dependant variable
    if (length(betas) == 1){
        standardisedB <- betas * sx[2]/sy
    }else{
        standardisedB <- betas * sx[betaNames]/sy
    }

    outTable$standardisedB <- c("",standardisedB)


    # if required, also include the tolerance...
    nTerms <- length(labels(terms(mod)))
    if ((nTerms > 1) & (tolerance == TRUE)){
        outTable$Tolerance <- getCoeffTolerance(mod)
    }


    # (2) get performance table...

    N <- nrow(mod$model)
    dof <- extractAIC(mod)[1]
    ll <- logLik(mod)
    aic <- extractAIC(mod)[2]
    kpenalty <- log(N)
    bic <- extractAIC(mod,k=kpenalty)[2]

    if (class(mod)[1] == "lm"){
        modelType <- "Linear (lm)"
        R_squared <- summary(mod)$r.squared
        AdjR_squared <- summary(mod)$adj.r.squared

        f <- summary(mod)$fstatistic
        ftest_p <- pf(f[1],f[2],f[3],lower.tail=F)

    }else{ # gaussian glm doesn't give r-squared
        modelType <- "Linear (glm)"
        R_squared <- NA
        AdjR_squared <- NA
        ftest_p <- NA
    }

    perfTable <- as.data.frame(c("Model","N","D.o.F.",
                                 "Log-likelihood","AIC","BIC","R-squared","Adj. R-squared", "F-test (p value)"))
    names(perfTable) <- names(mod$model)[1]
    perfTable$value <- c(modelType, N, dof, ll, aic, bic,R_squared, AdjR_squared,ftest_p)

    outTableList <- list("outTable" = outTable, "perfTable" = perfTable)

    return(outTableList)
}

getOutputTable.glm <- function(mod,tolerance = TRUE,...){
    modelFamily <- as.character(mod$call["family"])

    if (modelFamily == "gaussian"){
        outTableList <- getOutputTable.lm(mod)

    }else{
        if(modelFamily != "binomial"){
            warning("This function was not tested with this familiy of glm. Assuming model output is same as 'bionmial'")
        }
        # (1) get output table...
        outTable <- as.data.frame(coef(summary(mod)))

        # if required, also include the tolerance...
        nTerms <- length(labels(terms(mod)))
        if ((nTerms > 1) & (tolerance == TRUE)){
            outTable$Tolerance <- getCoeffTolerance(mod)
        }

        # (2) get performance table...
        modelType <- paste(modelFamily,"(glm)")

        N <- nrow(mod$model)
        dof <- extractAIC(mod)[1]
        ll <- logLik(mod)
        aic <- extractAIC(mod)[2]
        kpenalty <- log(N)
        bic <- extractAIC(mod,k=kpenalty)[2]

        perfTable <- as.data.frame(c("Model","N","D.o.F.","Log-likelihood","AIC","BIC"))
        names(perfTable) <- names(mod$model)[1]
        perfTable$value <- c(modelType,N,dof,ll,aic,bic)

        outTableList <- list("outTable" = outTable, "perfTable" = perfTable)
    }

    return(outTableList)
}

getOutputTable.mlogit <- function(mod,...){
    outTable <- summary(mod)$CoefTable

    # (2) get performance table...
    modelType <- "Multinomial Logit (mlogit)"
    Nalternatives <- dim(mod$probabilities)[2]
    N <- nrow(mod$model)/Nalternatives
    DoF <- length(mod$coefficients)
    aic <- (-2 * mod$logLik) + 2 * DoF
    bic <- (-2 * mod$logLik) + log(dim(mod$model)[1]) * DoF
    r2 <- summary(mod)$mfR2[1] #


    perfTable <- as.data.frame(c("Model","N","D.o.F.","Log-likelihood","AIC","BIC", "McFadden R^2"))
    names(perfTable) <- names(mod$model)[1]
    perfTable$value <- c(modelType,N,DoF,mod$logLik,aic,bic,r2)

    outTableList <- list("outTable" = outTable, "perfTable" = perfTable)

    return(outTableList)
}

getOutputTable.polr <- function(mod,...){
    # (1) get output table...
    outTable <- coef(summary(mod))
    # for ordinal logit, need to add p values...

    # use t value and a normal curve with  mu=0,sig=1
    # pnorm gives chance of seeing a value greater than abs(t), assuming this distribution
    # (x2 because we have two tails)
    p <- pnorm(abs(outTable[, "t value"]), lower.tail = FALSE) * 2
    outTable <- cbind(outTable, `p value` = p)

    # (2) get performance table...
    modelType <- "Ordinal Logit (polr)"
    N <- nrow(mod$model)
    dof <- extractAIC(mod)[1]
    ll <- logLik(mod)
    aic <- extractAIC(mod)[2]
    kpenalty <- log(N)
    bic <- extractAIC(mod,k=kpenalty)[2]

    perfTable <- as.data.frame(c("Model","N","D.o.F.","Log-likelihood","AIC","BIC"))
    names(perfTable) <- names(mod$model)[1]
    perfTable$value <- c(modelType,N,dof,ll,aic,bic)

    outTableList <- list("outTable" = outTable, "perfTable" = perfTable)

    return(outTableList)
}

getOutputTable.default <- function(mod,...){
    outTable <- coef(summary(mod))

    # (2) get performance table...
    N <- nrow(mod$model)
    dof <- extractAIC(mod)[1]
    ll <- logLik(mod)
    aic <- extractAIC(mod)[2]
    kpenalty <- log(N)
    bic <- extractAIC(mod,k=kpenalty)[2]

    perfTable <- as.data.frame(c("N","D.o.F.","Log-likelihood","AIC","BIC"))
    names(perfTable) <- names(mod$model)[1]
    perfTable$value <- c(N,dof,ll,aic,bic)

    outTableList <- list("outTable" = outTable, "perfTable" = perfTable)

    return(outTableList)
}





getOutput <- function(mod, performanceTableAtTop = TRUE,...){
    ###############################################################
    ##    Function to get results table from a regression model  ##
    ##    ...similar to that returned by SPSS/Stata              ##
    ##    ...with coefficients and performance measures          ##
    ###############################################################

    #-- get the output and performance table...
    outTableList <- getOutputTable(mod,...)
    outputTable <- outTableList[["outTable"]]
    perfTable <- outTableList[["perfTable"]]


    #-- bulk out performance table so it can be joined to output table
    performanceTable <- (array("",dim=c(3+nrow(perfTable),ncol(outputTable))))
    colnames(performanceTable) <- names(outputTable)

    #-- merge tables and paste to clipboard
    if(performanceTableAtTop == TRUE){
        rownames(performanceTable) <- c("Dep. Var.", "", as.character(perfTable[,1]), "")
        performanceTable[1,1] <- names(mod$model)[1]
        performanceTable[c(1:nrow(perfTable)) + 2, 1] <- perfTable[,2]

        # when merging performance table first, rbind doesn't like adding outputTable as a data.frame
        # ...especially if it has factors, so convert to character matrix
        # ...but need to save row names as these get dropped by apply (!?)
        outputRowNames <- row.names(outputTable)
        outputTable <- apply(outputTable,2,as.character)


        # merge tables together...
        finalTable <- rbind(performanceTable, colnames(outputTable),outputTable)
        row.names(finalTable)[(1:nrow(outputTable))+nrow(performanceTable)+1] <- outputRowNames

        write.table(finalTable,"clipboard-128",sep="\t",col.names=FALSE)
    }else{
        rownames(performanceTable) <- c("Dep. Var.","","Performance:",as.character(perfTable[,1]))
        performanceTable[1,1] <- names(mod$model)[1]
        performanceTable[-c(1:3),1] <- perfTable[,2]

        # merge tables together...
        finalTable <- rbind(outputTable,performanceTable)

        write.table(finalTable,"clipboard-128",sep="\t",col.names=NA)
    }

}


getCoeffTolerance <- function(mod) {


    vifTable <- getVIF(mod)

    # vif() function will either return vector or array...
    # ...It returns an array in cases were we have categorical vars. In this
    # ...situation a vif is calculated for the variable as a whole rather,
    # ...than for individual levels. So to add back into the table, the value
    # ...needs to be repeated for each, level so vector is the same length

    # check if vector or array...
    if (is.vector(vifTable)) {

        vifLength <- length(vifTable)

        VIF <- as.vector(vifTable)

    } else {
        VIF <- NULL
        for (i in 1:nrow(vifTable)) {
            # if array, have to repeat values for categorical levels...
            for (j in 1:vifTable[i,2]) {
                VIF <- append(VIF,vifTable[i,1])
            }
        }
    }


    # tolerance is just 1 over VIF....
    Tolerance <- c("",1/VIF)

    return(Tolerance)
}

getVIF <- function (mod, ...) {
    if (any(is.na(coef(mod)))) {
        stop("there are aliased coefficients in the model")
    }

    v <- vcov(mod)
    assign <- attr(stats::model.matrix(mod), "assign")

    if (names(coefficients(mod)[1]) == "(Intercept)") {
        v <- v[-1, -1]
        assign <- assign[-1]
    } else {
        warning("No intercept: vifs may not be sensible.")
    }
    terms <- labels(terms(mod))
    n.terms <- length(terms)
    if (n.terms < 2) {
        stop("model contains fewer than 2 terms")
    }

    R <- cov2cor(v)
    detR <- base::det(R)
    result <- matrix(0, n.terms, 3)
    rownames(result) <- terms
    colnames(result) <- c("GVIF", "Df", "GVIF^(1/(2*Df))")

    for (term in 1:n.terms) {
        subs <- which(assign == term)
        result[term, 1] <- base::det(as.matrix(R[subs, subs])) * base::det(as.matrix(R[-subs,
                                                                                       -subs]))/detR
        result[term, 2] <- length(subs)
    }
    if (all(result[, 2] == 1)) {
        result <- result[, 1]
    } else {
        result[, 3] <- result[, 1]^(1/(2 * result[, 2]))
    }
    return(result)
}
