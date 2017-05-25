####################################################################
# The functions in this file will get a consistent set of outputs  #
# ...and performance measures, across various regression types     #
####################################################################
#
# The main user facing function is getOutput which:
#   - takes various regression types as an input
#   - calls getOutputTable to get a table of coefficients and performance measures
#   - merges these togeter, and writes the result to the clipboard
#
# getOutputTable is an S3 method, which determines the type of regression
# ...model inputted, and then calls the appropriate function
# ...for example for a regression of class lm, getOutputTable.lm is called


getOutputTable <- function(mod, ...) UseMethod("getOutputTable")

getOutputTable.lm <- function(mod, tolerance = TRUE, ...) {
    # (1) get output table...

    out_table <- coef(summary(mod))
    out_table <- as.data.frame(out_table)

    #-- add standardised betas
    betas <- out_table[-1, 1] # get unstandardised betas
    beta_names <- row.names(out_table)[-1]
    model_data <- model.matrix(mod)

    # get standard deviations of all variables
    stdev_x <- apply(model_data, 2, sd, na.rm = T)
    stdev_y <- sapply(mod$model[1], sd) # sd of dependant variable
    if (length(betas) == 1) {
        standardised_beta <- betas * stdev_x[2] / stdev_y
    } else {
        standardised_beta <- betas * stdev_x[beta_names] / stdev_y
    }

    out_table$standardised_beta <- c("", standardised_beta)


    # if required, also include the tolerance...
    num_terms <- length(labels(terms(mod)))
    if ((num_terms > 1) & (tolerance == TRUE)) {
        out_table$Tolerance <- getCoeffTolerance(mod)
    }


    # (2) get performance table...

    num_rows <- nrow(mod$model)
    dof <- extractAIC(mod)[1]
    ll <- logLik(mod)
    aic <- extractAIC(mod)[2]
    kpenalty <- log(num_rows)
    bic <- extractAIC(mod, k = kpenalty)[2]

    if (class(mod)[1] == "lm") {
        model_type <- "Linear (lm)"
        r_squared <- summary(mod)$r.squared
        adjusted_r_squared <- summary(mod)$adj.r.squared

        f <- summary(mod)$fstatistic
        ftest_p <- pf(f[1], f[2], f[3], lower.tail = F)

    } else { # gaussian glm doesn't give r-squared
        model_type <- "Linear (glm)"
        r_squared <- NA
        adjusted_r_squared <- NA
        ftest_p <- NA
    }

    performance_table <- as.data.frame(c("Model", "N", "D.o.F.",
                                 "Log-likelihood", "AIC", "BIC", "R-squared", "Adj. R-squared", "F-test (p value)"))
    names(performance_table) <- names(mod$model)[1]
    performance_table$value <- c(model_type, num_rows, dof, ll, aic, bic, r_squared, adjusted_r_squared, ftest_p)

    out_table_list <- list("out_table" = out_table, "performance_table" = performance_table)

    return(out_table_list)
}

getOutputTable.glm <- function(mod, tolerance = TRUE, ...) {
    model_family <- as.character(mod$call["family"])

    if (model_family == "gaussian") {
        out_table_list <- getOutputTable.lm(mod)

    } else {
        if (model_family != "binomial") {
            warning("This function was not tested with this familiy of glm. Assuming model output is same as 'bionmial'")
        }
        # (1) get output table...
        out_table <- as.data.frame(coef(summary(mod)))

        # if required, also include the tolerance...
        num_terms <- length(labels(terms(mod)))
        if ((num_terms > 1) & (tolerance == TRUE)) {
            out_table$tolerance <- getCoeffTolerance(mod)
        }

        # (2) get performance table...
        model_type <- paste(model_family, "(glm)")

        num_rows <- nrow(mod$model)
        dof <- extractAIC(mod)[1]
        ll <- logLik(mod)
        aic <- extractAIC(mod)[2]
        kpenalty <- log(num_rows)
        bic <- extractAIC(mod, k = kpenalty)[2]

        performance_table <- as.data.frame(c("Model", "N", "D.o.F.", "Log-likelihood", "AIC", "BIC"))
        names(performance_table) <- names(mod$model)[1]
        performance_table$value <- c(model_type, num_rows, dof, ll, aic, bic)

        out_table_list <- list("out_table" = out_table, "performance_table" = performance_table)
    }

    return(out_table_list)
}

getOutputTable.mlogit <- function(mod, ...) {
    out_table <- summary(mod)$CoefTable

    # (2) get performance table...
    model_type <- "Multinomial Logit (mlogit)"
    num_alternatives <- dim(mod$probabilities)[2]
    num_rows <- nrow(mod$model) / num_alternatives
    dof <- length(mod$coefficients)
    aic <- (-2 * mod$logLik) + 2 * dof
    bic <- (-2 * mod$logLik) + log(dim(mod$model)[1]) * dof
    r_squared <- summary(mod)$mfR2[1] #


    performance_table <- as.data.frame(c("Model", "N", "D.o.F.", "Log-likelihood", "AIC", "BIC", "McFadden R^2"))
    names(performance_table) <- names(mod$model)[1]
    performance_table$value <- c(model_type, num_rows, dof, mod$logLik, aic, bic, r_squared)

    out_table_list <- list("out_table" = out_table, "performance_table" = performance_table)

    return(out_table_list)
}

getOutputTable.polr <- function(mod, ...) {
    # (1) get output table...
    out_table <- coef(summary(mod))
    # for ordinal logit, need to add p values...

    # use t value and a normal curve with  mu = 0, sig = 1
    # pnorm gives chance of seeing a value greater than abs(t), assuming this distribution
    # (x2 because we have two tails)
    p <- pnorm(abs(out_table[, "t value"]), lower.tail = FALSE) * 2
    out_table <- cbind(out_table, `p value` = p)

    # (2) get performance table...
    model_type <- "Ordinal Logit (polr)"
    num_rows <- nrow(mod$model)
    dof <- extractAIC(mod)[1]
    ll <- logLik(mod)
    aic <- extractAIC(mod)[2]
    kpenalty <- log(num_rows)
    bic <- extractAIC(mod, k = kpenalty)[2]

    performance_table <- as.data.frame(c("Model", "N", "D.o.F.", "Log-likelihood", "AIC", "BIC"))
    names(performance_table) <- names(mod$model)[1]
    performance_table$value <- c(model_type, num_rows, dof, ll, aic, bic)

    out_table_list <- list("out_table" = out_table, "performance_table" = performance_table)

    return(out_table_list)
}

getOutputTable.default <- function(mod, ...) {
    out_table <- coef(summary(mod))

    # (2) get performance table...
    num_rows <- nrow(mod$model)
    dof <- extractAIC(mod)[1]
    ll <- logLik(mod)
    aic <- extractAIC(mod)[2]
    kpenalty <- log(num_rows)
    bic <- extractAIC(mod, k = kpenalty)[2]

    performance_table <- as.data.frame(c("N", "D.o.F.", "Log-likelihood", "AIC", "BIC"))
    names(performance_table) <- names(mod$model)[1]
    performance_table$value <- c(num_rows, dof, ll, aic, bic)

    out_table_list <- list("out_table" = out_table, "performance_table" = performance_table)

    return(out_table_list)
}





getOutput <- function(mod, performanceTableAtTop = TRUE, ...) {
    ###############################################################
    ##    Function to get results table from a regression model  ##
    ##    ...similar to that returned by SPSS/Stata              ##
    ##    ...with coefficients and performance measures          ##
    ###############################################################

    #-- get the output and performance table...
    out_table_list <- getOutputTable(mod, ...)
    output_table <- out_table_list[["out_table"]]
    performance_table <- out_table_list[["performance_table"]]


    #-- bulk out performance table so it can be joined to output table
    formatted_perf_table <- (array("", dim = c(3 + nrow(performance_table), ncol(output_table))))
    colnames(formatted_perf_table) <- names(output_table)

    #-- merge tables and paste to clipboard
    if (performanceTableAtTop == TRUE) {
        rownames(formatted_perf_table) <- c("Dep. Var.", "", as.character(performance_table[, 1]), "")
        formatted_perf_table[1, 1] <- names(mod$model)[1]
        formatted_perf_table[c(1:nrow(performance_table)) + 2, 1] <- performance_table[, 2]

        # when merging performance table first, rbind doesn't like adding output_table as a data.frame
        # ...especially if it has factors, so convert to character matrix
        # ...but need to save row names as these get dropped by apply (!?)
        output_row_names <- row.names(output_table)
        output_table <- apply(output_table, 2, as.character)


        # merge tables together...
        final_table <- rbind(formatted_perf_table, colnames(output_table), output_table)
        row.names(final_table)[(1:nrow(output_table)) + nrow(formatted_perf_table) + 1] <- output_row_names

        write.table(final_table, "clipboard-128", sep = "\t", col.names = FALSE)
    } else {
        rownames(formatted_perf_table) <- c("Dep. Var.", "", "Performance:", as.character(performance_table[, 1]))
        formatted_perf_table[1, 1] <- names(mod$model)[1]
        formatted_perf_table[-c(1:3), 1] <- performance_table[, 2]

        # merge tables together...
        final_table <- rbind(output_table, formatted_perf_table)

        write.table(final_table, "clipboard-128", sep = "\t", col.names = NA)
    }

}


getCoeffTolerance <- function(mod) {


    vif_table <- getVIF(mod)

    # vif() function will either return vector or array...
    # ...It returns an array in cases were we have categorical vars. In this
    # ...situation a vif is calculated for the variable as a whole rather,
    # ...than for individual levels. So to add back into the table, the value
    # ...needs to be repeated for each, level so vector is the same length

    # check if vector or array...
    if (is.vector(vif_table)) {

        vifLength <- length(vif_table)

        VIF <- as.vector(vif_table)

    } else {
        VIF <- NULL
        for (i in 1:nrow(vif_table)) {
            # if array, have to repeat values for categorical levels...
            for (j in 1:vif_table[i, 2]) {
                VIF <- append(VIF, vif_table[i, 1])
            }
        }
    }


    # tolerance is just 1 over VIF....
    tolerance <- c("", 1 / VIF)

    return(tolerance)
}

getVIF <- function (mod, ...) {
    # This function is an almost identical copy of the function in car::vif
    # ...the difference is that the model.model and det functions have their
    # ... source packages explicitly stated, to avoid confusion with other libraries

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
                                                                                       -subs])) / detR
        result[term, 2] <- length(subs)
    }
    if (all(result[, 2] == 1)) {
        result <- result[, 1]
    } else {
        result[, 3] <- result[, 1]^(1 / (2 * result[, 2]))
    }
    return(result)
}
