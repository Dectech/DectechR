
runUnivariate <- function(mod, ...) UseMethod("runUnivariate")

runUnivariate.lm <- function(mod, returnIntercept = FALSE) { #mod = m1
    # run univariate regression for each input variable...
    # get all IV's in original call...
    IV_list <- attr(mod$terms , "term.labels")
    DV_name <- (mod$terms[[2]])

    if (returnIntercept == TRUE) {
        intercept_terms <- "(Intercept)"
    } else {
        intercept_terms <- NULL
    }

    #---- loop through each variable...----------------
    result_matrix <- NULL
    for (var in IV_list) { # var <- IV_list[2]
        # construct a formula for each variable and run model...
        this_formula <- as.formula(paste0(DV_name," ~ `", var,"`")) # NB: include [`] to deal with variables declared as "factor(var)" etc.

        this_mod <- lm(this_formula, data = mod$model)

        # extract the relevant information (beta, p-value)...
        coeff_table <- summary(this_mod)$coefficients

        # if it was a factor variable, create a row for each level...
        # ...only include the intercept once (as a way of indicating that it is a factor)
        # ...but perhaps there is a better way of doing this??

        this_result <- NULL

        this_intercept <- coeff_table[1,"Estimate"]
        for (i in 2:nrow(coeff_table)) {
            if (returnIntercept == TRUE) {
                this_subresult <- c(rownames(coeff_table)[i], this_intercept,coeff_table[i,"Estimate"],coeff_table[i,"Pr(>|t|)"])
            } else {
                this_subresult <- c(rownames(coeff_table)[i], coeff_table[i,"Estimate"],coeff_table[i,"Pr(>|t|)"])
            }
            this_result <- rbind(this_result, this_subresult)
            this_intercept <- NA
        }


        result_matrix <- rbind(result_matrix, this_result)
    }

    #---- format output -------------------
    rownames(result_matrix) <- 1:nrow(result_matrix)
    result_matrix <- as.data.frame(result_matrix, stringsAsFactors = FALSE)
    names(result_matrix) <- c("IV", intercept_terms, "beta", "p-value")

    result_matrix[,-1] <- data.matrix(result_matrix[,-1])

    result_matrix[,1] <- gsub("`", "", result_matrix[,1])

    return(result_matrix)
}

runUnivariate.glm <- function(mod, returnIntercept = FALSE) {
    # run univariate regression for each input variable...

    # get all IV's in original call...
    IV_list <- attr(mod$terms , "term.labels")
    DV_name <- (mod$terms[[2]])
    model_family <- as.character(mod$call["family"])

    if (returnIntercept == TRUE) {
        intercept_terms <- "(Intercept)"
    } else {
        intercept_terms <- NULL
    }

    #---- loop through each variable...----------------
    result_matrix <- NULL
    for (var in IV_list) { # var <- IV_list[1]
        # construct a formula for each variable and run model...
        this_formula <- as.formula(paste0(DV_name," ~ `", var,"`")) # NB: include [`] to deal with variables declared as "factor(var)" etc.

        this_mod <- glm(this_formula, data = mod$model, family = model_family)


        # extract the relevant information (beta, p-value)...
        coeff_table <- summary(this_mod)$coefficients

        # if it was a factor variable, create a row for each level...
        # ...only include the intercept once (as a way of indicating that it is a factor)
        # ...but perhaps there is a better way of doing this??

        this_result <- NULL
        this_intercept <- coeff_table[1,"Estimate"]
        for (i in 2:nrow(coeff_table)) {
            if (returnIntercept == TRUE) {
                this_subresult <- c(rownames(coeff_table)[i], this_intercept, coeff_table[i,"Estimate"],coeff_table[i,"Pr(>|z|)"])
            } else {
                this_subresult <- c(rownames(coeff_table)[i], coeff_table[i,"Estimate"], coeff_table[i,"Pr(>|z|)"])
            }
            this_result <- rbind(this_result,this_subresult)
            this_intercept <- NA
        }



        result_matrix <- rbind(result_matrix, this_result)
    }

    #---- format output -------------------
    rownames(result_matrix) <- 1:nrow(result_matrix)
    result_matrix <- as.data.frame(result_matrix,stringsAsFactors = FALSE)
    names(result_matrix) <- c("IV",intercept_terms,"beta","p-value")

    result_matrix[,-1] <- data.matrix(result_matrix[,-1])

    result_matrix[,1] <- gsub("`","",result_matrix[,1])

    return(result_matrix)
}

runUnivariate.polr <- function(mod, returnIntercept = FALSE) { # mod = ol1
    # run univariate regression for each input variable...

    # get all IV's in original call...
    IV_list <- attr(mod$terms , "term.labels")
    DV_name <- (mod$terms[[2]])
    DV_levels <- levels(mod$model[,which(colnames(mod$model) == DV_name)])
    n_levels <- length(DV_levels)


    if (returnIntercept == TRUE) {
        intercept_terms <- names(mod$zeta)
    } else {
        intercept_terms <- NULL
    }

    #---- loop through each variable...----------------
    result_matrix <- NULL
    for (var in IV_list) { # var <- IV_list[1]

        # construct a formula for each variable and run model...
        this_formula <- as.formula(paste0(DV_name," ~ `", var,"`")) # NB: include [`] to deal with variables declared as "factor(var)" etc.

        this_mod <- polr(this_formula, data = mod$model,Hess = T)


        # extract the relevant information (beta, p-value)...
        coeff_table <- DectechR::getOutputTable(this_mod)
        coeff_table <- coeff_table[["out_table"]]


        n_iv_levels <- nrow(coeff_table) - (n_levels - 1)



        # if it was a factor variable, create a row for each level...
        # ...only include the intercept once (as a way of indicating that it is a factor)
        # ...but perhaps there is a better way of doing this??

        this_result <- NULL
        this_intercept <- coeff_table[-(1:n_iv_levels),"Value"]

        for (i in 1:n_iv_levels) {
            if (returnIntercept == TRUE) {
                this_subresult <- c(rownames(coeff_table)[i], this_intercept,coeff_table[i,"Value"],coeff_table[i,"p value"])
            } else {
                this_subresult <- c(rownames(coeff_table)[i], coeff_table[i,"Value"],coeff_table[i,"p value"])
            }
            this_result <- rbind(this_result,this_subresult)
            this_intercept <- rep(NA,(n_levels - 1))
        }



        result_matrix <- rbind(result_matrix, this_result)
    }
    rownames(result_matrix) <- 1:nrow(result_matrix)
    result_matrix <- as.data.frame(result_matrix,stringsAsFactors = FALSE)
    names(result_matrix) <- c("IV",intercept_terms,"beta","p-value")

    result_matrix[,-1] <- data.matrix(result_matrix[,-1])

    result_matrix[,1] <- gsub("`","",result_matrix[,1])

    return(result_matrix)
}
