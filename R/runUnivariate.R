
runUnivariate <- function(mod, ...) UseMethod("runUnivariate")



runUnivariate.lm <- function(mod, returnIntercept = FALSE) { #mod = m1
    # run univariate regression for each input variable...
    # get all IV's in original call...
    IV_list <- names(mod$model)[-1]
    DV_name <- names(mod$model)[1]
    original_N = nrow(mod$model)
    #different_N_list = c()

    if (returnIntercept == TRUE) {
        intercept_terms <- "(Intercept)"
    } else {
        intercept_terms <- NULL
    }

    show_intercept_warning <- FALSE

    #---- loop through each variable...----------------
    result_matrix <- NULL
    for (var in IV_list) { # var <- IV_list[1]
        # construct a formula for each variable and run model...
        this_formula <- as.formula(paste0("`",DV_name,"` ~ `", var,"`")) # NB: include [`] to deal with variables declared as "factor(var)" etc.

        this_mod <- lm(this_formula, data = mod$model)

        # check if N differs from original...
        #this_N = nrow(this_mod$model)
        #if (this_N != original_N) {
        #    different_N_list = c(different_N_list, DV_name)
        #}

        # extract the relevant information (beta, p-value)...
        coeff_table <- summary(this_mod)$coefficients

        # if it was a factor variable, create a row for each level...
        # ...only include the intercept once (as a way of indicating that it is a factor)
        # ...but perhaps there is a better way of doing this??

        this_result <- NULL

        this_intercept <- coeff_table[1,"Estimate"]
        for (i in 2:nrow(coeff_table)) { # i = 2
            if (returnIntercept == TRUE) {
                this_subresult <- c(rownames(coeff_table)[i],
                                    this_intercept,coeff_table[i,])
                # if this is a factor/categorical variable and the intercept is shown...
                # ...warn that it is repeated
                if (nrow(coeff_table) > 2) {
                    show_intercept_warning <- TRUE
                }
            } else {
                this_subresult <- c(rownames(coeff_table)[i], coeff_table[i,])
            }
            this_result <- rbind(this_result, this_subresult)
            #this_intercept <- NA

        }


        result_matrix <- rbind(result_matrix, this_result)
    }

    if (show_intercept_warning == T) {
        print("NB: Intercept for factor/categorical variables are repeated")
    }

    #if (length(different_N_list) > 0 ) {
    #    warning(paste0(length(different_N_list), " univariate model(s) have differnt N: ", paste0(different_N_list, collapse = ", ")))
    #}

    #---- format output -------------------
    rownames(result_matrix) <- 1:nrow(result_matrix)
    result_matrix <- as.data.frame(result_matrix, stringsAsFactors = FALSE)
    names(result_matrix) <- c("IV", intercept_terms, "Beta", "Std. Error","t value","Pr(>|t|)")

    result_matrix[,-1] <- apply(result_matrix[,-1],2,as.numeric)
    # ...remove the ` symbol that was added earlier
    result_matrix[,1] <- gsub("`", "", result_matrix[,1])

    return(result_matrix)
}

runUnivariate.glm <- function(mod, returnIntercept = FALSE) {
    # run univariate regression for each input variable...

    # get all IV's in original call...
    IV_list <- names(mod$model)[-1]
    DV_name <- names(mod$model)[1]
    model_family <- as.character(mod$call["family"])

    if (returnIntercept == TRUE) {
        intercept_terms <- "(Intercept)"
    } else {
        intercept_terms <- NULL
    }

    show_intercept_warning <- FALSE

    #---- loop through each variable...----------------
    result_matrix <- NULL
    for (var in IV_list) { # var <- IV_list[1]
        # construct a formula for each variable and run model...
        this_formula <- as.formula(paste0("`",DV_name,"` ~ `", var,"`")) # NB: include [`] to deal with variables declared as "factor(var)" etc.

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
                this_subresult <- c(rownames(coeff_table)[i], this_intercept, coeff_table[i,])
                # if this is a factor/categorical variable and the intercept is shown...
                # ...warn that it is repeated
                if (nrow(coeff_table) > 2) {
                    show_intercept_warning <- TRUE
                }

            } else {
                this_subresult <- c(rownames(coeff_table)[i], coeff_table[i,])
            }
            this_result <- rbind(this_result,this_subresult)

        }



        result_matrix <- rbind(result_matrix, this_result)
    }

    if (show_intercept_warning == T) {
        print("NB: Intercept for factor/categorical variables is repeated")
    }

    #---- format output -------------------
    rownames(result_matrix) <- 1:nrow(result_matrix)
    result_matrix <- as.data.frame(result_matrix,stringsAsFactors = FALSE)
    names(result_matrix) <- c("IV", intercept_terms, "Beta", "Std. Error","z value","Pr(>|z|)")

    result_matrix[,-1] <- apply(result_matrix[,-1],2,as.numeric)

    # ...remove the ` symbol that was added earlier
    result_matrix[,1] <- gsub("`","",result_matrix[,1])

    return(result_matrix)
}

runUnivariate.polr <- function(mod, returnIntercept = FALSE) { # mod = ol1
    # run univariate regression for each input variable...

    # get all IV's in original call...
    IV_list <- names(mod$model)[-1]
    DV_name <- names(mod$model)[1]
    DV_levels <- levels(mod$model[,which(colnames(mod$model) == DV_name)])
    n_levels <- length(DV_levels)


    if (returnIntercept == TRUE) {
        intercept_terms <- names(mod$zeta)
    } else {
        intercept_terms <- NULL
    }

    show_intercept_warning <- FALSE

    #---- loop through each variable...----------------
    result_matrix <- NULL
    for (var in IV_list) { # var <- IV_list[1]

        # construct a formula for each variable and run model...
        this_formula <- as.formula(paste0("`",DV_name,"` ~ `", var,"`")) # NB: include [`] to deal with variables declared as "factor(var)" etc.

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
                this_subresult <- c(rownames(coeff_table)[i], this_intercept,coeff_table[i,])
                # if this is a factor/categorical variable and the intercept is shown...
                # ...warn that it is repeated
                if (n_iv_levels > 1) {
                    show_intercept_warning <- TRUE
                }
            } else {
                this_subresult <- c(rownames(coeff_table)[i], coeff_table[i,])
            }
            this_result <- rbind(this_result,this_subresult)

        }



        result_matrix <- rbind(result_matrix, this_result)
    }

    if (show_intercept_warning == T) {
        print("NB: Intercept for factor/categorical variables is repeated")
    }

    rownames(result_matrix) <- 1:nrow(result_matrix)
    result_matrix <- as.data.frame(result_matrix,stringsAsFactors = FALSE)
    names(result_matrix) <- c("IV", intercept_terms, "Beta", "Std. Error","t value","p value")

    result_matrix[,-1] <- apply(result_matrix[,-1],2,as.numeric)

    # ...remove the ` symbol that was added earlier
    result_matrix[,1] <- gsub("`","",result_matrix[,1])

    return(result_matrix)
}




getUnivariate <- function(mod,returnIntercept = FALSE, ...) {
    #--- First run the univariates...
    output_table <- runUnivariate(mod, returnIntercept, ...)

    #--- next get some model details for the "header" table...
    DV_name <- names(mod$model)[1]

    if (class(mod)[1] == "lm") {
        model_family = "Linear (lm)"
    } else if (class(mod)[1] == "glm") {
        mod_family = as.character(mod$call["family"])
        model_family = paste0(mod_family, " (glm)")
    } else if (class(mod)[1] == "polr") {
        model_family = "Ordinal Logit (polr)"
    } else {
        model_family = class(mod)[1]
        warning("Model type not yet supported")
    }
    num_rows <- nrow(mod$model)

    header_info = c("UNIVARIATE","Dep. Var.","Model", "N")
    header_table <- (array("", dim = c(length(header_info)+1, ncol(output_table))))

    header_table[1:4,1] = header_info
    header_table[1:4,2] = c("",DV_name, model_family, num_rows)

    #--- combine the header and output tables...
    colnames(header_table) = colnames(output_table)
    output_col_names <- colnames(output_table)
    output_col_names[1] = ""
    output_table <- apply(output_table, 2, as.character)

    final_table <- rbind(header_table,output_col_names, output_table)

    write.table(final_table, "clipboard-128", sep = "\t", col.names = FALSE, row.names = F)

}


