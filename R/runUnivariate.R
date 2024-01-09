getFormulaAndData <- function(model_class, mod = NULL, full_formula = NULL, df = NULL, model_family = NULL) {
    ###########################################################
    ### Function to get the formula, data and model family ###
    ###  ...from either a model object or a formula + data ###
    ##########################################################


    if (is.null(mod) == TRUE) {
        #---- If no model supplied

        #...then use full forumla and df

        if (is.null(full_formula) | is.null(df)) {
            stop("Function requires either a fitted model OR a formula plus the data")
        }

        #--- use the supplied data:
        mod_data = df

        if (is.null(model_family) & model_class == "glm") {
            warning("model_family hasn't been stated. So assuming binomial, as that's the most common option for glm")
            model_family = "binomial"
        }


    } else {
        #---- if model has been supplied....
        if (is.null(full_formula) == FALSE) {
            warning("You supplied a fitted model AND and a formula! This result will use the supplied formula, and will ignore the formula used in the fitted model.")
            # use the supplied formula
            full_formula <- full_formula

            #*** POTENTIAL ISSUE:
            #* if you supply a fitted model AND a formula, but NO data
            #* ...then the formula supplied may contain variables not in the models data set

        } else {
            # get forumla from supplied model
            full_formula <- formula(mod$terms)
        }

        if (is.null(df) == FALSE) {
            mod_data = df
        } else {
            mod_data = mod$model
        }



        if (model_class == "glm") {
            model_family <- as.character(mod$call["family"])
        }
    }


    # get all IV's in original call...
    this_formula_string = as.character(full_formula)
    IV_list = trimws(strsplit(this_formula_string[3],"\\+")[[1]])
    DV_name = trimws((this_formula_string[2]))

    # IV_list <- all.vars(mod_details$full_formula[[3]])
    # DV_name <- all.vars(mod_details$full_formula[[2]])

    return(list("full_formula" = full_formula,
                "mod_data" = mod_data,
                "model_family" = model_family,
                "IV_list" = IV_list,
                "DV_name" = DV_name))
}


runUnivariate <- function(mod, ...) UseMethod("runUnivariate")



runUnivariate.lm <- function(mod = NULL, full_formula = NULL, df = NULL, returnIntercept = FALSE) { #mod = m1
    # run univariate regression for each input variable...


    #--- Get the forumla and data from the supplied inputs...
    mod_details = getFormulaAndData(model_class = "lm",
                                         mod = mod,
                                         full_formula = full_formula,
                                         df = df,
                                         model_family = NULL)



    original_N = nrow(mod_details$mod_data)
    #different_N_list = c()

    if (returnIntercept == TRUE) {
        intercept_terms <- "(Intercept)"
    } else {
        intercept_terms <- NULL
    }

    show_intercept_warning <- FALSE

    #---- loop through each variable...----------------
    result_matrix <- NULL
    for (var in mod_details$IV_list) { # var <- mod_details$IV_list[1]

        # construct a formula for each variable and run model...
        # ...need to deal with cases where variables are declared in the formula...
        if (is.null(df) == FALSE) {
            # If we are using df i.e. data explicitly supplied, it doesn't matter, just construct a normal formula:
            this_formula <- as.formula(paste0(mod_details$DV_name," ~ ", var))
        } else {
            # if data is coming from the model object, then the var names will include the expressions used to create them
            # for example factor(age) will be stored as "factor(age)"
            # Hence need to include [`] to call these:
            this_formula <- as.formula(paste0("`",mod_details$DV_name,"` ~ `", var,"`"))
        }

        this_mod <- lm(this_formula,
                       data = mod_details$mod_data)

        # check if N differs from original...
        #this_N = nrow(this_mod$model)
        #if (this_N != original_N) {
        #    different_N_list = c(different_N_list, mod_details$DV_name)
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

runUnivariate.glm <- function(mod = NULL, full_formula = NULL, df = NULL, model_family = NULL, returnIntercept = FALSE) {
    # run univariate regression for each input variable...


    #--- Get the forumla and data from the supplied inputs...
    mod_details = getFormulaAndData(model_class = "glm",
                      mod = mod,
                      full_formula = full_formula,
                      df = df,
                      model_family = model_family)




    if (returnIntercept == TRUE) {
        intercept_terms <- "(Intercept)"
    } else {
        intercept_terms <- NULL
    }

    show_intercept_warning <- FALSE

    #---- loop through each variable...----------------
    result_matrix <- NULL
    for (var in mod_details$IV_list) { # var <- mod_details$IV_list[1]

        # construct a formula for each variable and run model...
        # ...need to deal with cases where variables are declared in the formula...
        if (is.null(df) == FALSE) {
            # If we are using df i.e. data explicitly supplied, it doesn't matter, just construct a normal formula:
            this_formula <- as.formula(paste0(mod_details$DV_name," ~ ", var))
        } else {
            # if data is coming from the model object, then the var names will include the expressions used to create them
            # for example factor(age) will be stored as "factor(age)"
            # Hence need to include [`] to call these:
            this_formula <- as.formula(paste0("`",mod_details$DV_name,"` ~ `", var,"`"))
        }

        this_mod <- glm(this_formula,
                        data = mod_details$mod_data,
                        family = mod_details$model_family)


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

runUnivariate.polr <- function(mod = NULL, full_formula = NULL, df = NULL, returnIntercept = FALSE) { # mod = ol1

    # run univariate regression for each input variable...

    # can either input an existing model
    # ...or a formula and a data frame...

    #--- Get the forumla and data from the supplied inputs...
    mod_details = getFormulaAndData(model_class = "polr",
                                         mod = mod,
                                         full_formula = full_formula,
                                         df = df,
                                         model_family = NULL)






    DV_levels <- levels(mod_details$mod_data[,mod_details$DV_name])
    n_levels <- length(DV_levels)


    if (returnIntercept == TRUE) {
        if (is.null(mod) == FALSE) {
            intercept_terms <- names(mod$zeta)
        } else {
            # Need to guess these terms:
            temp_from = DV_levels[-length(DV_levels)]
            temp_to = DV_levels[-1]

            intercept_terms <- paste0(temp_from, "|", temp_to)
        }

    } else {
        intercept_terms <- NULL
    }

    show_intercept_warning <- FALSE



    #---- loop through each variable...----------------
    result_matrix <- NULL
    for (var in mod_details$IV_list) { # var <- mod_details$IV_list[1]

        # construct a formula for each variable and run model...
        # ...need to deal with cases where variables are declared in the formula...
        if (is.null(df) == FALSE) {
            # If we are using df i.e. data explicitly supplied, it doesn't matter, just construct a normal formula:
            this_formula <- as.formula(paste0(mod_details$DV_name," ~ ", var))
        } else {
            # if data is coming from the model object, then the var names will include the expressions used to create them
            # for example factor(age) will be stored as "factor(age)"
            # Hence need to include [`] to call these:
            this_formula <- as.formula(paste0("`",mod_details$DV_name,"` ~ `", var,"`"))
        }

        this_mod <- polr(this_formula,
                         data = mod_details$mod_data,
                         Hess = T)


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




getUnivariate <- function(mod = NULL,
                          full_formula = NULL,
                          df = NULL,
                          model_class = NULL,
                          model_family = NULL,
                          returnIntercept = FALSE, ...) {

    #--- First run the univariates...
    if (is.null(mod) == FALSE) {
        #---- if a model has been supplied:
        output_table <- runUnivariate(mod, full_formula, df, returnIntercept, ...)

        model_class = class(mod)[1]

        # get all IV's in original call...
        this_formula_string = as.character(formula(mod$call))
        IV_list = trimws(strsplit(this_formula_string[3],"\\+")[[1]])
        DV_name = trimws((this_formula_string[2]))

        num_rows <- nrow(mod$model)

    } else {
        #---- if a model has NOT been supplied:

        if (is.null(model_class)) {
            stop("Please supply a model_class (e.g. 'lm', 'glm', 'polr')")
        } else {
            # otherwise, run the appropriate function for the model class:
            if (model_class == "lm") {
                output_table <- runUnivariate.lm(mod, full_formula, df, returnIntercept, ...)
            } else if (model_class == "glm") {
                output_table <- runUnivariate.glm(mod, full_formula, df, model_family, returnIntercept, ...)
            } else if (model_class == "polr") {
                output_table <- runUnivariate.polr(mod, full_formula, df, returnIntercept, ...)
            } else {
                warning("Model type not yet supported")
            }
        }

        DV_name <- all.vars(full_formula[[2]])

        num_rows <- nrow(df)
    }



    #--- next get some model details for the "header" table...


    if (model_class == "lm") {
        model_family_name = "Linear (lm)"
    } else if (model_class == "glm") {

        if (is.null(mod) == FALSE) {
            # if a model supplied, get its family:
            model_family = as.character(mod$call["family"])
        } else if (is.null(model_family)) {
            # if no model supplied, and no family supplied, assume binomial:
            model_family = "Assumed binomial"
        } else {
            # otherwise, use the supplied family
        }

        model_family_name = paste0(model_family, " (glm)")

    } else if (model_class == "polr") {
        model_family_name = "Ordinal Logit (polr)"
    } else {
        model_family_name = model_class
        warning("Model type not yet supported")
    }


    header_info = c("UNIVARIATE","Dep. Var.","Model", "N")
    header_table <- (array("", dim = c(length(header_info)+1, ncol(output_table))))

    header_table[1:4,1] = header_info
    header_table[1:4,2] = c("",DV_name, model_family_name, num_rows)

    #--- combine the header and output tables...
    colnames(header_table) = colnames(output_table)
    output_col_names <- colnames(output_table)
    output_col_names[1] = ""
    output_table <- apply(output_table, 2, as.character)

    final_table <- rbind(header_table,output_col_names, output_table)

    write.table(final_table, "clipboard-128", sep = "\t", col.names = FALSE, row.names = F)

}

