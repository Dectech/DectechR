
if (FALSE) {
    #--- function to get the formula, data and model family (if necessary)






    #----- test glm ----------------------------------

    bin_mod_1 <- glm(vs ~ mpg + cyl + disp, data = mtcars, family = binomial)
    alt_formula <- vs ~ mpg + cyl + disp + hp + qsec

    #----- test 1:
    test = getFormulaAndData(model_class = "glm",
                             mod = bin_mod_1,
                             full_formula = NULL,
                             df = NULL,
                             model_family = NULL)


    test$mod_data
    test$full_formula
    test$model_family


    #----- test 2a:
    test = getFormulaAndData(model_class = "glm",
                             mod = NULL,
                             full_formula = alt_formula,
                             df = mtcars,
                             model_family = "binomial")


    test$mod_data
    test$full_formula
    test$model_family

    #----- test 2b:
    test = getFormulaAndData(model_class = "glm",
                             mod = NULL,
                             full_formula = alt_formula,
                             df = mtcars,
                             model_family = NULL)


    test$mod_data
    test$full_formula
    test$model_family


    #----- test 3:
    test = getFormulaAndData(model_class = "glm",
                             mod = bin_mod_1,
                             full_formula = alt_formula,
                             df = mtcars,
                             model_family = NULL)


    test$mod_data
    test$full_formula
    test$model_family


    #----- test 4

    runUnivariate.glm(bin_mod_1)
    getUnivariate(bin_mod_1)


    runUnivariate.glm(full_formula = alt_formula, df = mtcars)

    getUnivariate(full_formula = alt_formula, df = mtcars, model_class = "glm")



    runUnivariate.glm(full_formula = alt_formula, df = mtcars, model_family = "binomial")
    getUnivariate(full_formula = alt_formula, df = mtcars, model_class = "glm",model_family = "binomial")


    runUnivariate.glm(mod = bin_mod_1,full_formula = alt_formula, df = mtcars, model_family = "binomial")

    getUnivariate(mod = bin_mod_1,full_formula = alt_formula, df = mtcars, model_class = "glm",model_family = "binomial")



    #----- test polr ----------------------------------
    library(MASS)

    mtcars$carb_fact = factor(mtcars$carb)
    ord_mod_1 <- polr(carb_fact ~ mpg + cyl + disp, data = mtcars)
    alt_formula <- carb_fact ~ mpg + cyl + disp + hp + disp

    #----- test 1:
    test = getFormulaAndData(model_class = class(ord_mod_1),
                             mod = ord_mod_1,
                             full_formula = NULL,
                             df = NULL,
                             model_family = NULL)


    test$mod_data
    test$full_formula
    test$model_family


    #----- test 2a:
    test = getFormulaAndData(model_class = class(ord_mod_1),
                             mod = NULL,
                             full_formula = alt_formula,
                             df = mtcars,
                             model_family = NULL)


    test$mod_data
    test$full_formula
    test$model_family

    #----- test 2b:
    test = getFormulaAndData(model_class = class(ord_mod_1),
                             mod = NULL,
                             full_formula = alt_formula,
                             df = mtcars,
                             model_family = NULL)


    test$mod_data
    test$full_formula
    test$model_family


    #----- test 3:
    test = getFormulaAndData(model_class = class(ord_mod_1),
                             mod = bin_mod_1,
                             full_formula = alt_formula,
                             df = mtcars,
                             model_family = NULL)


    test$mod_data
    test$full_formula
    test$model_family



    #----- test 4

    runUnivariate.polr(ord_mod_1)
    getUnivariate(ord_mod_1,returnIntercept = F)

    runUnivariate.polr(full_formula = alt_formula, df = mtcars)
    getUnivariate(full_formula = alt_formula, df = mtcars, model_class = "polr")

    runUnivariate.polr(mod = ord_mod_1,full_formula = alt_formula, df = mtcars)
    getUnivariate(mod = ord_mod_1,full_formula = alt_formula, df = mtcars, model_class = "polr")



    #----- test lm ----------------------------------

    lin_mod_1 <- lm(vs ~ mpg + cyl + disp, data = mtcars)
    alt_formula <- vs ~ mpg + cyl + disp + hp + qsec

    #----- test 1:
    test = getFormulaAndData(model_class = "lm",
                             mod = lin_mod_1,
                             full_formula = NULL,
                             df = NULL,
                             model_family = NULL)


    test$mod_data
    test$full_formula
    test$model_family


    #----- test 2a:
    test = getFormulaAndData(model_class = "lm",
                             mod = NULL,
                             full_formula = alt_formula,
                             df = mtcars,
                             model_family = "binomial")


    test$mod_data
    test$full_formula
    test$model_family

    #----- test 2b:
    test = getFormulaAndData(model_class = "lm",
                             mod = NULL,
                             full_formula = alt_formula,
                             df = mtcars,
                             model_family = NULL)


    test$mod_data
    test$full_formula
    test$model_family


    #----- test 3:
    test = getFormulaAndData(model_class = "lm",
                             mod = lin_mod_1,
                             full_formula = alt_formula,
                             df = mtcars,
                             model_family = NULL)


    test$mod_data
    test$full_formula
    test$model_family


    #----- test 4

    runUnivariate.lm(lin_mod_1)
    getUnivariate(lin_mod_1)

    runUnivariate.lm(full_formula = alt_formula, df = mtcars)
    getUnivariate(full_formula = alt_formula, df = mtcars, model_class = "lm")

    runUnivariate.lm(mod = lin_mod_1,full_formula = alt_formula, df = mtcars)
    getUnivariate(mod = lin_mod_1,full_formula = alt_formula, df = mtcars)




    #----- testing vars generated in the formula -------------



    lin_mod_2 <- lm(vs ~ mpg + factor(cyl) + abs(disp), data = mtcars)
    bin_mod_2 <- glm(vs ~ mpg + factor(cyl) + abs(disp), data = mtcars, family = binomial)
    ord_mod_2 <- polr(carb_fact ~ mpg + factor(cyl) + abs(disp), data = mtcars)


    #--- when variables are transformed within the formula, then the following loses that information:
    # get all IV's in original call...
    IV_list_lm <- all.vars(formula(lin_mod_2$call)[[3]])
    IV_list_glm <- all.vars(formula(bin_mod_2$call)[[3]])
    IV_list_polr <- all.vars(formula(ord_mod_2$call)[[3]])


    #--- if you have the model, this works:

    names(lin_mod_2$model)[-1]
    names(bin_mod_2$model)[-1]
    names(ord_mod_2$model)[-1]


    this_formula = formula(lin_mod_2$call)

    this_formula_string = as.character(this_formula)
    IV_list_lm_2 = trimws(strsplit(this_formula_string[3],"\\+")[[1]])
    DV_name = trimws((this_formula_string[2]))

    this_form[this_form != ""]
}

