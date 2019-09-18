library(DectechR)
library(MASS)

context("test runUnivariate()")


test_that("runUnivariate() gives an error when nothing sent", {
    expect_error(runUnivariate())
})


test_that("runUnivariate() on a simple linear regression", {
    m1 = lm(mpg ~ gear + carb + hp, mtcars)
    u1 = runUnivariate(m1)


    expect_identical(u1$IV, c("gear","carb","hp"))
    expect_equal(u1$Beta, c(3.92333333, -2.05571870, -0.06822828))
    expect_equal(u1$`Pr(>|t|)`, c(5.400948e-03, 1.084446e-03, 1.787835e-07))
    expect_identical(u1$`(Intercept)`, NULL)

    u2 = runUnivariate(m1,returnIntercept = T)
    expect_identical(u2$IV, c("gear","carb","hp"))
    expect_equal(u2$Beta, c(3.92333333, -2.05571870, -0.06822828))
    expect_equal(u2$`Pr(>|t|)`, c(5.400948e-03, 1.084446e-03, 1.787835e-07))
    expect_equal(u2$`(Intercept)`, c(5.6233333, 25.872334, 30.098861))


})

test_that("runUnivariate() on a lm() with factor var", {
    # check how it handles factor variables...
    mtcars2 = mtcars
    mtcars2$carb_factor = factor(mtcars2$carb)
    m1 = lm(mpg ~ gear + carb_factor + hp, mtcars2)

    u1 = runUnivariate(m1)

    expect_identical(u1$IV, c("gear","carb_factor2","carb_factor3","carb_factor4","carb_factor6","carb_factor8","hp"))
    expect_equal(u1$Beta, c(3.92333333333333, -2.94285714285714, -9.04285714285715, -9.55285714285715, -5.64285714285714, -10.3428571428571, -0.0682282780715636))
    expect_equal(u1$`Pr(>|t|)`, c(0.00540094822470767, 0.234346327455179, 0.0128499280141855, 0.000529547727433719, 0.291736245223966, 0.0592726905726684, 1.78783525412108e-07))
    expect_identical(u1$`(Intercept)`, NULL)

    #... with the intercept included....
    u2 = runUnivariate(m1,returnIntercept = T)
    expect_identical(u2$IV, c("gear","carb_factor2","carb_factor3","carb_factor4","carb_factor6","carb_factor8","hp"))
    expect_equal(u2$Beta, c(3.92333333333333, -2.94285714285714, -9.04285714285715, -9.55285714285715, -5.64285714285714, -10.3428571428571, -0.0682282780715636))
    expect_equal(u2$`Pr(>|t|)`, c(0.00540094822470767, 0.234346327455179, 0.0128499280141855, 0.000529547727433719, 0.291736245223966, 0.0592726905726684, 1.78783525412108e-07))
    expect_identical(u2$`(Intercept)`, c(5.62333333333333, 25.3428571428571, 25.3428571428571, 25.3428571428571, 25.3428571428571, 25.3428571428571, 30.0988605396225))

})

test_that("runUnivariate() on a lm() with factor var declared in line", {
    # check how it handles factor variables...
    m1 = lm(mpg ~ gear + factor(carb) + hp, mtcars)

    u1 = runUnivariate(m1)

    expect_identical(u1$IV, c("gear","factor(carb)2","factor(carb)3","factor(carb)4","factor(carb)6","factor(carb)8","hp"))
    expect_equal(u1$Beta, c(3.92333333333333, -2.94285714285714, -9.04285714285715, -9.55285714285715, -5.64285714285714, -10.3428571428571, -0.0682282780715636))
    expect_equal(u1$`Pr(>|t|)`, c(0.00540094822470767, 0.234346327455179, 0.0128499280141855, 0.000529547727433719, 0.291736245223966, 0.0592726905726684, 1.78783525412108e-07))
    expect_identical(u1$`(Intercept)`, NULL)

    #... with the intercept included....
    u2 = runUnivariate(m1,returnIntercept = T)
    expect_identical(u2$IV, c("gear","factor(carb)2","factor(carb)3","factor(carb)4","factor(carb)6","factor(carb)8","hp"))
    expect_equal(u2$Beta, c(3.92333333333333, -2.94285714285714, -9.04285714285715, -9.55285714285715, -5.64285714285714, -10.3428571428571, -0.0682282780715636))
    expect_equal(u2$`Pr(>|t|)`, c(0.00540094822470767, 0.234346327455179, 0.0128499280141855, 0.000529547727433719, 0.291736245223966, 0.0592726905726684, 1.78783525412108e-07))
    expect_identical(u2$`(Intercept)`, c(5.62333333333333, 25.3428571428571, 25.3428571428571, 25.3428571428571, 25.3428571428571, 25.3428571428571, 30.0988605396225))

})




test_that("runUnivariate() on a simple binary logit regression, glm()", {
    mtcars2 = mtcars
    mtcars2$mpg_20 = (mtcars2$mpg > 20)*1

    ml1 = glm(mpg_20 ~ gear + carb + disp, mtcars2, family = "binomial")

    u1 = runUnivariate(ml1)

    ## paste0(u1$Beta, collapse = ", ")
    ## paste0(u1$`Pr(>|t|)`, collapse = ", ")
    ## paste0(u2$`(Intercept)`, collapse = ", ")

    expect_identical(u1$IV, c("gear","carb","disp"))
    expect_equal(u1$Beta, c(1.14354382624539, -1.12294815882692, -0.0308400870797587))
    expect_equal(u1$`Pr(>|z|)`, c(0.0422395979083261, 0.00640050430527483, 0.0055543185620774))
    expect_identical(u1$`(Intercept)`, NULL)

    u2 = runUnivariate(ml1,returnIntercept = T)
    expect_identical(u2$IV, c("gear","carb","disp"))
    expect_equal(u2$Beta, c(1.14354382624539, -1.12294815882692, -0.0308400870797587))
    expect_equal(u2$`Pr(>|z|)`, c(0.0422395979083261, 0.00640050430527483, 0.0055543185620774))
    expect_equal(u2$`(Intercept)`, c(-4.48716959736612, 2.62780191265262, 5.74156215209908))

})

test_that("runUnivariate() on a glm() with factor var", {
    mtcars2 = mtcars
    mtcars2$mpg_20 = (mtcars2$mpg > 20)*1

    mtcars2$carb_factor = factor(mtcars2$carb)
    ml1 = glm(mpg_20 ~ gear + carb_factor + disp, mtcars2, family = "binomial")

    u1 = runUnivariate(ml1)

    expect_identical(u1$IV, c("gear", "carb_factor2", "carb_factor3", "carb_factor4", "carb_factor6", "carb_factor8", "disp"))
    expect_equal(u1$Beta, c(1.14354382624539, -1.38629436111989, -20.3578279790962, -3.17805383034795, -20.3578279790898, -20.3578279790972, -0.0308400870797587))
    expect_equal(u1$`Pr(>|z|)`, c(0.0422395979083261, 0.270585575135868, 0.995686729653739, 0.0175830266863531, 0.997509724050083, 0.997509724050099, 0.0055543185620774))
    expect_identical(u1$`(Intercept)`, NULL)

    u2 = runUnivariate(ml1,returnIntercept = T)
    expect_identical(u2$IV, c("gear", "carb_factor2", "carb_factor3", "carb_factor4", "carb_factor6", "carb_factor8", "disp"))
    expect_equal(u2$Beta, c(1.14354382624539, -1.38629436111989, -20.3578279790962, -3.17805383034795, -20.3578279790898, -20.3578279790972, -0.0308400870797587))
    expect_equal(u2$`Pr(>|z|)`, c(0.0422395979083261, 0.270585575135868, 0.995686729653739, 0.0175830266863531, 0.997509724050083, 0.997509724050099, 0.0055543185620774))
    expect_equal(u2$`(Intercept)`, c(-4.48716959736612, 1.79175946922806, 1.79175946922806, 1.79175946922806, 1.79175946922806, 1.79175946922806, 5.74156215209908))

})

test_that("runUnivariate() on a glm() with factor var declared in line", {
    mtcars2 = mtcars
    mtcars2$mpg_20 = (mtcars2$mpg > 20)*1

    ml1 = glm(mpg_20 ~ gear + factor(carb) + disp, mtcars2, family = "binomial")

    u1 = runUnivariate(ml1)


    expect_identical(u1$IV, c("gear", "factor(carb)2", "factor(carb)3", "factor(carb)4", "factor(carb)6", "factor(carb)8", "disp"))
    expect_equal(u1$Beta, c(1.14354382624539, -1.38629436111989, -20.3578279790962, -3.17805383034795, -20.3578279790898, -20.3578279790972, -0.0308400870797587))
    expect_equal(u1$`Pr(>|z|)`, c(0.0422395979083261, 0.270585575135868, 0.995686729653739, 0.0175830266863531, 0.997509724050083, 0.997509724050099, 0.0055543185620774))
    expect_identical(u1$`(Intercept)`, NULL)

    u2 = runUnivariate(ml1,returnIntercept = T)
    expect_identical(u2$IV, c("gear", "factor(carb)2", "factor(carb)3", "factor(carb)4", "factor(carb)6", "factor(carb)8", "disp"))
    expect_equal(u2$Beta, c(1.14354382624539, -1.38629436111989, -20.3578279790962, -3.17805383034795, -20.3578279790898, -20.3578279790972, -0.0308400870797587))
    expect_equal(u2$`Pr(>|z|)`, c(0.0422395979083261, 0.270585575135868, 0.995686729653739, 0.0175830266863531, 0.997509724050083, 0.997509724050099, 0.0055543185620774))
    expect_equal(u2$`(Intercept)`, c(-4.48716959736612, 1.79175946922806, 1.79175946922806, 1.79175946922806, 1.79175946922806, 1.79175946922806, 5.74156215209908))
})




test_that("runUnivariate() on a simple ordinal logit regression, polr()", {

    set.seed(123);

    df1 = as.data.frame(matrix(rnorm(1000*3),1000,3))
    df1$Y = 3*df1$V1 + 0.5*df1$V2 - df1$V3 + rnorm(1000)
    df1$Y_bin = cut(df1$Y,breaks = quantile(df1$Y,c(0,0.333,0.667,1)),include.lowest = T)


    ol1 = polr(Y_bin ~ V1 + V2 + V3, df1)

    u1 = runUnivariate(ol1)


    expect_identical(u1$IV, c("V1","V2","V3"))
    expect_equal(u1$Beta, c(3.54926206270649, 0.357406117502969, -0.542805602449396))
    expect_equal(u1$`p value`, c(1.21785966391788e-94, 1.59227282826942e-09, 9.40244493102174e-18))
    expect_equal(u1$`[-10.8,-1.35]|(-1.35,1.39]`, NULL)
    expect_equal(u1$`(-1.35,1.39]|(1.39,10.2]`, NULL)


    u2 = runUnivariate(ol1,returnIntercept = T)
    expect_identical(u2$IV, c("V1","V2","V3"))
    expect_equal(u2$Beta, c(3.54926206270649, 0.357406117502969, -0.542805602449396))
    expect_equal(u2$`p value`, c(1.21785966391788e-94, 1.59227282826942e-09, 9.40244493102174e-18))
    expect_equal(u2$`[-10.8,-1.35]|(-1.35,1.39]`, c(-1.62346998988311, -0.703315115838237, -0.737033435488431))
    expect_equal(u2$`(-1.35,1.39]|(1.39,10.2]`, c(1.71449585518792, 0.728721708424453, 0.741271958265798
))


})

test_that("runUnivariate() on a polr() with factor var", {
    set.seed(123);

    df1 = as.data.frame(matrix(rnorm(1000*3),1000,3))
    df1$Y = 3*df1$V1 + 0.5*df1$V2 - df1$V3 + rnorm(1000)
    df1$Y_bin = cut(df1$Y,breaks = quantile(df1$Y,c(0,0.333,0.667,1)),include.lowest = T)

    df1$V3_bin = cut(df1$V3, quantile(df1$V3, c(0,0.333,0.667,1)),include.lowest = T)
    ol1 = polr(Y_bin ~ V1 + V2 + V3_bin, df1)

    u1 = runUnivariate(ol1)

    ## writeClipboard(paste0(u1$IV, collapse = "\", \""))
    ## writeClipboard(paste0(u1$Beta, collapse = ", "))
    ## writeClipboard(paste0(u1$`Pr(>|t|)`, collapse = ", "))
    ## writeClipboard(paste0(u2$`[-10.8,-1.35]|(-1.35,1.39]`, collapse = ", "))
    ## writeClipboard(paste0(u2$`(-1.35,1.39]|(1.39,10.2]`, collapse = ", "))

    expect_identical(u1$IV, c("V1", "V2", "V3_bin(-0.466,0.391]", "V3_bin(0.391,3.42]"))
    expect_equal(u1$Beta, c(3.54926206270649, 0.357406117502969, -0.56403488387254, -1.08900722281179))
    expect_equal(u1$`p value`, c(1.21785966391788e-94, 1.59227282826942e-09, 9.71742704528518e-05, 1.13799741435731e-13))
    expect_identical(u1$`[-10.8,-1.35]|(-1.35,1.39]`, NULL)
    expect_identical(u1$`(-1.35,1.39]|(1.39,10.2]`, NULL)



    u2 = runUnivariate(ol1,returnIntercept = T)
    expect_identical(u2$IV, c("V1", "V2", "V3_bin(-0.466,0.391]", "V3_bin(0.391,3.42]"))
    expect_equal(u2$Beta, c(3.54926206270649, 0.357406117502969, -0.56403488387254, -1.08900722281179))
    expect_equal(u2$`p value`, c(1.21785966391788e-94, 1.59227282826942e-09, 9.71742704528518e-05, 1.13799741435731e-13))
    expect_equal(u2$`[-10.8,-1.35]|(-1.35,1.39]`, c(-1.62346998988311, -0.703315115838237, -1.28476323803524, -1.28476323803524))
    expect_equal(u2$`(-1.35,1.39]|(1.39,10.2]`, c(1.71449585518792, 0.728721708424453, 0.170377819294584, 0.170377819294584
))
})



test_that("getUnivariate() on a linear regression", {
    m1 = lm(mpg ~ gear + carb + hp, mtcars)
    getUnivariate(m1)
    ccContents = readClipboard()

    expect_identical(ccContents[2], "\"Dep. Var.\"\t\"mpg\"\t\"\"\t\"\"\t\"\"")
    expect_identical(ccContents[3],  "\"Model\"\t\"Linear (lm)\"\t\"\"\t\"\"\t\"\"")
    expect_identical(ccContents[4], "\"N\"\t\"32\"\t\"\"\t\"\"\t\"\"")

    expect_identical(ccContents[5+1],  "\"\"\t\"Beta\"\t\"Std. Error\"\t\"t value\"\t\"Pr(>|t|)\"")
    expect_identical(ccContents[5+2], "\"gear\"\t\" 3.92333333\"\t\"1.3081307\"\t\" 2.999191\"\t\"5.400948e-03\"")
    expect_identical(ccContents[5+3],  "\"carb\"\t\"-2.05571870\"\t\"0.5685456\"\t\"-3.615750\"\t\"1.084446e-03\"")
    expect_identical(ccContents[5+4],"\"hp\"\t\"-0.06822828\"\t\"0.0101193\"\t\"-6.742389\"\t\"1.787835e-07\"")

    getUnivariate(m1,returnIntercept = T)
    ccContents = readClipboard()

    expect_identical(ccContents[2], "\"Dep. Var.\"\t\"mpg\"\t\"\"\t\"\"\t\"\"\t\"\"")
    expect_identical(ccContents[3],  "\"Model\"\t\"Linear (lm)\"\t\"\"\t\"\"\t\"\"\t\"\"")
    expect_identical(ccContents[4], "\"N\"\t\"32\"\t\"\"\t\"\"\t\"\"\t\"\"")


    expect_identical(ccContents[5+1], "\"\"\t\"(Intercept)\"\t\"Beta\"\t\"Std. Error\"\t\"t value\"\t\"Pr(>|t|)\"")
    expect_identical(ccContents[5+2], "\"gear\"\t\" 5.623333\"\t\" 3.92333333\"\t\"1.3081307\"\t\" 2.999191\"\t\"5.400948e-03\"")
    expect_identical(ccContents[5+3], "\"carb\"\t\"25.872334\"\t\"-2.05571870\"\t\"0.5685456\"\t\"-3.615750\"\t\"1.084446e-03\"")
    expect_identical(ccContents[5+4], "\"hp\"\t\"30.098861\"\t\"-0.06822828\"\t\"0.0101193\"\t\"-6.742389\"\t\"1.787835e-07\"")



})
