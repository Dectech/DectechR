library(DectechR)

context("test getScreePlot()")


test_that("getScreePlot() gives an error when nothing sent", {
    expect_error(getScreePlot())
})

test_that("getScreePlot() is producing correct variance table", {

    variance_table = getScreePlot(mtcars,toClipboard = FALSE)

    expect_identical(round(variance_table[,1],3),  c(6.608, 2.65, 0.627, 0.27, 0.223, 0.212, 0.135, 0.123, 0.077, 0.052, 0.022))



})

context("test getFactorLoadingsTable()")


test_that("getFactorLoadingsTable() gives an error when nothing sent", {
    expect_error(getFactorLoadingsTable())
})

test_that("getFactorLoadingsTable() is producing ordered loadings table", {

    fa1 = factanal(mtcars, factors = 3)

    loadings_table = getFactorLoadingsTable(fa1,toClipboard = FALSE)


    expect_identical(round(loadings_table[,1],3),  c(0.908, 0.88, 0.804, -0.778, -0.719, 0.643, -0.177, 0.295, -0.291, -0.618, 0.114))




})
