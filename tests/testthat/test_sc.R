library(DectechR)

context("test sc()")

test_that("sc() gives an error when nothing sent", {
    expect_error(sc())
    expect_error(sc(mtcars))
})


test_that("sc() finds a string", {

    test = sc("gear",mtcars)

    expect_identical(test,  "gear")

    test = sc("g",mtcars)

    expect_identical(test,  c("mpg","gear"))
})


test_that("sc() returns indicies", {

    test = sc("e",mtcars,value = F)

    expect_equal(test,  c(7,10))

})


test_that("sc() ignores case", {

    test = sc("L",iris)

    expect_identical(test,  c("Sepal.Length","Petal.Length"))

    test = sc("L",iris,ignore.case = T)

    expect_identical(test,  c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width" ))
})



