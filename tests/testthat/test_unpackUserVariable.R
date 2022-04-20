library(DectechR)

context("test unpackUserVariable()")


test_that("unpackUserVariable() gives an error when nothing sent", {
    expect_error(getOutput())
})

test_that("unpackUserVariable() basic input", {
    test1 = c("a*1*2*3","b*10*20*30","c*11*22*33","d*1*2*3")
    thisResult = unpackUserVariable(test1)

    expect_identical(thisResult[3,], c("c","11","22","33"))
})

test_that("unpackUserVariable() gives an error when imput not 1-D", {
    test1 = c("a*1*2*3","b*10*20*30","c*11*22*33","d*1*2*3")
    expect_error(unpackUserVariable(cbind(test1,test1)))
})

test_that("unpackUserVariable() numeric input", {
    test2 = c("1*2*3","10*20*30","11*22*33","1*2*3")
    thisResult = unpackUserVariable(test2,numeric = T)

    expect_identical(thisResult[3,], c(11, 22, 33))
})


test_that("unpackUserVariable() input with non-even rows", {
    test3 = c("1*2*3","10*20","11*22*33","1*2*3")


    thisResult = unpackUserVariable(test3,numeric = T, maxCols = 3)
    expect_identical(thisResult[2,], c(10, 20, -99))

    thisResult = unpackUserVariable(test3,numeric = T, maxCols = 3, emptyReplacement = NA)
    expect_identical(thisResult[2,], c(10, 20, NA))

})





test_that("unpackUserVariable() splitting with different delimiter", {
    test4 = c("a*1,2*3","b*10,20*30","c*11,22*33","d*1,2*3")

    thisResult = unpackUserVariable(test4)
    expect_identical(thisResult[2,],c("b", "10,20", "30"))


    thisResult = unpackUserVariable(test4,delimiter = ",")
    expect_identical(thisResult[2,],c("b*10","20*30"))

})

