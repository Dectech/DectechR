library(DectechR)

context("test cc()")

test_that("cc() gives an error when nothing sent", {
    expect_error(cc2())
})

test_that("cc() writing something to clipboard", {
    cc(1)
    ccContents = readClipboard()

    expect_identical(ccContents[1],  "data")
    expect_identical(ccContents[2], "1")
})

test_that("cc() output as expected for 1 way table", {
    tab1 = with(mtcars, table(gear))
    cc(tab1)
    ccContents = readClipboard()

    expect_equal(length(ccContents), 4)
    expect_identical(ccContents[1], "gear\tFreq")
    expect_identical(ccContents[4], "5\t 5")
})


test_that("cc() output as expected for 2 way table", {
    tab2 = with(mtcars, table(carb,gear))
    cc(tab2)
    ccContents = readClipboard()
    reformedTable = t(sapply(ccContents[-1],function(x){strsplit(x,"\t")[[1]]}))

    expect_equal(length(ccContents), 8)
    expect_identical(ccContents[1], "\tgear")
    expect_identical(ccContents[2], "carb\t3\t4\t5")
    expect_equal(nrow(reformedTable), 7)
    expect_equal(ncol(reformedTable), 4)
})


test_that("cc() output as expected when including row names", {
    df1 = as.data.frame(with(mtcars, table(carb,gear)))
    cc(df1,includeRowNames = TRUE)
    ccContents = readClipboard()
    reformedTable = t(sapply(ccContents[-1],function(x){strsplit(x,"\t")[[1]]}))

    expect_equal(length(ccContents), 19)
    expect_identical(ccContents[1], "\tcarb\tgear\tFreq")
    expect_identical(ccContents[2], "1\t1\t3\t3")
    expect_equal(nrow(reformedTable), 18)
    expect_equal(ncol(reformedTable), 4)
})


test_that("cc() output as expected for 3 way table", {
    tab3 = with(mtcars, table(cyl,carb,gear))
    cc(tab3)
    ccContents = readClipboard()
    reformedTable = t(sapply(ccContents[-1],function(x){strsplit(x,"\t")[[1]]}))


    expect_equal(length(ccContents), 20)
    expect_identical(ccContents[1], "\t\tgear")
    expect_identical(ccContents[16], "8\t2\t4\t0\t0")
    expect_equal(nrow(reformedTable), 19)
    expect_equal(ncol(reformedTable), 5)
})

test_that("cc() output as expected for 3 way table, nestedOrderOutToIn = FALSE", {
    tab3 = with(mtcars, table(cyl,carb,gear))
    cc(tab3, nestedOrderOutToIn = F)
    ccContents = readClipboard()
    reformedTable = t(sapply(ccContents[-1],function(x){strsplit(x,"\t")[[1]]}))


    expect_equal(length(ccContents), 20)
    expect_identical(ccContents[1], "\t\tgear")
    expect_identical(ccContents[16], "6\t6\t0\t0\t1")
    expect_equal(nrow(reformedTable), 19)
    expect_equal(ncol(reformedTable), 5)
})


test_that("cc() output as expected for 2 way data frame", {
    df1 <- with(mtcars, aggregate(mpg,list("cyl" = cyl,"gear" = gear),mean))
    cc(df1)
    ccContents = readClipboard()
    reformedTable = t(sapply(ccContents[-1],function(x){strsplit(x,"\t")[[1]]}))

    expect_equal(length(ccContents), 9)
    expect_identical(ccContents[1], "cyl\tgear\tx")
    expect_identical(ccContents[2], "4\t3\t21.5")
    expect_equal(nrow(reformedTable), 8)
    expect_equal(ncol(reformedTable), 3)

})


test_that("cc() output as expected for 2 way data frame, force nested", {
    df1 <- with(mtcars, aggregate(mpg,list("cyl" = cyl,"gear" = gear),mean))
    cc(df1, forceNested = T)
    ccContents = readClipboard()
    reformedTable = t(sapply(ccContents[-1],function(x){strsplit(x,"\t")[[1]]}))

    expect_equal(length(ccContents), 5)
    expect_identical(ccContents[1], "\tgear")
    expect_identical(ccContents[2], "cyl\t3\t4\t5")
    expect_equal(nrow(reformedTable), 4)
    expect_equal(ncol(reformedTable), 4)

})

