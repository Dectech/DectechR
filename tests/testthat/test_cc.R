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


test_that("cc2() output as expected for 2 way table", {
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


test_that("cc2() output as expected for 3 way table", {
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




