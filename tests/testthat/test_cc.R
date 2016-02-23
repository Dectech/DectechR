library(DectechR)

context("test CC")

test_that("cc() gives an error when nothing sent", {
    expect_error(cc())
})


test_that("cc() writing something to clipboard", {
    cc(1)
    ccContents = readClipboard()

    expect_identical(ccContents[1],  "\"\"\t\"x\"")
    expect_identical(ccContents[2], "\"1\"\t1")
})

test_that("cc() output as expected for 1 way table", {
    tab1 = with(mtcars, table(gear))
    cc(tab1)
    ccContents = readClipboard()

    expect_equal(length(ccContents), 4)
    expect_identical(ccContents[1], "\"\"\t\"gear\"\t\"Freq\"")
    expect_identical(ccContents[4], "\"3\"\t\"5\"\t5")
})


test_that("cc() output as expected for 2 way table", {
    tab2 = with(mtcars, table(carb,gear))
    cc(tab2)
    ccContents = readClipboard()
    reformedTable = t(sapply(ccContents,function(x){strsplit(x,"\t")[[1]]}))


    expect_equal(length(ccContents), 7)
    expect_identical(ccContents[1], "\"\"\t\"3\"\t\"4\"\t\"5\"")
    expect_equal(nrow(reformedTable), 7)
    expect_equal(ncol(reformedTable), 4)
})


test_that("cc() output as expected for 3 way table", {
    tab3 = with(mtcars, table(cyl,carb,gear))
    cc(tab3)
    ccContents = readClipboard()
    reformedTable = t(sapply(ccContents,function(x){strsplit(x,"\t")[[1]]}))


    expect_equal(length(ccContents), 55)
    expect_identical(ccContents[1], "\"\"\t\"cyl\"\t\"carb\"\t\"gear\"\t\"Freq\"")
    expect_equal(nrow(reformedTable), 55)
    expect_equal(ncol(reformedTable), 5)
})


