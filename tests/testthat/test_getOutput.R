library(DectechR)

context("test getOutput()")


test_that("getOutput() gives an error when nothing sent", {
    expect_error(getOutput())
})



test_that("getOutput() linear regression output", {
    m1 = lm( mpg ~ cyl + disp + hp + gear + carb + gear*carb, data = mtcars)
    getOutput(m1)
    ccContents = readClipboard()
    #ccContents[1]

    expect_identical(ccContents[1],  "\"Dep. Var.\"\t\"mpg\"\t\"\"\t\"\"\t\"\"\t\"\"\t\"\"")
    expect_identical(ccContents[15],  "\"cyl\"\t\"-0.214367857\"\t\"0.84069066\"\t\"-0.2549902\"\t\"0.80081669\"\t\"-0.0635220667684882\"\t\"0.114589624052199\"")
    expect_identical(ccContents[20], "\"gear:carb\"\t\"-0.142073162\"\t\"0.43281912\"\t\"-0.3282507\"\t\"0.74545566\"\t\"-0.185656122871498\"\t\"0.0222300430460025\"")


})


test_that("getOutput() glm regression output", {
    mLogit = glm( am ~ cyl + disp + hp ,data=mtcars,family = "binomial")
    getOutput(mLogit)
    ccContents = readClipboard()
    #ccContents[1]

    expect_identical(ccContents[1],  "\"Dep. Var.\"\t\"am\"\t\"\"\t\"\"\t\"\"\t\"\"")
    expect_identical(ccContents[8],  "\"BIC\"\t\"30.0347437455948\"\t\"\"\t\"\"\t\"\"\t\"\"")
    expect_identical(ccContents[14], "\"hp\"\t\" 0.1444261\"\t\"0.07987953\"\t\" 1.8080486\"\t\"0.07059894\"\t\"0.016488328897354\"")


})


