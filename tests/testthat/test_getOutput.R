library(DectechR)

context("test getOutput()")


test_that("getOutput() gives an error when nothing sent", {
    expect_error(getOutput())
})

#---- lm -----------------
test_that("getOutput() linear regression output - simple", {
    m1 = lm( mpg ~ cyl, data = mtcars)
    getOutput(m1)
    ccContents = readClipboard()

    expect_identical(ccContents[1],  "\"Dep. Var.\"\t\"mpg\"\t\"\"\t\"\"\t\"\"\t\"\"")
    expect_identical(ccContents[15],  "\"cyl\"\t\"-2.87579\"\t\"0.3224089\"\t\"-8.919699\"\t\"6.112687e-10\"\t\"-0.852161959426613\"")


})

test_that("getOutput() linear regression output - simple, factor", {
    m1 = lm( mpg ~ factor(cyl), data = mtcars)
    getOutput(m1)
    ccContents = readClipboard()
    #ccContents[1]

    expect_identical(ccContents[1],  "\"Dep. Var.\"\t\"mpg\"\t\"\"\t\"\"\t\"\"\t\"\"")
    expect_identical(ccContents[15],  "\"factor(cyl)6\"\t\" -6.920779\"\t\"1.5583482\"\t\"-4.441099\"\t\"1.194696e-04\"\t\"-0.482303857166546\"")


})

test_that("getOutput() linear regression output - full", {
    m1 = lm( mpg ~ factor(cyl) + disp + hp + gear + carb + gear*carb, data = mtcars)
    getOutput(m1)
    ccContents = readClipboard()
    #ccContents[1]

    expect_identical(ccContents[1],  "\"Dep. Var.\"\t\"mpg\"\t\"\"\t\"\"\t\"\"\t\"\"\t\"\"")
    expect_identical(ccContents[15],  "\"factor(cyl)6\"\t\"-2.07688392\"\t\"2.04939890\"\t\"-1.0134113\"\t\"0.320972127\"\t\"-0.144736465553037\"\t\"0.0576328804207179\"")
    expect_identical(ccContents[21], "\"gear:carb\"\t\" 0.06938207\"\t\"0.45308830\"\t\" 0.1531315\"\t\"0.879574833\"\t\"0.0906660117624984\"\t\"0.0196165508848338\"")


})

#---- glm -----------------
test_that("getOutput() glm regression output - simple", {
    m1 = glm( am ~ cyl ,data=mtcars,family = "binomial")
    getOutput(m1)
    ccContents = readClipboard()
    #ccContents[1]

    expect_identical(ccContents[1],  "\"Dep. Var.\"\t\"am\"\t\"\"\t\"\"\t\"\"")
    expect_identical(ccContents[12], "\"cyl\"\t\"-0.691175096215854\"\t\"0.253614480011746\"\t\"-2.72529824079383\"\t\"0.00642434271181205\"")


})

test_that("getOutput() glm regression output - simple, factor", {
    m1 = glm( am ~ factor(cyl) ,data=mtcars,family = "binomial")
    getOutput(m1)
    ccContents = readClipboard()
    #ccContents[1]

    expect_identical(ccContents[1],  "\"Dep. Var.\"\t\"am\"\t\"\"\t\"\"\t\"\"")
    expect_identical(ccContents[12], "\"factor(cyl)6\"\t\"-1.26851132546093\"\t\"1.02062038204751\"\t\"-1.24288261117823\"\t\"0.213911094347626\"")


})

test_that("getOutput() glm regression output - full", {
    m1 = glm( am ~ cyl + disp + hp ,data=mtcars,family = "binomial")
    getOutput(m1)
    ccContents = readClipboard()
    #ccContents[1]

    expect_identical(ccContents[1],  "\"Dep. Var.\"\t\"am\"\t\"\"\t\"\"\t\"\"\t\"\"")
    expect_identical(ccContents[8],  "\"BIC\"\t\"30.0347437455948\"\t\"\"\t\"\"\t\"\"\t\"\"")
    expect_identical(ccContents[14], "\"hp\"\t\" 0.1444261\"\t\"0.07987953\"\t\" 1.8080486\"\t\"0.07059894\"\t\"0.016488328897354\"")


})

#---- mlogit -----------------
test_that("getOutput() mlogit regression output", {

    data("Fishing", package = "mlogit")
    Fish <- mlogit::mlogit.data(Fishing, shape="wide", varying=2:9, choice="mode")

    # ml.Fish <- mlogit::mlogit( mode ~ 1, Fish)
    # summary(ml.Fish)
    # getOutput(ml.Fish)
    # ccContents = readClipboard()
    #
    # expect_identical(ccContents[1],  "\"Dep. Var.\"\t\"mode\"\t\"\"\t\"\"\t\"\"")
    # expect_identical(ccContents[14],  "\"pier:(intercept)\"\t\"0.283943750065351\"\t\"0.114370730930667\"\t\"2.48266097239057\"\t\"0.0130405133530227\"")



    ml.Fish <- mlogit::mlogit( mode ~ price , Fish)
    getOutput(ml.Fish)
    ccContents = readClipboard()

    expect_identical(ccContents[15],  "\"price\"\t\"-0.0252548790145471\"\t\"0.00171849029442108\"\t\"-14.6959683720849\"\t\"0\"")


    ml.Fish <- mlogit::mlogit(mode~price | income | catch, Fish)
    getOutput(ml.Fish)
    ccContents = readClipboard()

    expect_identical(ccContents[15],  "\"price\"\t\"-0.0252814455277226\"\t\"0.0017550980215734\"\t\"-14.404577531834\"\t\"0\"")
    expect_identical(ccContents[16],  "\"income:boat\"\t\"5.54279865435417e-05\"\t\"5.21299150541517e-05\"\t\"1.06326638909663\"\t\"0.287661162900088\"")

    expect_identical(ccContents[22],  "\"catch:pier\"\t\"2.85121542912833\"\t\"0.77463607845712\"\t\"3.68071602707588\"\t\"0.000232579916174647\"")




})

test_that("getOutput() mlogit regression without intercept", {

    data("Fishing", package = "mlogit")
    Fish <- mlogit::mlogit.data(Fishing, shape="wide", varying=2:9, choice="mode")


    ml.Fish <- mlogit::mlogit( mode ~ 0 + price , Fish)
    getOutput(ml.Fish)
    ccContents = readClipboard()

    expect_identical(ccContents[9],  "\"McFadden R^2\"\tNA\t\"\"\t\"\"\t\"\"")
    expect_identical(ccContents[12],  "\"price\"\t\"-0.0179501475826265\"\t\"0.00106937458163833\"\t\"-16.7856501275036\"\t\"0\"")


})
#---- polr -----------------
test_that("getOutput() polr regression output", {
    m1 = MASS::polr( factor(gear) ~ cyl ,data=mtcars)
    getOutput(m1)
    ccContents = readClipboard()


    expect_identical(ccContents[1],  "\"Dep. Var.\"\t\"factor(gear)\"\t\"\"\t\"\"\t\"\"")
    expect_identical(ccContents[13],  "\"4|5\"\t\"-2.10778178293936\"\t\"1.28816053161331\"\t\"-1.63627260051163\"\t\"0.101782547836669\"")

    m1 = MASS::polr( factor(gear) ~ factor(cyl) ,data=mtcars)
    getOutput(m1)
    ccContents = readClipboard()
    expect_identical(ccContents[12],  "\"factor(cyl)8\"\t\"-3.02000434647666\"\t\"0.999778153691301\"\t\"-3.02067447195804\"\t\"0.00252212354571777\"")


    m1 = MASS::polr( factor(gear) ~ am + carb + am*carb ,data=mtcars)
    getOutput(m1)
    ccContents = readClipboard()
    expect_identical(ccContents[13],  "\"am:carb\"\t\"0.43966746501286\"\t\"0.675726797878391\"\t\"0.650658618828354\"\t\"0.515266881339311\"")

})


#---- biglm -----------------
test_that("getOutput() biglm regression output - simple", {
    m1 = biglm::biglm( mpg ~ cyl, data = mtcars)
    getOutput(m1)
    ccContents = readClipboard()

    expect_identical(ccContents[1],  "\"Dep. Var.\"\t\"mpg\"\t\"\"\t\"\"\t\"\"\t\"\"")
    expect_identical(ccContents[11],  "\"cyl\"\t\"-2.87579013906447\"\t\"-3.52060790438268\"\t\"-2.23097237374627\"\t\"0.322408882659104\"\t\"4.67557866045959e-19\"")


})

test_that("getOutput() biglm regression output - simple, factor", {
    m1 = biglm::biglm( mpg ~ factor(cyl), data = mtcars)
    getOutput(m1)
    ccContents = readClipboard()
    #ccContents[1]

    expect_identical(ccContents[1],  "\"Dep. Var.\"\t\"mpg\"\t\"\"\t\"\"\t\"\"\t\"\"")
    expect_identical(ccContents[11],  "\"factor(cyl)6\"\t\"-6.92077922077923\"\t\"-10.0374755877775\"\t\"-3.80408285378092\"\t\"1.55834818349916\"\t\"8.95004278086054e-06\"")


})

test_that("getOutput() biglm regression output - full", {
    m1 = biglm::biglm( mpg ~ factor(cyl) + disp + hp + gear + carb + gear*carb, data = mtcars)
    getOutput(m1)
    ccContents = readClipboard()
    #ccContents[1]

    expect_identical(ccContents[1],  "\"Dep. Var.\"\t\"mpg\"\t\"\"\t\"\"\t\"\"\t\"\"")
    expect_identical(ccContents[11],  "\"factor(cyl)6\"\t\"-2.07688391540811\"\t\"-6.1756817137716\"\t\"2.02191388295538\"\t\"2.04939889918175\"\t\"0.310863764501063\"")
    expect_identical(ccContents[17], "\"gear:carb\"\t\"0.0693820744876062\"\t\"-0.836794527442439\"\t\"0.975558676417652\"\t\"0.453088300965023\"\t\"0.87829460442902\"")


})
