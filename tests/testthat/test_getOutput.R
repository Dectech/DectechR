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
    Sys.sleep(1)
    ccContents = readClipboard()
    #ccContents[1]

    expect_identical(ccContents[1],  "\"Dep. Var.\"\t\"mpg\"\t\"\"\t\"\"\t\"\"\t\"\"\t\"\"")
    expect_identical(ccContents[15],  "\"factor(cyl)6\"\t\"-2.07688392\"\t\"2.04939890\"\t\"-1.0134113\"\t\"0.320972127\"\t\"-0.144736465553037\"\t\"0.0576328804207179\"")
    expect_identical(ccContents[21], "\"gear:carb\"\t\" 0.06938207\"\t\"0.45308830\"\t\" 0.1531315\"\t\"0.879574833\"\t\"0.0906660117624976\"\t\"0.0196165508848338\"")


})

#---- glm -----------------
test_that("getOutput() glm regression output - simple", {
    m1 = glm( am ~ cyl ,data=mtcars,family = "binomial")
    getOutput(m1)
    ccContents = readClipboard()
    #ccContents[1]

    expect_identical(ccContents[1],  "\"Dep. Var.\"\t\"am\"\t\"\"\t\"\"\t\"\"")
    expect_identical(ccContents[12], "\"cyl\"\t\"-0.691175096215854\"\t\"0.253614480011746\"\t\"-2.72529824079383\"\t\"0.00642434271181208\"")


})

test_that("getOutput() glm regression output - simple, factor", {
    m1 = glm( am ~ factor(cyl) ,data=mtcars,family = "binomial")
    getOutput(m1)
    ccContents = readClipboard()
    #ccContents[1]

    expect_identical(ccContents[1],  "\"Dep. Var.\"\t\"am\"\t\"\"\t\"\"\t\"\"")
    expect_identical(ccContents[12], "\"factor(cyl)6\"\t\"-1.26851132546093\"\t\"1.02062038204751\"\t\"-1.24288261117823\"\t\"0.213911094347627\"")


})

test_that("getOutput() glm regression output - full", {
    m1 = glm( am ~ cyl + disp + hp ,data=mtcars,family = "binomial")
    getOutput(m1)
    ccContents = readClipboard()
    #ccContents[1]

    expect_identical(ccContents[1],  "\"Dep. Var.\"\t\"am\"\t\"\"\t\"\"\t\"\"\t\"\"")
    expect_identical(ccContents[8],  "\"BIC\"\t\"30.0347437455948\"\t\"\"\t\"\"\t\"\"\t\"\"")
    expect_identical(ccContents[14], "\"hp\"\t\" 0.1444261\"\t\"0.07987953\"\t\" 1.8080486\"\t\"0.07059894\"\t\"0.0164883288973541\"")


})

#---- mlogit -----------------
test_that("getOutput() mlogit regression output", {

    data("Fishing", package = "mlogit")
    Fish <- dfidx::dfidx(Fishing, shape="wide", varying=2:9, choice="mode")

    # ml.Fish <- mlogit::mlogit( mode ~ 1 , Fish)
    # getOutput(ml.Fish)
    # ccContents = readClipboard()
    #
    # expect_identical(ccContents[1],  "\"Dep. Var.\"\t\"mode\"\t\"\"\t\"\"\t\"\"")
    # expect_identical(ccContents[14],  "\"pier:(intercept)\"\t\"0.283943750065351\"\t\"0.114370730930667\"\t\"2.48266097239057\"\t\"0.0130405133530227\"")



    ml.Fish <- mlogit::mlogit( mode ~ price , Fish)
    getOutput(ml.Fish, reshape = F)
    ccContents = readClipboard()

    expect_identical(ccContents[15],  "\"price\"\t\"-0.0252548790145471\"\t\"0.00171849029442108\"\t\"-14.6959683720849\"\t\"0\"")


    ml.Fish <- mlogit::mlogit( mode ~ 1 | income , Fish)
    getOutput(ml.Fish, reshape = F)
    ccContents = readClipboard()

    expect_identical(ccContents[15],  "\"income:boat\"\t\"9.19063628564497e-05\"\t\"4.06637402154583e-05\"\t\"2.26015517435141\"\t\"0.0238116218057445\"")


    ml.Fish <- mlogit::mlogit(mode~price | income | catch, Fish)
    getOutput(ml.Fish, reshape = F)
    ccContents = readClipboard()

    expect_identical(ccContents[15],  "\"price\"\t\"-0.0252814455277226\"\t\"0.0017550980215734\"\t\"-14.404577531834\"\t\"0\"")
    expect_identical(ccContents[16],  "\"income:boat\"\t\"5.54279865435417e-05\"\t\"5.21299150541517e-05\"\t\"1.06326638909663\"\t\"0.287661162900088\"")

    expect_identical(ccContents[22],  "\"catch:pier\"\t\"2.85121542912833\"\t\"0.77463607845712\"\t\"3.68071602707588\"\t\"0.000232579916174647\"")


})

test_that("getOutput() mlogit regression output, reshaped", {

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
    Sys.sleep(1)
    ccContents = readClipboard()

    expect_identical(ccContents[13],  "\"price\"\t\"-0.0252548790145471\"\t\"-0.0252548790145471\"\t\"-0.0252548790145471\"\t\"0\"\t\"0\"\t\"0\"")


    ml.Fish <- mlogit::mlogit( mode ~ 1 | income , Fish)
    getOutput(ml.Fish)
    Sys.sleep(1)
    ccContents = readClipboard()
    expect_identical(ccContents[13],  "\"income\"\t\"9.19063628564497e-05\"\t\"-3.16398780536123e-05\"\t\"-0.000143402914563964\"\t\"0.0238116218057445\"\t\"0.449590795838522\"\t\"0.00712229917164864\"")


    ml.Fish <- mlogit::mlogit(mode~price | income, Fish)
    getOutput(ml.Fish)
    Sys.sleep(1)
    ccContents = readClipboard()
    Sys.sleep(1)


    expect_identical(ccContents[13],  "\"price\"\t\"-0.0255648288761739\"\t\"-0.0255648288761739\"\t\"-0.0255648288761739\"\t\"0\"\t\"0\"\t\"0\"")
    expect_identical(ccContents[14],  "\"income\"\t\"9.33365874194238e-05\"\t\"-3.24829830225934e-05\"\t\"-0.000126715537529252\"\t\"0.0625790180491881\"\t\"0.518080972703957\"\t\"0.0123043926277129\"")


    ml.Fish <- mlogit::mlogit(mode~price | income | catch, Fish)
    getOutput(ml.Fish)
    Sys.sleep(1)
    ccContents = readClipboard()

    expect_identical(ccContents[12],  "\"(Intercept)\"\tNA\t\"0.841844985640212\"\t\"2.15486635778198\"\t\"1.04302556267659\"\tNA\t\"0.00500798454203633\"\t\"4.34763336443211e-13\"\t\"0.00041323863021292\"")
    expect_identical(ccContents[14],  "\"income\"\tNA\t\"5.54279865435417e-05\"\t\"-7.23372544257705e-05\"\t\"-0.000135500664231565\"\tNA\t\"0.287661162900088\"\t\"0.168708839162969\"\t\"0.00809770752794159\"")

    expect_identical(ccContents[15],  "\"catch\"\t\"3.1177105531067\"\t\"2.54248169242007\"\t\"0.759494299741477\"\t\"2.85121542912833\"\t\"1.22904706227889e-05\"\t\"1.15159957236877e-06\"\t\"8.41720624533693e-07\"\t\"0.000232579916174647\"")




})

test_that("getOutput() mlogit regression without intercept", {

    data("Fishing", package = "mlogit")
    Fish <- mlogit::mlogit.data(Fishing, shape="wide", varying=2:9, choice="mode")


    ml.Fish <- mlogit::mlogit( mode ~ 0 + price , Fish)
    getOutput(ml.Fish)
    Sys.sleep(1)
    ccContents = readClipboard()

    expect_identical(ccContents[9],  "\"McFadden R^2\"\tNA\t\"\"\t\"\"\t\"\"\t\"\"\t\"\"\t\"\"\t\"\"")
    expect_identical(ccContents[12],  "\"price\"\t\"-0.0179501475826265\"\t\"-0.0179501475826265\"\t\"-0.0179501475826265\"\t\"-0.0179501475826265\"\t\"0\"\t\"0\"\t\"0\"\t\"0\"")


})


#---- polr -----------------
test_that("getOutput() polr regression output", {
    m1 = MASS::polr( factor(gear) ~ cyl ,data=mtcars)
    getOutput(m1)
    Sys.sleep(1)
    ccContents = readClipboard()


    expect_identical(ccContents[1],  "\"Dep. Var.\"\t\"factor(gear)\"\t\"\"\t\"\"\t\"\"")
    expect_identical(ccContents[13],  "\"4|5\"\t\"-2.10778178293937\"\t\"1.28816053161238\"\t\"-1.63627260051283\"\t\"0.101782547836419\"")

    m1 = MASS::polr( factor(gear) ~ factor(cyl) ,data=mtcars)
    getOutput(m1)
    ccContents = readClipboard()
    expect_identical(ccContents[12],  "\"factor(cyl)8\"\t\"-3.02000434647665\"\t\"0.999778153688731\"\t\"-3.0206744719658\"\t\"0.00252212354565319\"")


    m1 = MASS::polr( factor(gear) ~ am + carb + am*carb ,data=mtcars)
    getOutput(m1)
    ccContents = readClipboard()
    expect_identical(ccContents[13],  "\"am:carb\"\t\"0.439667354557427\"\t\"0.675728613079995\"\t\"0.650656707510738\"\t\"0.515268115416907\"")

})


#---- biglm -----------------
test_that("getOutput() biglm regression output - simple", {
    m1 = biglm::biglm( mpg ~ cyl, data = mtcars)
    getOutput(m1)
    Sys.sleep(1)
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
    Sys.sleep(1)
    ccContents = readClipboard()
    #ccContents[1]

    expect_identical(ccContents[1],  "\"Dep. Var.\"\t\"mpg\"\t\"\"\t\"\"\t\"\"\t\"\"")
    expect_identical(ccContents[11],  "\"factor(cyl)6\"\t\"-2.07688391540811\"\t\"-6.1756817137716\"\t\"2.02191388295538\"\t\"2.04939889918175\"\t\"0.310863764501063\"")
    expect_identical(ccContents[17], "\"gear:carb\"\t\"0.0693820744876062\"\t\"-0.836794527442439\"\t\"0.975558676417652\"\t\"0.453088300965023\"\t\"0.87829460442902\"")


})

