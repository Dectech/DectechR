library(DectechR)

context("test addRespondentPlatformVars()")

test_that("addRespondentPlatformVars() gives an error when nothing sent", {
    expect_error(addRespondentPlatformVars())
})

test_that("addRespondentPlatformVars() gives errors with incorrect input", {

    expect_error(addRespondentPlatformVars(c("A","B","C")))

    df_test = data.frame("lfdn" = c(1,2,3), "var2" = c("A","B","C"))
    expect_error(addRespondentPlatformVars(df_test))

})


test_that("addRespondentPlatformVars() gives correct output", {

    df_test = data.frame("lfdn" = c(1,2,3,4), "browser" = c(
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/68.0.3440.106 Safari/537.36",
        "Mozilla/5.0 (iPad; CPU OS 11_4 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/11.0 Mobile/15E148 Safari/604.1",
        "Mozilla/5.0 (Linux; Android 7.0; LG-M200 Build/NRD90U) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/68.0.3440.91 Mobile Safari/537.36",
        "Mozilla/5.0 (Windows NT 6.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/68.0.3440.106 Safari/537.36"))

    df_test = addRespondentPlatformVars(df_test)

    expect_identical(df_test$operating_system,  c("Windows 10", "iPad", "Android", "Windows 7"))
    expect_identical(df_test$is_mobile,  c(0, 1, 1, 0))
    expect_identical(df_test$web_browser,  c("Chrome", "Safari", "Chrome", "Chrome"))

    df_test = addRespondentPlatformVars(df_test, detailed = FALSE)

    expect_identical(df_test$operating_system,  c("Windows", "iPad", "Android", "Windows"))
})


