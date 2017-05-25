library(DectechR)

context("test dectechXmlToDataframe()")

test_that("dectechXmlToDataframe() gives an error when nothing sent", {
    expect_error(dectechXmlToDataframe(),"\"filePath\" is missing, with no default")
})

test_that("dectechXmlToDataframe() gives an error when file doesn't exist", {
    expect_error(dectechXmlToDataframe(filePath = "xxxxxxx"),"Couldn't find that file!")
})



test_that("dectechXmlToDataframe() loads data correctly", {

    # make sure the file exists...
    expect_true(file.exists("test_data.xml"))

    test_df = dectechXmlToDataframe(filePath = "test_data.xml", verbose = FALSE)
    #test_df = dectechXmlToDataframe(filePath = "tests/testthat/test_data.xml", verbose = FALSE)

    # are the loaded dimensions correct...
    expect_equal(dim(test_df), c(19,52))


    # are the variable names correct...
    expectedVarList = c("lfdn", "external_lfdn", "tester", "dispcode", "lastpage",
                        "quality", "duration", "p_0001", "v_76", "v_77", "v_26",
                        "v_31", "v_36", "v_41", "v_46", "v_51", "v_56", "v_61", "v_66",
                        "v_71", "ext_host", "ip_addr", "browser", "referer",
                        "participant_browser", "participant_browser_version",
                        "participant_os", "participant_device", "participant_brand",
                        "participant_model", "participant_isbot", "participant_continent",
                        "participant_country", "participant_region", "participant_city",
                        "participant_latitude", "participant_longitude", "quota",
                        "quota_assignment", "page_history", "hflip", "vflip",
                        "output_mode", "javascript", "flash", "session_id", "language",
                        "cleaned", "ats", "datetime", "date_of_last_access",
                        "date_of_first_mail")


    expect_named(test_df,expectedVarList)

    # are the variable types correct...

    expect_equal(class(test_df[, "lfdn"]), "numeric")
    expect_equal(class(test_df[,"dispcode"]), "factor")
    expect_equal(class(test_df[,"lastpage"]), "character")


    # are the factor variable levels correct...
    dispLevels = c("Not yet invited (11)", "Active (12)",
                   "Inactive (13)", "E-mail could not be delivered (14)",
                   "Cannot be reached (15)", "Not yet started (20)",
                   "Currently responding (21)", "Suspended (22)",
                   "Resumed (23)", "Completed (31)",
                   "Completed after break (32)", "Rejected at login (quota closed) (35)",
                   "Rejected (quota closed) (36)", "Screened out (37)",
                   "Quota full (41)")

    expect_equal(levels(test_df[,"dispcode"]), dispLevels)


    # check some of the values...
    expect_equal(test_df[1,"lastpage"], "136836")
    expect_equal(as.character(test_df[19,"date_of_last_access"]), "2016-01-11 08:32:18")

    # check the saved labels...
    varLabels = attr(test_df,"labels")

    expect_equal(varLabels[7], "time to complete survey")
    expect_equal(varLabels[13], "Recreation and culture ()")

    ## check the effect of the various parameters...
    #test_df_2 = dectechXmlToDataframe(filePath = "tests/testthat/test_data.xml",
    test_df_2 = dectechXmlToDataframe(filePath = "test_data.xml",
                                    removeIncompletes = FALSE,
                                    dropTimeStamps = FALSE,
                                    verbose = FALSE)

    # are the loaded dimensions correct...
    expect_equal(dim(test_df_2), c(20,56))

    # are the variable names correct...
    expect_named(test_df_2,c(expectedVarList,"rts136829", "rts136831", "rts136834", "rts136835"))


})




