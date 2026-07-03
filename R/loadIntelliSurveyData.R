

#--- this function requires the "sss" package
#install.packages("sss")




#' loadIntelliSurveyTripleS() loads data exported from IntelliSurvey in Triple S format
#' The file is downloaded as a .zip file
#' and this can be directly loaded by this function
#'
#' Alternatively, if you unzip it, you will have a .sss file and a .csv file
#' these can be loaded by this function as well
#' via the sssFilename and csvFilename parameters
loadIntelliSurveyTripleS = function(zipFilename = NULL,
                                    sssFilename = NULL,
                                    csvFilename = NULL
                                    ) {


    if (!requireNamespace("sss", quietly = TRUE)) {
        stop("Package 'sss' is needed for this function. Please install it.", call. = FALSE)
    }

    #--- Determine input type:
    # input can either be a zip file OR and sss and asc file, but not both:
    EXTRACT_VIA_ZIP = NA

    if (!is.null(zipFilename)) {
        if (!is.null(sssFilename) || !is.null(csvFilename)) {
            stop("If zipFilename is provided, sssFilename and csvFilename should both be omitted")
        }
        EXTRACT_VIA_ZIP = TRUE
    } else {
        # if zipFilename is NULL, then both sssFilename and csvFilename must be provided:
        if (is.null(sssFilename) || is.null(csvFilename)) {
            stop("If zipFilename is NULL, both sssFilename and csvFilename must be provided")
        }
        EXTRACT_VIA_ZIP = FALSE
    }


    #--- if zip, then extract files to a temporary location
    if (EXTRACT_VIA_ZIP) {
        if (!file.exists(zipFilename)) {
            stop("Zip file not found: ", zipFilename)
        }

        # create a temporary directory to extract the files to
        exdir = tempfile("sss_")

        dir.create(exdir, showWarnings = FALSE, recursive = TRUE)

        # make sure the temporary directory is cleaned up when the function exits
        on.exit(unlink(exdir, recursive = TRUE, force = TRUE), add = TRUE)

        # unzip the zip file to the temporary directory
        unzip(zipfile = zipFilename, exdir = exdir)

        extracted_files = list.files(exdir, full.names = TRUE, recursive = TRUE)

        sssFilename = extracted_files[grepl("\\.sss$", extracted_files, ignore.case = TRUE)]
        csvFilename = extracted_files[grepl("\\.(asc|csv)$", extracted_files, ignore.case = TRUE)]

        if (length(sssFilename) != 1) {
            stop("Expected exactly one .sss file in the zip, found ", length(sssFilename))
        }
        if (length(csvFilename) != 1) {
            stop("Expected exactly one .asc/.csv file in the zip, found ", length(csvFilename))
        }
    }

    #--- now load the data
    # read in the data:
    raw_df = sss::read.sss(sssFilename = sssFilename, ascFilename = csvFilename)

    # get the answer labels table:
    label_table = attr(raw_df, "label.table")

    # parse the meta data:
    this_meta_data = sss::parseSSSmetadata(sss::readSSSmetadata(sssFilename))
    this_meta_data$variables = sss:::splitSSS(this_meta_data$variable, sep = "_")

    # map the values in label_table to the variable names:
    idmap = setNames(this_meta_data$variables$ident,
                     this_meta_data$variables$name)

    # now for each variable, if it has answer labels, make it a factor variable:

    for (this_var in names(raw_df)) {

        this_id = as.character(idmap[this_var])

        if (!is.na(this_id) && this_id %in% names(label_table)) { # if this is a valid name in lt...

            if (!all(is.na(label_table[[this_id]]))) { # if it has labels...
                # make it a factor variable:
                raw_df[ ,this_var] = factor(raw_df[ ,this_var], levels = names(label_table[[this_id]]))
            }

        }
    }

    return(raw_df)
}



if (FALSE) {
    # testing the function:

    #--- first lets load a zip file
    # This is the way I think we should normally do it:
    df1 = loadIntelliSurveyTripleS(zipFilename = "keith_test_1_260702_140250_BST.zip")


    #--- now lets load the same data from the unzipped files
    df1 = loadIntelliSurveyTripleS(sssFilename = "keith_test_1_extracted/S8VCWNDF.sss",
                                   csvFilename = "keith_test_1_extracted/S8VCWNDF.csv")


}
