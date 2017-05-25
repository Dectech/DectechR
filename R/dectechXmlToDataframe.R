dectechXmlToDataframe <- function(filePath, removeIncompletes = TRUE, saveLabels = TRUE, dropTimeStamps = TRUE, verbose = TRUE) {
    #####################################################################################
    #  Function to convert an XML file exported from Questback into an R data frame
    #  -----------------------------------------------------------------------------
    #
    # Questback uses a bespoke file format, this function converts it to a data frame
    # The main steps are:
    #  (1) print some output for user echoing input params
    #  (2) load raw xml file and get variable names/ labels/ type
    #  (3) convert to data frame....
    #  (4) go through each variable....
    #    (a) if it has a code, make it into an R factor...
    #    (b) if not, convert to the appropriate type...
    #
    #####################################################################################

    if (verbose == TRUE) {
        start_time <- proc.time()
    }
    # make sure file exists, before starting...
    if (file.exists(filePath) == FALSE) {
        stop("-- Couldn't find that file! Make sure you have the correct path and file name")
    }


    # ----- (1) print some output for user echoing input params -----------------------------
    if (verbose == TRUE) {
        print("------- Questback XML to dataframe ----------------")
        print("-   With the following options:                    -")
        if (removeIncompletes) {
            print("-       Remove incompetes: Yes                     -")
        } else {
            print("-       Remove incompetes: No                      -")
        }

        if (saveLabels) {
            print("-       Save labels as an attribute: Yes           -")
        } else {
            print("-       Save labels as an attribute: No            -")
        }

        if (dropTimeStamps) {
            print("-       Drop GP time stamp variables: Yes          -")
        } else {
            print("-       Drop GP time stamp variables: No           -")
        }
        print("----------------------------------------------------")
    }




    # ----- (2) load raw xml file and get variable names/ labels/ type  --------

    xml_data <- XML::xmlTreeParse(filePath, useInternalNodes = TRUE,
                              options = (XML::HUGE) | (XML::RECOVER), encoding = "UTF-8")

    if (verbose == TRUE) {
        print("---> Extracting variable labels...")
    }
    raw_var_list <- XML::getNodeSet(xml_data, "//variable[@name]")
    var_names <- sapply(raw_var_list, function(x) XML::xmlGetAttr(x, "name"))
    var_type <- sapply(raw_var_list, function(x) XML::xmlGetAttr(x, "type"))
    var_labels <- XML::xpathSApply(xml_data, "//variable[@name]/label[text()]", XML::xmlValue)


    # some nodes also have a "code" that define the levels of the variable...
    raw_vars_with_code <- XML::getNodeSet(xml_data, "//variable/codes/parent::*")
    var_names_with_codes <- sapply(raw_vars_with_code, function(x) XML::xmlGetAttr(x, "name"))


    # the fastest way to get variable labels is to extract all of them first
    # ...however this will be one long undifferentiated vector of keys and labels
    # NB: keys are the numeric codes that questback uses, labels are the level labels
    #    ....so for a yes/no question: 1, 2 would be the keys, "No", "Yes" the levels

    raw_key_list <- XML::xpathSApply(xml_data, "//variable[@name]/codes/code", XML::xmlGetAttr, "key")
    raw_label_list <- XML::xpathSApply(xml_data, "//variable[@name]/codes/code", XML::xmlValue)
    # ...therefore we will also need an index to link labesl back to their variable...
    # ...this will be vector of the appropriate break points in raw_key_list and raw_label_list
    label_list_index  <- c(0, cumsum(sapply(raw_vars_with_code, function(x) {
        length(XML::xmlChildren(XML::xmlChildren(x)$codes))
    })))


    #--- (3) convert to data frame....

    if (removeIncompletes == TRUE) {
        if (verbose == TRUE) {
            print("---> Dropping incompletes...")
        }
        # questback completes are numbered 31 and 32
        main_xml_data_cube <- XML::getNodeSet(xml_data, "//row/dispcode[text()=31 or text()=32]/parent::*")
    } else {
        main_xml_data_cube <- XML::getNodeSet(xml_data, "//row")
    }

    if (verbose == TRUE) {
        print("---> Getting raw data (this bit can take a few mins!)")
    }

    main_data <- XML::xmlToDataFrame(main_xml_data_cube, homogeneous = T, nodes = var_names, collectNames = F)

    if (verbose == TRUE) {
        print("  -->time so far (minutes):")
        print((proc.time() - start_time)/ 60)
    }

    # ----- (4) go through each variable....  ---------------------
    # -----   (a) if it has a code, make it into an R factor...
    # -----   (b) if not, convert to the appropriate type...

    if (verbose == TRUE) {
        print("---> Matching labels to values...")
    }

    for (v in var_names) {
        if (v %in% var_names_with_codes) { #--- (a) if has a code....


            #--- get the keys and labels for this variable....

            vi <- which(var_names_with_codes == v)
            this_key <- raw_key_list[(label_list_index[vi] + 1):label_list_index[vi + 1]]
            this_label_list <- raw_label_list[(label_list_index[vi] + 1):label_list_index[vi + 1]]

            #--- sometimes levels will not be in original order...
            main_data[, v] <- factor(main_data[, v],
                                  levels = sort(as.numeric(levels(main_data[, v]))))

            #--- map labels onto values....
            if (sum(table(this_label_list) > 1) > 0) {
                this_label_list <- paste(this_key, this_label_list)
            }
            main_data[, v] <- factor(main_data[, v], levels = this_key,
                                  labels = this_label_list)


        } else {#--- (b) if does not have a code....
            #...then doesn't need to be factor...
            this_var_type <- var_type[which(var_names == v)]
            if (this_var_type %in% c("integer", "decimal")) {
                main_data[, v] <- as.numeric(as.character(main_data[, v]))
            } else if (this_var_type %in% c("character", "blob")) {
                main_data[, v] <- as.character(main_data[, v])
            }
        }
    }

    if (verbose == TRUE) {
        print("  -->time so far (minutes):")
        print((proc.time() - start_time)/ 60)
    }

    if (dropTimeStamps == TRUE) {
        if (verbose == TRUE) {
            print("---> Removing timestamps...")
        }
        main_data <- main_data[, (var_type != "relative timestamp")]
        var_labels <- var_labels[(var_type != "relative timestamp")]
    }

    if (verbose == TRUE) {
        print("---> Finished...")
    }

    if (saveLabels == TRUE) {
        attr(main_data, "labels") <- var_labels
        if (verbose == TRUE) {
            print("   ...to get full labels type something like: attr(df, 'labels')")
        }
    }

    return(main_data)
}
