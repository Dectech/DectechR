
dectechXmlToDataframe_FIX <- function(filePath, removeIncompletes = TRUE, saveLabels = TRUE, dropTimeStamps = TRUE, verbose = TRUE, USE_AT_NAME_IN_XPATH = TRUE, checkLabelMismatches = FALSE) {
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
        cat("------- Questback XML to dataframe -----------------\n")
        cat("-   With the following options:                    -\n")
        if (removeIncompletes) {
            cat("-       Remove incompetes: Yes                     -\n")
        } else {
            cat("-       Remove incompetes: No                      -\n")
        }

        if (saveLabels) {
            cat("-       Save labels as an attribute: Yes           -\n")
        } else {
            cat("-       Save labels as an attribute: No            -\n")
        }

        if (dropTimeStamps) {
            cat("-       Drop GP time stamp variables: Yes          -\n")
        } else {
            cat("-       Drop GP time stamp variables: No           -\n")
        }
        cat("----------------------------------------------------\n")
    }




    # ----- (2) load raw xml file and get variable names/ labels/ type  --------

    xml_data <- XML::xmlTreeParse(filePath, useInternalNodes = TRUE,
                              options = (XML::HUGE) | (XML::RECOVER), encoding = "UTF-8")

    if (verbose == TRUE) {
        cat("---> Extracting variable labels...\n")
    }



    if (USE_AT_NAME_IN_XPATH) {
        raw_var_list <- XML::getNodeSet(xml_data, "//variable[@name]")
    } else {
        raw_var_list <- XML::getNodeSet(xml_data, "//variable")
    }

    var_names <- sapply(raw_var_list, function(x) XML::xmlGetAttr(x, "name"))
    var_type <- sapply(raw_var_list, function(x) XML::xmlGetAttr(x, "type"))

    if (USE_AT_NAME_IN_XPATH) {
        var_labels <- XML::xpathSApply(xml_data, "//variable[@name]/label[text()]", XML::xmlValue)
    } else {
        var_labels <- XML::xpathSApply(xml_data, "//variable/label[text()]", XML::xmlValue)
    }



    # some nodes also have a "code" that define the levels of the variable...
    raw_vars_with_code <- XML::getNodeSet(xml_data, "//variable/codes/parent::*")
    var_names_with_codes <- sapply(raw_vars_with_code, function(x) XML::xmlGetAttr(x, "name"))


    # the fastest way to get variable labels is to extract all of them first
    # ...however this will be one long undifferentiated vector of keys and labels
    # NB: keys are the numeric codes that questback uses, labels are the level labels
    #    ....so for a yes/no question: 1, 2 would be the keys, "No", "Yes" the levels

    if (USE_AT_NAME_IN_XPATH) {
        raw_key_list <- XML::xpathSApply(xml_data, "//variable[@name]/codes/code", XML::xmlGetAttr, "key")
        raw_label_list <- XML::xpathSApply(xml_data, "//variable[@name]/codes/code", XML::xmlValue)
    } else {
        raw_key_list <- XML::xpathSApply(xml_data, "//variable/codes/code", XML::xmlGetAttr, "key")
        raw_label_list <- XML::xpathSApply(xml_data, "//variable/codes/code", XML::xmlValue)
    }


    # ...therefore we will also need an index to link labesl back to their variable...
    # ...this will be vector of the appropriate break points in raw_key_list and raw_label_list
    label_list_index  <- c(0, cumsum(sapply(raw_vars_with_code, function(x) {
        length(XML::xmlChildren(XML::xmlChildren(x)$codes))
    })))


    #--- (3) convert to data frame....

    if (removeIncompletes == TRUE) {
        if (verbose == TRUE) {
            cat("---> Dropping incompletes...\n")
        }
        # questback completes are numbered 31 and 32
        main_xml_data_cube <- XML::getNodeSet(xml_data, "//row/dispcode[text()=31 or text()=32]/parent::*")
    } else {
        main_xml_data_cube <- XML::getNodeSet(xml_data, "//row")
    }

    if (verbose == TRUE) {
        cat("---> Getting raw data (this bit can take a few mins!)\n")
    }

    main_data <- XML::xmlToDataFrame(main_xml_data_cube, homogeneous = T, nodes = var_names, collectNames = F)

    if (verbose == TRUE) {
        cat("  -->time so far (minutes):\n")
        print((proc.time() - start_time)/ 60)
    }

    # ----- (4) go through each variable....  ---------------------
    # -----   (a) if it has a code, make it into an R factor...
    # -----   (b) if not, convert to the appropriate type...

    if (verbose == TRUE) {
        cat("---> Matching labels to values...\n")
    }


    unmatch_var_table = data.frame()

    for (v in var_names) {
        if (v %in% var_names_with_codes) { #--- (a) if has a code....

            #--- get the keys and labels for this variable....

            vi <- which(var_names_with_codes == v)
            this_key <- raw_key_list[(label_list_index[vi] + 1):label_list_index[vi + 1]]
            this_label_list <- raw_label_list[(label_list_index[vi] + 1):label_list_index[vi + 1]]

            #--- check variable class
            # ...in older versions this might be a factor already...
            v_data_class = class(main_data[, v])

            if (v_data_class == "factor") {
                #--- sometimes levels will not be in original order...
                main_data[, v] <- factor(main_data[, v],
                                         levels = sort(as.numeric(levels(main_data[, v]))))
            }


            #--- deal with repeated labels by attaching the key
            if (sum(table(this_label_list) > 1) > 0) {
                this_label_list <- paste(this_key, this_label_list)
            }


            APPLY_LABELS = TRUE

            if (checkLabelMismatches == TRUE) {
                #--- check for mismatches between labels key and raw values...

                unique_raw_values = unique(main_data[, v])
                # ignore NAs
                unique_raw_values = unique_raw_values[!is.na(unique_raw_values)]

                common_non_listed_values = c("0","-66","-77","-99")
                additional_values = setdiff(unique_raw_values, c(this_key,common_non_listed_values))

                if (length(additional_values) > 0) {

                    unmatch_var_table = rbind(unmatch_var_table,
                                              data.frame("var" = v,
                                                         "N_expected" = length(this_key),
                                                         "N_observed" = length(unique_raw_values),
                                                         "examples" = paste0(head(additional_values,3),collapse = ", ")))

                    APPLY_LABELS = FALSE
                }
            }

            if (APPLY_LABELS) {
                #--- map labels onto values....
                main_data[, v] <- factor(main_data[, v], levels = this_key,
                                         labels = this_label_list)
            } else {
                # otherwise treat as you would for vars without any codes...
                this_var_type <- var_type[which(var_names == v)]
                if (this_var_type %in% c("integer", "decimal")) {
                    main_data[, v] <- as.numeric(as.character(main_data[, v]))
                } else if (this_var_type %in% c("character", "blob")) {
                    main_data[, v] <- as.character(main_data[, v])
                }
            }




        } else {#--- (b) if does not have a code....
            #...then doesn't need to be factor...
            this_var_type <- var_type[which(var_names == v)]
            if (this_var_type %in% c("integer", "decimal")) {
                main_data[, v] <- as.numeric(as.character(main_data[, v]))
            } else if (this_var_type %in% c("character", "blob")) {
                main_data[, v] <- as.character(main_data[, v])
            }
        }
    } # end of loop through variables

    if (checkLabelMismatches == TRUE) {
        if (nrow(unmatch_var_table) > 0) {
            cat("WARNING: The following variables had values that differed from the expected labels EFS provided (henve EFS labels were ignored:\n")
            print(unmatch_var_table)
        }
    }

    if (verbose == TRUE) {
        cat("  -->time so far (minutes):\n")
        print((proc.time() - start_time)/ 60)
    }

    if (dropTimeStamps == TRUE) {
        if (verbose == TRUE) {
            cat("---> Removing timestamps...\n")
        }
        main_data <- main_data[, (var_type != "relative timestamp")]
        var_labels <- var_labels[(var_type != "relative timestamp")]
    }

    if (verbose == TRUE) {
        cat("---> Finished...\n")
    }

    if (saveLabels == TRUE) {
        attr(main_data, "labels") <- var_labels
        if (verbose == TRUE) {
            cat("   ...to get full labels type something like: attr(df, 'labels')\n")
        }
    }

    return(main_data)
}
