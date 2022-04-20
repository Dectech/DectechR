cc <- function(data, destination = NA, includeRowNames = FALSE, nestedOrderOutToIn = TRUE, forceNested = FALSE) {
    ####################################################
    ### Fucntion to write an object to the clipboard ###
    ###  ...making allowance for nested tables       ###
    ####################################################

    #--- get input variable name, in case we want to use later...
    raw_input_data_name <-deparse(substitute(data))

    #-- check if this is just a single object...
    is_single_item <- (is.null(dim(data))) & (length(data) == 1)

    #--(1) If file name given, write to file, otherwise write to clipboard...
    if (!is.na(destination)) {

        # if file name given then write to file...
        write.table(data, destination, sep = "\t", row.names = FALSE)

    } else if (is_single_item == TRUE) {
        # if this is just a single number or a string, then send it straight to clipboard...
        writeClipboard(as.character(data))

    } else {



        #---(2) will use the "writeClipboard" function, however...
        # ...this function only takes strings, so need to format data

        # if desired, save the row names...
        if (includeRowNames == TRUE) {
            original_row_names <- rownames(data)
            if (is.null(original_row_names)) {
                original_row_names <- names(data)
            }
        }


        #---(3) we will treat nested tables differently, so check status..
        is_nested_table <- FALSE
        if ("table" %in% class(data)) {
            # only care if there is more than one dimension
            if (length(dim(data)) > 1) {
                is_nested_table <- TRUE
            }

        } else if (forceNested == TRUE) { # if not a table, but want to force nesting...
            # ...can only nest if have more that two additional columns
            if (ncol(data) > 2) {
                is_nested_table <- TRUE
            }
        }

        #---(4) convert all objects to dataframes if not already
        if (class(data)[1] != "data.frame") {
            original_class = class(data)[1]
            data <- as.data.frame(data)

            #as.data.frame(table(mtcars$vs))
            #class(table(mtcars$vs))
            # i

            #--- if there is only one column of data...
            #class(mtcars$mpg)
            #class(as.Date(mtcars$carb))

            if (ncol(data) == 1) {
                #--- if the original wasn't a table or data frame, attempt to get the original name of it...
                if (original_class %in% c("numeric", "character","integer","factor")) {

                    # if this uses the default col name, then try and replace with a better one...
                    if (names(data) == "data") {
                        #print(raw_input_data_name)
                        this_name = raw_input_data_name

                        # if its in a simple recognisable format... then replace....

                        #--- FORMAT A:   abc$var
                        # example: raw_input_data_name = "abc_ABC.12$as1231"
                        if (grepl("^[a-zA-Z0-9_.]*\\$[a-zA-Z0-9_.]*$",raw_input_data_name)) {
                            this_name = gsub("^([a-zA-Z0-9_.]*)\\$([a-zA-Z0-9_.]*)$","\\2",raw_input_data_name)
                        }

                        #--- FORMAT B:   abc[,"var"] or abc[,'var']
                        # example: raw_input_data_name = "abc_ABC.12[,\"as1231\"]"
                        #raw_input_data_name = "iris[, \"Species\"]"
                        # grepl("^[a-zA-Z0-9_.]*\\[, ?.\"",raw_input_data_name)

                        # grepl("^[a-zA-Z0-9_.]*\\[, ?.\"",raw_input_data_name)
                        # #      ....raw_input_data_name = "abc_ABC.12[,'as1231']"
                        if (grepl("^[a-zA-Z0-9_.]*\\[, ?.[\"\'][a-zA-Z0-9_.]*[\"\']\\]$",raw_input_data_name)) {
                            this_name = gsub("^([a-zA-Z0-9_.]*)\\[, ?.[\"\']([a-zA-Z0-9_.]*)[\"\']\\]$","\\2",raw_input_data_name)
                        }



                        names(data) = this_name
                    }


                } else {
                    print(paste0("DEBUGING NOTE: input class is ", original_class))
                }
            }



        }




        #---(5) for multi dimensional nested tables...
        #  ...want to reshape into usable format
        if (is_nested_table) {
            nvars <- length(data)

            # create an extra header row, so show name of variable that will span columns
            extra_row <- paste(array("\t", nvars - 2), collapse = "")
            extra_row <- paste0(extra_row, names(data)[nvars - 1])


            #-- reshape so that last variable is split into columns...
            # first get some details about current shape...
            num_cols <- ncol(data)
            value_names <- names(data)[num_cols]
            horizontal_variable_name <- names(data)[num_cols - 1]
            horizontal_variable_levels <- as.character(unique(data[, horizontal_variable_name]))
            vertical_variable_names <- names(data)[-c(num_cols - 1, num_cols)]

            # now reshape so "horizontal_variable_name" is switched to wide format...
            data <- reshape(data, v.names = value_names, timevar = horizontal_variable_name,
                            idvar = vertical_variable_names, direction = "wide")

            if (nestedOrderOutToIn == TRUE) {
                for (v in vertical_variable_names[length(vertical_variable_names):1]) {
                    data <- data[order(data[, v]), ]
                }

            }
            # ...and use orginal levels for column headings
            names(data) <- c(vertical_variable_names, horizontal_variable_levels)

        }

        #---(6) get column headings for table...

        col_headings <- paste(names(data), collapse = "\t")
        if (is_nested_table) {
            col_headings <- append(extra_row, col_headings)
        }

        #---(7) Go through every row, make it a string sep. by tabs..
        output_vector <- apply(data, 1, paste, collapse = "\t")
        output_vector <- append(col_headings, output_vector)

        #--- if desired, include original row names....
        if ((includeRowNames == TRUE) & (is_nested_table == FALSE)) {
            num_extra_rows <- length(output_vector) - length(original_row_names)
            original_row_names <- append(rep("", num_extra_rows), original_row_names)
            output_vector <- paste0(original_row_names, "\t", output_vector)
        }



        #---(8) then write to clipboard
        writeClipboard(output_vector)
    }
}

cc2 <- cc


cc_varlist <- function(var_list, is_formula = F, separate_lines = T) {
    ###########################################################
    ### Function to write a vector of strings to clipboard ###
    ###  ...but nicely formatted for pasting into code     ###
    ##########################################################

    # the output can either have an item on each line...
    # ...or everything on the same line
    if (separate_lines == T) {
        item_sep = "\n"
    } else {
        item_sep = ""
    }
    # Can output either as a list of quoted strings e.g.
    #    "string 1",
    #    "string 2",
    #    "string 3"
    #
    # or as a list of vars for a formula e.g.
    #    + string 1
    #    + string 2
    #    + string 3

    if (is_formula == F) {
        writeClipboard(paste0("\"",
                              paste0(var_list,
                                     collapse = paste0("\", ",item_sep,"\"")),
                              "\""))

    } else if (is_formula == T) {
        writeClipboard(paste0(" + ",
                              paste0(var_list,
                                     collapse = paste0(item_sep, " + "))
        ))
    }
    print("...var list copied to clipboard...")
}
