cc <- function(data, destination = NA, includeRowNames = FALSE, nestedOrderOutToIn = TRUE, forceNested = FALSE) {
    ####################################################
    ### Fucntion to write an object to the clipboard ###
    ###  ...making allowance for nested tables       ###
    ####################################################

    #--(1) If file name given, write to file, otherwise write to clipboard...
    if (!is.na(destination)) {

        # if file name given then write to file...
        write.table(data, destination, sep = "\t", row.names = FALSE)

    } else {
        #---(2) will use the "writeClipboard" function, however...
        # ...this function only takes strings, so need to format data

        # if desired, save the row names...
        if (includeRowNames == TRUE) {
            original_row_names <- rownames(data)
        }


        #---(3) we will treat nested tables differently, so check status..
        is_nested_table <- FALSE
        if ((class(data)[1] == "table") | forceNested == TRUE) {
            # only care if there is more than one dimension
            if (length(dim(data)) > 1) {
                is_nested_table <- TRUE
            }
        }

        #---(4) convert all objects to dataframes if not already
        if (class(data)[1] != "data.frame") {
            data <- as.data.frame(data)
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
