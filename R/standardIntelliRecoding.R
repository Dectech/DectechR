standardIntelliRecoding <- function(df_raw,
                                    delete_if_flagged = TRUE,
                                    replace_missing_with_NA = TRUE,
                                    convert_char_to_binary = TRUE,
                                    trim_factor_whitespace = TRUE,
                                    showChangedVariables = FALSE) {


    #--- ...[1] remove any flagged as DELETE ---------------.
    if (delete_if_flagged) {

        # Sometimes we end up outputing variables that weren't used (e.g. if some questions about a different industry where left in, but hidden from respondent)

        # The variable lookup can be used to flag any unused variables for deletion...
        # The following will delete any variable that starts with the word "DELETE_"
        #
        # ...We can't have two variables with the same name, so should name them uniquely
        # ...e.g. by appending the word "DELETE_" to the original var name...


        #--- first, identify the variables to remove:
        vars_to_remove = grep("^DELETE_", names(df_raw), value = TRUE)


        if (length(vars_to_remove) > 0) {
            # if there are any...

            #...print how many:
            print(paste0("  -- Removing ",
                         length(vars_to_remove),
                         " variable(s) labeled DELETE_"), quote = F)

            #...drop them from df_raw:
            df_raw = df_raw[,(names(df_raw) %in% vars_to_remove) == F]

            dim(df_raw)

        }
    }




    #---- ...[2] replace missing with NA... -------------------------------.
    if (replace_missing_with_NA) {
        print("  -- Replacing blank entries (\"\") with NA", quote = F)

        # IntelliSurvey doesn't explictly flag NA's
        # instead these are just blank, so we could replace any "" with NAs
        entries_to_replace = c("")

        vars_with_blanks = names(df_raw)[sapply(df_raw, function(x) {
            sum(x %in% entries_to_replace, na.rm = T) > 0
        })]

        if (length(vars_with_blanks) > 0) {
            # cat(paste0("  -- Replacing blank entries with NAs for ",
            #              length(vars_with_blanks), " variable(s):\n    ",
            #            paste0(vars_with_blanks, collapse = "\n    ")))

            for (this_var in vars_with_blanks) {
                df_raw[df_raw[,this_var] %in% entries_to_replace, this_var] = NA
            }

        }

        print(paste0("     --> ", length(vars_with_blanks), " variables changed"), quote = F)

        if (showChangedVariables & length(vars_with_blanks) > 0) {
            cat(paste0("    ", paste0(vars_with_blanks, collapse = "\n    ")))
        }
    }


    #---- ...[3] replace quoted/not quoted  with 0/1... -------------------------------.
    if (convert_char_to_binary) {
        # when data loaded as XML binary responses are recorded as "not quoted"/"quoted"...
        # ...so lets replace these with 0/1
        print("  -- converting character vars to numeric if only 0/1", quote = F)

        # check which questions have only 0/1 non-NA responses

        has_only_01 = apply(df_raw, 2,function(x) {
            all_unique = unique(x[is.na(x) == F])


            # return true if it has non-na values
            # ...and all those values are either 0 or 1
            return(all(all_unique %in% c(0,1)) & length(all_unique) > 0)
        })


        is_character = sapply(df_raw, function(x) {class(x) %in% c("character")})

        has_only_01_and_char = has_only_01 & is_character

        vars_with_only_01 = names(df_raw)[has_only_01_and_char == T]

        if (length(vars_with_only_01) > 0) {

            for (this_var in vars_with_only_01) {
                df_raw[,this_var] = as.numeric(df_raw[,this_var])
            }
        }

        print(paste0("     --> ", length(vars_with_only_01), " variables changed"), quote = F)

        if (showChangedVariables & length(vars_with_only_01) > 0) {
            cat(paste0("    ", paste0(vars_with_only_01, collapse = "\n    ")))
        }
    }



    #---- ...[4] replace any leading or trailing white space in factor levels ---------------.
    if (trim_factor_whitespace) {
        print("  -- Replacing any leading/trailing white space from factor levels e.g. 'Strongly Agree  '", quote = F)
        # Ideally we would avoid these when writing labels in EFS
        # ...but sometimes they are hard to avoid
        # ...so this will remove from every factor variable
        df_raw = replaceFactorLevelWhiteSpace(df_raw,print_changed_var_names = showChangedVariables)
    }

    return(df_raw)
}
