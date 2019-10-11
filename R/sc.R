sc <- function(pattern, dataframe, value = TRUE, ignore.case = FALSE, ...){
    ### very simple wrapper to search column names of a data frame for a string ###
    searchResult <- grep(pattern,colnames(dataframe),value=value, ignore.case=ignore.case, ...)
    return(searchResult)
}


