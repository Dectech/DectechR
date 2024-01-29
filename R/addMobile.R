getUserAgentString <- function(df = NULL, useragent = NULL){

    # if a dataframe hasn't been entered, look for a useragent...
    if (is.null(df)) {
        # ...if that hasn't been entered either, throw an error...
        if (is.null(useragent)) {
            stop("enter either a data frame or a user agent string")
        }
    } else {
        # if df isn't null...
        # ...make sure it is a dataframe...
        if (!("data.frame" %in% class(df))) {
            stop("df entered is not a data frame. If you entered a user agent string, use the parameter name 'useragent'")
        }
        #...make sure df contains the browser, key word...
        if (!("browser" %in% names(df))) {
            stop("couldn't find the 'browser' variable in this dataframe. If user agent is stored under a different variable name, use the additional input parameters.")
        }

        if (!is.null(useragent)) {
            warning("You entered a dataframe and a useragent. Will ignore useragent, and use dataframe")
        }

        #...if all else is fine then...
        useragent <- df$browser
    }

    return(useragent)
}



getRespondentOS <- function(df = NULL, useragent = NULL, detailed = TRUE){

    this_useragent <- getUserAgentString(df, useragent)

    OS = rep("Other",length(this_useragent))

    OS[grep("Windows NT", this_useragent)] <- "Windows"
    OS[grep("iPhone", this_useragent)] <- "iPhone"
    OS[grep("iPad", this_useragent)] <- "iPad"
    OS[grep("Android", this_useragent)] <- "Android"
    OS[grep("Macintosh", this_useragent)] <- "Mac"
    OS[OS == "Other"][grep("Linux", this_useragent[OS == "Other"])] <- "Linux"

    OS[grep("BlackBerry",this_useragent)] <- "BlackBerry"
    OS[grep("RIM Tablet",this_useragent)] <- "BlackBerry"
    OS[grep("Windows Phone",this_useragent)] <- "WindowsPhone"
    OS[grep("X11",this_useragent)] <- "Linux"

    if (detailed == TRUE) {
        OS[grep("Windows NT 10",this_useragent)] <- "Windows 10"
        OS[grep("Windows NT 6.3",this_useragent)] <- "Windows 8"
        OS[grep("Windows NT 6.2",this_useragent)] <- "Windows 8"
        OS[grep("Windows NT 6.1",this_useragent)] <- "Windows 7"
        OS[grep("Windows NT 6.0",this_useragent)] <- "Windows Vista"
        OS[grep("Windows NT 5.2",this_useragent)] <- "Windows XP"
        OS[grep("Windows NT 5.1",this_useragent)] <- "Windows XP"
    }

    return(OS)
}

# convert the user agent into a list of browsers
getRespondentBrowser <- function(df = NULL, useragent = NULL){

    this_useragent <- getUserAgentString(df, useragent)

    browser_id = rep("Other",length(this_useragent))

    browser_id[grep("MSIE 6.0", this_useragent)] <- "IE6"
    browser_id[grep("MSIE 7.0", this_useragent)] <- "IE7"
    browser_id[grep("MSIE 8.0", this_useragent)] <- "IE8"
    browser_id[grep("MSIE 9.0", this_useragent)] <- "IE9"
    browser_id[grep("MSIE 10.0", this_useragent)] <- "IE10"
    browser_id[grep("Trident/7.0", this_useragent)] <- "IE11"

    # NB: Chrome user agents contains "safari"...
    # ...so code all as safari first, then recode as chrome
    browser_id[grep("Safari", this_useragent)] <- "Safari"
    browser_id[grep("Firefox", this_useragent)] <- "Firefox"
    browser_id[grep("Chrome", this_useragent)] <- "Chrome"
    browser_id[grep("Edge", this_useragent)] <- "Edge"
    browser_id[grep("IEMobile", this_useragent)] <- "IEMobile"

    return(browser_id)
}


# addRespondentPlatformVars <- function(df_in, detailed = TRUE, varname = NULL) {
#     if (!("data.frame" %in% class(df_in))) {
#         stop("Please enter a data frame")
#     }
#
#     if (is.null(varname)) {
#         useragent = getUserAgentString(df = df_in)
#     } else {
#         useragent = df_in[,varname]
#     }
#     df_in$operating_system <- getRespondentOS(useragent = useragent, detailed = detailed)
#     df_in$is_mobile <- (df_in$operating_system %in% c("Android","iPad","iPhone")) * 1
#     df_in$web_browser <- getRespondentBrowser(useragent = useragent)
#     cat("Added the following variables to the dataframe:\n")
#     cat("    - operating_system\n")
#     cat("    - is_mobile\n")
#     cat("    - web_browser\n")
#
#     return(df_in)
# }

addMobile <- function(df_in, varname = NULL) {
    if (!("data.frame" %in% class(df_in))) {
        stop("Please enter a data frame")
    }

    if (is.null(varname)) {
        useragent = getUserAgentString(df = df_in)
    } else {
        useragent = df_in[,varname]
    }
    operating_system = getRespondentOS(useragent = useragent, detailed = TRUE)
    df_in$is_mobile <- (operating_system %in% c("Android","iPad","iPhone")) * 1

    cat("Added the variable 'is_mobile' to the dataframe\n")

    return(df_in)
}

