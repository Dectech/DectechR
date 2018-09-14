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

    userAgent <- getUserAgentString(df, useragent)

    OS = rep("Other",length(userAgent))

    OS[grep("Windows NT", userAgent)] <- "Windows"
    OS[grep("iPhone", userAgent)] <- "iPhone"
    OS[grep("iPad", userAgent)] <- "iPad"
    OS[grep("Android", userAgent)] <- "Android"
    OS[grep("Macintosh", userAgent)] <- "Mac"
    OS[OS == "Other"][grep("Linux", userAgent[OS == "Other"])] <- "Linux"

    OS[grep("BlackBerry",userAgent)] <- "BlackBerry"
    OS[grep("RIM Tablet",userAgent)] <- "BlackBerry"
    OS[grep("Windows Phone",userAgent)] <- "WindowsPhone"
    OS[grep("X11",userAgent)] <- "Linux"

    if (detailed == TRUE) {
        OS[grep("Windows NT 10",userAgent)] <- "Windows 10"
        OS[grep("Windows NT 6.3",userAgent)] <- "Windows 8"
        OS[grep("Windows NT 6.2",userAgent)] <- "Windows 8"
        OS[grep("Windows NT 6.1",userAgent)] <- "Windows 7"
        OS[grep("Windows NT 6.0",userAgent)] <- "Windows Vista"
        OS[grep("Windows NT 5.2",userAgent)] <- "Windows XP"
        OS[grep("Windows NT 5.1",userAgent)] <- "Windows XP"
    }

    return(OS)
}

# convert the user agent into a list of browsers
getRespondentBrowser <- function(df = NULL, useragent = NULL){

    userAgent <- getUserAgentString(df, useragent)

    browserID = rep("Other",length(userAgent))

    browserID[grep("MSIE 6.0", userAgent)] <- "IE6"
    browserID[grep("MSIE 7.0", userAgent)] <- "IE7"
    browserID[grep("MSIE 8.0", userAgent)] <- "IE8"
    browserID[grep("MSIE 9.0", userAgent)] <- "IE9"
    browserID[grep("MSIE 10.0", userAgent)] <- "IE10"
    browserID[grep("Trident/7.0", userAgent)] <- "IE11"

    # NB: Chrome user agents contains "safari"...
    # ...so code all as safari first, then recode as chrome
    browserID[grep("Safari", userAgent)] <- "Safari"
    browserID[grep("Firefox", userAgent)] <- "Firefox"
    browserID[grep("Chrome", userAgent)] <- "Chrome"
    browserID[grep("Edge", userAgent)] <- "Edge"
    browserID[grep("IEMobile", userAgent)] <- "IEMobile"

    return(browserID)
}


addRespondentPlatformVars <- function(df_in, detailed = TRUE, varname = NULL) {
    if (!("data.frame" %in% class(df_in))) {
        stop("Please enter a data frame")
    }

    if (is.null(varname)) {
        useragent = getUserAgentString(df = df_in)
    } else {
        useragent = df_in[,varname]
    }
    df_in$operating_system <- getRespondentOS(useragent = useragent, detailed = detailed)
    df_in$is_mobile <- (df_in$operating_system %in% c("Android","iPad","iPhone")) * 1
    df_in$web_browser <- getRespondentBrowser(useragent = useragent)
    message("Added the following variables to dataframe:")
    message("    - operating_system")
    message("    - is_mobile")
    message("    - web_browser")

    return(df_in)
}

addMobile <- addRespondentPlatformVars

