\name{getUserAgentString}
\alias{getRespondentOS}
\alias{getRespondentBrowser}
\alias{getUserAgentString}
\title{
    %%  ~~function to do ... ~~
        User agent string functions
}
\description{
    %%  ~~ A concise (1-5 lines) description of what the function does. ~~
        Extract the operating system and browser a respondent is using based on their user agen string. NB: These variables are not 100\% accurate
}
\usage{
    getRespondentOS(df = NULL, useragent = NULL, detailed = TRUE)
    getRespondentBrowser(df = NULL, useragent = NULL)
    getUserAgentString(df = NULL, useragent = NULL)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{df}{
        %%     ~~Describe \code{dframe} here~~
            (optional) A dataframe from Questback. By default Questback output should contain a 'browser' variable. If not use \code{useragent} to enter the 'user agent string' as a character vector instead.
    }

    \item{useragent}{
        (optional) If df isn't supplied, then enter the 'user agent string' as a character vector.
    }
    \item{detailed}{
        (optional) By default the \code{operating_system} variable returned will split windows OS into the seperate versions (e.g. XP, Vista, 7, 10). If \code{detailed = FALSE} then all windows OS will be combined.
    }
}
\details{
    \code{getRespondentOS} returns a character vector of the estimated operating system for each respondent. By default Windows operating systems are split into versions e.g. XP, Vista, 7, 10) If \code{detailed = FALSE} then all windows OS will be combined.\cr\cr
    \code{getRespondentBrowser} returns a character vector of the estimated browser for each respondent.\cr\cr
    \code{getUserAgentString} checks the entered data contains a user agent string, and returns that string. This is used in \code{getRespondentOS} and \code{getRespondentBrowser}.\cr\cr


    These functions generally shouldn't be used directly. Instead it is recommend to use \code{addRespondentPlatformVars} or \code{addMobile}, which will call these functions and add the new variables to the data frame.\cr\cr

    All Questback surveys should store a respondents user agent string. It is usually stored in the "browser" variable. The user agent string is a peice of text that each browser gives to the websites it visits. It contains some information about their operating system and their browser version. \cr\cr
    The following is an example user agent string:\cr
    \code{Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/69.0.3497.92 Safari/537.36}\cr
    It tells you that this respodent was using Windows 10 ("Windows NT 10.0") and Chrome v69.\cr\cr



}


%% ~Make other sections like Warning with \section{Warning }{....} ~
\section{Warning }{
    User agent strings can easily manipulated (e.g. you can ask your browser to 'pretend' to be a different browser) and the format may change over time (as browsers and operating systems are updated), so the variables produced by this code shouldn't be considered 100\% accurate.
}

\examples{

# first lets create a small data set with a user agent variable:
df_test = data.frame("lfdn" = c(1,2,3,4), "browser" = c(
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/68.0.3440.106 Safari/537.36",
    "Mozilla/5.0 (iPad; CPU OS 11_4 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/11.0 Mobile/15E148 Safari/604.1",
    "Mozilla/5.0 (Linux; Android 7.0; LG-M200 Build/NRD90U) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/68.0.3440.91 Mobile Safari/537.36",
    "Mozilla/5.0 (Windows NT 6.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/68.0.3440.106 Safari/537.36"))

# now get the OS:
operating_system <- getRespondentOS(df_test)
table(operating_system)

# and the browser:
web_browser <- getRespondentBrowser(df_test)
table(web_browser)

# alternatively, the following will give the same result:
operating_system <- getRespondentOS(useragent = df_test$browser)
web_browser <- getRespondentBrowser(useragent = df_test$browser)


# Consider the operating_system variable created from the above code...
operating_system
# [1] "Windows 10" "iPad"       "Android"    "Windows 7"

# ...if you want to merge the Windows versions:
operating_system <- getRespondentOS(df_test, detailed = FALSE)

# ...the operating_system variable now looks like:
operating_system
# [1] "Windows" "iPad"       "Android"    "Windows"

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ mobile }
\keyword{ browser }
\keyword{ operating system }% __ONLY ONE__ keyword per line
