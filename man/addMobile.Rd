\name{addMobile}
\alias{addMobile}
\title{
    %%  ~~function to do ... ~~
        Add is_mobile variable to a data.frame. Returns the same data.frame with the added variable.
}
\description{
    %%  ~~ A concise (1-5 lines) description of what the function does. ~~
        Takes in a data.frame and creates an "is_mobile" flag. The dataframe must contain a user agent string (this is outputted by default by EFS/Questback, and is usually called "browser"). NB: The is_mobile variable is an approximate guess, it is not 100\% accurate.
}
\usage{
    df = addMobile(df, varname = NULL)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{df}{
        %%     ~~Describe \code{dframe} here~~
            A dataframe from Questback. By default Questback output should contain a 'browser' variable. If not use \code{varname} to identify the variable containing the 'user agent string'.
    }

    \item{varname}{
        (optional) By default the dataframe should contain a 'browser' variable corresponding to the respondents user agent string. If not, then use this parameter to declare the variable containing the user agent string.
    }
}
\details{
    Use this function to determine if the respondent was using mobile or not. This may be an important predictor in your analysis as the questions can appear differently across devices.\cr\cr

    All Questback surveys should store a respondents user agent string. It is usually stored in the "browser" variable. The user agent string is a peice of text that each browser gives to the websites it visits. It contains some information about their operating system and their browser version. \cr\cr
    The following is an example user agent string:\cr
    \code{Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/69.0.3497.92 Safari/537.36}\cr
    It tells you that this respodent was using Windows 10 ("Windows NT 10.0") and Chrome v69.\cr\cr

}


%% ~Make other sections like Warning with \section{Warning }{....} ~
\section{Warning }{
    User agent strings can easily manipulated and you can ask your browser to 'pretend' to be a different browser (e.g. mobile browsers often give the option to display a site as it would appear as a desktop, for this they send a desktop-like user agent). Also the formats may change over time (as browsers and operating systems are updated), so the variables produced by this code shouldn't be considered 100\% accurate.
}

\examples{

# first lets create a small data set with a user agent variable:
df_test = data.frame("lfdn" = c(1,2,3,4), "browser" = c(
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/68.0.3440.106 Safari/537.36",
    "Mozilla/5.0 (iPad; CPU OS 11_4 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/11.0 Mobile/15E148 Safari/604.1",
    "Mozilla/5.0 (Linux; Android 7.0; LG-M200 Build/NRD90U) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/68.0.3440.91 Mobile Safari/537.36",
    "Mozilla/5.0 (Windows NT 6.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/68.0.3440.106 Safari/537.36"))

# to add the new variables, just run:
df_test <- addMobile(df_test)



}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ mobile }
\keyword{ browser }
\keyword{ operating system }% __ONLY ONE__ keyword per line
