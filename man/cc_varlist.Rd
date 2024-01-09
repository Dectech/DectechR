\name{cc_varlist}
\alias{cc_varlist}
\title{
    %%  ~~function to do ... ~~
        Copy a list of strings to Clipboard
}
\description{
    %%  ~~ A concise (1-5 lines) description of what the function does. ~~
        Copy a list of strings to Clipboard in a nice format e.g. with quotes and a comma, so that it can be easily insterted into a script.
}
\usage{
    cc_varlist(var_list, is_formula = FALSE, separate_lines = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{var_list}{
        %%     ~~Describe \code{dframe} here~~
            A character vector
    }
    \item{is_formula}{
        %%     ~~Describe \code{myfile} here~~
            (optional) if FALSE (default) then returns each item in quotes. If TRUE then returns each item seperated by a '+' for use in a formula
    }
    \item{separate_lines}{
        (optional) if TRUE (default) then each item is output to a seperate line. If FALSE then it is output as a single line.
}

}
\details{
    A quick and simple way to get a character vector into a script in a useful format.\cr\cr

}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
    See also \code{\link{cc}} for more general output
}
\examples{

example_var_list = names(mtcars)

# Copy the list, and format as required
cc_varlist(example_var_list)
# ...Now paste into your script



    }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ copy }
\keyword{ clipboard }% __ONLY ONE__ keyword per line
