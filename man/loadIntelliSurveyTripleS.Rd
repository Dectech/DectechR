\name{loadIntelliSurveyTripleS}
\alias{loadIntelliSurveyTripleS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
IntelliSurvey TripleS output to data frame
}
\description{
Converts IntelliSurvey TripleS output to an R data frame, converting categorical data to 'factor' varibles.
}
\usage{
loadIntelliSurveyTripleS(zip_filename = NULL,
                         sss_filename = NULL,
                         csv_filename = NULL,
                         add_total_duration = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{zip_filename}{
    location of the TripleS file, in .zip format (this is the easiest format to use as its just one file)
}
  \item{sss_filename}{
    (optional) location of .sss file, if you want to load seperately (if so, you must also provide a .csv file)
}
  \item{csv_filename}{
    (optional) location of .csv file, if you want to load seperately (if so, you must also provide a .sss file)
}
  \item{add_total_duration}{
    (optional) use the start and end date to add a total duration (assumes the df has 'resp_start_ts' and 'resp_last_ts')
}
}
\details{
    IntelliSurvey offers a number of output formats for survey data: \cr\cr
    csv is the simplest, and easiest to understand, but will not store information on the scales used in the survey. A scale is either outputted numerically, so rather than "No children", "1 Child", "2 children", it will be stored as "1", "2", "3", which can be confusing (as in this example "2" = "1 Child". Or there  is an option to output the text of labels, but the ordering is not preserved. So "low", "medium", "high" is outputted, but if you make a table it will be in alphabetical order ("high","low","medium").\cr\cr
    Outputing in SPSS format, and then converting using the "foreign" package gets around the problems with raw csv. However there are a few issues with  this method: certain data types/character strings can cause errors; longer strings will get cut short; and if one level of a variable was never selected that level will not be retained (this becomes an issue if you are merging data from 2 or more surveys, as the levels may no longer match).\cr\cr
    TripleS is an open format that, like SPSS format, retains the  data variable labels, but which allows us to avoid the issues introduced by converting via the "foreign" package.\cr\cr
    It works by including a variable lookup file (.sss) alongside a raw data file (.csv). The csv is loaded and the lookup is used to .

}
\value{
    Returns a data frame. \cr\cr
    Attached to the data frame are two lookup tables that might be of use.\cr\cr
    After running \code{df = loadIntelliSurveyTripleS("exported_data.zip")}, if you run \code{attr(df, "var.labels")} it will return a table showing variable names and question text.\cr\cr
    Running \code{attr(df, "label.table")} it will return a large table of all the answer options.
}
\author{
Keith Simpson
}

\seealso{
    See also \code{\link{read.csv}}, for reading csv files, and the "foreign" package for reading SPSS and Stata data files.
}
\examples{

\dontrun{


df = loadIntelliSurveyTripleS("exported_data.zip")

# to get a table showing variable names and question text run:
attr(df, "var.labels")


    }
}

\keyword{ globalpark }
\keyword{ questback }
\keyword{ xml }
