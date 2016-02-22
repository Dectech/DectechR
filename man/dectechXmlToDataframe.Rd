\name{dectechXmlToDataframe}
\alias{dectechXmlToDataframe}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
GlobalPark/QuestBack XML Output to data frame
}
\description{
Converts GlobalPark/QuestBack XML output to an R data frame, converting categorical data to 'factor' varibles.
}
\usage{
dectechXmlToDataframe(filePath, removeIncompletes = TRUE,
                saveLabels = TRUE, dropTimeStamps = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filePath}{
    location of the XML file
}
  \item{removeIncompletes}{
    (optional) parameter to remove incomplete responeses i.e. anything that isn't 31 or 32. Default is to remove, set to FALSE to keep.
}
  \item{saveLabels}{
    (optional) parameter to save extra variable label details as an extra attribute. Default is to save.
}
  \item{dropTimeStamps}{
    (optional) parameter to drop the "rts" time stamp variables at the end of a data file. Default is to drop.
}
}
\details{
    GlobalPark/QuestBack offers a number of output formats for survey data: \cr\cr
    csv is the simplest, and easiest to understand, but will not store information on the scales used in the survey. A scale is either outputted numerically, so rather than "No children", "1 Child", "2 children", it will be stored as "1", "2", "3", which can be confusing. Or there  is an option to output the text of labels, but the ordering is not preserved. So "low", "medium", "high" is outputted, but if you make a table it will be in alphabetical order.\cr\cr
    Outputing in SPSS format, and then converting using the "foreign" package gets around the problems with raw csv. However there are a few issues with  this method: certain data types/character strings can cause errors; longer strings will get cut short; and if one level of a variable was never selected that level will not be retained (this becomes an issue if you are merging data from 2 or more surveys, as the levels may no longer match).\cr\cr
    XML is an open format that, like SPSS format, retains the  data variable labels, but which allows us to avoid the issues introduced by converting via the "foreign" package.\cr\cr
    The main drawbacks of the XML output is that the file is larger than the other formats (but can be zipped down to a much smaller file), and the actual layout of the data within the file is unintuitive, hence the need for this function to convert it into a sensible format.\cr\cr
    Currently this conversion process can take a while (a minute or two for very large files), but we will look into making it more efficient.

}
\value{
    Returns a data frame
}
\author{
Keith Simpson
}

\seealso{
    See also \code{\link{read.csv}}, for reading csv files, and the "foreign" package for reading SPSS and Stata data files.
}
\examples{

\dontrun{

# in most cases can just run as:
df = dectechXmlToDataframe("C:/.../data_project_1234_2016_01_01.xml")

# but if you wish to retain incomplete respondents and keep time data you would run
df = dectechXmlToDataframe("C:/.../data_project_1234_2016_01_01.xml",
                        removeIncompletes = FALSE, dropTimeStamps = FALSE)

    }
}
\keyword{ globalpark }
\keyword{ questback }
\keyword{ xml }
