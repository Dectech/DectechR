\name{addTotalDuration}
\alias{addTotalDuration}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Add total duration to complete
}
\description{
Adds a total duration in minues to survey data based on the start and end time
}
\usage{
addTotalDuration(df,
                 start_time = "resp_start_ts",
                 end_time = "resp_last_ts")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
    input data frame, which should contain a start and end date-time
}
  \item{start_time}{
    (optional) variable name containing the survey start time (by default will look for "resp_start_ts")
}
  \item{end_time}{
    (optional) variable name containing the survey time (by default will look for "resp_end_ts")
}
}
\details{
    This assumes the start and end date are in the format "YYYY-MM-DD HH:MM:SS"
}
\value{
    Returns the original data frame with the variable \code{total_duration_mins} added. \cr\cr
}
\author{
Keith Simpson
}

\examples{

\dontrun{
df = addTotalDuration(df)
    }
}

