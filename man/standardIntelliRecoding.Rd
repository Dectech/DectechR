\name{standardIntelliRecoding}
\alias{standardIntelliRecoding}
\title{Standard recoding of IntelliSurvey data}
\description{
Applies common cleaning and recoding steps to an IntelliSurvey data frame,
typically after importing it with \code{\link{loadIntelliSurveyTripleS}}.
}
\usage{
standardIntelliRecoding(df_raw,
                       delete_if_flagged = TRUE,
                       replace_missing_with_NA = TRUE,
                       convert_char_to_binary = TRUE,
                       convert_char_to_numeric = FALSE,
                       remove_incompletes = TRUE,
                       remove_test_ids = TRUE,
                       trim_factor_whitespace = TRUE,
                       showChangedVariables = FALSE)
}
\arguments{
  \item{df_raw}{A data frame, typically returned by
  \code{\link{loadIntelliSurveyTripleS}}. Must contain \code{status} when
  \code{remove_incompletes = TRUE}, and \code{testmode} when
  \code{remove_test_ids = TRUE}.}
  \item{delete_if_flagged}{remove columns whose names
  begin with \code{DELETE_} (case-sensitive). Defaults to \code{TRUE}.}
  \item{replace_missing_with_NA}{replace empty strings
  (\code{""}) with \code{NA}. Strings containing only whitespace are not
  replaced by this step. Defaults to \code{TRUE}.}
  \item{convert_char_to_binary}{convert character
  columns to numeric when all non-missing values are \code{"0"} or
  \code{"1"}. At least one non-missing value is required; columns containing
  only one of these values also qualify. Factor columns are not converted by
  this step. Defaults to \code{TRUE}.}
  \item{convert_char_to_numeric}{convert character
  columns to numeric when all non-missing, non-blank values can be parsed as
  numbers. At least one such value is required. Accepts signed numbers,
  decimals, scientific notation and valid comma-separated thousands
  (for example, \code{"1,000.5"}). Leading and trailing whitespace is ignored.
  Empty or whitespace-only strings become \code{NA} in converted columns,
  even when \code{replace_missing_with_NA = FALSE}. Factor columns are not
  converted. Defaults to \code{FALSE}.}
  \item{remove_incompletes}{retain only rows where
  \code{status} is exactly \code{"Completed"}. Other values, including
  \code{NA}, are removed. Defaults to \code{TRUE}.}
  \item{remove_test_ids}{retain only rows where
  \code{testmode} is exactly \code{"No"}. Other values, including \code{NA},
  are removed. Defaults to \code{TRUE}.}
  \item{trim_factor_whitespace}{remove leading and
  trailing whitespace from factor levels using \code{\link{trimws}}.
  Levels that become identical are merged. Character columns are not trimmed
  by this step. Defaults to \code{TRUE}.}
  \item{showChangedVariables}{also print the names of
  variables changed by the missing-value replacement, type conversion and
  factor-level trimming steps. Progress summaries are printed regardless of
  this setting. Defaults to \code{FALSE}.}
}
\details{
The import and recoding functions are intended to be used together: first
read the TripleS export, then assign the recoded result back to the data frame.
Each cleaning option can be disabled independently.

General numeric conversion runs after binary conversion and can also convert
character columns containing only zeros and ones. To preserve these columns
as character, set both \code{convert_char_to_binary = FALSE} and
\code{convert_char_to_numeric = FALSE}. Numeric conversion also removes
leading zeros from numeric-looking strings, such as respondent IDs.

Column deletion and value conversion take place before row filtering.
Factor levels are trimmed last, so the \code{status} and \code{testmode}
values must match the required labels before trimming. Disable the
corresponding filtering options if these columns are unavailable or if
incomplete or test responses should be retained.
}
\value{
The recoded data frame, with columns and rows removed according to the
selected options. Assign the return value to keep the changes.
}
\author{Keith Simpson}
\seealso{
\code{\link{loadIntelliSurveyTripleS}} for importing IntelliSurvey TripleS
exports.
}
\examples{
\dontrun{
df_raw = loadIntelliSurveyTripleS("data.zip")
df_raw = standardIntelliRecoding(df_raw)

# Retain incomplete and test responses, and list changed variables:
df_raw = loadIntelliSurveyTripleS("data.zip")
df_raw = standardIntelliRecoding(df_raw,
                               remove_incompletes = FALSE,
                               remove_test_ids = FALSE,
                               showChangedVariables = TRUE)
}
}
\keyword{manip}
