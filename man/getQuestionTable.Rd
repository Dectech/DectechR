\name{getQuestionTable}
\alias{getQuestionTable}
\alias{getQuestionLabel}
\title{Get IntelliSurvey question labels}
\description{
Retrieve the question lookup table or the question text for selected variables
from an IntelliSurvey data frame.
}
\usage{
getQuestionTable(df)

getQuestionLabel(df, var_name, use_internal_name = FALSE)
}
\arguments{
  \item{df}{A data frame returned by \code{\link{loadIntelliSurveyTripleS}},
  with its \code{var.labels} attribute.}
  \item{var_name}{A character vector of variable names to look up.}
  \item{use_internal_name}{Logical. If \code{TRUE}, look up names in
  \code{internal_var_name} rather than \code{var_name}.
  Defaults to \code{FALSE}.}
}
\value{
\code{getQuestionTable()} returns a data
frame with columns \code{internal_var_name} (the variable name you will see in IntelliSurvey),
\code{var_name} (the var name in the exported data) and \code{label}
(the question text). \code{internal_var_name} and \code{var_name} will be the same unless you have changed them on the IntelliSurvey platform (e.g. via a data map)

\code{getQuestionLabel()} returns a character vector of question labels in
the order requested, with \code{NA} for names not found in the lookup table.
}
\seealso{
\code{\link{loadIntelliSurveyTripleS}},
\code{\link{standardIntelliRecoding}}
}
\examples{
\dontrun{
df_raw = loadIntelliSurveyTripleS("data.zip")
df_raw = standardIntelliRecoding(df_raw)

question_table = getQuestionTable(df_raw)
head(question_table)
#   internal_var_name      var_name                      label
# 1          trans_id            id                  Record ID
# 2             panel         panel                      Panel
# 3            status        status                     Status
# 4     resp_start_ts resp_start_ts Respondent start timestamp
# 5      resp_last_ts  resp_last_ts  Respondent last timestamp
# 6          language      language            Survey language

getQuestionLabel(df_raw, "Age")
# [1] "How old are you?"

getQuestionLabel(df_raw, "Q0_1", use_internal_name = TRUE)
# [1] "How old are you?"

}
}
\keyword{utilities}
