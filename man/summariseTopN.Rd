% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/formatting.R
\name{summariseTopN}
\alias{summariseTopN}
\title{Truncate a dplyr summarisation using an "Other..." summary row}
\usage{
summariseTopN(df, n, sortVar, label = "Other...", ...)
}
\arguments{
\item{df}{- a grouped df - there must be some grouping for the summarise to return more than one row.}

\item{n}{the number of rows to keep - the result will have one more row than this}

\item{sortVar}{- the column containing the variable to sort by (syntax as in arrange(...))}

\item{label}{-  the name of the "Other..." row}

\item{...}{- the arguments to the dplyr summarise(...)}
}
\description{
Sometimes when you have a lot of results you want to show a short version
of the results with an "Other..." category. This function performs a summarisation and allows you to specify 
the number of rows you want and something to sort the rows by. It will then give you
strictly the top n results (ignoring ties) and group all the other results into a single summary row 
entitled "Other..."
}
\examples{
diamonds \%>\% group_by(clarity) \%>\% summariseTopN(n=5, sortVar = desc(count), count = n(), avPrice = mean(price))
}
