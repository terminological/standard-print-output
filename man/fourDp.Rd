% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/formatting.R
\name{fourDp}
\alias{fourDp}
\title{4 decimal place}
\usage{
fourDp(x, unit = "")
}
\arguments{
\item{x}{a numeric or vector of numerics}

\item{unit}{a unit specification}
}
\description{
4 decimal place
}
\examples{
df \%>\% mutate(display = fourDp(x))
df \%>\% mutate(display = fourDp(x,"\%"))
}
\keyword{format}
