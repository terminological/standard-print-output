% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/huxtableUtils.R
\name{saveMultiPageTableLandscape}
\alias{saveMultiPageTableLandscape}
\title{format a dataframe into a multi page table that is landscape in orientation}
\usage{
saveMultiPageTableLandscape(
  labelledDataFrame,
  filename,
  pageWidth = 5.9,
  pageLength = 8,
  defaultFontSize = 10,
  tableWidth = NULL,
  colWidths = NULL
)
}
\arguments{
\item{filename}{file of desired output without extension.}

\item{pageWidth}{maximum width of the desired pdf output in inches (5.9)}

\item{pageLength}{maximum width of the desired pdf output in inches (8)}

\item{defaultFontSize}{default size of font in points (10)}

\item{tableWidth}{width of the table in inches or NULL to fit contents automatically}

\item{colWidths}{a vector of relative column widths}
}
\description{
format a dataframe into a multi page table that is landscape in orientation
}
\examples{
setwd(tempdir())
library(dplyr)
mtcars \%>\% rownames_to_column() \%>\% arrange(gear,carb) \%>\% group_by(gear,carb) \%>\% saveMultiPageTableLandscape("carMultiTest",pageWidth=2,pageLength = 5.9)
}
\keyword{huxtable}
