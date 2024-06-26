% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rd.spec.pts.R
\name{rd.spec.pts}
\alias{rd.spec.pts}
\title{Read and correct spectral data}
\usage{
rd.spec.pts(
  file,
  type = c("csv", "text"),
  ID.point,
  ID.sample = NULL,
  spec.columns,
  asd.corr = F,
  ...
)
}
\arguments{
\item{file}{File location of the spectral data.}

\item{type}{Input format of the file, either "csv" or "text".}

\item{ID.point}{Name or index of the column representing the point ID (location ID).}

\item{ID.sample}{Name or index of the column representing the sample ID. If NULL, the sample ID will not be included in the output.}

\item{spec.columns}{Indices or names of the columns containing the spectral data.}

\item{asd.corr}{Logical value indicating whether to perform ASD data correction. If TRUE, the function applies correction calculations to the ASD data.}

\item{...}{Additional arguments to be passed to the underlying functions such as read.csv and read.table.}
}
\value{
A data frame containing the processed spectral data. The data frame includes the specified columns, along with the optional sample and point ID columns.
}
\description{
This function reads spectral data from a file and performs optional corrections on the data.
}
\examples{
# Read spectral data from a CSV file with sample and point IDs
data <- rd.spec.pts("spectra.csv", type = "csv", ID.point = "PointID", ID.sample = "SampleID", spec.columns = c(2:10), asd.corr = FALSE)
}
