% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/myci.R
\name{myci}
\alias{myci}
\title{A function to find the 95percent confidence interval of a single sample x}
\usage{
myci(x)
}
\arguments{
\item{x}{a sample}
}
\value{
the 95 percent confidence interval
}
\description{
A function to find the 95percent confidence interval of a single sample x
}
\examples{
x=rnorm(30, mean=10, sd=12)
myci(x)
}
