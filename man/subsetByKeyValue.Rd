% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Billing.R
\name{subsetByKeyValue}
\alias{subsetByKeyValue}
\title{filter a reckoning by 'label' retaining records associated with a particular key-value pair}
\usage{
subsetByKeyValue(mybilling, mykey, myvalue)
}
\arguments{
\item{mybilling}{instance of avReckoning}

\item{mykey}{character(1)}

\item{myvalue}{character(1)}
}
\value{
data.frame
}
\description{
filter a reckoning by 'label' retaining records associated with a particular key-value pair
}
\examples{
data(demo_rec) # makes rec
v = getValues(demo_rec@reckoning, "terra-submission-id")[1] # for instance
nt = subsetByKeyValue(demo_rec@reckoning, "terra-submission-id", v)
head(nt)
dim(nt)
}
