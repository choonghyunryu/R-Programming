\name{mydata}
\alias{mydata}
\docType{data}
\title{normal random numbers}
\description{100 normal random numbers}
\usage{data(mydata)}
\format{
  The format is:
 double [1:100] 
}
\details{
100 normal random numbers.
mean=0, sd=1
}
\source{rnorm(100)}
\examples{
data(mydata)
mean(mydata)
plot(mydata, type="o")
}
\keyword{datasets}
