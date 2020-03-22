\name{center}
\alias{center}
\title{The mean, median calculations}
\description{
Selected by calculating mean, trimmed mean and median.
}
\usage{
center(x, type = c("mean", "median", "trimmed"))
}
\arguments{
  \item{x}{numeric vector}
  \item{type}{character string. }
}
\details{
Depending on the type argument is a numeric vector of three kinds of statistics obtained.
}
\value{numeric}
\author{Ryu Choonghyun}
\seealso{\code{\link{my.mean}}}
\examples{
set.seed(1)
x <- rcauchy(10)
center(x, "t")		# trimmed mean
center(x, "med")	# median
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

