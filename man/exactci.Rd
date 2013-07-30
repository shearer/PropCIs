\name{exactci}
\alias{exactci}
\title{
  Clopper-Pearson exact CI
}
\description{
  Clopper-Pearson exact CI
}
\usage{
exactci(x, n, conf.level)
}
\arguments{
  \item{x}{Number of successes}
  \item{n}{Total sample size}
  \item{conf.level}{Confidence level}
}
\value{
  A list with class '"htest"' containing the following components:
  \item{conf.int }{a confidence interval for the proportion}
}
\references{
  Clopper, C. and Pearson, E.S. (1934) The use of confidence or fiducial
  limits illustrated in the case of the binomial. \emph{Biometrika} 26, 404--413.
}
\keyword{ htest }

