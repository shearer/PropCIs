\name{scoreci.mp}
\alias{scoreci.mp}
\title{
  Tango's score confidence interval for a difference of proportions with
  matched pairs}
\description{
  Tango's score confidence interval for a difference of proportions with
  matched pairs
 }
\usage{
scoreci.mp(b, c, n, conf.level)
}
\arguments{
  \item{b}{off-diagonal count}
  \item{c}{off-diagonal count}
  \item{n}{sample size}
  \item{conf.level}{confidence coefficient}
}
\value{
  A list with class '"htest"' containing the following components:
  \item{conf.int }{a confidence interval for the difference in proportions.}
}
\references{
  Agresti, A. and Min, Y. (2005) Simple improved confidence intervals for
  comparing matched proportions \emph{Statistics in Medicine} 24 (5), 729--740.

  Tango T. (1998) Equivalence test and confidence interval for the
  difference in proportions for the paired-sample design \emph{Statistics in Medicine} 17, 891--908
}
\examples{scoreci.mp(b = 40, c = 20, n = 160, conf.level = 0.95)}
\keyword{ htest }
