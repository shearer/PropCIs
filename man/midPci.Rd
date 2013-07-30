\name{midPci}
\alias{midPci}
\title{
mid-P confidence interval adaptation of the Clopper-Pearson interval
}
\description{
mid-P confidence interval adaptation of the Clopper-Pearson interval
}
\usage{
midPci(x, n, conf.level)
}
\arguments{
  \item{x}{
 number of successes
}
  \item{n}{
 number of trials
}
  \item{conf.level}{
    confidence coefficient
  }
}
\value{
  A list with class '"htest"' containing the following components:
  \item{conf.int }{a confidence interval for the difference in proportions.}
}
\references{
  Agresti, A., Gottard, A. (2005) Randomized confidence intervals and
  the mid-P approach, discussion of article by C. Geyer and G. Meeden,
  \emph{Statistical Science} 20, 367--371.
}
\examples{
midPci(x = 15, n = 112, conf.level = 0.95)
  }
\keyword{ htest }
