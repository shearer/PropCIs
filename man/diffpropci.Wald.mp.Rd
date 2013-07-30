\name{diffpropci.Wald.mp}
\alias{diffpropci.Wald.mp}
\title{
  Wald interval for a difference of proportions with matched pairs
}
\description{
  Wald interval for a difference of proportions with matched
  pairs.
}
\usage{
diffpropci.Wald.mp(b, c, n, conf.level)
}
\arguments{
  \item{b}{
    off-diag count
  }
  \item{c}{
    off-diag count
  }
  \item{n}{
   sample size
  }
  \item{conf.level}{
    confidence coefficient
  }
}
\details{
  The interval is truncated, when it overshoots the boundary
}
\value{
  A list with class '"htest"' containing the following components:
  \item{conf.int }{a confidence interval for the difference in proportions.}
  \item{estimate }{estimated difference in proportions c-b/n}
}
\references{
  D. G. Altman (1999) Practical statistics for medical research. London,
  Chapman & Hall
}
\examples{
diffpropci.Wald.mp(b = 3, c = 9, n = 32, conf.level = 0.95)
}

\keyword{ htest }

