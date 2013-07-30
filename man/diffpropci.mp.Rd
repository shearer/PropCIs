\name{diffpropci.mp}
\alias{diffpropci.mp}
\title{
  Adjusted Wald interval for a difference of proportions with matched pairs
}
\description{
  Adjusted Wald interval for a difference of proportions with matched
  pairs. This is the interval called Wald+2 in Agresti and Min
  (2005). Adds 0.5 to each cell before constructing the Wald CI
}
\usage{
diffpropci.mp(b, c, n, conf.level)
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
    confidence coefficient \eqn{1-\alpha}
  }
}
\details{
  The interval is truncated, when it overshoots the boundary
}
\value{
  A list with class '"htest"' containing the following components:
  \item{conf.int }{a confidence interval for the difference in proportions.}
  \item{estimate }{estimated difference in proportions }
}
\references{
  Agresti, A. and Min, Y. (2005) Simple improved confidence intervals for
  comparing matched proportions. \emph{Statistics in Medicine} 24 (5), 729--740. 
}
\examples{
diffpropci.mp(b = 40, c = 20, n = 160, conf.level = 0.95)
}

\keyword{ htest }

