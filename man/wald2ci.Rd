\name{wald2ci}
\alias{wald2ci}
\title{
  Wald interval with the possibility to adjust according to Agresti,
  Caffo (2000) for difference in proportions and independent samples.
}
\description{
  Wald interval with the possibility to adjust according to Agresti,
  Caffo (2000) for difference in proportions and independent
  samples. The Agresti-Caffo interval adds 1 to x1 and x2 and adds 2 to
  n1 and n2.
}
\usage{
wald2ci(x1, n1, x2, n2, conf.level, adjust)
}
\arguments{
  \item{x1}{
    success counts in sample 1
  }
  \item{n1}{
    sample size in sample 1
  }
  \item{x2}{
    success counts in sample 2
  }
  \item{n2}{
    sample size in sample 2
  }
  \item{conf.level}{
    confidence coefficient
  }
  \item{adjust}{
    option to adjust the Wald interval to the Agresti-Caffo interval for
    better performance
  }
}
\details{
  If \code{adjust=AC} is chosen, the standard Wald interval is modified
  to the Agresti-Caffo adjusted CI (American Statistician, 2000)
}
\value{
  A list with class '"htest"' containing the following components:
  \item{conf.int }{a confidence interval for the difference in proportions.}
  \item{estimate }{estimated difference in proportions}
}
\references{
  Agresti, A. (2002) Categorical Data Analysis. Wiley, 2nd Edition.
  Agresti, A., Caffo, B.(2000) Simple and effective confidence intervals
  for proportions and difference of proportions result from adding two
  successes and two failures. \emph{The American Statistician} 54 (4), 280--288.
}
\keyword{ htest }
