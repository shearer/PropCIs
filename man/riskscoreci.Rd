\name{riskscoreci}
\alias{riskscoreci}
\title{
  score confidence interval for the relative risk in a 2x2 table
}
\description{
  score confidence interval for the relative risk in a 2x2 table
}
\usage{
riskscoreci(x1, n1, x2, n2, conf.level)
}
\arguments{
  \item{x1}{
    number of successes in sample 1
  }
  \item{n1}{
    sample size in sample 1
  }
  \item{x2}{
    number of successes in sample 2
  }
  \item{n2}{
    sample size in sample 2
  }
  \item{conf.level}{
    confidence coefficient \eqn{1-\alpha}
  }
}
\value{
  A list with class '"htest"' containing the following components:
  \item{conf.int }{a confidence interval for the difference in proportions.}
}
\references{
  Nam, J. M. (1995) Confidence limits for the ratio of two binomial
  proportions based on likelihood scores: Non-iterative
  method. \emph{Biom. J.} 37 (3), 375--379.

  Koopman PAR. (1985) Confidence limits for the ratio of two binomial
  proportions. \emph{Biometrics} 40, 513--517.

  Miettinen OS, Nurminen M. (1985) Comparative analysis of two
  rates. \emph{Statistics in Medicine} 4, 213--226.

  Nurminen, M. (1986) Analysis of trends in proportions with an ordinally
  scaled determinant. \emph{Biometrical J} 28, 965--974

  Agresti, A. (2002) Categorical Data Analysis. Wiley, 2nd Edition.
}
\keyword{ htest }
