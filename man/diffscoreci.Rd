\name{diffscoreci}
\alias{diffscoreci}
\title{
  Score interval for difference of proportions
}
\description{
  Score interval for difference of proportions and independent samples (p1 - p2)
}
\usage{
diffscoreci(x1, n1, x2, n2, conf.level)
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
}
\value{
    A list with class '"htest"' containing the following components:
    \item{conf.int }{a confidence interval for the difference in proportions.}
}
\references{
  Agresti, A. (2002) Categorical Data Analysis. Wiley, 2nd Edition.

  Mee, RW. (1984) Confidence bounds for the difference between two
  probabilities. \emph{Biometrics} 40, 1175--1176.

  Miettinen OS, Nurminen M. (1985) Comparative analysis of two
  rates. \emph{Statistics in Medicine} 4, 213--226.

  Nurminen, M. (1986) Analysis of trends in proportions with an ordinally
  scaled determinant. \emph{Biometrical J.} 28, 965--974
}
\keyword{ htest }
