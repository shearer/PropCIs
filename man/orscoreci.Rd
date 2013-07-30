\name{orscoreci}
\Rdversion{0.1-1}
\alias{orscoreci}
\title{
  score confidence interval for an odds ratio in a 2x2 table [p1(1-p1)/(p2(1-p2))]

}
\description{
  score confidence interval for an odds ratio in a 2x2 table [p1(1-p1)/(p2(1-p2))]}
\usage{
orscoreci(x1, n1, x2, n2, conf.level)
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
  Cornfield, J. (1956) A statistical problem arising from retrospective
  studies. In Neyman J. (ed.), \emph{Proceedings of the third Berkeley
  Symposium on Mathematical Statistics and Probability} 4, pp. 135--148.

  Miettinen O. S., Nurminen M. (1985) Comparative analysis of two
  rates. \emph{Statistics in Medicine} 4, 213--226.

  Agresti, A. 2002. Categorical Data Analysis. Wiley, 2nd Edition.
}
\keyword{ htest }
