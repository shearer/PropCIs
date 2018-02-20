\name{orci.bayes}
\alias{orci.bayes}
\title{
  Bayesian tail confidence interval for an odds ratio
}
\description{
  Approximate Bayesian tail confidence interval for an odds ratio using simulation method
  }
\usage{
orci.bayes(x1,n1,x2,n2,a,b,c,d,conf.level, nsim)
}
\arguments{
  \item{x1}{Binomial variate group 1
}
  \item{n1}{Sample size group 1
}
  \item{x2}{Binomial variate group 2
}
  \item{n2}{Sample size group 2
}
  \item{a}{beta prior for x1
}
  \item{b}{beta prior for x2
}
  \item{c}{beta prior for n1
}
  \item{d}{beta prior for n2
}
  \item{conf.level}{confidence level
}
  \item{nsim}{number of simulations with default 10M
}
}
\value{
Confidence interval for an odds ratio with given confidence level.
}
\references{
  Agresti, A. (2002) Categorical Data Analysis. Wiley, 2nd Edition.
}
\keyword{ htest }