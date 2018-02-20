\name{diffci.bayes.hpd}
\alias{diffci.bayes.hpd}
\title{
Bayesian HPD confidence interval for different of independent proportions
}
\description{
  Approximate Bayesian HPD confidence interval for different of proportions using independent priors
}
\usage{
diffci.bayes.hpd(x1,n1,x2,n2,a,b,c,d,conf.level)
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
}
\value{
Confidence interval with given confidence level.
}
\references{
  Agresti, A. (2002) Categorical Data Analysis. Wiley, 2nd Edition.
}
\keyword{ htest }