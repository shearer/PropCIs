\name{scoreci}
\Rdversion{0.1-1}
\alias{scoreci}
\title{Wilson's confidence interval for a single proportion
}
\description{Wilson's confidence interval for a single proportion. Score CI based on inverting the asymptotic normal test
  using the null standard error}
\usage{
scoreci(x, n, conf.level)
}
\arguments{
  \item{x}{Number of successes}
  \item{n}{Total sample size}
  \item{conf.level}{Confidence level}
}
\value{
  A list with class '"htest"' containing the following components:
  \item{conf.int }{a confidence interval for the difference in proportions.}
}
\references{
  Wilson, E.B. (1927) Probable inference, the law of succession, and
  statistical inference \emph{J. Amer. Stat. Assoc} 22, 209--212
}
\keyword{ htest }
