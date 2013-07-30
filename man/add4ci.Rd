\name{add4ci}
\alias{add4ci}
\title{
Agresti-Coull add-4 CI for a binomial proportion
}
\description{
Agresti-Coull add-4 CI for a binomial proportion, based on adding 2
successes and 2 failures before computing the Wald CI. The CI is
truncated, when it overshoots the boundary
}
\usage{
add4ci(x, n, conf.level)
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
  \item{conf.int }{The confidence intervall for the proportion}
  \item{estimate }{The estimator for the proportion}
}
\references{
  Agresti, A., Coull, B. (1998) Approximate is better than exact for
  interval estimation of binomial proportions. \emph{The American
    Statistician} 52, 119--126.

  Agresti, A., Caffo, B.(2000) Simple and effective confidence intervals
  for proportions and difference of proportions result from adding two
  successes and two failures. \emph{The American Statistician} 54 (4), 280--288.
}
\examples{
add4ci(x = 15, n = 112, conf.level = 0.95)
}
\keyword{ htest }
