scoreci <-
function(x,n,conflev)
{
  zalpha <- abs(qnorm((1-conflev)/2))
  phat <- x/n
  bound <- (zalpha*((phat*(1-phat)+(zalpha**2)/(4*n))/n)**(1/2))/(1+(zalpha**2)/n)
  midpnt <- (phat+(zalpha**2)/(2*n))/(1+(zalpha**2)/n)

  uplim <- round(midpnt + bound,digits=4)
  lowlim <- round(midpnt - bound,digits=4)

  results <- data.frame(lowlim,uplim)

  cat("\n")
  cat("With confidence level",conflev," and sample proportion",
      round(phat,digits=4),
      " \nthe lower and upper limits for the score confidence interval are: \n")
  cat("\n")
  print(results)
  cat("\n")
}

