diffpropci.mp <-
function(b,c,n,conflev)
{
  z  <- qnorm(1-(1-conflev)/2)
  diff <- (c-b)/(n+2)
  sd <- sqrt((b+c+1)-(c-b)^2/(n+2))/(n+2)
  ll <- diff - z*sd
  ul <- diff + z*sd
  if(ll < -1) ll = -1
  if(ul > 1) ul = 1
  c(ll, ul)
}

