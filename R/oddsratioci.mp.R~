oddsratioci.mp <-
function(b,c,conflev)
{
  z  <- qchisq(conflev,1)
  A <- b + c + z
  B <- 2*c + z
  C <- c^2/(b+c)
  l <- (B - sqrt(B^2-4*A*C))/(2*A)
  u <- (B + sqrt(B^2-4*A*C))/(2*A)
  ll <- l/(1-l)
  ul <- u/(1-u)
  c(ll,ul)
}

