addz2ci <-
function(x,n,conflev){
   z = abs(qnorm((1-conflev)/2))
   tr = z^2     #the number of trials added
   suc = tr/2   #the number of successes added
   ptilde = (x+suc)/(n+tr)
   stderr = sqrt(ptilde * (1-ptilde)/(n+tr))
   ul = ptilde + z * stderr
   ll = ptilde - z * stderr
   if(ll < 0) ll = 0
   if(ul > 1) ul = 1
   c(ll,ul)
}

