# Totino and Euler Phi are working together. Need both
gcd <- function (m, n) {
  while (1) {
    rem <- m %% n                         # calculate the remainder here 
    ifelse (rem == 0, break, { m = n; n = rem; }) # if the remainder is greater than 0 then we continue by changing m to the value of n and n to the value of the remainder
  }
  n
}   

epf <- function (n) {
  if (n < 0) { -1 }                       # function's domain are positive integers only
  sum = 0
  for (k in 1:n) {                        # epf(n) is number of positive integers in 1 <= k <= n
    if (gcd(n, k) == 1) { sum = sum + 1 } # for which gcd (n, k) is equal 1. (when gcd (n, k) is 1, the number
  }                                       # k is a totient of n. And epf(n) is the sum of these totients (numbers relatively prime to n)
  sum                                     # return the result
}


factors<-function(num){
  factors <- integer(num - 2) # initialize factors
  
  # loop from 2 to num-1
  for (i in 2:(num-1)) {
    # check if i is a factor of num
    if ((num%%i)==0) {
      # add i to vector of factors
      factors[i-1] <- i 
    }
  }
  factors <- factors[factors!=0]
  factors
}
# remove factors==0


# if the number is a power of 2, return TRUE
p<-function(number){n1s <- sum(as.numeric(intToBits(number)));return(n1s==1)}


# fracts(n) are all fractions 1/n to n/n if x does not divide n
fracts<-function(n){
  acc<-c();
  for(i in 1:(n)){
    if(gcd(i,n)==1){
      acc<-c(acc,i/n)
    } else {}
  };
  c(acc,1/n)
}

isPrime<-function(p) (length(factors(p))==0)
z<-complex(argument=0,modulus=1)
rootOfUnity<-function(frac){
  complex(argument=2*pi*frac,modulus=1)
  }
Polygon<-function(n){
  c(mapply(rootOfUnity,fracts(n)))
  }
colorPick<-function(j) if(p(epf(j))){'blue'} else {if(isPrime(j)){'red'} else{'black'}}

scalePoly<-function(poly,i,maximum){
  scalePoint<-function(z){complex(argument=Arg(z),modulus=(i+Mod(z))/maximum)}
  mapply(scalePoint,poly)
}


for(i in 200:1){
  j<-i
  pts<-Polygon(j)
  wind<-20
  plot(scalePoly(pts,j,10),xlim=c(-wind,wind),ylim=c(-wind,wind),bty='n',xaxt='n',yaxt='n',main='',xlab='',ylab='',type='p',pch=20,col=colorPick(j))
  par(new=TRUE)
  
}
par(new=FALSE)








