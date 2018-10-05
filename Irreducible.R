

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

# factorize a number
factors<-function (num) {
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
isPowerof2 <- function (number) {
  n1s <- sum(as.numeric(intToBits(number)));
  return (n1s==1)
}


# fracts(n) is a list of rational numbers 1/n to n/n if the numerator does not divide n.
fracts <- function (n) {
  acc<-c();
  for(i in 1:(n)){
    if(gcd(i,n)==1){ 
      acc<-c(acc,i/n)
    } else {}
  };
  c(acc,1/n)
}

# is p a prime number? uses factors.
isPrime <- function (p) if (p>2) { length(factors(p))==0 } else { TRUE }

# maps frac to the complex number with modulus 1, argument 2*pi*frac.
rootOfUnity <- function (frac) {
  complex(argument=2*pi*frac,modulus=1)
}

# Uses rootOfUnity to create the set of Nth primitive roots of unity.
Polygon<-function(n){
  c(mapply(rootOfUnity,fracts(n)))
}

# paints numbers with prime denominators red 
colorPick<-function(j) if(isPrime(j)){'red'} else{'black'}

# Scales sets of complex numbers by a quantity.
scalePoly<-function(poly,i){
  scalePoint<-function(z){complex(argument=Arg(z),modulus=(i+Mod(z)-1))}
  mapply(scalePoint,poly)
}

par(new=FALSE)

maxN<-150
wind1<-100
shift<-complex(real=0,imaginary=0)
# Creates a plot of N-scaled primitive Nth roots of unity up to some N.
plotIrreducibleFracts<-function(){
  for (i in 1:maxN) {
    j <- i
    pts<-Polygon(j)                     #Creates a set of jth primitive roots of unity.
    currentPoly<-shift+scalePoly(pts,j) #Optionally we can translate each polygon to examine certain parts of the plot.
                                        #Next, with a lot of graphical parameters we plot another layer.
    plot(currentPoly,xlim=c(-wind1,wind1),ylim=c(-wind1,wind1),bty='n',xaxt='n',yaxt='n',main='',xlab='',ylab='',type='p',pch='.')
    par(new=TRUE)
  }
}


PolygonAll<-function(n){
  c(mapply(rootOfUnity,(1:n)/n))
}
# Then, we want to use the same idea to ask what happens when the set of points is transformed.
# First we need the set of points.
fullImage<-function(maxD){
  result<-c()
  for(i in 1:maxD){
    result<-c(result,i*Polygon(i))
  }
  result
}
fullImageAll<-function(max){
  result<-c()
  for(i in 1:max){
    result<-c(result,i*PolygonAll(i))
  }
  result
}
# Plot plotMe with graphical parameters that make nice images, with a window size of wind.
plotNicely<-function(plotMe,wind){
  plot(plotMe,xlim=c(-wind,wind),ylim=c(-wind,wind),bty='n',xaxt='n',yaxt='n',main='',xlab='',ylab='',type='p',pch='.')
}

# Plots the whole set of Polygon(*) points up to g.
fullPlot<-function(g){
  plotNicely(fullImage(g),g/sqrt(3))
}
fullPlot(250)


