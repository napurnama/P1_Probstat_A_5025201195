#4.a
x = 2
v = 10

ad_hoc <- function(x, v){
  dividend = x ** (v/2 - 1) * exp(-x/2)
  divisor = 2 ** (v/2) * factorial(v/2-1)
  
  return(dividend/divisor)
}

print(paste("4.a) P(x): ", ad_hoc(x, v)), quote = FALSE)
print("", quote=FALSE)

#4.b
hist(rchisq(100, v))

#4.c
mean_chisq <- function(v){
  return(v) 
}

variance_chisq <- function(v){
  return(2*v)
}

print(paste("4.c.i) E[x]: ", mean_chisq(v)), quote=FALSE)
print(paste("4.c.ii) var[x]: ", variance_chisq(v)), quote=FALSE)
print("", quote=FALSE)
