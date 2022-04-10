#5.a
avg_value = 3

pdf_exp <- function(avg_value, target_value){
  return(avg_value * exp(-1 * avg_value * target_value))
}

#5.b
set.seed(1)

hist(rexp(10, rate=avg_value), xaxp=c(0,5,5))
# hist(rexp(100, rate=avg_value), xaxp=c(0,5,5))
# hist(rexp(1000, rate=avg_value), xaxp=c(0,5,5))
# hist(rexp(10000, rate=avg_value), xaxp=c(0,5,5))

#5.c
mean <- function(avg_value){
  return(1 / avg_value)
}

variance <- function(avg_value){
  return(1 / avg_value ** 2)
}

print(paste("5.c.i) E[x]: ", mean(lambda)), quote=FALSE)
print(paste("5.c.ii) Var[x]: ", variance(lambda)), quote=FALSE)
print("", quote=FALSE)