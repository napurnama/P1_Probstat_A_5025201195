#2.a <- peluang 20 pasien, 4 sembuh
equation <- function(n_success, n_failure, p_success, p_failure){
  return((p_success**n_success)*(p_failure**n_failure))
}

combination <- function(n, r){
  return(factorial(n)/(factorial(n-r)*factorial(r)))
}

probability <- function(n, n_success, n_failure, p_success, p_failure){
  return(equation(n_success, n_failure, p_success, p_failure) * combination(n, n_success) * 100)
}

n = 20
n_success = 4
n_failure = n - n_success
p_success = 0.2
p_failure = 1 - p_success

print(paste("2.a) P(x): ", probability(n, n_success, n_failure, p_success, p_failure)), quote=FALSE)
print("", quote=FALSE)

#2.b <- histogram
n_of_success <- append(rep(c(1), n_success), rep(c(0), n_failure))
hist(n_of_success, breaks = 2, xaxp = c(0,1,1), ylim = c(0,n), yaxp = c(0, n, n), main = "Histogram 2.b", xlab = "succeed")

#2.c.i <- Rataan
mean <- function(n, p_success){
  return(n * p_success)
}

print(paste("2.c.i) E(x): ", mean(n, p_success)), quote=FALSE)

#2.c.ii <- Variansi

variance <- function(n, p_success){
  return(n * p_success * (1 - p_success))
}

print(paste("2.c.ii) variance: ",variance(n, p_success)), quote=FALSE)
print("", quote=FALSE)
