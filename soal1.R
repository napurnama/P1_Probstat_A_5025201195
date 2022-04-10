#1.a 0.8*0.8*0.8*0.2
nth_success = 4
p_success = 0.2

geometric_distribution <- function(nth_success, p_success, p_failure){
  return(p_failure ** (nth_success - 1) * p_success)
}

print(paste("1.a) P(x): ",geometric_distribution(nth_success, p_success, 1-p_success)), quote=FALSE)
print("", quote=FALSE)

#1.b
n = 10000
success_at_n <- c(rgeom(n, p_success))
simulation <- data.frame(table(success_at_n))

mean_geom <- function(p_success){
  return(1/p_success)
}

# print(simulation)

mean_sim <- function(data){
  mean = 0
  for(i in 1:(max(as.numeric(data$success_at_n)))){
    mean <- mean + (strtoi(data$Freq[i]) * (strtoi(data$success_at_n[i])) / sum(as.numeric(data$Freq)))
  }
  return(mean)
}

print(paste("1.b.i) E[x]: ",mean_geom(p_success)), quote=FALSE)
print(paste("1.b.ii) Simulated E[x]: ",mean_sim(simulation)), quote=FALSE)
print("", quote=FALSE)


# #1.c
# Setelah menjalankan program beberapa kali, pola yang ditemukan adalah
# nilai E(x) dari simulasi cenderung mendekati 4. Selain itu, dicoba juga
# variasi dengan mengubah peluang sukses dan pola yang ditemukan adalah nilai
# E(x) yang dihasilkan simulasi cenderung mendekati nilai E(x) - 1.

#1.d
hist(success_at_n, breaks=max(success_at_n))

#1.e
variance <- function(p_success){
  return((1-p_success)/p_success ** 2)
}

print(paste("1.e.i) E[x]: ", mean_geom(p_success)), quote=FALSE)
print(paste("1.e.ii) Var[x]: ", variance(p_success)), quote=FALSE)
