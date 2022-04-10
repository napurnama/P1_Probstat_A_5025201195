#3.a
poisson_distribution <- function(average_success, targeted_success){
  return(exp((-1 * average_success)) * average_success ** targeted_success / factorial(targeted_success))
}

average_success <- 4.5
targeted_success <- 6

print(paste("3.a) P(x): ", poisson_distribution(average_success, targeted_success)), quote=FALSE)
print("", quote=FALSE)

#3.b
simulation <- round(rpois(365, average_success))

hist(simulation)

#3.c
simulation_data <- data.frame(table(simulation))
print(paste("3.c.i) Poisson distribution: ", poisson_distribution(average_success, targeted_success)), quote = FALSE)
print(paste("3.c.ii) Simulated distribution: ",simulation_data$Freq[targeted_success+1]/365), quote = FALSE)
print("", quote=FALSE)

# Nilai yang didapatkan dari Poisson cukup mendekati nilai
# dari simulasi yang dijalankan. Poisson distribution dapat 
# dilihat sebagai formula yang cukup handal membuat 
# aproksimasi dari kasus di mana terdapat sebuah jumlah 
# rata-rata terjadinya kejadian dan jumlah kejadian yang 
# diinginkan. Data ini dapat berguna dalam sebuah bisnis
# yang mungkin memikirkan untuk ekspansi bisnis. Data dari
# sebuah survei atau studi lapangan dapat digunakan untuk 
# menentukan tempat yang cocok untuk membuka cabang baru.

#3.d
mean_pois <- function(average_success){
  return(average_success)
}

variance_pois <- function(average_success){
  return(average_success)
}

print(paste("3.d.i) mean: ",mean_pois(average_success)), quote=FALSE)
print(paste("3.d.ii) variance: ",variance_pois(average_success)),quote=FALSE)
print("", quote=FALSE)