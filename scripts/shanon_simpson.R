## Mi manera de hacerlo
#Mi función solo funciona con vectores
shan_pielo <- function(x) { {
  n <- length(x)
  total <- sum(x)
  shannon <- 0
  
  for (i in 1:n) {
    shannon <- shannon + ((-1) * ((x[i] / total) * log(x[i] / total))) #el ciclo for que necesitaba
  }
  
  pielou <- shannon / (log(n)) #x tiene que ser necesariamente un vector !!!!
}
  return (pielou)
}

#Índice de Simpson
simpson <- function (x) {
  {
  N <- sum (x)
  simpson <- 0
  
  for (i in length(x)) {
  simpson <- simpson + ( (x[i] * (x[i] - 1) ) / ( N * (N - 1) ) )
  }
  
  simp <- sum (simpson)
  }
  return (simp)
}

#Vector de abundancias
v1 <- c (12, 34, 8, 26)

simpson (v1)

#Inverso de simpson 
inverso <- (1/simpson(v1))
inverso

#Gini y simpson 
gini-simps <-function()
  #iba a hacer esta



