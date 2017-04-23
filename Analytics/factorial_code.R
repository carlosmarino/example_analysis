#############################################
#######  CARLOS ANTONIO MARINO  #############
#############################################
######  Part 1: Factorial Function ##########
#############################################

factorial_loop <- function(x){
  y <- 1
  for(i in 1:x){
    y <-y*((1:x)[i])
  }
  print(y)
}

factorial_reduce <- function(x) {
  if (x == 0)
    return (1)
  rag <- seq(x, 1)
  Reduce("*", rag)
}

factorial_recursive <- function(x) {
  if (x == 0)    return (1)
  x * factorial_recursive(x-1)
}


factorial_mem <- function(){ 
  res<-1
  memfactorial<-function(x){
  if (x == 0)    return (1)
  if(!is.na(res[x])) return(res[x])
  res[x]<<- x *factorial(x-1)
  res[x]
}
memfactorial
}
memfactorial<-factorial_mem()

#############################################
######### Summary of Performance ############
#############################################

microbenchmark(factorial_loop(5),
               factorial_reduce(5), 
               factorial_recursive(5),
               memfactorial(5), times = 100)

microbenchmark(factorial_loop(10),
               factorial_reduce(10), 
               factorial_recursive(10),
               memfactorial(10), times = 100)
