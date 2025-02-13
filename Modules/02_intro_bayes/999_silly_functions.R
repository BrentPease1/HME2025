silly <- function(x){
  cat(paste("Silly", x, "\n"))
}


silly('beans')
silly(1)
silly(0:5)

less_silly <- function(x = 0:10, y){
  a <- 2*x
  return(a+y)
}

less_silly(1:2, 2)
