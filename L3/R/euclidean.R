euclidean <- function(a, b){
  while(b != 0){
    r <- b
    b <- a %% b
    a <- r
  }
  a
}