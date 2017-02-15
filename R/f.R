

#'@export
f <- function(x,col=NULL) UseMethod("f",x)

#'@export
f.ts <- function(x,col=NULL){
  "This is a ts object"
}

#'@export
f.list <- function(x,col=NULL){
  m <- do.call("cbind",x)
  f(m,col=col)
}

#'@export
f.mts <- function(x,col=NULL){
  plot(x,col=col)
}


