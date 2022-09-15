#' @title Implementation of the Euclidean Algorithm
#' 
#' @description The function \code{euclidean} finds the greatest common divisor
#'     of two numbers using the euclidean algorithm.
#'
#' @param a A numeric scalar representing the first number.
#' @param b A numeric scalar representing the second number.
#'
#' @return The greatest common divisor of \code{a} and \code{b} as a numeric
#'     scalar.
#'     
#' @export
#' 
#' @examples 
#' euclidean(123612, 13892347912)
#' euclidean(100, 1000)
#' 
#' 
#' @references \url{https://en.wikipedia.org/wiki/Euclidean_algorithm}

euclidean <- function(a,b){
  stopifnot(length(a)==1,length(b)==1,is.numeric(a),is.numeric(b))
  while(b != 0){
    temp <- b
    b <- a %% b
    a <- temp
  }
  return(a)
}
