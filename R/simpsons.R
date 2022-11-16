#' Title
#'
#' @param f a function with a single argument x
#' @param range a vector specifying the lower ans upper end points of the range of integration(a and b),and
#' @param n the number of intervals for the Riemann approximation
#'
#' @return a single value which is the Riemann approximation to integral the f(x) from b to a.
#' @export
#'
#' @examples
#' f = function(x) x^4+x^2
#' simpsons(f,c(4,6),1000)
simpsons <- function(f,range,n){
  b = range[2]
  a = range[1]
  c <- (b-a)/n
  x1 <- seq(a+c,b,by=c)
  x2 <- seq(a,b-c,by=c)
  #y = f(x)
  #x_i-1=x2
  #x_i=x1
  total <- (f(x2)+f(x1)+4*f((x2+x1)/2))*((x1-x2)/6)
  print(sum(total))
}


