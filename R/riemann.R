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
#' riemann(f,c(4,6),1000)
riemann <- function(f,range,n){
  b = range[2]
  a = range[1]
  rectangle <- (b-a)/n
  x1 <- seq(a+rectangle,b,by=rectangle)
  x2 <- seq(a,b-rectangle,by=rectangle)
  y = f(x2)
  print(rectangle*sum(y))
}
