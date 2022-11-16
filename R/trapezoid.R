library(roxygen2)
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
#'  f = function(x) x^4+x^2
#' trapezoid(f,c(4,6),1000)
trapezoid <- function(f,range,n){
  b = range[2]
  a = range[1]
  trap <- (b-a)/n
  x1 <- seq(a+trap,b,by=trap)
  x2 <- seq(a,b-trap,by=trap)
  y1 = f(x1)
  y2 = f(x2)
  total <- sum(y1+y2)/2
  print(trap*total)
}

