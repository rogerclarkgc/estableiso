#' Calculate the euclideanmetric distance of two points in the real number space
#'
#' This function is the core function of this package, which could calculate the 'distance' between sources and target mixture
#'
#' @param a A vector, the coordinate of point a in real number space, in this package, it represents the delta of two stable isotope
#' @param b Just the paramater a
#' @export
#' @examples
#' rho <- euclideanmetric(a = c(3,4), b = c(1, 2))
#' cat("The euclideanmetric distance is")
#' print(rho)
euclideanmetric <-
function(a, b)
{
    if(length(a) != length(b)) stop("the length of a and b is different")
    rho <- sqrt(sum((a-b)^2))
    #cat("rho = ", rho)
    return(rho)
}
