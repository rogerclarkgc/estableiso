#' Draw the dot plot of mixture and sources
#'
#' This funcion is used to draw a dot plot of your source and mixture, use with function iso.loaddata usually
#' iso.plot(datalist = list(), group = 0)
#'
#' @param datalist the output of iso.loaddata
#' @export
#' @examples
#' source.matrix <- data.frame(d15NPl = c(6.50, 4.42, 11.19, 9.82), d13CPl = c(-11.17, -30.88, -10.19, -15.01), sd.N = c(1.4594632, 2.2680709, 1.1124385, 0.8271039), mean.N = c(6.488984, 4.432160, 11.192613, 9.816280), sd.C = c(1.2149562, 0.6413182, 1.9593306, 1.1724677), mean.C = c(-11.17023, -30.87984, -11.17090, -14.05701))
#' mixture.matrix <- data.frame(d15NPl = 10.30, d13CPl = -11.58)
#' iso.plot(datalist = list(mixture = mixture.matrix, sources = source.matrix))

iso.plot <-
function(datalist = list(), group = 0){
  sources <- datalist$sources
  mixture <- datalist$mixture
  snum <- nrow(sources)
  mnum <- nrow(mixture)
  xr <- max(sources[, 4]) + 2 * sources[which.max(sources[, 4]), 3]
  xl <- min(sources[, 4]) - 2 * sources[which.min(sources[, 4]), 3]
  yr <- max(sources[, 6]) + 2 * sources[which.max(sources[, 6]), 5]
  yl <- min(sources[, 6]) - 2 * sources[which.min(sources[, 6]), 5]
  plot(1,1, type = "n", xlim = c(xl, xr), ylim = c(yl, xr), main = "Mixture and Source data dot plot", xlab = colnames(mixture)[1], ylab = colnames(mixture)[2])
  for(i in 1 : snum){
    points(sources[i, 4], sources[i, 6], col = i, pch = i + 5, bg = i)
    lines(c(qnorm(p = 0.05, mean = sources[i, 4], sd = sources[i, 3]), qnorm(p = 0.95, mean = sources[i, 4], sd = sources[i, 3])), 
          c(sources[i, 6], sources[i, 6]), col = i) #error bar at x axis
    lines(c(sources[i, 4], sources[i, 4]), 
          c(qnorm(p = 0.05, mean = sources[i, 6], sd = sources[i, 5]), qnorm(p = 0.95, mean = sources[i, 6], sd = sources[i, 5])), col = i) #errow bar at y axis
  }
  for(j in 1 : mnum){
    points(mixture[j, 1], mixture[j, 2], cex = 1.2, pch = 16, col = "darkblue")
  }
  le.pch <- c(1:snum) + 5
  le.text <- rownames(sources)
  legend("topright", legend = le.text, pch = le.pch)
}
