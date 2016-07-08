#' Draw a densityplot to show the result of the model
#'
#' This function is can generate a densityplot by using the output of function iso.mcm
#'
#' @param mcm.result the output of function iso.mcm
#' @param numbers the numbers of sources in your model
#' @param cycle the core paramater of whole package, do not use if your don't a advanced user
#' @export
#' @examples
#' source.matrix <- data.frame(d15NPl = c(6.50, 4.42, 11.19, 9.82), d13CPl = c(-11.17, -30.88, -10.19, -15.01), sd.N = c(1.4594632, 2.2680709, 1.1124385, 0.8271039), mean.N = c(6.488984, 4.432160, 11.192613, 9.816280), sd.C = c(1.2149562, 0.6413182, 1.9593306, 1.1724677), mean.C = c(-11.17023, -30.87984, -11.17090, -14.05701))
#' mixture.matrix <- data.frame(d15NPl = 10.30, d13CPl = -11.58)
#' sample.list <- iso.euclidean2(source.matrix, mixture.matrix)
#' mcm.result <- iso.mcm(sample.list, numbers = 4)
#' iso.densityplot2(mcm.result, numbers = 4)

iso.densityplot2 <-
function(mcm.result, numbers, cycle = NULL) #mcm.result is the output of function "iso.mcm", numbers is the number of the source in the research
{
  if(numbers == 0)
  {
    stop("you must enter the number of source")
  }
  appor <-  mcm.result[, c((numbers+2) : (numbers*2 + 1))]
  top <- 0
  right <- 0
  mybreaks <- seq(0, 1, length = 50)
  source.app <-  paste("source.app", 1 : numbers, sep = ".")
  for(j in 1 : numbers)
  {
    top <- max(c(top, max(hist(appor[, j], plot = FALSE, breaks = mybreaks)$density))) #find the UP LIMIT of Y 
    right <- max(c(right, max(hist(appor[, j], plot = FALSE, breaks = mybreaks)$mids))) #find the right limit of x
  }
  plot(1, 1, xlim = c(0, 1), ylim = c(0, (top+10)), type = "n", main = "Proportion Densities", xlab = "Proportion", ylab = "Density" )
  for(k in 1 : numbers)
  {
    lines(density(appor[, k]), col = k, lty = k)
  }
  #source.labels <- factor(rep(source.app, each = 1000), levels = source.app)
  #mcm.new <- cbind(mcm.new, source.labels)
  #colors <- c(1 : numbers)
  #lines <- c(1 : numbers)
  #points <- c(16 : 16 + numbers)
  #key.mcm <- list(title = "source.labels", space = "right", columns = 1, text = list(levels(mcm.new$source.labels)), points = list(pch = points, col = colors), lines = list(col = colors, lty = lines), cex.title = 1, cex = .9)
  #densityplot(~ source.app, group = source.labels, data = mcm.new, main = "Density plot of source apportionment distribution", xlab = "Apportionment", pch = points, lty = lines, col = colors, lwd = 2, key = key.mcm, jitter = 0.5)
  #minor.tick(nx = 3, tick.ratio = .5)
}
