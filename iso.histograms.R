iso.histograms <- function(mcm.result, numbers = 0)
{
  if(numbers == 0)
  {
    stop("you must enter the number of source")
  }
  appor <-  mcm.result[, c((numbers+2) : (numbers*2 + 1))]
  mybreaks <- seq(0, 1, length = 50)
  halfwidth <- diff(mybreaks)[1]/2
  top <- 0
  right <- 0
  source.app <-  paste("source.app", 1 : numbers, sep = ".")
  maxvalue <- c()
  for(j in 1 : numbers)
  {
    nonhist <- hist(appor[, j], plot = FALSE, breaks = mybreaks)
    top <- max(c(top, max(hist(appor[, j], plot = FALSE, breaks = mybreaks)$density))) #find the UP LIMIT of Y 
    right <- max(c(right, max(hist(appor[, j], plot = FALSE, breaks = mybreaks)$mids)))
    which <- which.max(nonhist$density)
    maxvalue[j] <- which
  }
  plot(1, 1, xlim = c(0, 1), ylim = c(0, (top+15)), type = "n", main = "Proportion Densities", xlab = "Proportion", ylab = "Density" )
  for(i in 1 : numbers)
  {
    ans <- hist(appor[, i], plot = FALSE, breaks = mybreaks)
    for(k in 1 : 1 : length(ans$mids))
    {
      lines(c(ans$mids[k] + (i/((numbers + 1)/2) - 1) * halfwidth , ans$mids[k] + (i/((numbers + 1)/2) - 1) * halfwidth), c(0, ans$density[k]), col = i, lwd = (numbers + 1)/2, lend = 1)
      lines(c(ans$mids[k] + (i/((numbers + 1)/2) - 1) * halfwidth , ans$mids[k] + (i/((numbers + 1)/2) - 1) * halfwidth), c(0, ans$density[k]), col = i, lwd = (numbers + 1)/2, lend = 1)
      
    }
    max.count <- maxvalue[i]
    lines(density(appor[, i], bw = "nrd"), col = i, lty = i, lwd = (numbers + 1)/2.5)
    rug(jitter(appor[, i]), col = i, ticksize = .01, lwd = 0.001, quiet = T)
    lines(c(ans$mids[max.count], ans$mids[max.count]), c(0, ans$density[max.count]), col = "darkblue", lwd = (numbers + 1)/1.1)
  }
  legend(x = "topright", legend = source.app, fill = c(1:numbers), lty = c(1:numbers), col = c(1:numbers), bty = "n",pt.cex = .5)
  #return(maxvalue)
}