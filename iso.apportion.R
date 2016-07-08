#' Caculate the apportion of each source
#' 
#' This function use the output of iso.mcmc to caculate the apportion vector
#' iso.apportion(mcmc.result = list(), run = 1000)
#' @param mcmc.result the output of function iso.mcmc
#' @param run the rownumber of mcmc table
#' @export
iso.apportion <- function(mcmc.result = list(), run = 1000){ 
  random.eu <- c(NULL)
  random.eumatrix <- data.frame()
  eu.v <- c(NULL)
  eu.m <- data.frame()
  source.list <- mcmc.result$sample.list
  mcmc.table <- mcmc.result$mcmc.table
  numbers <- mcmc.result$numbers
  if(length(source.list) == 0) stop("source.list cannot be 0")
  for(i in 1 : run)
  {
    rowno <- as.numeric(mcmc.table[i, ])
    for(j in 1 : numbers)
    {
      n <- rowno[j]
      random.eu[j] <- 1/(source.list[[j]][n, 3])
      
    }
    random.eumatrix <- rbind(random.eumatrix, random.eu)
    
  }
  names(random.eumatrix) <- paste('source', 1:numbers, sep = ".")
  alleu <- apply(random.eumatrix, 1, sum)
  random.eumatrix <- cbind(random.eumatrix, alleu)
  for(k in 1 : run)
  {
    for(t in 1 : numbers)
    {
      eu.v[t] <- random.eumatrix[k, t]/random.eumatrix[k, numbers + 1]
      
    }
    eu.m <- rbind(eu.m, eu.v)
  }
  names(eu.m) <- paste("source.app", 1:numbers, sep = ".")
  random.eumatrix <- cbind(random.eumatrix, eu.m)
  cat("ALL JOB DONE!", "\n","this is the resample matrix, row 1 ~ 10","\n")
  print(random.eumatrix[c(1:10), ])
  return(random.eumatrix)
  
}
