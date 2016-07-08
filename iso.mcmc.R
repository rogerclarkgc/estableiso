#' Resample the sample list using  Markov chain random sampling method
#' 
#' This function is kind of resample function
#' iso.mcmc(threshold.result = 0, numbers, iter = 1000)
#' @param threshold.result the output of function iso.threshold
#' @param numbers the numbers of your sources
#' @param iter how many iteration do you want? actually run = 1000 is enough for most situation
#' @export
iso.mcmc <- function(threshold.result = 0, numbers, iter = 1000){
  sample.list <- threshold.result$sample_list
  threshold <- threshold.result$threshold
  source.matrix <- threshold.result$source.matrix
  cumulative <- 0
  mcmc.table <- data.frame()
  x <- rep(0, numbers) # front state of Markov chain
  n <- rep(0, numbers) # next state of Markov chain
  choose <- 0
  i <- 1
  while(i <= iter){
    prob1 <- 1
    prob2 <- 1
    for(j in 1 : numbers){
      x[j] <- sample(1000, 1) # set the initial state of Markov chain
      #cat("select1:", x[j], "\n")
      sample1 <- sample.list[[j]][x[j], 1]
      mean1 <- source.matrix[j, 4]
      sd1 <- source.matrix[j, 3]
      prob1 <- pnorm(sample1, mean = mean1, sd = sd1) * prob1
      #cat("prob1:", prob1, "\n")
    }
    for(k in 1 : numbers){ # caculate the next state of Markov chain
      n[k] <- floor(x[k] + 999 * runif(1, max = 1, min = -1))
      if(n[k] > 1000 || n[k] <= 0){
        #cat("false", "\n")
        while(n[k] > 1000 || n[k] <= 0)
          n[k] <- floor(x[k] + 999 * runif(1, max = 1, min = -1))
      }
      #cat("select2:",n[k], "\n")
      sample2 <- sample.list[[k]][n[k], 1]
      mean2 <- source.matrix[k, 4]
      sd2 <- source.matrix[k, 3]
      prob2 <- pnorm(sample2, mean = mean2, sd = sd2) * prob2
      #cat("prob2:", prob2, "\n")
    }
    if(max(prob1, prob2) > threshold){
      i <- i + 1
      if(((i - 1) %% 10 == 0))
        cat("iteration:", i - 1, "\n")
      if(prob1 > prob2)
        choose <- x
      else
        choose <- n
      mcmc.table <- rbind(mcmc.table, choose)
    }
  }
colnames(mcmc.table) <- paste('source', 1:numbers, sep = ".")
return(list(sample.list = sample.list, mcmc.table = mcmc.table, numbers = numbers, iter = iter))
}