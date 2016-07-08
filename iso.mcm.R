#' Resample the sample list using random sampling method
#' 
#' This function is kind of resample function
#' iso.mcm(source.list, numbers, run = 1000)
#' @param source.list the output of function iso.euclidean2
#' @param numbers the numbers of your sources
#' @param run how many resample cycle do you want? actually run = 1000 is enough for most situation
#' @export
#' @examples
#' source.matrix <- data.frame(d15NPl = c(6.50, 4.42, 11.19, 9.82), d13CPl = c(-11.17, -30.88, -10.19, -15.01), sd.N = c(1.4594632, 2.2680709, 1.1124385, 0.8271039), mean.N = c(6.488984, 4.432160, 11.192613, 9.816280), sd.C = c(1.2149562, 0.6413182, 1.9593306, 1.1724677), mean.C = c(-11.17023, -30.87984, -11.17090, -14.05701))
#' mixture.matrix <- data.frame(d15NPl = 10.30, d13CPl = -11.58)
#' sample.list <- iso.euclidean2(source.matrix, mixture.matrix)
#' mcm.table <- iso.mcm(sample.list.list, 4)
#' print(mcm.table[1:10, ])

iso.mcm <-
function(source.list, numbers, run = 1000) #source.list is the output of function "iso.euclidean2", numbers represent the how many sources in your research, run represent how many MCM cycle in this function 
{
random.eu <- c(NULL)
random.eumatrix <- data.frame()
eu.v <- c(NULL)
eu.m <- data.frame()
if(length(source.list) == 0) stop("source.list cannot be 0")
for(i in 1 : run)
{
  rowno <- sample(1 : 1000, size  = numbers, replace = T) #the fist arg in this resample function should be the row numbers of sample list, not the arg "run", because of run == rownames(sample.list[i]), it's just a coincidence here
  for(j in 1 : numbers)
  {
    
    random.eu[j] <- 1/(source.list[[j]][rowno[j], 3])
    
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
