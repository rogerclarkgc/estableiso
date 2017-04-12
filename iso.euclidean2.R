#' Generate the sample list of your input
#'
#' Running this function is always the first step of this model, a sample list, which contains the euclideanmetric distance between the mixture and sources is the output of this function
#' iso.euclidean2(source.matrix = 0, mixture.matrix = 0, correctiso.matrix = NULL)
#' @param source.matrix the data of source, must be a data.frame
#' @param mixture.matrix the data of mixture, must be a data.frame
#' @param correctiso the data of correctiso matrix, must be a data.frame
#' @export
#' @examples
#' source <- data.frame(d15NPl = c(6.50, 4.42, 11.19, 9.82), d13CPl = c(-11.17, -30.88, -10.19, -15.01), sd.N = c(1.4594632, 2.2680709, 1.1124385, 0.8271039), mean.N = c(6.488984, 4.432160, 11.192613, 9.816280), sd.C = c(1.2149562, 0.6413182, 1.9593306, 1.1724677), mean.C = c(-11.17023, -30.87984, -11.17090, -14.05701))
#' mixture <- data.frame(d15NPl = 10.30, d13CPl = -11.58)
#' samplelist <- iso.euclidean2(source, mixture)
#' print(samplelist[[1]][1:10, ])
iso.euclidean2 <-
function(source.matrix = 0 , mixture.matrix = 0 ,correctiso.matrix = NULL)
{
  #time.start <- Sys.time()
  #source.eu <- c(NULL)
  #source.ap <- c(NULL)
  
  sample.matrix <- data.frame()
  sample.list <- list()
  if(length(source.matrix) == 0  | length(mixture.matrix) == 0) stop("source matrix and mixture matrix cannot be zero") 
  if(length(correctiso.matrix) != 0)
  {
    source.matrix <- source.matrix + correctiso.matrix
    cat("Source matrix has been corrected ")
  }
  for(i in 1:nrow(source.matrix))			#the first circulation structure is the source numbers 
  {
    j <- 1
    while(j <= 1000)
    {
      source.sample <- c(rnorm(1, mean = source.matrix[i, 4], sd = source.matrix[i, 3]), rnorm(1, mean = source.matrix[i, 6], sd = source.matrix[i, 5]))  	#get the rnorm density for source i
      
      
      
      
        
        rho <- euclideanmetric(source.sample, mixture.matrix) #caculate the euclidean metric between mixture and 1 rnorm sample
        source.sample[3] <- rho
        sample.matrix[j, 1] <- source.sample[1] # get a sample matrix for source i 
        sample.matrix[j, 2] <- source.sample[2]
        sample.matrix[j, 3] <- source.sample[3]
        cat("random source sample :", source.sample,"  ")
        cat("this is the",j, "round!\n")
        j <- j + 1
      
      attributes(sample.matrix)$names <- c(colnames(source.matrix)[1], colnames(source.matrix)[2], "rho")    
    }
    sample.list [[i]] <- sample.matrix
    
    
  }
  return(sample.list)
}
