#' Set the probability threshold of the source combination vector
#' 
#' This funcition is used to set a "threshold" of montel carlo markov chain 
#' iso.threshold(source.matrix = 0, mixture.matrix = 0, correctiso.matrix = NULL, simplemode = FALSE, howmany = 0.1)
#' @param source.matrix a dataframe of source
#' @param mixture.matrix a dataframe of mixture
#' @param correctiso.matrix a dataframe of correct matrix of sources
#' @param simplemode the result will be a list if set to FALSE
#' @param howmany how many simple will be used to caculate the threshold, 0.1 means 10% of data will be used to caculate
#' @export
#' @examples
iso.threshold <-
  function(source.matrix = 0 , mixture.matrix = 0 ,correctiso.matrix = NULL, simplemode = FALSE, howmany = 0.1)
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
    rowno <- nrow(sample.list[[1]])
    sourceno <- length(sample.list)
    likehood <- 0
    TH <- 0
    likehoodframe <- data.frame()
    probframe <- data.frame()
    for(k in 1 : (rowno * howmany) ){
      prob <- 1
      for(l in 1 : sourceno){
        sampleone<- sample.list[[l]][(sample(rowno, 1, replace = FALSE)), ]
        sample.prob1 <- pnorm(sampleone[, 1], sd = source.matrix[l, 3], mean = source.matrix[l, 4])
        prob <- prob * sample.prob1
        #cat("prob:", prob, "\n")
      }
      #cat("prob:", prob, "\n")
      probframe <- rbind(probframe, prob)
      if(prob > likehood)
        likehood <- prob
      #likehoodframe <- rbind(likehoodframe, likehood)
    }
    TH <- apply(probframe, 2, function(x) quantile(x, 2, prob = .9)) #TH will determinde the running time of function "iso.mcmc"
    if(simplemode)
      return(sample.list)
    else
      return(list(sample_list = sample.list, threshold = TH, frame = likehoodframe, source.matrix = source.matrix, probframe = probframe))
  }
