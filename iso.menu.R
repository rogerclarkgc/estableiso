iso.menu <- function(){
  cat("Isotope euclideanmetric mixture model ver 0.99\n")
  cat("by roger\n")
  exit <- FALSE
  nonedata <- TRUE
  while(!exit){
    choieces <- c("Load in some data(run this first!)" , "Plot the mixture and source data", "Run the model and show the result","Run the test demo", "exit\n")
    choose <- menu(choieces, title =  c("EUCLIDEANMETRIC MODEL V0.99-MENU"))
    if(choose == 1){
      datalist <- iso.loaddata()
      nonedata <- FALSE
    }
    if(choose == 2){
      if(!nonedata)
        iso.plot(datalist)
      else{
          cat("Load the data first!\n")
          cat("Press <Enter> to continue\n")
          readline()
          invisible()
      }
    }
    if(choose == 3){
      choose1 <- menu(c("Using Monte carlo sampling Method.(faster)", "Using Monte carlo Markov chain sampling Method and Probability threshold(better robust, but slowly)"), title = ("Choose the method"))
      if(choose1 == 1){
        if(!nonedata){
          start <- Sys.time()
          cat("---step.1 Creating random sample list---\n")
          sample.list <- iso.euclidean2(source.matrix = datalist$sources, mixture.matrix = datalist$mixture)
          cat("---step.1 finished!---\n")
          cat("---step.2 Creating resample list---\n")
          mcm.table <- iso.mcm(source.list = sample.list, numbers = length(sample.list))
          cat("---step.2 finished---\n")
          cat("---step.3 Drawing histograms...---\n")
          iso.histograms(mcm.table, numbers = length(sample.list))
          end <- Sys.time()
          cat("All steps done!\n")
          cat("Time use: ")
          print(end - start)
          cat("Press <Enter> to continue\n")
          readline()
          invisible()
        }
        else{
          cat("Load the data first!\n")
          cat("Press <Enter> to continue\n")
          readline()
          invisible()
        }
      }
      if(choose1 == 2){
        if(!nonedata){
          start <- Sys.time()
          cat("---step.1 Creating random sample list and set the probability threshold---\n")
          threshold.list <- iso.threshold(source.matrix = datalist$sources, mixture.matrix = datalist$mixture, simplemode = FALSE, correctiso.matrix = NULL)
          cat("---step.1 finished!---\n")
          cat("---step.2 Creating resample list use MCMC method and probability threshold\n")
          mcmc.list <- iso.mcmc(threshold.result = threshold.list, numbers = length(threshold.list$sample_list), iter = 1000)
          apportion.table <- iso.apportion(mcmc.result = mcmc.list, run = 1000)
          cat("---step.2 finished---\n")
          cat("---step.3 Drawing histograms...---\n")
          iso.histograms(apportion.table, numbers = length(threshold.list$sample_list))
          end <- Sys.time()
          cat("All steps done!\n")
          cat("Time use: ")
          print(end - start)
          cat("Press <Enter> to continue\n")
          readline()
          invisible()
        }
        else{
          cat("Load the data first!\n")
          cat("Press <Enter> to continue\n")
          readline()
          invisible()
        }
      }
    }
    if(choose == 4){
      iso.demo()
    }
    if(choose == 5){
      cat("Thanks, have a nice day!\n")
      exit <- TRUE
    }
  }
}