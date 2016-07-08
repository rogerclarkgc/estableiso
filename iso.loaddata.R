#' Load the data from R objects or files
#'
#' This function can guide the user load data from R objects or files, don't forget to use '<-' to assign a object
#'
#' @export
#' @examples
#' datalist <- iso.loaddata()

iso.loaddata <-
function(){
  choieces <- c("Load data from R objects", "Load data from files")
  examplesource<- data.frame(n = c(6.5, 4.42,11.19), c = c(-11.17, -30.88, -10.19), sd.n = c(1.46, 2.27, 1.11), mean.n = c(6.49, 4.43, 11.20), sd.c = c(1.12, 0.64, 1.96), mean.c = c(-11.17, -30.88, -11.17))
  cat("you can load data from files or R objects\n")
  again <- TRUE
  while(again){
    choose <- menu(choieces, title = c("which one do you want?"))
    if (choose == 1){
    cat("Load data from R objects\n")
    cat("1. Enter the name of object which contains the target(mixture) data. \n")
    dataexists <- FALSE
    while(dataexists == FALSE){
      datatemp <- scan(what = "", nlines = 1, quiet = TRUE)
      while(length(datatemp) == 0)
        datatemp <- scan(what = "", nlines = 1, quiet = TRUE)
      if(exists(datatemp) == FALSE)
        cat("Object not found. check you typing. \n")
      else{
        mixture <- get(datatemp)
        dataexists <- TRUE
      }
    }
    cat("2.Enter the name of the object which contains the source \n")
    cat("the source data must look like this one\n")
    print(examplesource)
    cat("\n")
    sourcesexists <- FALSE
    while(sourcesexists == FALSE){
      sourcestemp <- scan(what = "", nlines = 1, quiet = TRUE)
      while(length(sourcestemp) == 0)
        sourcestemp <- scan(what = "", nlines = 1, quiet = TRUE)
      if(!exists(sourcestemp)){
        cat("Object not found. check your typing")
      }
      else{
        sources <- get(sourcestemp)
        sourcesexists <- TRUE
      }
      PATH = NULL
      
    }
  }
    if(choose == 2){
    cat("Load data from files \n")
    BADPATH <- TRUE
    while(BADPATH == TRUE){
      cat("First input the directory of your source data:\n")
      cat("The PATH should write like this blow:\n", "E:/ \n")
      PATH <- scan(what = "", nlines = 1, quiet = TRUE)
      while(length(PATH) == 0)
        PATH <- scan(what = "", nlines = 1, quiet = TRUE)
      if(file.exists(PATH) == TRUE)
        BADPATH <- FALSE
      else
        cat("Cannot find the directory, check your input.\n")
    }
    BADDATA <- TRUE
    while(BADDATA == TRUE){
      cat("Now input the name of the target(mixture) file \n")
      DATAFILE <- scan(what = "", nlines = 1, quiet = TRUE)
      while(length(DATAFILE) == 0)
        DATAFILE <- scan(what = "", nlines = 1, quiet = TRUE)
      if(file.exists(paste(PATH, "/", DATAFILE, sep = "")) == TRUE)
        BADDATA = FALSE
      else
        cat("Cannot find this file, check your typing\n")	
    }
    BADSOURCES <- TRUE
    while(BADSOURCES == TRUE){
      cat("Now input the name of the source isotope file \n")
      cat("The data file must look like this below: \n")
      print(examplesource)
      SOURCEFILE <- scan(what = "", nlines = 1, quiet = TRUE)
      while(length(SOURCEFILE ) == 0)
        SOURCEFILE <- scan(what = "", nlines = 1, quiet = TRUE)
      if(file.exists(paste(PATH, "/", SOURCEFILE, sep = "")) == TRUE)
        BADSOURCES <- FALSE
      else
        cat("Cannot find this file, check your typing \n")
    }
    cat("Loading in data... \n", "DONE\n")
    mixture <- as.data.frame(read.csv(paste(PATH, "/", DATAFILE, sep = ""), header = TRUE))
    sources <- as.data.frame(read.csv(paste(PATH, "/", SOURCEFILE, sep = ""), header = TRUE))
    
  }
    cat("This is your data\n")
    print(list(mixture = mixture, sources = sources, PATH = PATH))
    cat("Check your input carefully, do you want to reset your data?\n")
    againchoose <- menu(c("NO", "YES"))
    if(againchoose == 1)
      again <- FALSE
    else
      again <- TRUE
  }
  return(list(mixture = mixture, sources = sources, PATH = PATH))
}
