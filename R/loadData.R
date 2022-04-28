#' @title loadData
#' @description This function loads all of the csv files in a specified folder into the global 
#' environment.  The object names will be identical to the original file names.
#' @param dataPath default is \code{NULL} the folder containing files to be loaded into R.  If left 
#' as NULL, csv files in the\code{sampleAndesData} folder will be loaded
#' @param quiet default is \code{FALSE} Determines whether or not the script should output 
#' informational messages 
#' @return nothing - just loads data to environment
#' @family general_use
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export

loadData <- function(dataPath = NULL, quiet = FALSE){
  #add trailing "/" if necess
  if(substr(dataPath ,(nchar(dataPath)+1)-1,nchar(dataPath)) != "/")dataPath = paste0(dataPath,"/")
  
  filenames <- list.files(dataPath, pattern="\\.csv$")
  if (length(filenames)<1)stop("No csv files found")
  for(i in 1:length(filenames))
  {
    thisFile = filenames[i]
    thisFileName <- sub('\\.csv$', '', thisFile) 

    assign(thisFileName, utils::read.csv(file.path(dataPath,thisFile), stringsAsFactors=FALSE), envir = .GlobalEnv)
    if (!quiet) message("loaded ", dataPath,thisFile)
  }
}