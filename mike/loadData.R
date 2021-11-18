#' @title loadData
#' @description This function loads all of the csv files in a specified folder into the global 
#' environment.  The object names will be identical to the original file names.
#' @param dataPath default is \code{NULL} the folder containing files to be loaded into R.  If left 
#' as NULL, csv files in the\code{sampleAndesData} folder will be loaded
#' @return nothing - just loads data to environment
#' @family general_use
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
loadData <- function(dataPath = NULL){
  if (is.null(dataPath)){
    setwd(here::here())
    dataPath="samplAndesData"
    message("Loading sample data")
  }
  filenames <- list.files(dataPath, pattern=".*csv")
  for(i in 1:length(filenames))
  {
    thisFile = filenames[i]
    thisFileName <- sub('\\.csv$', '', thisFile) 
    assign(thisFileName, read.csv(file.path(dataPath,thisFile)))
  }
}
