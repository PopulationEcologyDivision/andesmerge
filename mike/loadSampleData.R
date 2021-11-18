loadSampleData <- function(dataPath = NULL){
  if (is.null(dataPath)){
    setwd(here::here())
    dataPath="samplAndesData"
  }
  filenames <- list.files(dataPath, pattern=".*csv")
  
  for(i in 1:length(filenames))
  {
    thisFile = filenames[i]
    thisFileName <- sub('\\.csv$', '', thisFile) 
    assign(thisFileName, read.csv(file.path(dataPath,thisFile)))
  }
}
