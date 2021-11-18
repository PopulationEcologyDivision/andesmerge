dataPath <- "c:/git/MMMcMahon/andesmerge/samplAndesData/"

setwd("./samplAndesData")
# filenames <- list.files(full.names=TRUE)  
# All <- lapply(filenames,function(i){
#   read.csv(i, header=F)
# })


filenames <- list.files(path="./samplAndesData", pattern="xyz+.*csv")
oname = paste("file", i, sep="")
assign(oname, read.csv(paste(oname, ".txt", sep="")))



load_sammple_data <- function(dataPath = NULL){
  basket <- utils::read.csv(paste0(dataPath, "tmp_catch_data.csv"))
  catch <- utils::read.csv(paste0(dataPath, "tmp_cruise_data.csv"))
  cruise <- utils::read.csv(paste0(dataPath, "tmp_basket_data.csv"))
  obs_types <- utils::read.csv(paste0(dataPath, "tmp_obs_types_data.csv"))
  set <- utils::read.csv(paste0(dataPath, "tmp_set_data.csv"))
  specimen <- utils::read.csv(paste0(dataPath, "tmp_specimen_data.csv"))
}