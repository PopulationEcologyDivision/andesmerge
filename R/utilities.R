#' @title getEseTables
#' @description This function returns the names of the ese tables.
#' @return character vector of the 6 core ESE tables
#' @family internal
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
#' 
getEseTables <- function(){
  return(c("ESE_MISSIONS", "ESE_SETS", "ESE_CATCHES", "ESE_BASKETS", "ESE_SPECIMENS", "ESE_LV1_OBSERVATIONS")) 
}

#' @title cleanEse
#' @description This function removes any ESE objects from the R environment
#' @return nothing - just remove data frames from environment
#' @family internal
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
cleanEse <- function(){
 eseObs <- getEseTables()
 rm(list = eseObs, envir = .GlobalEnv)
}

#' @title meters2Fathoms
#' @description This function converts a vector of values from meter to fathoms
#' @param field default is \code{NULL}. This is the field that should be converted.
#' @return vector
#' @family internal
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
#' 
meters2Fathoms <- function(field = NULL) {
  field <- field/1.8288
  return(field)
}


# trunc_digits <- function(x,digits){e <- 10^digits; trunc(x*e) / e}

#' @title cleanStrata
#' @description This function takes the strata field, and ensures only the first 3 characters are 
#' returned#' @param x default is \code{NULL}.  This is a field in a data frame 
#' @family internal
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
cleanStrata <- function(x){
  x[grepl("z", x, ignore.case = T) & nchar(x)>3] <- substr(x[grepl("z", x, ignore.case = T) & nchar(x)>3],1,3)
  return(x)
}

#' @title addSizeClassToCatch
#' @description This function fills in the catch with the missing size class 2 entries.   
#' Andes does not provide a way to add Size class directly, Size classes are entered via the basket form.
#' @param basket A data frame containing the basket information from Andes
#' @param catch A data frame containing the catch information from Andes
#' @return the merged catch card
#' @family general_use
#' @author  Pablo Vergara, \email{Pablo.Vergara@@dfo-mpo.gc.ca}
#' @export
addSizeClassToCatch <- function(basket, catch){
  
  # find all Size class 2 entries in basket to add to catch
  index =  basket$SIZE_CLASS == 2
  
  if(length(which(index)) == 0){
    message("No Size Class 2 entries in Basket, nothing to do")
    return(catch)
  } 
  
  # Create a generic catch row
  temp = catch[1,]
  
  for(i in 1:length(which(index)))
  {
    temp$SETNO  = basket[index,][i,]$SETNO
    temp$SPEC = basket[index,][i,]$SPEC
    temp$NOTE = "New entry of Size Class 2 created from Basket information"
    temp$UNWEIGHED_BASKETS = NA
    temp$SIZE_CLASS = 2
    
    # We only want to create one entry in the catch card. Sometimes we will have more than one basket with size class 2, 
    index2 = catch$SETNO == temp$SETNO & catch$SPEC == temp$SPEC & catch$SIZE_CLASS == 2
    if(length(which(index2)) > 0){
      message(paste0("Class Size 2 was already entered for set#: ",temp$SETNO, " , species: ", temp$SPEC))
    }else{
      catch = rbind(catch, temp)
    }
  }
  
  # return the catch card
  return(catch)
}

#' @title reFormatSpecimen
#' @description This function takes the tempSpecimen object, and populates the DATA_DEC 
#' appropriately depending on the values found
#' @param x default is \code{NULL}.  This is a field in a data frame 
#' @importFrom dplyr %>%
#' @family internal
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
reFormatSpecimen <- function(x = NULL){ 
  
  colnames(x)[colnames(x)=="set_number"]    <- "SETNO"
  colnames(x)[colnames(x)=="species_code"]  <- "SPEC"
  colnames(x)[colnames(x)=="size_class"]    <- "SIZE_CLASS"
  colnames(x)[colnames(x)=="id"]            <- "SPECIMEN_ID"
  
  y <- list()
  y$specimen <- x[,c("SETNO", "SPEC", "SIZE_CLASS", "SPECIMEN_ID")]
  
  if (!require(dplyr)) install.packages('dplyr')
  library(dplyr)
  # Match data for level 1 observations 
  # Need to turn specimen table from wide format to long in order to have each observation on its own row

  specDets <- c(8,9,13:ncol(x))
  x_1 <- tidyr::gather(x, variable, value, dplyr::all_of(specDets))
  x_1<-x_1[,c("SETNO", "SPEC", "SIZE_CLASS", "SPECIMEN_ID", "variable", "value")]
  colnames(x_1)[colnames(x_1)=="variable"] <- "LV1_OBSERVATION"
  colnames(x_1)[colnames(x_1)=="value"] <- "DATA_VALUE"
  
  # remove 1) all NA values, 2) empty cells (i.e. ""), 3) cases of 1 character entries
  x_1 = x_1[!is.na(x_1$DATA_VALUE) & nchar(x_1$DATA_VALUE)>1,]
  
  x_1[,"DATA_VALUE"]<-cleanfields(x_1[,"DATA_VALUE"])
  x_1[,"LV1_OBSERVATION"]     <-cleanfields(x_1[,"LV1_OBSERVATION"])
  # periods were introduced in the data frame names - remove them 
  x_1$LV1_OBSERVATION = gsub("\\."," ", x_1$LV1_OBSERVATION)    
  x_1$LV1_OBSERVATION = gsub("_"," ", x_1$LV1_OBSERVATION)
  #the following two values show up that cannot be matched against obstypes
  x_1$LV1_OBSERVATION[x_1$LV1_OBSERVATION=="comment"]<-"comments"
  x_1$LV1_OBSERVATION[x_1$LV1_OBSERVATION=="maturity maritimes"]<-"Maturity"
  x_1$LV1_OBSERVATION =  stringi::stri_trans_totitle(x_1$LV1_OBSERVATION)
  
  #add LV1_OBSERVATION_ID by grouping by Specimen_id, and then numbering all measurements within the group
  x_1 <- x_1 %>%
    dplyr::group_by(SETNO, SPEC, SIZE_CLASS, SPECIMEN_ID) %>%
    dplyr::arrange(LV1_OBSERVATION, .by_group = TRUE) %>% 
    dplyr::mutate(LV1_OBSERVATION_ID =dplyr::row_number()) %>%
    as.data.frame()
  
  y$LV1_OBSERVATION <- x_1
  return(y)
}

#' @title populate_DATA_DESC
#' @description This function populates the DATA_DESCR field of the LV1_OBSERVATION DATA_DESC 
#' appropriately depending on the values found
#' @param x default is \code{NULL}.  This is a field in a data frame 
#' @family internal
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
populate_DATA_DESC <- function(x){
  x$DATA_DESC <- NA
  x[x$LV1_OBSERVATION == "Collect Otoliths" & x$DATA_VALUE ==0 ,c("LV1_OBSERVATION","DATA_DESC")] <- c("Otolith not taken", "Age Material Type")
  x[x$LV1_OBSERVATION == "Collect Otoliths" & x$DATA_VALUE ==1 ,c("LV1_OBSERVATION","DATA_DESC")] <- c("Otolith Taken","Age Material Type")
  x[grepl(x$LV1_OBSERVATION, pattern = "Collect .*", ignore.case = T) & x$DATA_VALUE ==0 ,c("DATA_DESC")] <- c("Not collected")
  x[grepl(x$LV1_OBSERVATION, pattern = "Collect .*", ignore.case = T) & x$DATA_VALUE ==1 ,c("DATA_DESC")] <- c("Collected")
  x[x$LV1_OBSERVATION == "Sex" & x$DATA_VALUE ==0 ,c("DATA_DESC")] <- c("Undetermined") 
  x[x$LV1_OBSERVATION == "Sex" & x$DATA_VALUE ==1 ,c("DATA_DESC")] <- c("Male")
  x[x$LV1_OBSERVATION == "Sex" & x$DATA_VALUE ==2 ,c("DATA_DESC")] <- c("Female")
  x[x$LV1_OBSERVATION == "Sex" & x$DATA_VALUE ==3 ,c("DATA_DESC")] <- c("Berried or Hermaphrodite")
  x[x$LV1_OBSERVATION == "Stomach Fullness" & x$DATA_VALUE ==0 ,c("DATA_DESC")] <- c("Empty; No food contents")
  x[x$LV1_OBSERVATION == "Stomach Fullness" & x$DATA_VALUE ==1 ,c("DATA_DESC")] <- c("Less than 1/4 full")
  x[x$LV1_OBSERVATION == "Stomach Fullness" & x$DATA_VALUE ==2 ,c("DATA_DESC")] <- c("1/4 to 1/2 full")
  x[x$LV1_OBSERVATION == "Stomach Fullness" & x$DATA_VALUE ==3 ,c("DATA_DESC")] <- c("1/2 to 3/4 full")
  x[x$LV1_OBSERVATION == "Stomach Fullness" & x$DATA_VALUE ==4 ,c("DATA_DESC")] <- c("3/4 to full")
  x[x$LV1_OBSERVATION == "Stomach Fullness" & x$DATA_VALUE ==5 ,c("DATA_DESC")] <- c("Everted")
  x[x$LV1_OBSERVATION == "Stomach Fullness" & x$DATA_VALUE ==6 ,c("DATA_DESC")] <- c("Regurgitated")
  x[x$LV1_OBSERVATION == "Stomach Fullness" & x$DATA_VALUE ==7 ,c("DATA_DESC")] <- c(NA)
  x[x$LV1_OBSERVATION == "Stomach Fullness" & tolower(x$DATA_VALUE) =="swf" ,c("DATA_DESC")] <- c("Saved Whole Frozen")
  x[x$LV1_OBSERVATION == "Maturity" & x$DATA_VALUE == 0,c("DATA_DESC")] <- c("Undetermined")
  x[x$LV1_OBSERVATION == "Maturity" & x$DATA_VALUE == 1,c("DATA_DESC")] <- c("Immature")
  x[x$LV1_OBSERVATION == "Maturity" & x$DATA_VALUE == 2,c("DATA_DESC")] <- c("Ripening 1")
  x[x$LV1_OBSERVATION == "Maturity" & x$DATA_VALUE == 3,c("DATA_DESC")] <- c("Ripening 2")
  x[x$LV1_OBSERVATION == "Maturity" & x$DATA_VALUE == 4,c("DATA_DESC")] <- c("Ripe (Mature)")
  x[x$LV1_OBSERVATION == "Maturity" & x$DATA_VALUE == 5,c("DATA_DESC")] <- c("Spawning (Running)")
  x[x$LV1_OBSERVATION == "Maturity" & x$DATA_VALUE == 6,c("DATA_DESC")] <- c("Spent")
  x[x$LV1_OBSERVATION == "Maturity" & x$DATA_VALUE == 7,c("DATA_DESC")] <- c("Recovering")
  x[x$LV1_OBSERVATION == "Maturity" & x$DATA_VALUE == 8,c("DATA_DESC")] <- c("Resting")
  return(x)
}
#' @title cleanfields
#' @description This function takes a field and replaces various odd "smart" curly quotes and 
#' apostrophes and replaces them with normal versions
#' @param data default is \code{NULL}.  This is a field in a data frame 
#' @family internal
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
cleanfields <- function(data= NULL){
  #remove leading & trailing whitespace; replace multiple space/tabs with single space
  data <- trimws(gsub(x=data, pattern = ("\\s+"), ' '))
  
  #replace other stuff with nothing
  data <- gsub(x = data, pattern = "\u00e2\u20ac\u2122", replacement = "'") # replace curly apostrophe (â€™) with '
  data <- gsub(x = data, pattern = '\u00e2\u20ac\u0153', replacement = '"') # replace curly open quote (â€œ) with "
  data <- gsub(x = data, pattern = '\u00e2\u20ac\u009d', replacement = '"') # replace curly close quote (â€)with "
  return(data)
}

#' @title setExperimentType
#' @description This function sets the proper experiment type to match existing data structures in Maritime and Gulf region
#' . Based on a mix of experiment typer and set results  
#' @param x set object from ANdes raw data
#' @return The experiment code
#' @family general_use
#' @author  Pablo Vergara, \email{Pablo.Vergara@@dfo-mpo.gc.ca}
#' @export
setExperimentType <- function(x){
 
  #exp.num =  x$experiment_type_id
  exp.num =  as.numeric(gsub("(^[0-9]{1,2})(.*$)", "\\1", x$experiment_type))
  set.res =  x$set_result_id
  
  
  # possible experiment types (from : andesdb.shared_models_experimenttype)
  #   1	Stratified random survey set		
  #   5	Comparative fishing experiment	
  #   6	Tagging set		
  #   9	Hydrography		
  #   7	Gear testing		
  #   99	Systematic		
  valid.exp.num = c(1, 5, 6, 7, 9, 99)
  if(!all(exp.num %in% valid.exp.num)) stop("Unknown Experiment type(s) found")
  x$experiment_type_id_tweaked <- NA
  x$experiment_type_id_tweaked <- as.numeric(gsub("(^[0-9]{1,2})(.*$)", "\\1", x$experiment_type))
  
  # Possible set result values (from  : andesdb.shared_models_setresult)
  # 1	NORMAL - No damage to gear	
  # 2	NORMAL - Minor damage to gear - Catch unaffected	NORMAL 
  # 3	FAILED - Major damage to gear
  # 4	FAILED - Bad depth
  # 5	FAILED - Fishing gears
  # 6	FAILED - Wrong gear operation
  # MMM handle hydrography - exp.num = 9 and is.na(set.res)
  # NA Normal - only if exp.num ==9 - Hydrography
  valid.result.num = c(1, 2, 3, 4, 5, 6, NA)
  if(!all(set.res %in% valid.result.num)) stop("Set result not in list")
  
  #NORMAL - GOOD
  index = (exp.num %in% valid.exp.num & set.res %in% c(1, 2))
  if(length(which(index)) > 0)
  {
    x[index,]$experiment_type_id_tweaked = 1
  }
  
  #NORMAL - FAIL
  index = (exp.num %in% valid.exp.num & set.res %in% c(3, 4, 5, 6))
  if(length(which(index)) > 0 ){
    x[index,]$experiment_type_id_tweaked = 3
  }
  
  #NORMAL - HYDROGRAPHY
  #Added by MMM - not sure about using an NA as an identifier, but maybe in combo with exp.num==9 
  index = (exp.num == 9 & is.na(set.res))
  if(length(which(index)) > 0 ){
    x[index,]$experiment_type_id_tweaked = 9
  }
  
  return(as.numeric(x$experiment_type_id_tweaked))
}

applySubsampling <- function(catch=NULL, basket = NULL){

  browser()  
}