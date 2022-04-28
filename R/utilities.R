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

#' @title getKeyFields
#' @description This function returns the key fields for any of the ESE tables
#' @param table default is \code{NULL}.  This is the name of the table for which the key fields are 
#' desired
#' @return nothing - just loads data to environment
#' @family internal
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' 
getKeyFields <- function(table=NULL){
  #for each known table, create a key from fields needed to create unique value
  #each table will need it's own group of fields to do so
  keyFields <- switch(table, 
                      "ESE_MISSIONS" =         c("MISSION"),
                      "ESE_SETS" =             c("MISSION", "SETNO"),
                      "ESE_CATCHES" =          c("MISSION", "SETNO", "SPEC"),
                      "ESE_BASKETS" =          c("MISSION", "SETNO", "SPEC", "SIZE_CLASS"),
                      "ESE_SPECIMENS" =        c("MISSION", "SETNO", "SPEC", "SIZE_CLASS","SPECIMEN_ID"),
                      "ESE_LV1_OBSERVATIONS" = c("MISSION", "SETNO", "SPEC", "SIZE_CLASS","SPECIMEN_ID","LV1_OBSERVATION_ID")
  )
  return(keyFields)
}

#' @title getTblKey
#' @description This function takes a data frame and a vector of field names from that data frame.
#' It concatenates together all of the combinations of values from those key fields into a vector.  
#' When appropriate fields are chosen as \code{keyFields}, the result can be used to create a 
#' primary key for identifying unique rows
#' @param df default is \code{NULL}.  This is a connection object from Mar.utils::make_oracle_cxn(). 
#' @param keyFields default is \code{NULL}.  This is a connection object from Mar.utils::make_oracle_cxn(). 
#' @return nothing - just loads data to environment
#' @family internal
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
getTblKey <- function(df=NULL, keyFields = NULL){
  #paste together keyFields of submitted data to create vector of unique values
  if (!all(keyFields %in% names(df))) {
    #the specified df does not have all of the keyfields
    return(NA)
  }
  if (length(keyFields)>1){
    uvec <- apply( df[ , keyFields ] , 1 , paste0 , collapse = "_" )
  }else{ 
    uvec <- sort(unique(df[,keyFields]))
  }
  uvec <- gsub(pattern = " ", replacement = "", x = uvec) 
  return(uvec)
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


#' @title addSizeClassToCatch
#' @description This function fills in the catch with the missing size class 2 entries.   
#' Andes does not provide a way to add Size class directly, Size classes are entered via the basket form.
#' @param basket A data frame containing the basket information from Andes
#' @param catch A data frame containing the catch information from Andes
#' @param quiet default is \code{FALSE} Determines whether or not the script should output 
#' informational messages 
#' @return the merged catch card
#' @family general_use
#' @author  Pablo Vergara, \email{Pablo.Vergara@@dfo-mpo.gc.ca}
#' @export
addSizeClassToCatch <- function(basket, catch, quiet = FALSE){
  
  # find all Size class 2 entries in basket to add to catch
  index =  basket$SIZE_CLASS == 2
  
  if(length(which(index)) == 0){
    if(!quiet) message("No Size Class 2 entries in Basket, nothing to do")
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
      if(!quiet) message(paste0("Class Size 2 was already entered for set#: ",temp$SETNO, " , species: ", temp$SPEC))
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
#' @param quiet default is \code{FALSE} Determines whether or not the script should output 
#' informational messages 
#' @importFrom magrittr %>%
#' @family internal
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
reFormatSpecimen <- function(x = NULL, quiet = FALSE){ 
  # Match data for level 1 observations 
  # Need to turn specimen table from wide format to long in order to have each observation on its own row
  specDets <- c(8,9,13:ncol(x))
  
  x_1 <- tidyr::gather(x, variable, value, dplyr::all_of(specDets))
  x_1<-x_1[,c("set_number", "species_code", "size_class", "id", "variable", "value")]
  colnames(x_1)[colnames(x_1)=="set_number"] <- "SETNO"
  colnames(x_1)[colnames(x_1)=="species_code"] <- "SPEC"
  colnames(x_1)[colnames(x_1)=="size_class"] <- "SIZE_CLASS"
  colnames(x_1)[colnames(x_1)=="id"] <- "SPECIMEN_ID"
  colnames(x_1)[colnames(x_1)=="variable"] <- "LV1_OBSERVATION"
  colnames(x_1)[colnames(x_1)=="value"] <- "DATA_VALUE"
  
  # remove 1) all NA values, 2) empty cells (i.e. ""), 3) cases of 1 character entries
  x_1 = x_1[!is.na(x_1$DATA_VALUE) & nchar(x_1$DATA_VALUE)>1,]
  
  #MMM - removed bit that auto-populated DATA_DESC from tmp_obs_types_data
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
  x_1$DATA_DESC <- NA
  x_1[x_1$LV1_OBSERVATION == "Collect Otoliths" & x_1$DATA_VALUE ==0 ,c("LV1_OBSERVATION","DATA_DESC")] <- c("Otolith not taken", "Age Material Type")
  x_1[x_1$LV1_OBSERVATION == "Collect Otoliths" & x_1$DATA_VALUE ==1 ,c("LV1_OBSERVATION","DATA_DESC")] <- c("Otolith Taken","Age Material Type")
  x_1[grepl(x_1$LV1_OBSERVATION, pattern = "Collect .*", ignore.case = T) & x_1$DATA_VALUE ==0 ,c("DATA_DESC")] <- c("Not collected")
  x_1[grepl(x_1$LV1_OBSERVATION, pattern = "Collect .*", ignore.case = T) & x_1$DATA_VALUE ==1 ,c("DATA_DESC")] <- c("Collected")
  x_1[x_1$LV1_OBSERVATION == "Sex" & x_1$DATA_VALUE ==0 ,c("DATA_DESC")] <- c("Undetermined") 
  x_1[x_1$LV1_OBSERVATION == "Sex" & x_1$DATA_VALUE ==1 ,c("DATA_DESC")] <- c("Male")
  x_1[x_1$LV1_OBSERVATION == "Sex" & x_1$DATA_VALUE ==2 ,c("DATA_DESC")] <- c("Female")
  x_1[x_1$LV1_OBSERVATION == "Sex" & x_1$DATA_VALUE ==3 ,c("DATA_DESC")] <- c("Berried or Hermaphrodite")
  x_1[x_1$LV1_OBSERVATION == "Stomach Fullness" & x_1$DATA_VALUE ==0 ,c("DATA_DESC")] <- c("Empty; No food contents")
  x_1[x_1$LV1_OBSERVATION == "Stomach Fullness" & x_1$DATA_VALUE ==1 ,c("DATA_DESC")] <- c("Less than 1/4 full")
  x_1[x_1$LV1_OBSERVATION == "Stomach Fullness" & x_1$DATA_VALUE ==2 ,c("DATA_DESC")] <- c("1/4 to 1/2 full")
  x_1[x_1$LV1_OBSERVATION == "Stomach Fullness" & x_1$DATA_VALUE ==3 ,c("DATA_DESC")] <- c("1/2 to 3/4 full")
  x_1[x_1$LV1_OBSERVATION == "Stomach Fullness" & x_1$DATA_VALUE ==4 ,c("DATA_DESC")] <- c("3/4 to full")
  x_1[x_1$LV1_OBSERVATION == "Stomach Fullness" & x_1$DATA_VALUE ==5 ,c("DATA_DESC")] <- c("Everted")
  x_1[x_1$LV1_OBSERVATION == "Stomach Fullness" & x_1$DATA_VALUE ==6 ,c("DATA_DESC")] <- c("Regurgitated")
  x_1[x_1$LV1_OBSERVATION == "Stomach Fullness" & x_1$DATA_VALUE ==7 ,c("DATA_DESC")] <- c(NA)
  x_1[x_1$LV1_OBSERVATION == "Stomach Fullness" & tolower(x_1$DATA_VALUE) =="swf" ,c("DATA_DESC")] <- c("Saved Whole Frozen")
  x_1[x_1$LV1_OBSERVATION == "Maturity" & x_1$DATA_VALUE == 0,c("DATA_DESC")] <- c("Undetermined")
  x_1[x_1$LV1_OBSERVATION == "Maturity" & x_1$DATA_VALUE == 1,c("DATA_DESC")] <- c("Immature")
  x_1[x_1$LV1_OBSERVATION == "Maturity" & x_1$DATA_VALUE == 2,c("DATA_DESC")] <- c("Ripening 1")
  x_1[x_1$LV1_OBSERVATION == "Maturity" & x_1$DATA_VALUE == 3,c("DATA_DESC")] <- c("Ripening 2")
  x_1[x_1$LV1_OBSERVATION == "Maturity" & x_1$DATA_VALUE == 4,c("DATA_DESC")] <- c("Ripe (Mature)")
  x_1[x_1$LV1_OBSERVATION == "Maturity" & x_1$DATA_VALUE == 5,c("DATA_DESC")] <- c("Spawning (Running)")
  x_1[x_1$LV1_OBSERVATION == "Maturity" & x_1$DATA_VALUE == 6,c("DATA_DESC")] <- c("Spent")
  x_1[x_1$LV1_OBSERVATION == "Maturity" & x_1$DATA_VALUE == 7,c("DATA_DESC")] <- c("Recovering")
  x_1[x_1$LV1_OBSERVATION == "Maturity" & x_1$DATA_VALUE == 8,c("DATA_DESC")] <- c("Resting")
  return(x_1)
}

#' @title cleanfields
#' @description This function takes a field and replaces various odd "smart" curly quotes and 
#' apostrophes and replaces them with normal versions
#' @param data default is \code{NULL}.  This is a field in a data frame 
#' @family internal
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
cleanfields <- function(data= NULL){
  #remove leading trailing whitespace; replace multiple space/tabs with single space
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
#' @param quiet default is \code{FALSE} Determines whether or not the script should output 
#' informational messages 
#' @param x set object from ANdes raw data
#' @return The experiment code
#' @family general_use
#' @author  Pablo Vergara, \email{Pablo.Vergara@@dfo-mpo.gc.ca}
#' @export
setExperimentType <- function(x, quiet = FALSE){
  
  
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
  # 3	FAILED - Major damage to gea
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
    if(!quiet){ message("Experiment type set to 1")}
  }
  
  #NORMAL - FAIL
  index = (exp.num %in% valid.exp.num & set.res %in% c(3, 4, 5, 6))
  if(length(which(index)) > 0 ){
    x[index,]$experiment_type_id_tweaked = 3
    if(!quiet){ message("Experiment type set to 3")}
  }
  
  #NORMAL - HYDROGRAPHY
  #Added by MMM - not sure about using an NA as an identifier, but maybe in combo with exp.num==9 
  index = (exp.num == 9 & is.na(set.res))
  if(length(which(index)) > 0 ){
    x[index,]$experiment_type_id_tweaked = 9
    if(!quiet){ message("Experiment type set to 9")}
  }
  
  
  return(as.numeric(x$experiment_type_id_tweaked))
}

##### PATH DEFINITON #####

# default path in repo where CSV files are stored
#MMM removed these - makes less sense as a package
# .andesData = list()
# .andesData$defaultCsvPath <- "inst/sampleData/"

# other paths...

