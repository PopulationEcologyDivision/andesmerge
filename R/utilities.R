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

#' @title cleanStrata
#' @description This function takes the strata field, and ensures only the first 3 characters are 
#' returned
#' @param x default is \code{NULL}.  This is a field in a data frame 
#' @family internal
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
cleanStrata <- function(x){
  x[grepl("z", x, ignore.case = T) & nchar(x)>3] <- substr(x[grepl("z", x, ignore.case = T) & nchar(x)>3],1,3)
  return(x)
}

#' @title reFormatSpecimen
#' @description This function takes the tempSpecimen object, reformats it from wide to long, 
#' dropping all NA/empty data.
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
  y$specimen <- x[,c("MISSION", "SETNO", "SPEC", "SIZE_CLASS", "SPECIMEN_ID")]
  # Match data for level 1 observations 
  # Need to turn specimen table from wide format to long in order to have each observation on its own row
  specDets <- names(x[, !names(x) %in% c("MISSION", "SETNO", "SPEC", "SIZE_CLASS", "SPECIMEN_ID","basket_id")])
  x <- x %>% dplyr::mutate_all(as.character) %>% tidyr::pivot_longer(dplyr::all_of(specDets), names_to = "variable", values_to = "value") %>% as.data.frame()
  colnames(x)[colnames(x)=="variable"] <- "LV1_OBSERVATION"
  colnames(x)[colnames(x)=="value"] <- "DATA_VALUE"  
  # remove 1) all NA values, 2) empty cells (i.e. ""), 3) cases of 1 character entries
  x <- x[!is.na(x$DATA_VALUE) & nchar(x$DATA_VALUE)>0,]
  x[,"DATA_VALUE"]      <-cleanfields(x[,"DATA_VALUE"])
  x[,"LV1_OBSERVATION"] <-cleanfields(x[,"LV1_OBSERVATION"])
  # periods were introduced in the data frame names - remove them 
  x$LV1_OBSERVATION = gsub("\\."," ", x$LV1_OBSERVATION)    
  x$LV1_OBSERVATION = gsub("_"," ", x$LV1_OBSERVATION)
  #the following two values show up that cannot be matched against obstypes
  x$LV1_OBSERVATION[x$LV1_OBSERVATION=="comment"]<-"comments"
  x$LV1_OBSERVATION[x$LV1_OBSERVATION=="maturity maritimes"]<-"maturity"
  x$LV1_OBSERVATION =  stringi::stri_trans_totitle(x$LV1_OBSERVATION)
  
  #add LV1_OBSERVATION_ID by grouping by Specimen_id, and then numbering all measurements within the group
  x <- x %>%
    dplyr::group_by(MISSION,SETNO, SPEC, SIZE_CLASS, SPECIMEN_ID) %>%
    dplyr::arrange(LV1_OBSERVATION, .by_group = TRUE) %>% 
    dplyr::mutate(LV1_OBSERVATION_ID =dplyr::row_number()) %>%
    as.data.frame()
  x$basket_id <- NULL
  y$LV1_OBSERVATION <- x
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

#' @title charToBinary
#' @description This function converts the values of "True" and "False" output by andes into either
#' TRUE/FALSE or Y/N
#' @param x default is \code{NULL}.  This is a vector of values.  Case-insensitive, and anything
#' other than "true" or "false" will become NA 
#' @param bool default is \code{TRUE}. By default, this function turns values into TRUE/FALSE, but 
#' if \code{bool} is F, it will output "Y" and "N" (for "yes" and "no")
#' @family internal
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
charToBinary <- function(x = NULL, bool=T){
  x <- tolower(x)
  if (bool){
    x <- ifelse(x == "true", T, ifelse(x == "false", F, NA))
  }else{
    x <- ifelse(x == "true", "Y", ifelse(x == "false", "N", NA))
  }
  return(x)
}

#' @title reDistributeMixedCatch
#' @description This function does stuff
#' @param catch default is \code{NULL}.  
#' @param basket default is \code{NULL}.  
#' @family internal
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
reDistributeMixedCatch <- function(catch=NULL, basket = NULL){
  x <- list()
message("Mixed catches:")
theMsg <- NA
allMixedCatch        <- catch[(catch$is_parent | !is.na(catch$parent_catch_id)),]
parentIDs            <- unique(allMixedCatch[!is.na(allMixedCatch$parent_catch_id),"parent_catch_id"])

if (length(parentIDs)<1){
  message("(No mixed catches found - skipping subsampling)")
  x$basket <- basket
  x$catch <- catch
  if (!is.na(theMsg)) message(theMsg)
  return(x)
}
for (i in 1:length(parentIDs)){
  mixedBaskets                     <- basket[basket$catch_id == parentIDs[i],]
  if (nrow(mixedBaskets[!mixedBaskets$SAMPLED,])==0 && nrow(allMixedCatch[allMixedCatch$id %in% parentIDs[i],])>0){
      message("\tThe following CATCH record was flagged as having mixed catch child records, but none were found.  It was deleted.")
      print(allMixedCatch[allMixedCatch$id %in% parentIDs[i],])
      catch <- catch[-which(catch$id %in% parentIDs[i]),]
      next
    }else if (nrow(mixedBaskets[!mixedBaskets$SAMPLED,])>1){
      warning("check that reDistributeMixedCatch() handles this many mixed baskets properly")
    }
 
  sampB <- basket[basket$catch_id %in% allMixedCatch[allMixedCatch$parent_catch_id %in% parentIDs[i],"id"],]
  colnames(sampB)[colnames(sampB)=="id"] <- "basket_id"
  colnames(sampB)[colnames(sampB)=="catch_id"] <- "basket_catch_id"
  sampC <- catch[catch$id %in% sampB$basket_catch_id,]
  sampled <- merge(sampC, sampB, all.y=T)
  unsampC <- allMixedCatch[allMixedCatch$id == parentIDs[i],]
  unsampB <- basket[basket$catch_id %in% unsampC$id & !basket$SAMPLED,]
  colnames(unsampB)[colnames(unsampB)=="id"] <- "basket_id"
  colnames(unsampB)[colnames(unsampB)=="catch_id"] <- "basket_catch_id"
  unsampled <- merge(unsampC, unsampB, all.y=T)
  newRecs <- sampled
  newRecs$id <- unsampled$id
  newRecs$parent_catch_id <- unsampled$parent_catch_id

  newRecs$PROP_WT           <- round(newRecs$BASKET_WEIGHT/sum(newRecs$BASKET_WEIGHT),5)
  newRecs$WT_EACH           <- round(newRecs$BASKET_WEIGHT/newRecs$NUMBER_CAUGHT,5)
  colnames(newRecs)[colnames(newRecs)=="NUMBER_CAUGHT"] <- "NUM_smpld"
  colnames(newRecs)[colnames(newRecs)=="BASKET_WEIGHT"] <- "BW_smpld"

  newRecs$Parent_unsampled <- unsampled$BASKET_WEIGHT
  newRecs$NUM <- NA
  newRecs$BW_calc       <- round(newRecs$Parent_unsampled*newRecs$PROP_WT,3)
  newRecs$NUM_calc      <- round(newRecs$BW_calc/newRecs$WT_EACH,0)
  newRecs$Parent_sampled<- mixedBaskets[mixedBaskets$SAMPLED,"BASKET_WEIGHT"]

  oldCatch <- allMixedCatch[allMixedCatch$id %in% parentIDs[i],]
  catch  <- catch[-which(catch$id %in% parentIDs[i]),]
  oldBaskets <- basket[ which(basket$catch_id %in% parentIDs[i]),]
  basket <- basket[-which(basket$catch_id %in% parentIDs[i]),]
  
  newRecs$NUM_new <- newRecs$NUM_calc + newRecs$NUM_smpld
  evidenceTable <- newRecs[,c("MISSION", "SETNO", "SPEC", "Parent_sampled", "Parent_unsampled", "BW_smpld", "PROP_WT", "NUM_smpld", "WT_EACH", "BW_calc", "NUM_calc", "NUM_new")]

  
  newRecs$PROP_WT <- newRecs$WT_EACH <- newRecs$Parent_unsampled <- newRecs$Parent_sampled <- newRecs$BW_smpld <- newRecs$NUM_calc <- newRecs$NUM_smpld <- newRecs$NUM <- newRecs$id <- NULL
  
  colnames(newRecs)[colnames(newRecs)=="BW_calc"] <- "BASKET_WEIGHT"
  colnames(newRecs)[colnames(newRecs)=="NUM_new"] <- "NUMBER_CAUGHT"
  newCatch  <- newRecs[,c("MISSION", "SETNO", "SPEC", "basket_catch_id","NOTE", "UNWEIGHED_BASKETS", "NUMBER_CAUGHT", "is_parent", "parent_catch_id", "SIZE_CLASS")]
  colnames(newCatch)[colnames(newCatch)=="basket_catch_id"] <- "id"
  catch <- catch[-which(catch$id %in% newCatch$id),]
  newBaskets <- newRecs[,c("MISSION", "SETNO", "SPEC", "SIZE_CLASS", "BASKET_WEIGHT", "SAMPLED", "basket_id", "basket_catch_id")]
  colnames(newBaskets)[colnames(newBaskets)=="basket_id"] <- "id"
  colnames(newBaskets)[colnames(newBaskets)=="basket_catch_id"] <- "catch_id"
  newBaskets$id <- oldBaskets[!oldBaskets$SAMPLED,"id"]
  sampledBaskets <- sampled[,c("MISSION", "SETNO", "SPEC", "SIZE_CLASS", "BASKET_WEIGHT", "SAMPLED", "basket_id", "basket_catch_id")] 
  colnames(sampledBaskets)[colnames(sampledBaskets)=="basket_id"] <- "id"
  colnames(sampledBaskets)[colnames(sampledBaskets)=="basket_catch_id"] <- "catch_id"
  message("##########")
  message("Bumping up unsampled mixed catch data:")
  message("CATCH records:")
  message("\tDeleted records:")
  print(oldCatch)
  message("\tInitial, existing, sampled Catch records:")
  print(sampC)
  message("\tReplacement Catch records (NUM_CAUGHT now includes estimates from unsampled baskets):")
  print(newCatch)
  message("BASKET records:")
  message("\tDeleted Basket records:")
  print(oldBaskets)
  message("\tExisting, sampled Basket records:")
  print(sampledBaskets)
  message("\tReplacement Basket records:")
  print(newBaskets)
  message("\n\tThe data below can be used to check the calculation of the bumped up values")
  print(evidenceTable)
  message("##########")
  basket <- rbind(basket, newBaskets)
  catch <- rbind(catch, newCatch)
}
basket$id <- basket$catch_id <- NULL
catch$id <- catch$is_parent <- catch$parent_catch_id <- NULL
x$basket <- basket
x$catch <- catch
if (!is.na(theMsg)) message(theMsg)
return(x)
#######
  }
