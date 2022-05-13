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

#' @title charToBoolean
#' @description This function converts the values of "True" and "False" output by andes into their
#' boolean R equivalents
#' @param x default is \code{NULL}.  This is a vector of values.  Case-insensitive, and anything
#' other than "true" or "false" will become NA 
#' @family internal
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
charToBoolean <- function(x = NULL){
  x <- tolower(x)
  x <- ifelse(x == "true", T, ifelse(x == "false", F, NA))
  return(x)
}

#' @title applySubsampling
#' @description This function does stuff
#' @param catch default is \code{NULL}.  
#' @param basket default is \code{NULL}.  
#' @family internal
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
applySubsampling <- function(catch=NULL, basket = NULL){
  message("Subsampling Mixed catches:\n")
  theMsg <- NA
  #Each mixed catch record should have a "sampled" basket where every species within was 
  #identified and weighed.  We then take the proportional weights determined for the sampled 
  #basket, and apply them to all of the other unsampled (but weighed) baskets.  
  
  #find the mixed catch parent records
  bumpableCatchParents   <- catch[catch$is_parent == T,]
  
  if (nrow(bumpableCatchParents)<1){
    message("(No mixed catches found - skipping subsampling)")
    x <- list()
    x$basket <- basket
    x$catch <- catch
    if (!is.na(theMsg)) message(theMsg)
    return(x)
  }
  parentIDs <- bumpableCatchParents$id
  
  for (i in 1:length(parentIDs)){
    
    knownMixedCatch        <- catch[catch$parent_catch_id %in% parentIDs[i],]
    knownReferenceBaskets  <- basket[basket$catch_id %in% knownMixedCatch$id,]
    basketsSampled         <- basket[basket$catch_id %in% parentIDs[i] & basket$SAMPLED,]
    basketsUnsampled       <- basket[basket$catch_id %in% parentIDs[i] & !basket$SAMPLED,]
    if(nrow(basketsUnsampled)==0){
      #turns out nothing is unsampled, and a catch records exists for each spp delete the catch record
      message("\tThe following CATCH record was flagged as a mixed-basket 'parent', but had no 
      \tunsampled child record. 'is_parent ' was set to FALSE")
      print(catch[which(catch$id %in% parentIDs[i]),])
      
      catch[which(catch$id %in% parentIDs[i]),"is_parent"] <- FALSE
      
      next
    }
    bumpedBaskets                    <- knownReferenceBaskets
    bumpedBaskets$PROPORTION         <- round(bumpedBaskets$BASKET_WEIGHT/sum(bumpedBaskets$BASKET_WEIGHT),5)
    bumpedBaskets$id                 <- basketsUnsampled$id
    bumpedBaskets$catch_id           <- basketsUnsampled$catch_id
    bumpedBaskets$BASKET_WEIGHT_orig <- basketsUnsampled$BASKET_WEIGHT
    bumpedBaskets$BASKET_WEIGHT      <- NA
    bumpedBaskets$BASKET_WEIGHT      <- round(bumpedBaskets$BASKET_WEIGHT_orig*bumpedBaskets$PROPORTION,3)
    
    #add the new, bumped baskets
    #delete the catch record
    message("\n\tThe following 'mixed catch' records from CATCH and BASKET were replaced with records 
    \twith the correct species codes. ")
    message("\tCatch:")
    print(catch[(catch$id %in% parentIDs[i]),])
    catch  <- catch[-which(catch$id %in% parentIDs[i]),]
    message("\tBaskets")
    print(basket[which(basket$catch_id %in% parentIDs[i]),])
    basket <- basket[-which(basket$catch_id %in% parentIDs[i]),]
    
    message("\tThe following BASKET records were added - their weights were calculated using 
    \tthe proportions determined from the SAMPLED basket")
    print(bumpedBaskets)
    bumpedBaskets$PROPORTION <- NULL
    bumpedBaskets$BASKET_WEIGHT_orig <- NULL
    #add the new, bumped baskets
    basket <- rbind(basket, bumpedBaskets)
  }
  basket$id <- basket$catch_id <- NULL
  catch$id <- catch$is_parent <- catch$parent_catch_id <- NULL
  x <- list()
  x$basket <- basket
  x$catch <- catch
  if (!is.na(theMsg)) message(theMsg)
  return(x)
}
