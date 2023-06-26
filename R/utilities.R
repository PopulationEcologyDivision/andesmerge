#' @title get_value
#' @description This function returns the  lookup value in a vector
#' @param myKey default is \code{NULL}.
#' @param mylookupvector default is \code{NULL}.
#' @return lookup value
#' @family internal
#' @author  Pablo Vergara, \email{pablo.vergara@@dfo-mpo.gc.ca}
#' @export
#' 
get_value <- function(myKey, mylookupvector){
  myvalue = mylookupvector[myKey]
  myvalue = unname(myvalue)
  return(myvalue)
}

# @title convertFORCE
# @description This function returns converted FORCE codes
# @param x default is \code{NULL}. This is the string to be processed.
# @return converted FORCE codes
# @family internal
# @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
# @export
# 
# convertFORCE <- function(x){
#   if (is.character(any(x))){
#     ESE.vals <- list("0" = 0,
#                      "1" = 1,
#                      "2" = 2,
#                      "3" = 3,
#                      "4" = 4,
#                      "5" = 5,
#                      "6" = 6,
#                      "7" = 7,
#                      "8" = 8,
#                      "9" = NA)
#     
#     ESE.vals = unlist(ESE.vals)
#     x = get_value(x, ESE.vals)
#   }
#   return(x)
# }

# @title convertHOWOBT
# @description This function returns converted obtained_code codes
# @param x default is \code{NULL}. This is the string to be processed.
# @return converted FORCE codes
# @family internal
# @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
# @export
# 
#' convertHOWOBT <- function(x){
#'   # browser()
#'   # if (any(grepl(" - ", x = x))){
#'     x <- as.numeric(stringi::stri_extract_first_regex(x, "[0-9]+"))
#'   # }else{
#'   #   ESE.vals <- list("1" = 1, # "Ships log or distance program"
#'   #                    "2" = 2, # "Radar bouy"
#'   #                    "3" = 3, # "DECCA bearings"
#'   #                    "4" = 0, # "LORAN bearings or GPS"
#'   #                    "5" = 5, # "DECCA radar"
#'   #                    "6" = 6, # "LORAN radar"
#'   #                    "7" = 7, # "DECCA and LORAN"
#'   #                    "8" = 8, # "Satelite navigation"
#'   #                    "9" = 9) # "No observation / hydrographic station"
#'   #   
#'   #   ESE.vals = unlist(ESE.vals)
#'   #   x = get_value(x, ESE.vals)
#'   # }
#'   return(x)
#' }

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
  colnames(x)[colnames(x)=="size_class"]    <- "SIZE_CLASS"
  colnames(x)[colnames(x)=="id"]            <- "SPECIMEN_ID"
  y <- list()
  y$specimen <- x[,c("MISSION", "SETNO", "SPEC", "SIZE_CLASS", "SPECIMEN_ID", "basket_id")]

  # Match data for level 1 observations 
  # Need to turn specimen table from wide format to long in order to have each observation on its own row
  x$basket_id <- x$basket <- NULL
  specDets <- names(x[, !names(x) %in% c("MISSION", "SETNO", "SPEC", "SIZE_CLASS", "SPECIMEN_ID")])
  x <- x %>% dplyr::mutate_all(as.character) %>% tidyr::pivot_longer(dplyr::all_of(specDets), names_to = "variable", values_to = "value") %>% as.data.frame()
  colnames(x)[colnames(x)=="variable"] <- "LV1_OBSERVATION"
  colnames(x)[colnames(x)=="value"] <- "DATA_VALUE"  
  
  #remove records where the value is 0 for collect.specimen/collect.otoliths
  #these have no impact
  if (nrow(x[which(x$LV1_OBSERVATION == "collect.specimen" & x$DATA_VALUE == 0),])>0){
    x <- x[-which(x$LV1_OBSERVATION == "collect.specimen" & x$DATA_VALUE == 0),]
  }
  if (nrow(x[which(x$LV1_OBSERVATION == "collect.otoliths" & x$DATA_VALUE == 0),])>0){
    x <- x[-which(x$LV1_OBSERVATION == "collect.otoliths" & x$DATA_VALUE == 0),]
  }
  
  # remove 1) all NA values, 2) empty cells (i.e. ""), 3) cases of 1 character entries
  x <- x[!is.na(x$DATA_VALUE) & nchar(x$DATA_VALUE)>0,]
# remove entries of NAN from DATA_VALUE
    x <-x[!grepl(pattern = "^nan$", x$DATA_VALUE, ignore.case = T),]
    # remove entries of Print Label from LV1_OBSERVATION
    x <-x[!grepl(pattern = "^Print Label$", x$LV1_OBSERVATION, ignore.case = T),]
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
  x <- merge(x, y$specimen, all.x=T)
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
  x[x$LV1_OBSERVATION == "Otolith Taken"& x$DATA_VALUE =="1" ,c("LV1_OBSERVATION","DATA_DESC")]     <- as.data.frame(t(as.matrix(c("Age Material Type","Otolith Taken"))))
  
  x$DATA_DESC <- NA
  x[grepl(x$LV1_OBSERVATION, pattern = "Collect .*", ignore.case = T) & x$DATA_VALUE =="0" ,c("DATA_DESC")] <- c("Not collected")
  x[grepl(x$LV1_OBSERVATION, pattern = "Collect .*", ignore.case = T) & x$DATA_VALUE =="1" ,c("DATA_DESC")] <- c("Collected")
  x[x$LV1_OBSERVATION == "Collect Otoliths" & x$DATA_VALUE =="1" ,c("LV1_OBSERVATION","DATA_DESC")] <- as.data.frame(t(as.matrix(c("Age Material Type","Otolith Taken"))))
  x[x$LV1_OBSERVATION == "Otolith Taken"& x$DATA_VALUE =="1" ,c("LV1_OBSERVATION","DATA_DESC")]     <- as.data.frame(t(as.matrix(c("Age Material Type","Otolith Taken"))))
  x[x$LV1_OBSERVATION == "Sex" & x$DATA_VALUE =="0" ,c("DATA_DESC")] <- c("Undetermined") 
  x[x$LV1_OBSERVATION == "Sex" & x$DATA_VALUE =="1" ,c("DATA_DESC")] <- c("Male")
  x[x$LV1_OBSERVATION == "Sex" & x$DATA_VALUE =="2" ,c("DATA_DESC")] <- c("Female")
  x[x$LV1_OBSERVATION == "Sex" & x$DATA_VALUE =="3" ,c("DATA_DESC")] <- c("Berried or Hermaphrodite")
  x[x$LV1_OBSERVATION == "Stomach Fullness" & x$DATA_VALUE =="0" ,c("DATA_DESC")] <- c("Empty; No food contents")
  x[x$LV1_OBSERVATION == "Stomach Fullness" & x$DATA_VALUE =="1" ,c("DATA_DESC")] <- c("Less than 1/4 full")
  x[x$LV1_OBSERVATION == "Stomach Fullness" & x$DATA_VALUE =="2" ,c("DATA_DESC")] <- c("1/4 to 1/2 full")
  x[x$LV1_OBSERVATION == "Stomach Fullness" & x$DATA_VALUE =="3" ,c("DATA_DESC")] <- c("1/2 to 3/4 full")
  x[x$LV1_OBSERVATION == "Stomach Fullness" & x$DATA_VALUE =="4" ,c("DATA_DESC")] <- c("3/4 to full")
  x[x$LV1_OBSERVATION == "Stomach Fullness" & x$DATA_VALUE =="5" ,c("DATA_DESC")] <- c("Everted")
  x[x$LV1_OBSERVATION == "Stomach Fullness" & x$DATA_VALUE =="6" ,c("DATA_DESC")] <- c("Regurgitated")
  x[x$LV1_OBSERVATION == "Stomach Fullness" & x$DATA_VALUE =="7" ,c("DATA_DESC")] <- c(NA)
  x[x$LV1_OBSERVATION == "Stomach Fullness" & tolower(x$DATA_VALUE) =="swf" ,c("DATA_DESC")] <- c("Saved Whole Frozen")
  x[x$LV1_OBSERVATION == "Maturity" & x$DATA_VALUE == "0",c("DATA_DESC")] <- c("Undetermined")
  x[x$LV1_OBSERVATION == "Maturity" & x$DATA_VALUE == "1",c("DATA_DESC")] <- c("Immature")
  x[x$LV1_OBSERVATION == "Maturity" & x$DATA_VALUE == "2",c("DATA_DESC")] <- c("Ripening 1")
  x[x$LV1_OBSERVATION == "Maturity" & x$DATA_VALUE == "3",c("DATA_DESC")] <- c("Ripening 2")
  x[x$LV1_OBSERVATION == "Maturity" & x$DATA_VALUE == "4",c("DATA_DESC")] <- c("Ripe (Mature)")
  x[x$LV1_OBSERVATION == "Maturity" & x$DATA_VALUE == "5",c("DATA_DESC")] <- c("Spawning (Running)")
  x[x$LV1_OBSERVATION == "Maturity" & x$DATA_VALUE == "6",c("DATA_DESC")] <- c("Spent")
  x[x$LV1_OBSERVATION == "Maturity" & x$DATA_VALUE == "7",c("DATA_DESC")] <- c("Recovering")
  x[x$LV1_OBSERVATION == "Maturity" & x$DATA_VALUE == "8",c("DATA_DESC")] <- c("Resting")
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
  # possible experiment types (from : andesdb.shared_models_experimenttype)
  #   1	Stratified random survey set		
  #   5	Comparative fishing experiment	
  #   6	Tagging set		
  #   9	Hydrography		
  #   7	Gear testing		
  #   99	Systematic		
  valid.exp.num = c(1, 5, 6, 7, 9, 99)
  #exp.num =  x$experiment_type_id
  exp.num =  as.numeric(gsub("(^[0-9]{1,2})(.*$)", "\\1", x$experiment_type))
  set.res =  x$set_result_id

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
    x <- ifelse(x %in% c("true","yes", "t", "y"), T, ifelse(x %in% c("false","no", "f", "n"), F, NA))
  }else{
    x <- ifelse(x %in% c("true","yes", "t", "y"), "Y", ifelse(x %in% c("false","no", "f", "n"), "N", NA))
  }
  return(x)
}

#' @title colTypeConverter
#' @description This function takes a dataframe and tries to convert the various columns into the 
#' appropriate type according to their contents
#' @param df default is \code{NULL} the dataframe that needs column types changed
#' @return the same df, but with attributes
#' @family general_use
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
colTypeConverter <- function(df = NULL){
  df[] <- lapply(df, function(x) utils::type.convert(as.character(x), as.is = TRUE))
  return(df)
}

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
  #add trailing "/" if necess
  if(substr(dataPath ,(nchar(dataPath)+1)-1,nchar(dataPath)) != "/")dataPath = paste0(dataPath,"/")
  filenames <- list.files(dataPath, pattern="^.*_(basket|catch|cruise|mission|observation|set|specimen|species)_(data|export).*\\.csv$")
  if (length(filenames)<1)stop("No csv files found")
  res<-list()
  for(i in 1:length(filenames))
  {
    # message("Working on ",filenames[i])
    thisFile = filenames[i]
    if(grepl('basket', thisFile))thisFileName <- "basket_data"
    if(grepl('catch', thisFile))thisFileName <- "catch_data"
    if(grepl('cruise', thisFile))thisFileName <- "cruise_data"
    if(grepl('mission', thisFile))thisFileName <- "mission_data"
    if(grepl('observation', thisFile))thisFileName <- "observation_data"
    if(grepl('set', thisFile))thisFileName <- "set_data"
    if(grepl('specimen', thisFile))thisFileName <- "specimen_data"
    if(grepl('species', thisFile))thisFileName <- "species_data"

    res[[thisFileName]]<- utils::read.csv(file.path(dataPath,thisFile), stringsAsFactors=FALSE)
  }
  return(res)
}

uuidToGulf <-function(x=NULL){
  # can do ESE_BASKETS("SPEC"), ESE_CATCHES("SPEC"), ESE_SPECIMENS ("SPEC"), ESE_LV1_OBSERVATIONS ("SPEC") 
  x$specimen_data<- merge(x$specimen_data, x$species_data[,c("uuid","code")], all.x=T, by.x="species_uuid", by.y="uuid")
  x$basket_data<- merge(x$basket_data, x$species_data[,c("uuid","code")], all.x=T, by.x="species_uuid", by.y="uuid")
  x$catch_data<- merge(x$catch_data, x$species_data[,c("uuid","code")], all.x=T, by.x="species_uuid", by.y="uuid")
  colnames(x$specimen_data)[colnames(x$specimen_data)=="code"] <- "SPEC"
  colnames(x$basket_data)[colnames(x$basket_data)=="code"] <- "SPEC"
  colnames(x$catch_data)[colnames(x$catch_data)=="code"] <- "SPEC"
  x$specimen_data$uuid <- NULL
  x$basket_data$uuid <- NULL
  x$catch_data$uuid <- NULL
  return(x)
}
uuidToCODE <-function(x=NULL){
  MARCodes<- RVSurveyData::GSSPECIES[, c("UUID", "CODE")]
  # can do ESE_BASKETS("SPEC"), ESE_CATCHES("SPEC"), ESE_SPECIMENS ("SPEC"), ESE_LV1_OBSERVATIONS ("SPEC") 
  x$specimen_data<- merge(x$specimen_data, MARCodes, all.x=T, by.x="species_uuid", by.y="UUID")
  x$basket_data<- merge(x$basket_data, MARCodes, all.x=T, by.x="species_uuid", by.y="UUID")
  x$catch_data<- merge(x$catch_data, MARCodes, all.x=T, by.x="species_uuid", by.y="UUID")
  colnames(x$specimen_data)[colnames(x$specimen_data)=="CODE"] <- "SPEC"
  colnames(x$basket_data)[colnames(x$basket_data)=="CODE"] <- "SPEC"
  colnames(x$catch_data)[colnames(x$catch_data)=="CODE"] <- "SPEC"
  x$specimen_data$species_uuid <- NULL
  x$basket_data$species_uuid <- NULL
  x$catch_data$species_uuid <- NULL
  if ("observation_data" %in% names(x)){
    x$observation_data<- merge(x$observation_data, MARCodes, all.x=T, by.x="species_uuid", by.y="UUID")
    colnames(x$observation_data)[colnames(x$observation_data)=="CODE"] <- "SPEC"
    x$observation_data$species_uuid <- NULL
  }
  
  return(x)
}
# gulfToMarSpp <- function(x=NULL, groundfish.username=groundfish.username, groundfish.password=groundfish.password){
  # con <- ROracle::dbConnect(DBI::dbDriver("Oracle"), groundfish.username, groundfish.password, "PTRAN")
  # sqlStatement <- paste0("select APHIAID, CODE AS CODE_MAR, SPEC, COMM, GULF,
  #                        APHIAID_SUGG, SPEC_SUGG, TSN
  #                        from GROUNDFISH.GSSPECIES_APHIAS")
  # MAR_ORA <- ROracle::dbGetQuery(con, sqlStatement)
#   
#   b<- merge(x$ESE_BASKETS, MAR_ORA[,c("CODE_MAR", "GULF")], by.x="SPEC", by.y="GULF", all.x=T)
#   c<- merge(x$ESE_CATCHES, MAR_ORA[,c("CODE_MAR", "GULF")], by.x="SPEC", by.y="GULF", all.x=T)
#   o<-merge(x$ESE_LV1_OBSERVATIONS, MAR_ORA[,c("CODE_MAR", "GULF")], by.x="SPEC", by.y="GULF", all.x=T)
#   
#   browser()
# }