#' @title convert_to_DDMM
#' @description This function converts coordinates from decimal degrees to DDMM.MM
#' @param decimal_degrees default is \code{NULL}. This is the value you wish to convert
#' @param roundDec default is \code{NULL}.  This is how many decimals you would like your final 
#' output to have.  If you do not want to round the result, you can leave it as NULL
#' @return la coordinate in DDMM.MM
#' @author  Mike McMahon, \email{mike.mcmahon@@dfo-mpo.gc.ca}
#' @export
convert_to_DDMM <- function(decimal_degrees, roundDec = NULL) {
  degrees <- trunc(decimal_degrees)
  minutes <- abs(decimal_degrees - degrees) * 60
  if(!is.null(roundDec)) minutes <- round(minutes, roundDec)
  DDMM <- abs(degrees) * 100 + minutes
  DDMM <- DDMM * sign(decimal_degrees)
  return(DDMM)
}

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

#' @title convertFORCE
#' @description This function returns converted FORCE codes
#' @param x default is \code{NULL}. This is the string to be processed.
#' @return converted FORCE codes
#' @family internal
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
#' 
convertFORCE <- function(x) {
  wind_force <- function(knots) {
    if (is.na(knots)) return(NA)
    if (knots == 0) return(0)
    if (knots >= 1 && knots <= 3) return(1)
    if (knots >= 4 && knots <= 6) return(2)
    if (knots >= 7 && knots <= 10) return(3)
    if (knots >= 11 && knots <= 16) return(4)
    if (knots >= 17 && knots <= 21) return(5)
    if (knots >= 22 && knots <= 27) return(6)
    if (knots >= 28 && knots <= 33) return(7)
    if (knots >= 34 && knots <= 40) return(8)
    return(NA)  # For values outside the defined ranges
  }
  
  return(sapply(x, wind_force))
}

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
  # colnames(x)[colnames(x)=="sample_class"]    <- "SIZE_CLASS"
  # colnames(x)[colnames(x)=="id"]            <- "SPECIMEN_ID" 
  y <- list()
  y$specimen <- x[,c("MISSION", "SETNO", "catch_code", "sample_class", "specimen_id", "basket_id")]

  # Match data for level 1 observations 
  # Need to turn specimen table from wide format to long in order to have each observation on its own row
  x$basket_id <-NULL
  dropFields <- c("mission_number","station_name","scientific_name","common_name_en","common_name_fr","is_mixed_catch",
                    "length_type","length_unit", "rounding_rule","weight_type","weight_unit","fin_clip_with_label","pre_anal_length",
                    "ripe_or_running","sex_display","basket_id","specimen_uuid")
  x<-x[,!names(x) %in% dropFields] 
  specDets <- names(x[, !names(x) %in% c("MISSION", "SETNO", "catch_code", "sample_class", "specimen_id")])
  x <- x %>% dplyr::mutate_all(as.character) %>% tidyr::pivot_longer(dplyr::all_of(specDets), names_to = "variable", values_to = "value") %>% as.data.frame()
  colnames(x)[colnames(x)=="variable"] <- "LV1_OBSERVATION"
  colnames(x)[colnames(x)=="value"] <- "DATA_VALUE"  
  
  #remove records where the value is 0 for collect.specimen/collect.otoliths
  #these have no impact
  if (nrow(x[which(x$LV1_OBSERVATION == "collect_specimen" & x$DATA_VALUE == 0),])>0){
    x <- x[-which(x$LV1_OBSERVATION == "collect_specimen" & x$DATA_VALUE == 0),]
  }
  if (nrow(x[which(x$LV1_OBSERVATION == "collect_otoliths" & x$DATA_VALUE == 0),])>0){
    x <- x[-which(x$LV1_OBSERVATION == "collect_otoliths" & x$DATA_VALUE == 0),]
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
  x$LV1_OBSERVATION[x$LV1_OBSERVATION=="maturity halibut"]<-"maturity"
  x$LV1_OBSERVATION[x$LV1_OBSERVATION=="maturity redfish"]<-"maturity"
  x$LV1_OBSERVATION[x$LV1_OBSERVATION=="maturity winterfl"]<-"maturity"
  x$LV1_OBSERVATION =  stringi::stri_trans_totitle(x$LV1_OBSERVATION)
  
  #add LV1_OBSERVATION_ID by grouping by Specimen_id, and then numbering all measurements within the group
  x <- x %>%
    dplyr::group_by(MISSION,SETNO, catch_code, sample_class, specimen_id) %>%
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
  data <- iconv(data, from = "ISO-8859-1", to = "UTF-8")

  data <- trimws(gsub(x=data, pattern = ("\\s+"), ' '))
  
  #replace other stuff with nothing
  data <- gsub(x = data, pattern = "\u00e2\u20ac\u2122", replacement = "'") # replace curly apostrophe (â€™) with '
  data <- gsub(x = data, pattern = '\u00e2\u20ac\u0153', replacement = '"') # replace curly open quote (â€œ) with "
  data <- gsub(x = data, pattern = '\u00e2\u20ac\u009d', replacement = '"') # replace curly close quote (â€)with "
  
  data <- gsub(x = data, pattern = '\u0092', replacement = "'") # replace curly apostrophe with normal one
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
  #df$experiment_type, df$set_result
  # possible experiment types (from : andesdb.shared_models_experimenttype)
  #   1	Stratified random survey set		
  #   5	Comparative fishing experiment	
  #   6	Tagging set		
  #   9	Hydrography		
  #   7	Gear testing		
  #   99	Systematic		
  valid.exp.num = c(1, 5, 6, 7, 9, 99)
  exp.num =  x$experiment_type
  # exp.num =  as.numeric(gsub("(^[0-9]{1,2})(.*$)", "\\1", x$experiment_type))
  set.res =  x$set_result
  if(!all(exp.num %in% valid.exp.num)) stop("Unknown Experiment type(s) found")
  x$experiment_type_id_tweaked <- 0
  # x$experiment_type_id_tweaked <- as.numeric(gsub("(^[0-9]{1,2})(.*$)", "\\1", x$experiment_type))
  # x$experiment_type_id_tweaked <- exp.num
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
  indexG = (exp.num %in% valid.exp.num & set.res %in% c(1, 2))
  if(length(which(indexG)) > 0)
  {
    x[indexG,]$experiment_type_id_tweaked <- 1
  }
  
  #NORMAL - FAIL
  indexF = (exp.num %in% valid.exp.num & set.res %in% c(3, 4, 5, 6))
  if(length(which(indexF)) > 0 ){
    x[indexF,]$experiment_type_id_tweaked <- 3
  }
  
  #NORMAL - HYDROGRAPHY
  #Added by MMM - not sure about using an NA as an identifier, but maybe in combo with exp.num==9 
  indexH = (exp.num == 9 & (is.na(set.res)|set.res==1))
  if(length(which(indexH)) > 0 ){
    x[indexH,]$experiment_type_id_tweaked <- 9
  }
 x[x$experiment_type_id_tweaked ==1 & x$valid_outcome =="False","experiment_type_id_tweaked"] <- 3
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
  filenames <- list.files(dataPath, pattern="^.*_(specimen_and_observation|observation|specimen|basket|catch|cruise|mission|set|species)_(data|export).*\\.csv$") #
  if (length(filenames)<1)stop("No csv files found")
  res<-list()
  for(i in 1:length(filenames)){
    message("Working on ",filenames[i])
    thisFile = filenames[i]
    if(grepl('specimen_and_observation', thisFile)) {
      thisFileName <- "specimen_and_observation_data"
    } else if(grepl('observation', thisFile)) {
      thisFileName <- "observation_data"
    } else if(grepl('specimen', thisFile)) {
      thisFileName <- "specimen_data"
    } else if(grepl('basket', thisFile)) {
      thisFileName <- "basket_data"
    } else if(grepl('catch', thisFile)) {
      thisFileName <- "catch_data"
    } else if(grepl('cruise', thisFile)) {
      thisFileName <- "cruise_data"
    } else if(grepl('mission', thisFile)) {
      thisFileName <- "mission_data"
    } else if(grepl('set', thisFile)) {
      thisFileName <- "set_data"
    } else if(grepl('species', thisFile)) {
      thisFileName <- "species_data"
    }
    res[[thisFileName]]<- utils::read.csv(file.path(dataPath,thisFile), stringsAsFactors=FALSE, fileEncoding="ISO-8859-1")
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
uuidToCODE <-function(x=NULL, spCodes=NULL){
  MARCodes<- spCodes[, c("UUID", "CODE")]
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
df.diff <- function (df1, df2) {
  is.dup <- duplicated(rbind(df2, df1))
  is.dup <- tail(is.dup, nrow(df1))
  return(df1[!is.dup, ])
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