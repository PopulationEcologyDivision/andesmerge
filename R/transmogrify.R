#' @title transmogrifyMissions
#' @description This function takes the fields from the andes cruise_data, and does the various 
#' processing necessary to make them equivalent to the formats used in the ESE_MISSIONS table
#' @param df default is \code{NULL}.  This is the data frame containing the usable fields from the 
#' cruise_data 
#' @family internal
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
transmogrifyMissions  <- function(df = NULL){
  theMsg <- NA
  df$MISSION <- gsub( "-","",df$mission_number)
  colnames(df)[colnames(df)=="area_of_operation"] <- "SAMPLING_REQUIREMENT"
  colnames(df)[colnames(df)=="description"]       <- "NOTE"
  
  df$PROGRAM_TITLE         = 'Maritimes Bottom Trawl Surveys'
  df$DATA_VERSION          = gsub("\\.","", utils::packageDescription('andesmerge')$Version)
  df$NOTE                  = cleanfields(df$NOTE)
  df$mission_number <- NULL
  
  if (!is.na(theMsg)) message("MISSIONS (General): \n\t", theMsg,"\n")
  return(df)
}
#' @title transmogrifySets
#' @description This function takes the fields from the andes set_data, and does the various 
#' processing necessary to make them equivalent to the formats used in the ESE_SETS table
#' @param df default is \code{NULL}.  This is the data frame containing the usable fields from the 
#' set_data 
#' @family internal
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
transmogrifySets      <- function(df = NULL){
  theMsg <- NA
  if (any(4 %in% c(df$ship_speed_obtained_code,df$distance_towed_obtained_code))) theMsg <- 'Assuming entries of "4 - LORAN bearings or GPS" for HOWD/HOWS should be  "0 - GPS"'
  
  colnames(df)[colnames(df)=="set_number"] <- "SETNO"
  df$START_DATE               = format.Date(strftime(as.POSIXlt(df$start_date, tz="UTC",format = "%Y-%m-%d %H:%M:%S")), format = "%d%m%Y")
  df$START_TIME               = as.integer(as.character(as.POSIXlt(df$start_date, tz="UTC",format = "%Y-%m-%d %H:%M:%S") , format = '%H%M'))
  df$END_DATE                 = format.Date(strftime(as.POSIXlt(df$end_date, tz="UTC",format = "%Y-%m-%d %H:%M:%S")), format = "%d%m%Y")
  df$END_TIME                 = as.integer(as.character(as.POSIXlt(df$end_date, tz="UTC",format = "%Y-%m-%d %H:%M:%S") , format = '%H%M'))
  df$STRAT                    = cleanStrata(df$stratum)
  df$SLAT                     = paste0(df$start_latitude_DD,sprintf("%05.2f",df$start_latitude_MMmm))  #sprintf used to ensure leading zeroes added as necessary.
  df$SLONG                    = paste0(df$start_longitude_DD,sprintf("%05.2f",df$start_longitude_MMmm))
  df$ELAT                     = paste0(df$end_latitude_DD,sprintf("%05.2f",df$end_latitude_MMmm))
  df$ELONG                    = paste0(df$end_longitude_DD,sprintf("%05.2f",df$end_longitude_MMmm))
  df$AREA                     = NA # MMM - used, but not sure what it would map to yet
  df$DIST                     = df$distance_towed                 #MMM - is this nautical miles
  df$HOWD                     = convertHOWOBT(df$distance_towed_obtained_code)
  df$SPEED                    = round(df$ship_speed,2)
  df$HOWS                     = convertHOWOBT(df$ship_speed_obtained_code)
  df$DMIN                     = NA # = df$dmin/1.8288 # MMM - converting from meters to fathoms, but not sure what it would map to yet
  df$DMAX                     = NA # = df$dmax/1.8288 # MMM - converting from meters to fathoms, but not sure what it would map to yet
  df$START_DEPTH              = round(meters2Fathoms(df$start_depth_m),0)
  df$END_DEPTH                = round(meters2Fathoms(df$end_depth_m),0)
  df$WIND                     = df$wind_direction_degree          # MMM - verified that actual degree is entered 
  df$FORCE                    = convertFORCE(df$wind_force_code)  # MMM - force of "9" is actually NA
  df$CURNT                    = df$tide_direction_code            # verified
  df$EXPERIMENT_TYPE_CODE     = setExperimentType(df)
  df$GEAR                     = as.numeric(stringi::stri_extract_first_regex(df$gear_type, "[0-9]+"))  #assume this is correct (and not gear_type_id)
  df$AUX                      = as.numeric(stringi::stri_extract_first_regex(df$auxiliary_equipment, "[0-9]+"))
  df$WARPOUT                  = df$port_warp
  df$NOTE                     = cleanfields(df$remarks)
  df$SURFACE_TEMPERATURE      = NA # data to come later, not captured during survey
  df$BOTTOM_TEMPERATURE       = NA # data to come later, not captured during survey
  df$BOTTOM_SALINITY          = NA # data to come later, not captured during survey
  df$HYDRO                    = NA # data to come later, not captured during survey
  df$STATION                  = stringi::stri_extract_first_regex(df$new_station, "\\d{3}")
  df$BOTTOM_TYPE_CODE         = NA
  df$BOTTOM_TEMP_DEVICE_CODE  = NA
  df$WAVE_HEIGHT_CODE         = NA
  df$LIGHT_LEVEL_CODE         = NA
  df$GEAR_MONITOR_DEVICE_CODE = NA
  
  if (!is.na(theMsg)) message("SETS (General): \n\t", theMsg,"\n")
  return(df)
}
#' @title transmogrifyBaskets
#' @description This function takes the fields from the andes basket_data, and does the various 
#' processing necessary to make them equivalent to the formats used in the ESE_BASKETS table
#' @param df default is \code{NULL}.  This is the data frame containing the usable fields from the 
#' basket_data 
#' @family internal
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
transmogrifyBaskets   <- function(df = NULL){
  theMsg <- NA
  df$sampled <-charToBoolean(df$sampled)
  # df$sampled <- tolower(df$sampled)
  # df$sampled <- ifelse(df$sampled == "true", T, ifelse(df$sampled == "false", F, NA))
  colnames(df)[colnames(df)=="set_number"]    <- "SETNO"
  colnames(df)[colnames(df)=="species_code"]  <- "SPEC"  
  colnames(df)[colnames(df)=="size_class"]    <- "SIZE_CLASS"
  colnames(df)[colnames(df)=="basket_wt_kg"]  <- "BASKET_WEIGHT"  
  colnames(df)[colnames(df)=="sampled"]       <- "SAMPLED"
  
  #match expected field order
  df <- df[,c("MISSION","SETNO", "SPEC", "SIZE_CLASS", "BASKET_WEIGHT", "SAMPLED", "id", "catch_id")]
  if (!is.na(theMsg)) message("BASKETS (General): \n\t", theMsg,"\n")
  return(df)
}
#' @title transmogrifyCatches
#' @description This function takes the fields from the andes catch_data, and does the various 
#' processing necessary to make them equivalent to the formats used in the ESE_CATCHES table
#' @param df default is \code{NULL}.  This is the data frame containing the usable fields from the 
#' catch_data 
#' @family internal
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
transmogrifyCatches   <- function(df = NULL){
  theMsg <- NA
  colnames(df)[colnames(df)=="set_number"]        <- "SETNO"
  colnames(df)[colnames(df)=="species_code"]      <- "SPEC"
  colnames(df)[colnames(df)=="notes"]             <- "NOTE"
  colnames(df)[colnames(df)=="unweighed_baskets"] <- "UNWEIGHED_BASKETS"
  colnames(df)[colnames(df)=="specimen_count"]    <- "NUMBER_CAUGHT"
  df$is_parent <-charToBoolean(df$is_parent)
  # df$SIZE_CLASS        = 999
  # message("need to add size class here")
  #' size_class info only available in the basket - put the default size
  # x$catch = addSizeClassToCatch(x$basket,x$catch)       # add correct size_class as needed 
  
  #match expected field order
  df <- df[,c("MISSION","SETNO", "SPEC", "NOTE", "UNWEIGHED_BASKETS", "NUMBER_CAUGHT",
              "id", "is_parent", "parent_catch_id")]
  if (!is.na(theMsg)) message("CATCH (general): \n\t", theMsg,"\n")
  return(df) 
}
#' @title transmogrifySpecimens
#' @description This function takes the fields from the andes specimen_data, and does the various 
#' processing necessary to make them equivalent to the formats used in the ESE_SPECIMENS table.  
#' @param df default is \code{NULL}.  This is the data frame containing the usable fields from the 
#' specimen_data. Note that both ESE_SPECIMENS and ESE_LV1_OBSERVATIONS are derived from the andes 
#' specimen_data. 
#' @family internal
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
transmogrifySpecimens <- function(df = NULL){
  theMsg <- NA
  
  if (!is.na(theMsg)) message("SPECIMENS (general): \n\t", theMsg,"\n")
  return(df)
}
#' @title transmogrifyLV1_OBS
#' @description This function takes the fields from the andes specimen_data, and does the various 
#' processing necessary to make them equivalent to the formats used in the ESE_LV1_OBSERVATIONS table
#' @param df default is \code{NULL}.  This is the data frame containing the usable fields from the 
#' specimen_data. Note that both ESE_SPECIMENS and ESE_LV1_OBSERVATIONS are derived from the andes 
#' specimen_data. 
#' @family internal
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
transmogrifyLV1_OBS   <- function(df = NULL){
  theMsg <- NA
  
  if(nrow(df[nchar(df$DATA_VALUE)>50,])>0){
    theMsg <- "At least one value for LV1_OBSERVATIONS$DATA_VALUE must be truncated to 50 characters"
    df$DATA_VALUE	       = substr(df$DATA_VALUE, 1, 50) #Oracle table only allows length of 50
  }
  
  df <- populate_DATA_DESC(df)
  
  if (!is.na(theMsg)) message("LV1_OBS (general): \n\t", theMsg,"\n")
  return(df) 
}