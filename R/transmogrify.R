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

  #####
  valid.exp.num = c(1, 5, 6, 7, 9, 99)

  if (nrow(df[!(as.numeric(gsub("(^[0-9]{1,2})(.*$)", "\\1", df$experiment_type)) %in% valid.exp.num) &
             is.na(df$start_latitude_DD) &
             is.na(df$start_latitude_MMmm)&
             is.na(df$start_longitude_DD) &
             is.na(df$start_longitude_MMmm ) &
             is.na(df$end_latitude_DD) &
             is.na(df$end_latitude_MMmm) &
             is.na(df$end_longitude_DD) &
             is.na(df$end_longitude_MMmm),])>0){
    stoppedSets<- data.frame()
    stoppedSets<- df[!(as.numeric(gsub("(^[0-9]{1,2})(.*$)", "\\1", df$experiment_type)) %in% valid.exp.num) &
                      is.na(df$start_latitude_DD) &
                      is.na(df$start_latitude_MMmm)&
                      is.na(df$start_longitude_DD) &
                      is.na(df$start_longitude_MMmm ) &
                      is.na(df$end_latitude_DD) &
                      is.na(df$end_latitude_MMmm) &
                      is.na(df$end_longitude_DD) &
                      is.na(df$end_longitude_MMmm),]
    if (nrow(stoppedSets) > 0 ){
      message("The following set(s) were detected that had almost no information.  These were likely started accidentally and will be dropped")
      print(stoppedSets)
      df <-setdiff(df,stoppedSets)
    }
  }
  #####

  theMsg <- NA
  colnames(df)[colnames(df)=="set_number"] <- "SETNO"
  df$start_date_utc          <- as.POSIXlt(df$start_date, tz="UTC",format = "%Y-%m-%d %H:%M:%S")
  df$end_date_utc            <- as.POSIXlt(df$end_date, tz="UTC",format = "%Y-%m-%d %H:%M:%S")
  df$start_date_loc          <- lubridate::with_tz(df$start_date_utc, tz="America/Halifax")
  df$end_date_loc            <- lubridate::with_tz(df$end_date_utc, tz="America/Halifax")
  df$START_DATE              <- format.Date(strftime(df$start_date_loc), format = "%d%m%Y")
  df$END_DATE                <- format.Date(strftime(df$end_date_loc), format = "%d%m%Y")
  df$START_TIME              <- as.character(df$start_date_loc , format = '%H%M')
  df$END_TIME                <- as.character(df$end_date_loc , format = '%H%M')
  df$STRAT                   <- cleanStrata(df$stratum)
  df$SLAT                    <- paste0(df$start_latitude_DD,sprintf("%05.2f",df$start_latitude_MMmm))  #sprintf used to ensure leading zeroes added as necessary.
  df$SLONG                   <- paste0(abs(df$start_longitude_DD),sprintf("%05.2f",df$start_longitude_MMmm))
  df$ELAT                    <- paste0(df$end_latitude_DD,sprintf("%05.2f",df$end_latitude_MMmm))
  df$ELONG                   <- paste0(abs(df$end_longitude_DD),sprintf("%05.2f",df$end_longitude_MMmm))
  df$SLAT_dd                 <- df$start_latitude_DD+(df$start_latitude_MMmm/60)
  df$SLONG_dd                <- abs(abs(df$start_longitude_DD)+(df$start_longitude_MMmm/60))*-1
  df$DIST                    <- df$crow_distance
  df$HOWD                    <- as.numeric(stringi::stri_extract_first_regex(df$distance_towed_obtained_code, "[0-9]+"))
  if (any(grepl("4 - ", c(df$ship_speed_obtained_code,df$distance_towed_obtained_code))))theMsg <- 'Assuming entries of "4 - LORAN bearings or GPS" for HOWD/HOWS should be  "0 - GPS"'
  df[df$HOWD==4,"HOWD"] <- 0
  # df$SPEED                 <- round(df$ship_speed,2)
  # df$HOWS                  <- convertHOWOBT(df$ship_speed_obtained_code)
  df$SPEED                   <- round(df$speed,2)
  df$HOWS                    <- 0 #hardcoding GPS
  df$DMIN                    <- NA # = df$dmin/1.8288 # MMM - converting from meters to fathoms, but not sure what it would map to yet
  df$DMAX                    <- NA # = df$dmax/1.8288 # MMM - converting from meters to fathoms, but not sure what it would map to yet
  df$START_DEPTH             <- round(meters2Fathoms(df$start_depth_m),0)
  df$END_DEPTH               <- round(meters2Fathoms(df$end_depth_m),0)
  df$WIND                    <- df$wind_direction_degree          # MMM - verified that actual degree is entered 
  # df$FORCE                   <- convertFORCE(df$wind_force_code)  # MMM - force of "9" is actually NA
  df$FORCE                    <- as.numeric(stringi::stri_extract_first_regex(df$wind_force_code, "[0-9]+"))
  df[which(df$FORCE==9),"FORCE"] <- NA
  df$CURNT                   <- as.numeric(stringi::stri_extract_first_regex(df$tide_direction_code, "[0-9]+")) #df$tide_direction_code            # verified
  df$EXPERIMENT_TYPE_CODE    <- setExperimentType(df)
  df$GEAR                    <- as.numeric(stringi::stri_extract_first_regex(df$gear_type, "[0-9]+"))  #assume this is correct (and not gear_type_id)
  df$AUX                     <- as.numeric(stringi::stri_extract_first_regex(df$auxiliary_equipment, "[0-9]+"))
  df$WARPOUT                 <- round(meters2Fathoms(df$port_warp),0)
  df$NOTE                    <- cleanfields(df$remarks)
  df$SURFACE_TEMPERATURE     <- NA # data to come later, not captured during survey
  df$BOTTOM_TEMPERATURE      <- NA # data to come later, not captured during survey
  df$BOTTOM_SALINITY         <- NA # data to come later, not captured during survey
  df$HYDRO                   <- NA # data to come later, not captured during survey
  df$STATION                 <- stringi::stri_extract_first_regex(df$station_number, "\\d{1,3}")
  df$BOTTOM_TYPE_CODE        <- NA
  df$BOTTOM_TEMP_DEVICE_CODE <- NA
  df$WAVE_HEIGHT_CODE        <- NA
  df$LIGHT_LEVEL_CODE        <- NA
  df$GEAR_MONITOR_DEVICE_CODE<- NA
  
  df <- Mar.utils::identify_area(df, lat.field = "SLAT_dd", lon.field = "SLONG_dd", agg.poly.shp = RVSurveyData::nafo_sf, agg.poly.field = "AREA_ID")
  colnames(df)[colnames(df)=="AREA_ID"] <- "AREA"
  df[df$AREA %in% "<outside known areas>", "AREA"] <- NA
  if (any(is.na(df$AREA))){
    warning("One or more sets could not be assigned to a NAFO area.")
  }
  df$AREA <- as.integer(df$AREA)
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
  df$sampled <-charToBinary(df$sampled, bool=T)
  # df$sampled <- tolower(df$sampled)
  # df$sampled <- ifelse(df$sampled == "true", T, ifelse(df$sampled == "false", F, NA))
  colnames(df)[colnames(df)=="set_number"]    <- "SETNO"
  colnames(df)[colnames(df)=="size_class"]    <- "SIZE_CLASS"
  colnames(df)[colnames(df)=="basket_wt_kg"]  <- "BASKET_WEIGHT"  
  colnames(df)[colnames(df)=="sampled"]       <- "SAMPLED"
  #match expected field order
  df <- df[,c("MISSION","SETNO", "SPEC", "SIZE_CLASS", "BASKET_WEIGHT", "SAMPLED", "basket_id", "catch_id")]
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
  colnames(df)[colnames(df)=="notes"]             <- "NOTE"
  colnames(df)[colnames(df)=="unweighed_baskets"] <- "UNWEIGHED_BASKETS"
  colnames(df)[colnames(df)=="specimen_count"]    <- "NUMBER_CAUGHT"
  df$is_parent <-charToBinary(df$is_parent, bool=T)
  #match expected field order
  df <- df[,c("MISSION","SETNO", "SPEC", "NOTE", "UNWEIGHED_BASKETS", "NUMBER_CAUGHT",
              "catch_id", "is_parent", "parent_catch_id")]
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