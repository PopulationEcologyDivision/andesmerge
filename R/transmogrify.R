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
  df$NOTE	       = substr(df$NOTE, 1, 255) #Oracle table only allows length of 255
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
  stoppedSets <- df[df$start_latitude_dd > 90 | (is.na(df$calculated_distance_nm_xy) &
                                                   is.na(df$start_latitude_dd) &
                                                   is.na(df$start_longitude_dd) &
                                                   is.na(df$end_latitude_dd) &
                                                   is.na(df$end_longitude_dd)),]
  if(nrow(stoppedSets)>0){
    message("The following set(s) were detected that had almost no information, or glaringly incorrect latitudes.  These were likely started accidentally and will be dropped")
    print(stoppedSets)
    df <- dplyr::anti_join(df, stoppedSets, by = names(stoppedSets))
  }
  theMsg <- NA
  colnames(df)[colnames(df)=="set_number"] <- "SETNO"
  df$start_date_utc          <- as.POSIXlt(df$start_date, tz="UTC",format = "%Y-%m-%d %H:%M:%S")
  df$end_date_utc            <- as.POSIXlt(df$end_date, tz="UTC",format = "%Y-%m-%d %H:%M:%S")
  df$start_date_loc          <- lubridate::with_tz(df$start_date_utc, tz="America/Halifax")
  df$end_date_loc            <- lubridate::with_tz(df$end_date_utc, tz="America/Halifax")
  df$START_DATE              <- format.Date(strftime(df$start_date_loc), format = "%d%m%Y")
  df$END_DATE                <- format.Date(strftime(df$end_date_loc), format = "%d%m%Y")
  df$START_TIME              <- format(df$start_date_loc , format = '%H%M')
  df$END_TIME                <- format(df$end_date_loc , format = '%H%M')
  df$STRAT                   <- cleanStrata(df$stratum)
  df$SLAT_dd                 <- df$start_latitude_dd
  df$SLONG_dd                <- df$start_longitude_dd
  df$SLAT                    <- convert_to_DDMM(df$start_latitude_dd)
  df$SLONG                   <- convert_to_DDMM(df$start_longitude_dd)
  # df$SLAT                   coords in DDMM.MM - not sure if necess for anything
  # df$SLONG                  coords in DDMM.MM
  df$ELAT                    <- convert_to_DDMM(df$end_latitude_dd)   
  df$ELONG                   <- convert_to_DDMM(df$end_longitude_dd)
  df$DIST                    <- df$calculated_distance_nm_xy
  df$HOWD                    <- df$distance_towed_obtained_code
  df$HOWS                   <- df$ship_speed_obtained_code
  # 'Assuming entries of "4 - LORAN bearings or GPS" for HOWD/HOWS should be  "0 - GPS"'
  df[which(df$HOWD==4),"HOWD"] <- 0
  df[which(df$HOWS==0),"HOWS"] <- 0
  df$SPEED                   <- round(df$ship_speed,2)
  df$DMIN                    <- round(meters2Fathoms(df$min_depth_m),0)
  df$DMAX                    <- round(meters2Fathoms(df$max_depth_m),0)
  df$START_DEPTH             <- round(meters2Fathoms(df$start_depth_m),0)
  df$END_DEPTH               <- round(meters2Fathoms(df$end_depth_m),0)
  df$WIND                    <- df$wind_direction_degree        # MMM - verified that actual degree is entered 
  df$FORCE                  <- convertFORCE(df$wind_force_kts)
  df$CURNT                   <- as.numeric(stringi::stri_extract_first_regex(df$tide_direction_code, "[0-9]+")) #df$tide_direction_code            # verified
  df$EXPERIMENT_TYPE_CODE    <- setExperimentType(df)
  # browser()
  df$GEAR                    <- as.numeric(stringi::stri_extract_first_regex(df$gear_type, "[0-9]+"))  #assume this is correct (and not gear_type_id)
  df$AUX                     <- as.numeric(stringi::stri_extract_first_regex(df$auxiliary_equipment, "[0-9]+"))
  df$WARPOUT                 <- round(meters2Fathoms(df$port_warp),0)
  df$NOTE                    <- cleanfields(df$remarks)
  df$NOTE	       = substr(df$NOTE, 1, 255) #Oracle table only allows length of 255
  df$SURFACE_TEMPERATURE     <- NA # data to come later, not captured during survey
  df$BOTTOM_TEMPERATURE      <- NA # data to come later, not captured during survey
  df$BOTTOM_SALINITY         <- NA # data to come later, not captured during survey
  df$HYDRO                   <- NA # data to come later, not captured during survey
  df$STATION                 <- stringi::stri_extract_first_regex(df$station_number, "\\d{1,4}")
  df$BOTTOM_TYPE_CODE        <- NA
  df$BOTTOM_TEMP_DEVICE_CODE <- NA
  df$WAVE_HEIGHT_CODE        <- NA
  df$LIGHT_LEVEL_CODE        <- NA
  df$GEAR_MONITOR_DEVICE_CODE<- NA
  
  df <- Mar.utils::identify_area(df, lat.field = "SLAT_dd", lon.field = "SLONG_dd", agg.poly.shp = Mar.data::NAFOSubunits_sf, agg.poly.field = "AREA_ID")
  colnames(df)[colnames(df)=="AREA_ID"] <- "AREA"
  df[df$AREA %in% c("<outside known areas>","<missing coord>"), "AREA"] <- NA
  if (any(is.na(df$AREA))){
    warning("One or more sets could not be assigned to a NAFO area.")
  }
  df$AREA <- as.integer(df$AREA)
  if (!any(is.na(theMsg))) message("SETS (General): \n\t", theMsg,"\n")
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
  df <- df[df$catch_code!=9990,]
  df$was_sampled <-charToBinary(df$was_sampled, bool=T)
  colnames(df)[colnames(df)=="set_number"]    <- "SETNO"
  # colnames(df)[colnames(df)=="sample_class"]    <- "SIZE_CLASS"
  colnames(df)[colnames(df)=="adjusted_basket_weight_kg"]  <- "BASKET_WEIGHT_e"  
  df$BASKET_COUNT_e <- df$adjusted_unmeasured_specimen_count
  # df$BASKET_COUNT_e <- rowSums(df[c("adjusted_unmeasured_specimen_count", "measured_specimen_count")], na.rm = TRUE)
  colnames(df)[colnames(df)=="was_sampled"]       <- "SAMPLED"
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
  df <- df[df$catch_code!=9990,]
  
  df$is_mixed_catch <-charToBinary(df$is_mixed_catch, bool=T)
  df$is_parent <-charToBinary(df$is_mixed_catch, bool=T)
  colnames(df)[colnames(df)=="set_number"]        <- "SETNO"
  colnames(df)[colnames(df)=="notes"]             <- "NOTE"
  colnames(df)[colnames(df)=="unweighed_baskets"]             <- "UNWEIGHED_BASKETS"
  
  # colnames(df)[colnames(df)=="total_adjusted_basket_weight"] <- "CATCH_WEIGHT_e"
  colnames(df)[colnames(df)=="unmeasured_specimen_count"]    <- "CATCH_COUNT_e"
  df$NOTE	       = substr(df$NOTE, 1, 255) #Oracle table only allows length of 255
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