#' @title formatForROracle
#' @description This function applies field-specific oracle formatting to the ESE objects so that 
#' they each get the appropriate field types when they are loaded (e.g. numeric, date, correct # of 
#' decimal places, etc) 
#' @param source_df default is \code{NULL}.This is a data frame containing the data that should be 
#' loaded to Oracle.It should correspond with the structure used by one of ESE_MISSIONS, ESE_SETS, 
#' ESE_CATCHES, ESE_BASKETS, ESE_SPECIMENS, ESE_LV1_OBSERVATIONS
#' @return data frame, but with attr() 
#' @family general_use
#' @author Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @note This may 
#' @export
formatForOracle <- function(source_df= NULL){
  ora.type = ""                       # "clob", "blob", "char"
  ora.maxlength =                     # 10
  ora.encoding = ""                   # "UTF-8"
  ora.fractional_seconds_precision =  # 9
  
attrMissions <- function(x){
  message("ESE_MISSIONS")
  attr(x$MISSION, "ora.xxx") <- "stuff"
  attr(x$SAMPLING_REQUIREMENT, "ora.xxx") <- "stuff" 
  attr(x$NOTE, "ora.xxx") <- "stuff"
  attr(x$DATA_VERSION, "ora.xxx") <- "stuff" 
  attr(x$PROGRAM_TITLE, "ora.xxx") <- "stuff" 
 return(x) 
}
attrSets <- function(x){
  message("ESE_SETS")
  attr(x$MISSION, "ora.xxx") <- "stuff"
  attr(x$SETNO, "ora.xxx") <- "stuff"
  attr(x$SDATE, "ora.xxx") <- "stuff"
  attr(x$TIME, "ora.xxx") <- "stuff" 
  attr(x$DUR, "ora.xxx") <- "stuff" 
  attr(x$ETIME, "ora.xxx") <- "stuff"
  attr(x$STRAT, "ora.xxx") <- "stuff"
  attr(x$SLAT, "ora.xxx") <- "stuff" 
  attr(x$ELAT, "ora.xxx") <- "stuff" 
  attr(x$SLONG, "ora.xxx") <- "stuff" 
  attr(x$ELONG, "ora.xxx") <- "stuff"
  attr(x$DIST, "ora.xxx") <- "stuff" 
  attr(x$HOWD, "ora.xxx") <- "stuff" 
  attr(x$SPEED, "ora.xxx") <- "stuff"
  attr(x$HOWS, "ora.xxx") <- "stuff"
  attr(x$START_DEPTH, "ora.xxx") <- "stuff"
  attr(x$END_DEPTH, "ora.xxx") <- "stuff"
  attr(x$WIND, "ora.xxx") <- "stuff" 
  attr(x$FORCE, "ora.xxx") <- "stuff"
  attr(x$CURNT, "ora.xxx") <- "stuff" 
  attr(x$EXPERIMENT_TYPE_CODE, "ora.xxx") <- "stuff" 
  attr(x$GEAR, "ora.xxx") <- "stuff" 
  attr(x$AUX, "ora.xxx") <- "stuff"
  attr(x$NOTE, "ora.xxx") <- "stuff" 
  attr(x$STATION, "ora.xxx") <- "stuff" 
  attr(x$WARPOUT, "ora.xxx") <- "stuff"
  attr(x$AREA, "ora.xxx") <- "stuff" 
  attr(x$DMIN, "ora.xxx") <- "stuff" 
  attr(x$DMAX, "ora.xxx") <- "stuff" 
  attr(x$SURFACE_TEMPERATURE, "ora.xxx") <- "stuff" 
  attr(x$BOTTOM_TEMPERATURE, "ora.xxx") <- "stuff" 
  attr(x$BOTTOM_SALINITY, "ora.xxx") <- "stuff"
  attr(x$HYDRO, "ora.xxx") <- "stuff" 
  return(x) 
}
attrBaskets <- function(x){
  message("ESE_BASKETS")
  attr(x$BASKET_WEIGHT, "ora.xxx") <- "stuff"
  attr(x$MISSION, "ora.xxx") <- "stuff" 
  attr(x$SAMPLED, "ora.xxx") <- "stuff"
  attr(x$SETNO, "ora.xxx") <- "stuff" 
  attr(x$SIZE_CLASS, "ora.xxx") <- "stuff"
  attr(x$SPEC, "ora.xxx") <- "stuff"
  return(x) 
}
attrCatches <- function(x){
  message("ESE_CATCHES")
  attr(x$MISSION, "ora.xxx") <- "stuff" 
  attr(x$SETNO, "ora.xxx") <- "stuff" 
  attr(x$SPEC, "ora.xxx") <- "stuff"
  attr(x$SIZE_CLASS, "ora.xxx") <- "stuff"
  attr(x$NOTE, "ora.xxx") <- "stuff"
  attr(x$UNWEIGHED_BASKETS, "ora.xxx") <- "stuff"
  attr(x$NUMBER_CAUGHT, "ora.xxx") <- "stuff"
  return(x) 
  }
attrSpecimens <- function(x){
  message("ESE_SPECIMENS")
  attr(x$MISSION, "ora.xxx") <- "stuff" 
  attr(x$SETNO, "ora.xxx") <- "stuff" 
  attr(x$SIZE_CLASS, "ora.xxx") <- "stuff"
  attr(x$SPEC, "ora.xxx") <- "stuff"
  attr(x$SPECIMEN_ID, "ora.xxx") <- "stuff"
  return(x) 
  }
attrLV1 <- function(x){ 
  message("ESE_LV1_OBSERVATIONS")
  attr(x$MISSION, "ora.xxx") <- "stuff"
  attr(x$SETNO, "ora.xxx") <- "stuff"
  attr(x$SPEC, "ora.xxx") <- "stuff" 
  attr(x$SIZE_CLASS, "ora.xxx") <- "stuff" 
  attr(x$SPECIMEN_ID, "ora.xxx") <- "stuff"
  attr(x$LV1_OBSERVATION, "ora.xxx") <- "stuff" 
  attr(x$DATA_VALUE, "ora.xxx") <- "stuff" 
  attr(x$LV1_OBSERVATION_ID, "ora.xxx") <- "stuff"
  return(x) 
  }

  
  if ("DATA_VERSION" %in% names(source_df)) r <- attrMissions(source_df)
  if ("SDATE" %in% names(source_df)) r <- attrSets(source_df)
  if ("BASKET_WEIGHT" %in% names(source_df)) r <- attrBaskets(source_df)
  if ("UNWEIGHED_BASKETS" %in% names(source_df)) r <- attrCatches(source_df)
  if ("SPECIMEN_ID" %in% names(source_df) & length(names(source_df))==5) r <- attrSpecimens(source_df)
  if ("LV1_OBSERVATION" %in% names(source_df)) r <- attrLV1(source_df)
  
}