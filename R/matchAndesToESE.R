#' @title matchAndesToESE
#' @description This function matches the new CSV dump from Andes to the old ESE table schema so that data can
#' be loaded into Oracle. 
#' @param dataPath default is \code{NULL}. This is the location of the csv files exported from andes
#' @return a list with all objects needed to load to Oracle
#' @family general_use
#' @author Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
matchAndesToESE <- function(dataPath = NULL){
  
  # load Andes CSV files extracted from server at end of survey 
  tmp                    <- loadData(dataPath = dataPath)

  # keepFieldsXxx  - extracts required/usable fields from andes data
  # tansmogrifyXxx - translate andes fields into formats/units, etc as used by ESE tables 
  # tweakXxx       - mission-specific modifications to the various ESE objects
  
  ESE_MISSIONS           <- keepFieldsMissions(tmp$cruise_data) 
  ESE_MISSIONS           <- transmogrifyMissions(ESE_MISSIONS)
  
  #capture formatted version of 'mission' for use with all other datasets       
  mission                <- ESE_MISSIONS[1,"MISSION"]
  #perform any mission level tweaks that impact multiple tables
  tmp                    <- tweakUniversal(tmp, mission)
         
  ESE_SETS               <- keepFieldsSets(tmp$set_data, mission)
  ESE_SETS               <- transmogrifySets(ESE_SETS)
  ESE_SETS               <- tweakSets(ESE_SETS)
    
  ESE_BASKETS            <- keepFieldsBaskets(tmp$basket_data, mission)
  ESE_BASKETS            <- transmogrifyBaskets(ESE_BASKETS)
  ESE_BASKETS            <- tweakBaskets(ESE_BASKETS)
         
  ESE_CATCHES            <- keepFieldsCatches(tmp$catch_data, mission)
  ESE_CATCHES            <- transmogrifyCatches(ESE_CATCHES)
  ESE_CATCHES            <- tweakCatches(ESE_CATCHES)
  
  #get the SIZE_CLASS from the basket data
  ESE_CATCHES            <- merge(ESE_CATCHES, 
                                  unique(ESE_BASKETS[,c("MISSION", "SETNO", "SPEC", "catch_id", 
                                                        "SIZE_CLASS")]), 
                                  all.x = T, 
                                  by.x=c("MISSION", "SETNO", "SPEC", "id"), 
                                  by.y = c("MISSION", "SETNO", "SPEC", "catch_id")) 
  
  # both specimen and lv1 observations are kept together in specimen_data, so they are
  # initially handled together
  specimensRaw           <- keepFieldsSpecimens(tmp$specimen_data, mission)
  specimensRaw           <- tweakSpecimensRaw(specimensRaw)
  specimenList           <- reFormatSpecimen(specimensRaw)

  ESE_SPECIMENS          <- specimenList$specimen
  ESE_SPECIMENS          <- transmogrifySpecimens(ESE_SPECIMENS)
  ESE_SPECIMENS          <- tweakSpecimens(ESE_SPECIMENS)
    
  ESE_LV1_OBSERVATIONS   <- specimenList$LV1_OBSERVATION
  ESE_LV1_OBSERVATIONS   <- transmogrifyLV1_OBS(ESE_LV1_OBSERVATIONS)
  ESE_LV1_OBSERVATIONS   <- tweakLv1(ESE_LV1_OBSERVATIONS)
  rm(list=c("tmp", "specimensRaw", "specimenList"))
  
  subsampled             <- applySubsampling(catch = ESE_CATCHES, basket = ESE_BASKETS)
  ESE_CATCHES            <- subsampled$catch
  ESE_BASKETS            <- subsampled$basket
  
  #ensure fields names are identical to what the tables expect
  #some uncessary, but for consistency they're all here.
  ESE_MISSIONS           <- ESE_MISSIONS[,c("MISSION", "SAMPLING_REQUIREMENT", "NOTE", 
                                            "DATA_VERSION", "PROGRAM_TITLE")]
  ESE_SETS               <- ESE_SETS[,c("MISSION", "SETNO", "START_DATE", "START_TIME", "END_DATE", 
                                        "END_TIME", "STRAT", "SLAT", "SLONG", "ELAT", "ELONG", 
                                        "AREA", "DIST", "HOWD", "SPEED", "HOWS", "DMIN", "DMAX", 
                                        "START_DEPTH", "END_DEPTH", "WIND", "FORCE", "CURNT", 
                                        "EXPERIMENT_TYPE_CODE", "GEAR", "AUX", "WARPOUT", "NOTE", 
                                        "SURFACE_TEMPERATURE", "BOTTOM_TEMPERATURE", 
                                        "BOTTOM_SALINITY", "HYDRO", "STATION", "BOTTOM_TYPE_CODE", 
                                        "BOTTOM_TEMP_DEVICE_CODE", "WAVE_HEIGHT_CODE", 
                                        "LIGHT_LEVEL_CODE", "GEAR_MONITOR_DEVICE_CODE")]
  ESE_BASKETS            <- ESE_BASKETS[,c("MISSION", "SETNO", "SPEC", "SIZE_CLASS", 
                                           "BASKET_WEIGHT", "SAMPLED")]
  ESE_CATCHES            <- ESE_CATCHES[,c("MISSION", "SETNO", "SPEC", "SIZE_CLASS", "NOTE", 
                                    "UNWEIGHED_BASKETS", "NUMBER_CAUGHT")]
  ESE_SPECIMENS          <- ESE_SPECIMENS[,c("MISSION", "SETNO", "SPEC", "SIZE_CLASS", 
                                             "SPECIMEN_ID")]
  ESE_LV1_OBSERVATIONS   <- ESE_LV1_OBSERVATIONS[,c("MISSION", "SETNO", "SPEC", "SIZE_CLASS",
                                                  "SPECIMEN_ID", "LV1_OBSERVATION_ID", 
                                                  "LV1_OBSERVATION", "DATA_VALUE", "DATA_DESC")]
  x <- list()
  x$ESE_MISSIONS         <- ESE_MISSIONS 
  x$ESE_SETS             <- ESE_SETS
  x$ESE_BASKETS          <- ESE_BASKETS
  x$ESE_CATCHES          <- ESE_CATCHES
  x$ESE_SPECIMENS        <- ESE_SPECIMENS
  x$ESE_LV1_OBSERVATIONS <- ESE_LV1_OBSERVATIONS
  
  return(x) 
}
