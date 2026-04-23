#' @title matchAndesToESE
#' @description This function matches the new CSV dump from Andes to the old ESE table schema so that data can
#' be loaded into Oracle. 
#' @param dataPath default is \code{NULL}. This is the location of the csv files exported from andes
#' @return a list with all objects needed to load to Oracle
#' @family general_use
#' @author Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
matchAndesToESE <- function(dataPath = NULL, gulfCodes=F, spCodes=NULL){
  # load Andes CSV files extracted from server at end of survey 
  tmp                    <- loadData(dataPath = dataPath)

  if ("cruise_data" %in% names(tmp)) {
    ESE_MISSIONS           <- keepFieldsMissions(tmp$cruise_data, new=F) 
    ESE_MISSIONS           <- transmogrifyMissions(ESE_MISSIONS)
    mission                <- ESE_MISSIONS[1,"MISSION"]
    tmp$specimensRaw      <- keepFieldsSpecimens(tmp$specimen_data, mission)
  }else{
    ESE_MISSIONS           <- keepFieldsMissions(tmp$mission_data, new=T) 
    ESE_MISSIONS           <- transmogrifyMissions(ESE_MISSIONS)
    mission                <- ESE_MISSIONS[1,"MISSION"]
    mrg <- merge(tmp$specimen_data[,!names(tmp$specimen_data) %in% c("species_en", "species_fr", "species_scientific", "aphia_id")],
                 tmp$observation_data[,c("specimen_id","observation_type", "value")], all.y=T, by="specimen_id")
    colnames(mrg)[colnames(mrg)=="value"] <- "observation_value_raw"
    mrg <- cbind(mrg, mission)
    colnames(mrg)[colnames(mrg)=="mission"] <- "MISSION"

    message("Got the merge working")

    colnames(mrg)[colnames(mrg)=="mission"] <- "MISSION"
    mrg$observation_type <- gsub("[()]", "", mrg$observation_type)
    mrg$observation_type <- tolower(mrg$observation_type)
    mrg$observation_type <- gsub(" w/ fish number", "", mrg$observation_type)
    mrg$observation_type <- trimws(mrg$observation_type)
    mrg$observation_type <- gsub(" ", ".", mrg$observation_type)
    tmp$specimensRaw <- mrg %>% 
    tidyr::pivot_wider(id_cols = c("specimen_id", "MISSION", "basket_id", "comment", "fish_number", "set_number", "sample_class", "catch_code"), names_from = "observation_type", values_from = "observation_value_raw") %>% as.data.frame()
  }
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

  newCATCH <- ESE_BASKETS %>%
    group_by(MISSION, SETNO, catch_code, sample_class) %>%
    summarise(NUMBER_CAUGHT = sum(BASKET_COUNT_e, na.rm = T), .groups = "keep")
  
  ESE_CATCHES2            <- merge(newCATCH,
                                   unique(ESE_CATCHES[,c("MISSION", "SETNO", "catch_code", "NOTE", "UNWEIGHED_BASKETS")]),
                                   by = c("MISSION", "SETNO", "catch_code"), all.x = T)
  specimensRaw           <- tweakSpecimensRaw(tmp$specimensRaw)  
  specimenList           <- reFormatSpecimen(specimensRaw)
  ESE_SPECIMENS          <- specimenList$specimen
  ESE_SPECIMENS          <- transmogrifySpecimens(ESE_SPECIMENS)
  
  ESE_LV1_OBSERVATIONS   <- specimenList$LV1_OBSERVATION
  ESE_LV1_OBSERVATIONS   <- transmogrifyLV1_OBS(ESE_LV1_OBSERVATIONS)
  rm(list=c("specimensRaw", "specimenList"))
  
  ESE_LV1_OBSERVATIONS   <- tweakLv1(x = ESE_LV1_OBSERVATIONS)

  ESE_BASKETS           <- tweakBasketsPostProcessing(basket = ESE_BASKETS, lv1 = ESE_LV1_OBSERVATIONS)

  ESE_BASKETS$SAMPLED    <-charToBinary(ESE_BASKETS$SAMPLED, bool=F)
  
  #ensure fields names are identical to what the tables expect
  #some uncessary, but for consistency they're all here.
  
  ESE_MISSIONS           <- ESE_MISSIONS[,c("MISSION", "SAMPLING_REQUIREMENT", "NOTE", 
                                            "DATA_VERSION", "PROGRAM_TITLE")]
  
  
  
  # colnames(ESE_SETS)[colnames(ESE_SETS)=="start_latitude_dd"] <- "SLAT"
  # colnames(ESE_SETS)[colnames(ESE_SETS)=="start_longitude_dd"] <- "SLONG"
  ESE_SETS               <- ESE_SETS[,c("MISSION", "SETNO", "START_DATE", "START_TIME", "END_DATE", 
                                        "END_TIME", "STRAT", "SLAT", "SLONG", "ELAT", "ELONG", 
                                        "AREA", "DIST", "HOWD", "SPEED", "HOWS", "DMIN", "DMAX", 
                                        "START_DEPTH", "END_DEPTH", "WIND", "FORCE", "CURNT", 
                                        "EXPERIMENT_TYPE_CODE", "GEAR", "AUX", "WARPOUT", "NOTE", 
                                        "SURFACE_TEMPERATURE", "BOTTOM_TEMPERATURE", 
                                        "BOTTOM_SALINITY", "HYDRO", "STATION", "BOTTOM_TYPE_CODE", 
                                        "BOTTOM_TEMP_DEVICE_CODE", "WAVE_HEIGHT_CODE", 
                                        "LIGHT_LEVEL_CODE", "GEAR_MONITOR_DEVICE_CODE")]
  
  colnames(ESE_BASKETS)[colnames(ESE_BASKETS)=="BASKET_WEIGHT_e"] <- "BASKET_WEIGHT"
  colnames(ESE_BASKETS)[colnames(ESE_BASKETS)=="catch_code"] <- "SPEC"
  colnames(ESE_BASKETS)[colnames(ESE_BASKETS)=="sample_class"] <- "SIZE_CLASS"
  ESE_BASKETS            <- ESE_BASKETS[,c("MISSION", "SETNO", "SPEC", "SIZE_CLASS", 
                                           "BASKET_WEIGHT", "SAMPLED")]
  ESE_BASKETS$SIZE_CLASS <- as.numeric(ESE_BASKETS$SIZE_CLASS)
  
  colnames(ESE_CATCHES2)[colnames(ESE_CATCHES2)=="catch_code"] <- "SPEC"
  colnames(ESE_CATCHES2)[colnames(ESE_CATCHES2)=="sample_class"] <- "SIZE_CLASS"
  ESE_CATCHES            <- ESE_CATCHES2[,c("MISSION", "SETNO", "SPEC", "SIZE_CLASS", "NOTE", 
                                            "UNWEIGHED_BASKETS", "NUMBER_CAUGHT")]
  ESE_CATCHES$SIZE_CLASS <- as.numeric(ESE_CATCHES$SIZE_CLASS)
  
  colnames(ESE_SPECIMENS)[colnames(ESE_SPECIMENS)=="catch_code"] <- "SPEC"
  colnames(ESE_SPECIMENS)[colnames(ESE_SPECIMENS)=="sample_class"] <- "SIZE_CLASS"
  colnames(ESE_SPECIMENS)[colnames(ESE_SPECIMENS)=="specimen_id"] <- "SPECIMEN_ID"
  ESE_SPECIMENS          <- ESE_SPECIMENS[,c("MISSION", "SETNO", "SPEC", "SIZE_CLASS", 
                                             "SPECIMEN_ID")]
  ESE_SPECIMENS$SIZE_CLASS <- as.numeric(ESE_SPECIMENS$SIZE_CLASS)
  
  colnames(ESE_LV1_OBSERVATIONS)[colnames(ESE_LV1_OBSERVATIONS)=="catch_code"] <- "SPEC"
  colnames(ESE_LV1_OBSERVATIONS)[colnames(ESE_LV1_OBSERVATIONS)=="sample_class"] <- "SIZE_CLASS"
  colnames(ESE_LV1_OBSERVATIONS)[colnames(ESE_LV1_OBSERVATIONS)=="specimen_id"] <- "SPECIMEN_ID"
  ESE_LV1_OBSERVATIONS   <- ESE_LV1_OBSERVATIONS[,c("MISSION", "SETNO", "SPEC", "SIZE_CLASS",
                                                    "SPECIMEN_ID", "LV1_OBSERVATION_ID", 
                                                    "LV1_OBSERVATION", "DATA_VALUE", "DATA_DESC")]
  ESE_LV1_OBSERVATIONS <- colTypeConverter(df = ESE_LV1_OBSERVATIONS)
  ESE_LV1_OBSERVATIONS$SIZE_CLASS <- as.numeric(ESE_LV1_OBSERVATIONS$SIZE_CLASS)
  if (ESE_BASKETS$MISSION[1]=="CAR2024010"){
  ESE_BASKETS[ESE_BASKETS$SPEC == 8100 & ESE_BASKETS$SETNO == 21,"SIZE_CLASS"] <- 1
  ESE_BASKETS[ESE_BASKETS$SPEC == 2214 & ESE_BASKETS$SETNO == 26,"SIZE_CLASS"] <- 1
  ESE_BASKETS[ESE_BASKETS$SPEC == 6118 & ESE_BASKETS$SETNO == 26,"SIZE_CLASS"] <- 1
  ESE_BASKETS[ESE_BASKETS$SPEC == 2214 & ESE_BASKETS$SETNO == 31,"SIZE_CLASS"] <- 1
  
  ESE_CATCHES[ESE_CATCHES$SPEC == 8100 & ESE_CATCHES$SETNO == 21,"SIZE_CLASS"] <- 1
  ESE_CATCHES[ESE_CATCHES$SPEC == 2214 & ESE_CATCHES$SETNO == 26,"SIZE_CLASS"] <- 1
  ESE_CATCHES[ESE_CATCHES$SPEC == 6118 & ESE_CATCHES$SETNO == 26,"SIZE_CLASS"] <- 1
  ESE_CATCHES[ESE_CATCHES$SPEC == 2214 & ESE_CATCHES$SETNO == 31,"SIZE_CLASS"] <- 1
  
  }
  x <- list()
  x$ESE_MISSIONS         <- ESE_MISSIONS 
  x$ESE_SETS             <- ESE_SETS
  x$ESE_BASKETS          <- ESE_BASKETS
  x$ESE_CATCHES          <- ESE_CATCHES
  x$ESE_SPECIMENS        <- ESE_SPECIMENS
  x$ESE_LV1_OBSERVATIONS <- ESE_LV1_OBSERVATIONS
  
  
  #x$SPP_LIST_GULF <- tmp$species_data[,c("code","common_name_en", "scientific_name", "aphia_id", "common_name_fr", "uuid")]
  
  return(x) 
}
