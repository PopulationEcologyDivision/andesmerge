#' @title matchAndesToESE
#' @description This function matches the new CSV dump from Andes to the old ESE table schema so that data can
#' be loaded into Oracle. 
#' @param dataPath default is \code{NULL}. This is the location of the csv files exported from andes
#' @return a list with all objects needed to load to Oracle
#' @family general_use
#' @author Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
matchAndesToESE <- function(dataPath = NULL, gulfCodes=F){
  # load Andes CSV files extracted from server at end of survey 
  tmp                    <- loadData(dataPath = dataPath)
 
  catch_data_wo_baskets <- tmp$catch_data[!paste0(tmp$catch_data$mission_number ,"_",tmp$catch_data$set_number,"_",tmp$catch_data$species_code) %in% 
                                   paste0(tmp$basket_data$mission_number ,"_",tmp$basket_data$set_number,"_",tmp$basket_data$species_code),
                                   c("created_at", "id", "mission_number" , "set_number","species_aphia_id", "species_code", "species_en", "species_fr", "species_scientific", "species_uuid" )]
  colnames(catch_data_wo_baskets)[colnames(catch_data_wo_baskets)=="created_at"] <- "creation_date"
  colnames(catch_data_wo_baskets)[colnames(catch_data_wo_baskets)=="id"] <- "catch_id"
  catch_data_wo_baskets$basket_wt_kg   <- NA
  catch_data_wo_baskets$sampled  <- "No"
  catch_data_wo_baskets$size_class  <- 1
  catch_data_wo_baskets$size_class_display  <- 1
  catch_data_wo_baskets$id   <- 888888
  catch_data_wo_baskets$last_modified_date <- catch_data_wo_baskets$creation_date 
  
  # tmp$basket_data <- rbind.data.frame(tmp$basket_data, catch_data_wo_baskets)
  if (gulfCodes) {
    tmp                    <- uuidToGulf(tmp)
  }else{
    tmp$specimen_data<- merge(tmp$specimen_data, RVSurveyData::GSSPECIES[, c("UUID", "CODE")], all.x=T, by.x="species_uuid", by.y="UUID")
    tmp$basket_data<- merge(tmp$basket_data, RVSurveyData::GSSPECIES[, c("UUID", "CODE")], all.x=T, by.x="species_uuid", by.y="UUID")
    tmp$catch_data<- merge(tmp$catch_data, RVSurveyData::GSSPECIES[, c("UUID", "CODE")], all.x=T, by.x="species_uuid", by.y="UUID")
    colnames(tmp$specimen_data)[colnames(tmp$specimen_data)=="CODE"] <- "SPEC"
    colnames(tmp$basket_data)[colnames(tmp$basket_data)=="CODE"] <- "SPEC"
    colnames(tmp$catch_data)[colnames(tmp$catch_data)=="CODE"] <- "SPEC"
    if ("observation_data" %in% names(tmp)){
      colnames(tmp$observation_data)[colnames(tmp$observation_data)=="species_code"] <- "SPEC"
    }
  }
  # keepFieldsXxx  - extracts required/usable fields from andes data
  # tansmogrifyXxx - translate andes fields into formats/units, etc as used by ESE tables 
  # tweakXxx       - mission-specific modifications to the various ESE objects
  if ("cruise_data" %in% names(tmp)) {
    ESE_MISSIONS           <- keepFieldsMissions(tmp$cruise_data, new=F) 
    ESE_MISSIONS           <- transmogrifyMissions(ESE_MISSIONS)
    mission                <- ESE_MISSIONS[1,"MISSION"]
    tmp$specimensRaw      <- keepFieldsSpecimens(tmp$specimen_data, mission)
  }else{
    ESE_MISSIONS           <- keepFieldsMissions(tmp$mission_data, new=T) 
    ESE_MISSIONS           <- transmogrifyMissions(ESE_MISSIONS)
    mission                <- ESE_MISSIONS[1,"MISSION"]
    mrg <- merge(tmp$specimen_data[,!names(tmp$specimen_data) %in% c("species_en", "species_fr", "species_scientific", "species_aphia_id")], 
                 tmp$observation_data[,c("specimen_id","observation_type", "observation_value_raw")], all.y=T, by.x = "id", by.y="specimen_id")
    mrg <- cbind(mrg, mission)
    colnames(mrg)[colnames(mrg)=="mission"] <- "MISSION"
    mrg$observation_type <- gsub("[()]", "", mrg$observation_type)
    mrg$observation_type <- tolower(mrg$observation_type)
    mrg$observation_type <- gsub(" w/ fish number", "", mrg$observation_type)
    mrg$observation_type <- trimws(mrg$observation_type)
    mrg$observation_type <- gsub(" ", ".", mrg$observation_type)
    tmp$specimensRaw <- mrg %>% 
      tidyr::pivot_wider(id_cols = c("id", "MISSION", "basket_id", "comment", "fish_number", "set_number", "size_class", "SPEC"), names_from = "observation_type", values_from = "observation_value_raw") %>% as.data.frame()
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


# nullBaskets<-setdiff(ESE_CATCHES[,c("MISSION", "SETNO", "SPEC", "catch_id")], ESE_BASKETS[,c("MISSION", "SETNO", "SPEC", "catch_id")])
# nullBaskets$SIZE_CLASS <- 1
# nullBaskets$BASKET_WEIGHT <- NA
# nullBaskets$SAMPLED <- F
# nullBaskets$basket_id <- 99999
  # ESE_BASKETS <- rbind.data.frame(ESE_BASKETS, nullBaskets)

  ESE_CATCHES            <- merge(ESE_CATCHES, 
                                  unique(ESE_BASKETS[,c("MISSION", "SETNO", "SPEC", "catch_id", "SIZE_CLASS")]), 
                                  by = c("MISSION", "SETNO", "SPEC", "catch_id"), all.x = T) 

  # print(ESE_BASKETS[ESE_BASKETS$SETNO==217 & ESE_BASKETS$SPEC %in% c(2521, 2526, 6411), !names(ESE_BASKETS) %in% "NOTE"])
  subsampled             <- redistributeMixedCatch2(catch = ESE_CATCHES, basket = ESE_BASKETS, quiet = T)
#   message("subsampled")
#   print(subsampled$basket[subsampled$basket$SETNO==217 & subsampled$basket$SPEC %in% c(2521, 2526, 6411), !names(subsampled$basket) %in% "NOTE"])
# browser()
  ESE_CATCHES            <- subsampled$catch
  
  ESE_BASKETS            <- subsampled$basket
  
  
  # ESE_BASKETS <- ESE_BASKETS[!is.na(ESE_BASKETS$BASKET_WEIGHT),]


  # both specimen and lv1 observations are kept together in specimen_data, so they are
  # initially handled together
  # browser()
  # colnames(specimensRaw)[colnames(specimensRaw)=="set_number"] <- "new_name"
  specimensRaw            <- tmp$specimensRaw
  specimensRaw           <- tweakSpecimensRaw(specimensRaw)  

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
  
  ESE_LV1_OBSERVATIONS <- colTypeConverter(df = ESE_LV1_OBSERVATIONS)

  
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
