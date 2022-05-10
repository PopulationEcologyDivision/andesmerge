#' @title matchAndesToESE
#' @description This function matches the new CSV dump from Andes to the old ESE table schema so that data can
#' be loaded into Oracle.  
#' @param dataPath default is \code{NULL}.  This is the location of the csv files exported from andes
#' @return a list with all objects needed to load to Oracle
#' @family general_use
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
matchAndesToESE <- function(dataPath = NULL, quiet = FALSE){

  # load Andes  CSV files extracted from server at end of survey
  
  tmp <- loadData(dataPath = dataPath)
  
  # keepFieldsXxx  - extracts required/usable fields from andes data
  # tansmogrifyXxx - translate andes fields into formats/units, etc as used by ESE tables 
  # tweakXxx       - mission-specific modifications to the various ESE objects
 
  ESE_MISSIONS         <- keepFieldsMissions(tmp$cruise_data)  
  ESE_MISSIONS         <- transmogrifyMissions(ESE_MISSIONS)
  mission              <- ESE_MISSIONS[1,"MISSION"]
  
  ESE_SETS             <- keepFieldsSets(tmp$set_data, mission)
  ESE_SETS             <- transmogrifySets(ESE_SETS)
  ESE_SETS             <- tweakSets(ESE_SETS)
  
  ESE_BASKETS          <- keepFieldsBaskets(tmp$basket_data, mission)
  ESE_BASKETS          <- transmogrifyBaskets(ESE_BASKETS)
  ESE_BASKETS          <- tweakBaskets(ESE_BASKETS)
  
  ESE_CATCHES          <- keepFieldsCatches(tmp$catch_data, mission)
  ESE_CATCHES          <- transmogrifyCatches(ESE_CATCHES)
  ESE_CATCHES          <- tweakCatches(ESE_CATCHES)
  
  # both specimen and lv1 observations are kept together in specimen_data, so they are
  # initially handled together
  specimensRaw         <- keepFieldsSpecimens(tmp$specimen_data, mission)
  specimenList         <- reFormatSpecimen(specimensRaw)
  
  ESE_SPECIMENS        <- specimenList$specimen
  ESE_SPECIMENS        <- transmogrifySpecimens(ESE_SPECIMENS)
  ESE_SPECIMENS        <- tweakSpecimens(ESE_SPECIMENS)
  
  ESE_LV1_OBSERVATIONS <- specimenList$LV1_OBSERVATION
  ESE_LV1_OBSERVATIONS <- transmogrifyLV1_OBS(ESE_LV1_OBSERVATIONS)
  ESE_LV1_OBSERVATIONS <- tweakLv1(ESE_LV1_OBSERVATIONS)
  
  rm(list=c("tmp", "specimensRaw","specimenList"))

  x <- list()
  x$ESE_MISSIONS         <- ESE_MISSIONS 
  x$ESE_SETS             <- ESE_SETS
  x$ESE_BASKETS          <- ESE_BASKETS
  x$ESE_CATCHES          <- ESE_CATCHES
  x$ESE_SPECIMENS        <- ESE_SPECIMENS
  x$ESE_LV1_OBSERVATIONS <- ESE_LV1_OBSERVATIONS
  
  
  # # If experiment_type, start_date and end date are all blank, we drop the set
  # if (nrow(set[nchar(set[["experiment_type"]])==0 & nchar(set[["start_date"]])==0 & nchar(set[["end_date"]])==0,])>0){
  #   bad <- sort(unique(set[nchar(set[["experiment_type"]])==0 & nchar(set[["start_date"]])==0 & nchar(set[["end_date"]])==0,"set_number"]))
  #   warning("\n!!One or more sets had a bunch of empty fields.  These sets have been dropped to allow the loading to proceed, but should be dealt with before finalizing the load: \nSet(s):", paste0(bad, collapse=","))
  #   set<-set[!set$set_number %in% bad,]
  #   #do I need to specify mission
  #   #do we need to drop these sets from other tables?
  # }
  # 
  # test <- applySubsampling(catch = x$catch[x$catch$is_parent,], basket = )
  # 
  # names(x)[which(names(x) == "cruise")]   <- "ESE_MISSIONS"
  # names(x)[which(names(x) == "set")]      <- "ESE_SETS"
  # names(x)[which(names(x) == "basket")]   <- "ESE_BASKETS"
  # names(x)[which(names(x) == "catch")]    <- "ESE_CATCHES"
  # names(x)[which(names(x) == "specimen")] <- "ESE_SPECIMENS"
  # names(x)[which(names(x) == "lv1_obs")]  <- "ESE_LV1_OBSERVATIONS"
  
  # message("TEMPORARY - removing all species w 5 digit codes")
  # 
  # x$ESE_CATCHES <- x$ESE_CATCHES[nchar(x$ESE_CATCHES$SPEC)<=4,]
  # x$ESE_BASKETS <- x$ESE_BASKETS[nchar(x$ESE_BASKETS$SPEC)<=4,]
  # x$ESE_SPECIMENS <- x$ESE_SPECIMENS[nchar(x$ESE_SPECIMENS$SPEC)<=4,]
  # x$ESE_LV1_OBSERVATIONS <- x$ESE_LV1_OBSERVATIONS[nchar(x$ESE_LV1_OBSERVATIONS$SPEC)<=4,]
  
  # if (getBadSpp){
  #   x$SP_BAD <- unique(catch[nchar(catch$species_code)>4, c("species_code",  "species", "notes")])
  #   x$SP_specimen <- unique(specimen[nchar(specimen$species_code)>4, c("species_code",  "comment")])
  #   # x$SP_ESE_BASKETS <- x$ESE_BASKETS[nchar(x$ESE_BASKETS$SPEC)>4,c("SPEC" , "NOTE")]
  #   # x$SP_ESE_SPECIMENS <- x$ESE_SPECIMENS[nchar(x$ESE_SPECIMENS$SPEC)>4,c("SPEC" , "NOTE")]
  # }
  return(x) 
}
