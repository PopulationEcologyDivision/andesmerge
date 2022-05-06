#' @title matchAndesToESE
#' @description This function matches the new CSV dump from Andes to the old ESE table schema so that data can
#' be loaded into Oracle.  
#' @param dataPath default is \code{NULL}.  This is the location of the csv files exported from andes
#' @param quiet default is \code{FALSE} Determines whether or not the script should output 
#' informational messages 
#' @return a list with all objects needed to load to Oracle
#' @family general_use
#' @author  Pablo Vergara, \email{Pablo.Vergara@@dfo-mpo.gc.ca}
#' @export
matchAndesToESE <- function(dataPath = NULL, quiet = FALSE){
  getBadSpp <- TRUE
  # load Andes  CSV files extracted from server at end of survey
  tmp <<- loadData(dataPath = dataPath)
  
  # Object names created from load could change, Change here if needed
  set      = tmp$set_data
  catch    = tmp$catch_data
  basket   = tmp$basket_data
  # obs_type = tmp$obs_types_data
  specimen = tmp$specimen_data
  cruise   = tmp$cruise_data
  
  # If experiment_type, start_date and end date are all blank, we drop the set
  if (nrow(set[nchar(set[["experiment_type"]])==0 & nchar(set[["start_date"]])==0 & nchar(set[["end_date"]])==0,])>0){
    bad <- sort(unique(set[nchar(set[["experiment_type"]])==0 & nchar(set[["start_date"]])==0 & nchar(set[["end_date"]])==0,"set_number"]))
    warning("\n!!One or more sets had a bunch of empty fields.  These sets have been dropped to allow the loading to proceed, but should be dealt with before finalizing the load: \nSet(s):", paste0(bad, collapse=","))
    set<-set[!set$set_number %in% bad,]
    #do I need to specify mission
    #do we need to drop these sets from other tables?
  }
  
  # All tables will need this number
  missionNumber = gsub( "-","",cruise$mission_number)
  
  x = list()
  #' Match against various fields identified in 
  #' https://github.com/dfo-gulf-science/andes/blob/master/shared_models/data_fixtures/
  

  x$cruise                       =  data.frame(matrix(NA, nrow = dim(cruise)[1], ncol = 0))
  x$set                          =  data.frame(matrix(NA, nrow = dim(set)[1], ncol =0 ))
  x$basket                       =  data.frame(matrix(NA, nrow = dim(basket)[1], ncol = 0))
  x$catch                        =  data.frame(matrix(NA, nrow = dim(catch)[1], ncol = 0))
  x$specimen                     =  data.frame(matrix(NA, nrow = dim(specimen)[1], ncol = 0))  
  
  #' Match data for cruise table   
  x$cruise$MISSION <- missionNumber
  x$cruise$SAMPLING_REQUIREMENT <- cruise$area_of_operation
  x$cruise$NOTE <- cleanfields(cruise$description)
  x$cruise$DATA_VERSION <- NA
  x$cruise$PROGRAM_TITLE <- 'Maritimes Bottom Trawl Surveys'
  


  x$set$MISSION                  = missionNumber
  x$set$SETNO                    = set$set_number 
  x$set$START_DATE               = format.Date(strftime(as.POSIXlt(set$start_date, tz="UTC",format = "%Y-%m-%d %H:%M:%S")), format = "%d%m%Y")
  x$set$START_TIME               = as.integer(as.character(as.POSIXlt(set$start_date, tz="UTC",format = "%Y-%m-%d %H:%M:%S") , format = '%H%M'))
  x$set$END_DATE                 = format.Date(strftime(as.POSIXlt(set$end_date, tz="UTC",format = "%Y-%m-%d %H:%M:%S")), format = "%d%m%Y")
  x$set$END_TIME                 = as.integer(as.character(as.POSIXlt(set$end_date, tz="UTC",format = "%Y-%m-%d %H:%M:%S") , format = '%H%M'))
  x$set$STRAT                    = cleanStrata(set$stratum)
  x$set$SLAT                     = paste0(set$start_latitude_DD,sprintf("%05.2f",set$start_latitude_MMmm))  #sprintf used to ensure leading zeroes added as necessary.
  x$set$SLONG                    = paste0(set$start_longitude_DD,sprintf("%05.2f",set$start_longitude_MMmm))
  x$set$ELAT                     = paste0(set$end_latitude_DD,sprintf("%05.2f",set$end_latitude_MMmm))
  x$set$ELONG                    = paste0(set$end_longitude_DD,sprintf("%05.2f",set$end_longitude_MMmm))
  x$set$AREA                     = NA # MMM - used, but not sure what it would map to yet
  x$set$DIST                     = set$distance_towed                 #MMM - is this nautical miles
  x$set$HOWD                     = convertHOWOBT(set$distance_towed_obtained_code)
  x$set$SPEED                    = round(set$ship_speed,2)
  x$set$HOWS                     = convertHOWOBT(set$ship_speed_obtained_code)
  x$set$DMIN                     = NA # = set$dmin/1.8288 # MMM - converting from meters to fathoms, but not sure what it would map to yet
  x$set$DMAX                     = NA # = set$dmax/1.8288 # MMM - converting from meters to fathoms, but not sure what it would map to yet
  x$set$START_DEPTH              = round(meters2Fathoms(set$start_depth_m),0)  # MMM - converting from meters to fathoms
  x$set$END_DEPTH                = round(meters2Fathoms(set$end_depth_m),0)    # MMM - converting from meters to fathoms
  x$set$WIND                     = set$wind_direction_degree          # MMM - verified that actual degree is entered 
  x$set$FORCE                    = convertFORCE(set$wind_force_code)  # MMM - force of "9" is actually NA
  x$set$CURNT                    = set$tide_direction_code            # verified
  x$set$EXPERIMENT_TYPE_CODE     = setExperimentType(set)
  x$set$GEAR                     = as.numeric(stringi::stri_extract_first_regex(set$gear_type, "[0-9]+"))  #assume this is correct (and not gear_type_id)
  x$set$AUX                      = as.numeric(stringi::stri_extract_first_regex(set$auxiliary_equipment, "[0-9]+"))
  x$set$WARPOUT                  = set$port_warp
  x$set$NOTE                     = cleanfields(set$remarks)
  x$set$SURFACE_TEMPERATURE      = NA # data to come later, not captured during survey
  x$set$BOTTOM_TEMPERATURE       = NA # data to come later, not captured during survey
  x$set$BOTTOM_SALINITY          = NA # data to come later, not captured during survey
  x$set$HYDRO                    = NA # data to come later, not captured during survey
  x$set$STATION                  = stringi::stri_extract_first_regex(set$new_station, "\\d{3}")
  x$set$BOTTOM_TYPE_CODE         = NA
  x$set$BOTTOM_TEMP_DEVICE_CODE  = NA
  x$set$WAVE_HEIGHT_CODE         = NA
  x$set$LIGHT_LEVEL_CODE         = NA
  x$set$GEAR_MONITOR_DEVICE_CODE = NA
  
  # x$set$DUR                      = round(difftime(as.POSIXct(set$end_date, tz="UTC",format='%Y-%m-%d %H:%M:%S'), as.POSIXct(set$start_date, tz="UTC",format='%Y-%m-%d %H:%M:%S'), units = "mins"))

  #following need to be reviewed with Maritimes data
  
  # perform tweaks to base data here
  x$set = setTweaks(x$set)
  # Match data for BASKET table  
  x$basket$MISSION     = missionNumber
  x$basket$SETNO       = basket$set_number
  x$basket$SPEC        = basket$species_code
  x$basket$SIZE_CLASS  = basket$size_class
  x$basket$BASKET_WEIGHT     = basket$basket_wt_kg
  x$basket$SAMPLED     = TF2YN(basket$sampled)
  
  # perform tweaks to base data here
  x$basket = basketTweaks(x$basket)
  
  # Match data for CATCH table
  x$catch$MISSION           = missionNumber
  x$catch$SETNO             = catch$set_number
  x$catch$SPEC              = catch$species_code
  #CATCH data does not have size class info. That info is only available in the basket. 
  # We will simply put the default size class for now and then create the missing size class 2
  x$catch$SIZE_CLASS        = 1
  x$catch$NOTE              = cleanfields(catch$notes)
  x$catch$UNWEIGHED_BASKETS = catch$unweighed_baskets
  x$catch$NUMBER_CAUGHT     = catch$specimen_count  # need to verify that this field is what I think
  # New entries based on basket with size class of 2 will be entered here
  x$catch = addSizeClassToCatch(x$basket,x$catch)
  # perform tweaks to base data here
  x$catch = catchTweaks(x$catch)
  
  # Match data for SPECIMENS table
  x$specimen$MISSION     = missionNumber
  x$specimen$SETNO       = specimen$set_number
  x$specimen$SPEC        = specimen$species_code
  x$specimen$SIZE_CLASS  = specimen$size_class
  x$specimen$SPECIMEN_ID = specimen$id
  
  x$specimen = specimenTweaks(x$specimen)
  
  
  tempSpecimen = reFormatSpecimen(specimen)
  x$lv1_obs  =  data.frame(matrix(NA, nrow = dim(tempSpecimen)[1], ncol = 0))
  x$lv1_obs$MISSION            = missionNumber
  x$lv1_obs$SETNO              = tempSpecimen$SETNO
  x$lv1_obs$SPEC	             = tempSpecimen$SPEC
  x$lv1_obs$SIZE_CLASS         = tempSpecimen$SIZE_CLASS
  x$lv1_obs$SPECIMEN_ID        = tempSpecimen$SPECIMEN_ID
  x$lv1_obs$LV1_OBSERVATION_ID = tempSpecimen$LV1_OBSERVATION_ID
  x$lv1_obs$LV1_OBSERVATION    = tempSpecimen$LV1_OBSERVATION
  x$lv1_obs$DATA_VALUE	       = substr(tempSpecimen$DATA_VALUE, 1, 50) #Oracle table only allows length of 50
  x$lv1_obs$DATA_DESC	         = tempSpecimen$DATA_DESC
  
  names(x)[which(names(x) == "cruise")]   <- "ESE_MISSIONS"
  names(x)[which(names(x) == "set")]      <- "ESE_SETS"
  names(x)[which(names(x) == "basket")]   <- "ESE_BASKETS"
  names(x)[which(names(x) == "catch")]    <- "ESE_CATCHES"
  names(x)[which(names(x) == "specimen")] <- "ESE_SPECIMENS"
  names(x)[which(names(x) == "lv1_obs")]  <- "ESE_LV1_OBSERVATIONS"
  
  message("TEMPORARY - removing all species w 5 digit codes")

  x$ESE_CATCHES <- x$ESE_CATCHES[nchar(x$ESE_CATCHES$SPEC)<=4,]
  x$ESE_BASKETS <- x$ESE_BASKETS[nchar(x$ESE_BASKETS$SPEC)<=4,]
  x$ESE_SPECIMENS <- x$ESE_SPECIMENS[nchar(x$ESE_SPECIMENS$SPEC)<=4,]
  x$ESE_LV1_OBSERVATIONS <- x$ESE_LV1_OBSERVATIONS[nchar(x$ESE_LV1_OBSERVATIONS$SPEC)<=4,]
  
  if (getBadSpp){
    x$SP_BAD <- unique(catch[nchar(catch$species_code)>4, c("species_code",  "species", "notes")])
    x$SP_specimen <- unique(specimen[nchar(specimen$species_code)>4, c("species_code",  "comment")])
  # x$SP_ESE_BASKETS <- x$ESE_BASKETS[nchar(x$ESE_BASKETS$SPEC)>4,c("SPEC" , "NOTE")]
  # x$SP_ESE_SPECIMENS <- x$ESE_SPECIMENS[nchar(x$ESE_SPECIMENS$SPEC)>4,c("SPEC" , "NOTE")]
  }
  return(x) 
}
