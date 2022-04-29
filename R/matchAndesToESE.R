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
  # load Andes  CSV files extracted from server at end of survey
  tmp <- loadData(dataPath = dataPath)
  # Object names created from load could change, Change here if needed
  set      = tmp$set_data
  catch    = tmp$catch_data
  basket   = tmp$basket_data
  obs_type = tmp$obs_types_data
  specimen = tmp$specimen_data
  cruise   = tmp$cruise_data

  rm(tmp)
  ### This was added in response to some missing cursory data - seems like exporting the data too
  ### early will likely result in this not being as rare an event as we might hope
  if (nrow(set[nchar(set[["experiment_type"]])==0 &
               nchar(set[["start_date"]])==0 &
              nchar(set[["end_date"]])==0,])>0){
    bad <- sort(unique(set[nchar(set[["experiment_type"]])==0 &
                 nchar(set[["start_date"]])==0 &
                 nchar(set[["end_date"]])==0,"set_number"]))
    warning("\n!!One or more sets had a bunch of empty fields.  These sets have been dropped to allow 
the loading to proceed, but should be dealt with before finalizing the load: \nSet(s):", paste0(bad, collapse=","))
    set<-set[!set$set_number %in% bad,]
     }
  
  # All tables will need this number
  missionNumber = gsub( "-","",cruise$mission_number)
  
  x = list()
  
  # Match data for cruise table 
  x$cruise =   data.frame(matrix(NA, nrow = dim(cruise)[1], ncol = 0))
  x$cruise$MISSION <- missionNumber
  x$cruise$SAMPLING_REQUIREMENT <- cruise$area_of_operation
  x$cruise$NOTE <- cleanfields(cruise$description)
  x$cruise$DATA_VERSION <- NA
  x$cruise$PROGRAM_TITLE <- 'Maritimes Bottom Trawl Surveys'
  
  x$set                          =  data.frame(matrix(NA, nrow = dim(set)[1], ncol =0 ))
  x$basket                       =  data.frame(matrix(NA, nrow = dim(basket)[1], ncol = 0))
  x$catch                        =  data.frame(matrix(NA, nrow = dim(catch)[1], ncol = 0))
  x$specimen                     =  data.frame(matrix(NA, nrow = dim(specimen)[1], ncol = 0))

  # Match data for SET table 
  x$set$MISSION                  = missionNumber
  x$set$SETNO                    = set$set_number 
  x$set$SDATE                    = strftime(as.POSIXlt(set$start_date, tz="UTC",format = "%Y-%m-%d %H:%M:%S"))
  x$set$TIME                     = as.integer(as.character(as.POSIXlt(set$start_date, tz="UTC",format = "%Y-%m-%d %H:%M:%S") , format = '%H%M'))
  x$set$DUR                      = round(difftime(as.POSIXct(set$end_date, tz="UTC",format='%Y-%m-%d %H:%M:%S'), as.POSIXct(set$start_date, tz="UTC",format='%Y-%m-%d %H:%M:%S'), units = "mins"))
  x$set$ETIME                    = as.integer(as.character(as.POSIXlt(set$end_date, tz="UTC",format = "%Y-%m-%d %H:%M:%S") , format = '%H%M'))

  
  x$set$STRAT                    = stringi::stri_extract_last_regex(set$new_station, "\\d{3}")
  x$set$SLAT                     = round(as.numeric(paste0(set$start_latitude_DD,set$start_latitude_MMmm)),2)
  x$set$ELAT                     = round(as.numeric(paste0(set$end_latitude_DD,set$end_latitude_MMmm)),2)
  x$set$SLONG                    = abs(round(as.numeric(paste0(set$start_longitude_DD,set$start_longitude_MMmm)),2))
  x$set$ELONG                    = abs(round(as.numeric(paste0(set$end_longitude_DD,set$end_longitude_MMmm)),2))
  x$set$DIST                     = set$distance_towed #MMM - is this nautical miles
  x$set$HOWD                     = set$distance_towed_obtained_code
  x$set$SPEED                    = set$ship_speed
  x$set$HOWS                     = set$ship_speed_obtained_code
  x$set$START_DEPTH              = set$start_depth_m/1.8288 # MMM - converting from meters to fathoms
  x$set$END_DEPTH                = set$end_depth_m/1.8288 # MMM - converting from meters to fathoms

  x$set$WIND                     = set$wind_direction_degree 
  x$set$FORCE                    = set$wind_force_code
  x$set$CURNT                    = set$tide_direction_code # need to check if used
  x$set$EXPERIMENT_TYPE_CODE     = setExperimentType(set)
  x$set$GEAR                     = as.numeric(stringi::stri_extract_first_regex(set$gear_type, "[0-9]+"))  #assume this is correct (and not gear_type_id)
  x$set$AUX                      = as.numeric(stringi::stri_extract_first_regex(set$auxiliary_equipment, "[0-9]+"))
  x$set$NOTE                     = cleanfields(set$remarks)
  x$set$STATION                  = stringi::stri_extract_first_regex(set$new_station, "\\d{3}")
  
  #following need to be reviewed with Maritimes data
  x$set$WARPOUT                  = set$port_warp
  x$set$AREA                     = NA # MMM - used, but not sure what it would map to yet
  x$set$DMIN                     = NA # = set$dmin/1.8288 # MMM - converting from meters to fathoms, but not sure what it would map to yet
  x$set$DMAX                     = NA # = set$dmax/1.8288 # MMM - converting from meters to fathoms, but not sure what it would map to yet
  x$set$SURFACE_TEMPERATURE      = NA # data to come later, not captured during survey
  x$set$BOTTOM_TEMPERATURE       = NA # data to come later, not captured during survey
  x$set$BOTTOM_SALINITY          = NA # data to come later, not captured during survey
  x$set$HYDRO                    = NA # data to come later, not captured during survey
 
  # perform tweaks to base data here
  x$set = setTweaks(x$set)
  
  # Match data for BASSKET table  
  x$basket$MISSION     = missionNumber
  x$basket$SETNO       = basket$set_number
  x$basket$SPEC        = basket$species_code
  x$basket$SIZE_CLASS  = basket$size_class
  x$basket$SPECIMEN_ID = basket$id  # not sure about this one
  
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
  
  # Match data for SPECIMEN table
  
  x$specimen$MISSION     = missionNumber
  x$specimen$SETNO       = specimen$set_number
  x$specimen$SPEC        = specimen$species_code
  x$specimen$SIZE_CLASS  = specimen$size_class
  x$specimen$SPECIMEN_ID = specimen$id

  x$specimen = specimenTweaks(x$specimen)
  
    
  tempSpecimen = reFormatSpecimen(specimen)
  x$lv1_obs  =  data.frame(matrix(NA, nrow = dim(tempSpecimen)[1], ncol = 0))
  
  x$lv1_obs$MISSION            = missionNumber
  x$lv1_obs$SETNO              =tempSpecimen$SETNO
  x$lv1_obs$SPEC	             = tempSpecimen$SPEC
  x$lv1_obs$SIZE_CLASS         = tempSpecimen$SIZE_CLASS
  x$lv1_obs$SPECIMEN_ID        = tempSpecimen$SPECIMEN_ID
  x$lv1_obs$LV1_OBSERVATION    = tempSpecimen$LV1_OBSERVATION
  x$lv1_obs$DATA_VALUE	       = tempSpecimen$DATA_VALUE
  x$lv1_obs$LV1_OBSERVATION_ID = tempSpecimen$LV1_OBSERVATION_ID
  
  names(x)[which(names(x) == "cruise")]   <- "ESE_MISSIONS"
  names(x)[which(names(x) == "set")]      <- "ESE_SETS"
  names(x)[which(names(x) == "basket")]   <- "ESE_BASKETS"
  names(x)[which(names(x) == "catch")]    <- "ESE_CATCHES"
  names(x)[which(names(x) == "specimen")] <- "ESE_SPECIMENS"
  names(x)[which(names(x) == "lv1_obs")]  <- "ESE_LV1_OBSERVATIONS"
  return(x) 
}
