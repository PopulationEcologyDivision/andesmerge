#' @title matchAndesToESE
#' @description This function matches the new CSV dump from Andes to the old ESE table schema so that data can
#' be loaded into Oracle.  
#' @param quiet default is \code{FALSE} Determines whether or not the script should output 
#' informational messages 
#' @return a list with all objects needed to load to Oracle
#' @family general_use
#' @author  Pablo Vergara, \email{Pablo.Vergara@@dfo-mpo.gc.ca}
#' @export
matchAndesToESE <- function(quiet = FALSE){
#library(stringi)

# load Andes  CSV files extracted from server at end of survey
andesmerge::loadData()

# Object names created from load could change, Cahnge here if needed
set      = tmp_set_data
catch    = tmp_catch_data
basket   = tmp_basket_data
obs_type = tmp_obs_types_data
specimen = tmp_specimen_data
cruise   = tmp_cruise_data

# All tables will need this number
missionNumber = gsub( "-","",cruise$mission_number)

x = list()
x$set =  data.frame(matrix(NA, nrow = dim(set)[1], ncol =0 ))
x$basket =  data.frame(matrix(NA, nrow = dim(basket)[1], ncol = 0))
x$catch =  data.frame(matrix(NA, nrow = dim(catch)[1], ncol = 0))
x$specimen =  data.frame(matrix(NA, nrow = dim(specimen)[1], ncol = 0))

# Match data for SET table 
x$set$MISSION                  = missionNumber
x$set$SETNO                    = set$set_number 
x$set$START_DATE               = as.POSIXct(set$start_date, tz="UTC")
x$set$START_TIME               = as.POSIXct(set$start_date, tz="UTC")
x$set$END_DATE                 = as.POSIXct(set$start_date, tz="UTC")
x$set$END_TIME                 = as.POSIXct(set$start_date, tz="UTC")
x$set$STRAT                    = stri_extract_last_regex(set$new_station, "\\d{3}")
x$set$STATION                  = stri_extract_first_regex(set$new_station, "\\d{3}")
x$set$SLAT                     = round(as.numeric(paste0(set$start_latitude_DD,set$start_latitude_MMmm)),2)
x$set$ELAT                     = round(as.numeric(paste0(set$end_latitude_DD,set$end_latitude_MMmm)),2)
x$set$SLONG                    = abs(round(as.numeric(paste0(set$start_longitude_DD,set$start_longitude_MMmm)),2))
x$set$ELONG                    = abs(round(as.numeric(paste0(set$end_longitude_DD,set$end_longitude_MMmm)),2))
x$set$AREA                     = NA # need to check if used
x$set$DIST                     = set$distance_towed
x$set$HOWD                     = set$distance_towed_obtained_code
x$set$SPEED                    = set$ship_speed
x$set$HOWS                     = set$ship_speed_obtained_code
x$set$DMIN                     = NA # need to check if used
x$set$DMAX                     = NA # need to check if used
x$set$START_DEPTH              = set$start_depth_m
x$set$END_DEPTH                = set$end_depth_m
x$set$WIND                     = NA # need to check if used
x$set$FORCE                    = set$wind_force_code
x$set$CURNT                    = NA # need to check if used
x$set$EXPERIMENT_TYPE_CODE     = as.numeric(stri_extract_first_regex(set$experiment_type, "\\d{1}"))
x$set$GEAR                     = as.numeric(stri_extract_first_regex(set$gear_type, "[0-9]+"))
x$set$AUX                      = as.numeric(stri_extract_first_regex(set$auxiliary_equipment, "[0-9]+"))
x$set$WARPOUT                  = set$port_warp
x$set$NOTE                     = set$remarks
x$set$SURFACE_TEMPERATURE      = NA # data to come later, not captured during survey
x$set$BOTTOM_TEMPERATURE       = NA # data to come later, not captured during survey
x$set$BOTTOM_SALINITY          = NA # data to come later, not captured during survey
x$set$HYDRO                    = NA # data to come later, not captured during survey
x$set$BOTTOM_TYPE_CODE         = NA # unused ?
x$set$BOTTOM_TEMP_DEVICE_CODE  = NA # unused ?
x$set$WAVE_HEIGHT_CODE         = NA # unused ?
x$set$LIGHT_LEVEL_CODE         = NA # unused ?
x$set$GEAR_MONITOR_DEVICE_CODE = NA # unused ?


# Match data for BASSKET table  
x$basket$MISSION     = missionNumber
x$basket$SETNO       = basket$set_number
x$basket$SPEC        = basket$species_code
x$basket$SIZE_CLASS  = basket$size_class
x$basket$SPECIMEN_ID = basket$id  # not sure about this one

#TWEAK FOR 2021 data.  Should go in separate function to be called by year (will talk to Mike about this)
 index = x$basket$SIZE_CLASS == 3 & x$basket$SPEC == 23
 x$basket[index,]$SIZE_CLASS = 2





 # Match data for CATCH table

x$catch$MISSION           = missionNumber
x$catch$SETNO             = catch$set_number
x$catch$SPEC              = catch$species_code

#CATCH data does not have size class info. That info is only available in the basket. 
# We will simply put the default size class for now and then create the missing size class 2
x$catch$SIZE_CLASS        = 1
x$catch$NOTE              = catch$notes
x$catch$UNWEIGHED_BASKETS = catch$unweighed_baskets
x$catch$NUMBER_CAUGHT     = catch$specimen_count  # need to verify that this field is what I think
# New entries based on basket with size class of 2 will be entered here
x$catch = addSizeClassToCatch(x$basket,x$catch)



# Match data for SPECIMEN table

x$specimen$MISSION     = missionNumber
x$specimen$SETNO       = specimen$set_number
x$specimen$SPEC        = specimen$species_code
x$specimen$SIZE_CLASS  = specimen$size_class
x$specimen$SPECIMEN_ID = specimen$id



# Match data for level 1 observations 

# Need to turn specimen table from wide format to long in order to have each observation on its own row
tempSpecimen <- gather(specimen, variable, value, 13:22)
# remove all NA values
tempSpecimen = tempSpecimen[!is.na(tempSpecimen$value),]
# periods were introduced in the data frame names, they must be taken out before we can compare to other table  
tempSpecimen$variable = gsub("\\."," ", tempSpecimen$variable)

x$lv1_obs  =  data.frame(matrix(NA, nrow = dim(tempSpecimen)[1], ncol = 0))

x$lv1_obs$MISSION = missionNumber
x$lv1_obs$SETNO =  tempSpecimen$set_number
x$lv1_obs$SPEC	 = tempSpecimen$species_code
x$lv1_obs$SIZE_CLASS = tempSpecimen$size_class
x$lv1_obs$SPECIMEN_ID = tempSpecimen$id
x$lv1_obs$LV1_OBSERVATION= tempSpecimen$variable
x$lv1_obs$DATA_VALUE	= tempSpecimen$value

  
  tempSpecimen$variable = tolower(tempSpecimen$variable)
  obs_type$name = tolower(obs_type$name)
  obs_type = obs_type[,c(1:2,4)]
  names(obs_type)  = c("LV1_OBSERVATION_ID","name", "DATA_DESC")
  merged=merge(obs_type, tempSpecimen,  by.y = "variable", by.x = "name" , all.y = TRUE)

  x$lv1_obs$LV1_OBSERVATION_ID = merged$LV1_OBSERVATION_ID
  x$lv1_obs$DATA_DESC = merged$DATA_DESC
 
  return(x) 
}