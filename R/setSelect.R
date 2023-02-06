#' @title setSelect
#' @description This function randomly selects stations within particular strata.  A specified 
#' number of stations of 3 different categories can be selected, and the minimum distance required 
#' between the stations can be specified.  Additionally, station selection can avoid particular 
#' areas through the inclusion of an additional shapefile.
#' @param MaritimesSurvey default is \code{NULL}.  By default, the following parameters must be 
#' supplied for this function to work:
#' \itemize{
#'   \item \code{stationData}
#'   \item \code{stationData_StratField}
#'   \item \code{strataShp}
#'   \item \code{strataShp_StratField}
#'   \item \code{localCRS}
#'}
#'However, if \code{MaritimesSurvey} is set to one of the Maritimes-specific identifiers below, the
#'fields above will be auto-populated:
#' \itemize{
#'  \item \code{"4VW_Spring"}
#'  \item \code{"4X_Spring"}
#'  \item \code{"4VWX_Summer"}
#'  \item \code{"5Z_Georges"}
#'  }
#' @param stationData default is \code{NULL}.  This is the path to a csv file containing the all of 
#' the strata for which stations should be generated.  In addition to the strata, this file must 
#' also include the fields  \code{"PRIMARY"}, \code{"SECONDARY"}, and \code{"ALTERNATE"}.  These 
#' three fields should contain integers corresponding with how many of that type of station should 
#' be generated.  If no stations of a particular type should be generated, a value of NA should be 
#' present. Strata where \code{PRIMARY} is set to NA will not have any stations generated. Below is 
#' an example of how this file might look:
#' > head(stationFile)
#'   STRATUM PRIMARY ALTERNATE SECONDARY
#'       5Z1       6         3         4
#'       5Z2      11         6        NA
#'       5Z3       5         2         2
#'       5Z4       4         2         2
#'
#' @param stationData_StratField default is \code{"STRATUM"}.  This is just the name of the field 
#' that contains the strata.  For the example above, this would be "STRATUM".
#' @param strataShp default is \code{NULL}. This should point to the ".shp" file of a shapefile for 
#' the strata to use. This shapefile must have a field that has values identical to those found in 
#' the \code{"stationData_StratField"} within the \code{"stationData"} file.
#' @param strataShp_StratField default is \code{NULL}.  This is the name of the field within 
#' \code{strataShp} that contains the identifier for the strata.  Continuing with the example above,
#' this field would contain values including "5Z1", "5Z2", "5Z3" and "5Z4".
#' @param localCRS default is \code{2961}.  In order to create sampling stations, the function 
#' reprojects any spatial data to a locally appropriate projection.  The default value is UTM Zone 
#' 20N, and is appropriate only for Maritimes data.  Any valid CRS can be entered here, and should 
#' be appropriate for your data.  
#' @param minDistNM default is \code{4}. This is the minimum required distance between the sets.  By
#' default, sets will be no closer than 4 nautical miles from each other.
#' @param avoidShp default is \code{NULL}. This should point to the ".shp" file of a shapefile 
#' containing polygons of the areas where stations should not be located.   For example, one might 
#' populate a file with areas known to contain unexploded ordinance, or areas where bottom contact 
#' is forbidden.  No stations will be generated where polygons exist in this file.
#' @examples \dontrun{
#' NonMaritimesExample <-setSelect(minDistNM = 4, 
#'                                 stationData = "mySurveyStrata_requirements.csv", 
#'                                 stationData_StratField = "STRATUM", 
#'                                 strataShp = "myStrata.shp", 
#'                                 strataShp_StratField = "StrataID",
#'                                 avoidShp = "areasWeCantTrawl.shp", 
#'                                 localCRS = 2961, 
#'                                 MaritimesSurvey = NULL)
#'
#' MaritimesExample   <- setSelect(MaritimesSurvey = "GEORGES_5Z",
#'                                 avoidShp = "Maritimes2022_AvoidAreas.shp")
#'                        }
#' @family surveyTools
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export

setSelect <- function(MaritimesSurvey = NULL,
                      stationData = NULL,
                      stationData_StratField = NULL,
                      strataShp = NULL,
                      strataShp_StratField = NULL,
                      avoidShp = NULL,
                      localCRS = NULL,
                      minDistNM = 4){
  
  TYPE <- LABEL <- polygon_ <- SPRING_4VW <- SPRING_4X <- SUMMER_4VWX <- GEORGES_5Z <- NA 

  if (!is.null(MaritimesSurvey)){
    localCRS_ <- 2961
    # stationData_ <- switch(toupper(MaritimesSurvey),
    #                       "SPRING_4VW" =  loadRData(SPRING_4VW),
    #                       "SPRING_4X" = get(SPRING_4X),
    #                       "SUMMER_4VWX" = get(SUMMER_4VWX),
    #                       "GEORGES_5Z" = loadRData(GEORGES_5Z)
    #                       )
    if(toupper(MaritimesSurvey)=="SPRING_4VW")stationData_ <- SPRING_4VW
    if(toupper(MaritimesSurvey)=="SPRING_4X")stationData_ <- SPRING_4X
    if(toupper(MaritimesSurvey)=="SUMMER_4VWX")stationData_ <- SUMMER_4VWX
    if(toupper(MaritimesSurvey)=="GEORGES_5Z")stationData_ <- GEORGES_5Z

    stationData_StratField_ <- "STRATUM"

    strataShp_ <- RVSurveyData::strataMar_sf %>% sf::st_transform(crs = localCRS_) 
    colnames(stationData_)[colnames(stationData_)=="STRATUM"] <- "filterField_"
    colnames(strataShp_)[colnames(strataShp_)=="STRATA_ID"] <- "filterField_"
    
  }else{  
    localCRS_ <- localCRS
    stationData_ <- utils::read.csv(stationData)
    stationData_StratField_ <- stationData_StratField
    strataShp_ <- sf::st_read(strataShp, quiet = T) %>% sf::st_transform(crs = localCRS_) 
    strataShp_StratField_ <- strataShp_StratField
    #referencing fields via variables is annoying, so add a field of a known name to each
    stationData_$filterField_ <- stationData_[,stationData_StratField]
    strataShp_$filterField_ <- sf::st_drop_geometry(strataShp_[, strataShp_StratField_])[,1]
    #delete the user-selected fields so they don't  cause issues with merging
    stationData_[,stationData_StratField] <- NULL
    strataShp_[, stationData_StratField_]<- NULL
  }

  #buffer is in meters, and is 1/2 the min distance
  buffSize <- (minDistNM * 1852)

  assignStrata <- function(df = NULL, type = "PRIMARY", n_sets=NULL){
    #randomly select n_sets stations from the unassigned
    df[sample(which(is.na(df$TYPE)),n_sets),"TYPE"]<-type
    
    return(df)
  }
  convert.dd.dddd<-function(x){
    dat<-data.frame(ddmm.mm=NA,dd.dddd=x)
    
    #degrees-minutes-seconds -> degrees-minutes
    ddmmss<-dat$dd.dddd[!is.na(dat$dd.dddd)&abs(dat$dd.dddd)>9000]
    ddmm.ss<-ddmmss/100
    ddmm<-trunc(ddmm.ss)
    ss<-(ddmm.ss-ddmm)*100
    dat$ddmm.mm[!is.na(dat$dd.dddd)&abs(dat$dd.dddd)>9000]<-ddmm+ss/60
    
    #degrees-minutes -> degrees-minutes
    dat$ddmm.mm[!is.na(dat$dd.dddd)&abs(dat$dd.dddd)>90&abs(dat$dd.dddd)<9000]<-dat$dd.dddd[!is.na(dat$dd.dddd)&abs(dat$dd.dddd)>90&abs(dat$dd.dddd)<9000]
    
    #degrees -> degrees-minutes
    dd.dddd<-dat$dd.dddd[!is.na(dat$dd.dddd)&abs(dat$dd.dddd)<90]
    dd<-trunc(dd.dddd)
    mm.mm<-(dd.dddd-dd)*60
    dat$ddmm.mm[!is.na(dat$dd.dddd)&abs(dat$dd.dddd)<90]<-dd*100+mm.mm
    
    return(dat$ddmm.mm)
  }
  
  # Clips the areas of x out of y
  # st_erase from https://bit.ly/3WuRj1b
  st_erase <- function(x, y) sf::st_difference(x, sf::st_union(sf::st_combine(y)))
  

  
  #handle submitted station data - replace "NA" with NA, and convert fields to numeric/character as appropriate
  stationData_[stationData_ == "NA"] <- NA  
  stationData_[] <- lapply(stationData_, function(x) utils::type.convert(as.character(x), as.is = TRUE))
  stationData_ <- stationData_[!is.na(stationData_$PRIMARY),]
  stationData_$TOT <- rowSums(stationData_[,c("PRIMARY", "SECONDARY", "ALTERNATE")], na.rm = T)
  
  #create filtered version of strata file of only those strata present in the stationData_ csv and add the stationData_ into the sf
  filtStrata <- strataShp_[strataShp_$filterField_ %in% stationData_$filterField_,]
  filtStrata <- merge(filtStrata, stationData_, all.x=T, by.x="filterField_", by.y="filterField_")

  if(!is.null(avoidShp)){
    # remove untrawlable areas from the selected strata prior to station selection
    # generates warning "attribute variables are assumed to be spatially constant throughout all geometries"
    avoidShp <- sf::st_read(avoidShp, quiet = T) %>% sf::st_transform(crs = localCRS_) 
    filtStrata <- suppressWarnings(st_erase(filtStrata, avoidShp))
  }
  
  allStrat <- unique(sf::st_drop_geometry(filtStrata$filterField_))
  stations <-list()
  
  for (s in 1:length(allStrat)){ 
    message("Working on ",allStrat[s])
    x = filtStrata[filtStrata$filterField_==allStrat[s],]
    x.owin <- spatstat.geom::as.owin(sf::as_Spatial(x))
    
    polySet <- sf::st_as_sf(spatstat.random::rSSI(r = buffSize, n = x$TOT, win = x.owin, giveup = 100))
    polySet <- polySet[polySet$label=="point",]
    polySet <- sf::st_set_crs(polySet, localCRS_)
    
    stations[[allStrat[s]]] <- sf::st_as_sf(polySet)
    stations[[allStrat[s]]]$polygon_ <- allStrat[s]
    
    stations[[allStrat[s]]]<- merge(stations[[allStrat[s]]], sf::st_drop_geometry(x), by.x= "polygon_", by.y="filterField_")
    stations[[allStrat[s]]]$TYPE <- NA
    
    n_prim <- stations[[allStrat[s]]]$PRIMARY[1]
    n_alt  <- stations[[allStrat[s]]]$ALTERNATE[1]
    n_sec  <- stations[[allStrat[s]]]$SECONDARY[1]
    n_tot  <- stations[[allStrat[s]]]$TOT[1]
    
    if (!is.na(n_prim)){
      stations[[allStrat[s]]] <- assignStrata(df= stations[[allStrat[s]]], 
                                              type = "PRIMARY", 
                                              n_sets = n_prim)
    }
    if (!is.na(n_alt)){
      stations[[allStrat[s]]] <- assignStrata(df= stations[[allStrat[s]]], 
                                              type = "ALTERNATE", 
                                              n_sets = n_alt)
    }     
    if (!is.na(n_sec)){
      stations[[allStrat[s]]] <- assignStrata(df= stations[[allStrat[s]]], 
                                              type = "SECONDARY", 
                                              n_sets = n_sec)
    }   
  } 
  stations <- do.call(rbind, stations)
  stations <- sf::st_transform(x = stations, crs = 4326) 
  stations <- cbind(stations, round(sf::st_coordinates(stations$geometry),6))
  colnames(stations)[colnames(stations)=="X"] <- "LON_DD"
  colnames(stations)[colnames(stations)=="Y"] <- "LAT_DD"
  stations$LON_DDMM <- convert.dd.dddd(stations$LON_DD)
  stations$LAT_DDMM <- convert.dd.dddd(stations$LAT_DD)
  stations <- stations[,c("polygon_","TYPE","LON_DD", "LAT_DD", "LON_DDMM", "LAT_DDMM")]
  

  
  # apply station names
  # primaries     1 - 299
  # secondaries 501 - 699
  # alternates  901 - 1099
  stations <- stations %>% 
    dplyr::arrange(TYPE,polygon_) %>% 
    dplyr::group_by(TYPE) %>% 
    dplyr::mutate(LABEL = dplyr::row_number()) %>% 
    dplyr::mutate(LABEL = ifelse(TYPE == "SECONDARY", LABEL+500, 
                          ifelse(TYPE == "ALTERNATE", LABEL+900, LABEL))) %>% 
    dplyr::ungroup()%>% 
    dplyr::arrange(polygon_,LABEL)


  
  timestamp <- format(Sys.time(),"%Y%m%d_%H%M")
  #add original field name back, and delete the one I made

  if (!is.null(MaritimesSurvey)){
    #add dmin dmax from gsstratum
    depths <- RVSurveyData::GSSTRATUM
    stations = merge(stations, depths[,c("STRAT", "DMIN", "DMAX")], by.x = "polygon_", by.y="STRAT")
    colnames(stations)[colnames(stations)=="polygon_"] <- "STRATUM"
  }else{
    stations[, stationData_StratField] <- stations$polygon_
    stations$polygon_<-NULL
  }

  #write files
  sf::st_write(stations, paste0("stations_",timestamp,".shp"), delete_layer = TRUE) 
  xlsx::write.xlsx2(stations , paste0("stations_",timestamp,".xlsx"), sheetName = "stations", col.names = TRUE, row.names = FALSE, append = FALSE)
  message("wrote files to ", getwd())
  # res <- list()
  # 
  # res[["data"]] <- stations
  return(stations)
}

# eastern canyons


