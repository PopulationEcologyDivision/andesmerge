#' @title tweakUniversal
#' @description This function will perform tweaks on data from Andes that exists in multiple 
#' exported csv files  
#' @param x default is \code{NULL}.  This is a list of all of the objects from the csv files
#' @param mission default is \code{NULL}. This is the cleaned mission id (e.g. CAR2022102)
#' @return list
#' @family general_use
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
tweakUniversal <- function(x = NULL, mission=NULL){
  theMsg <- NA
  #get rid of text species name for overwritten spp, we will really entirely on the species codes
  x$catch_data$species <- x$catch_data$species_id <- NULL
  if(mission == "CAR2022102"){  
    theMsg <- paste0(theMsg[!is.na(theMsg)], "\tRemapping species in  the catch_data\n")
    x$catch_data[x$catch_data$species_code == 18411,"species_code"] <- 6102
    x$catch_data[x$catch_data$species_code == 13519,"species_code"] <- 4213
    x$catch_data[x$catch_data$species_code == 14878,"species_code"] <- 4508
    x$catch_data[x$catch_data$species_code == 12161,"species_code"] <- 8216
    x$catch_data[x$catch_data$species_code == 19218,"species_code"] <- 9331
    x$catch_data[x$catch_data$species_code == 19950,"species_code"] <- 1510
    x$catch_data[x$catch_data$species_code == 18680,"species_code"] <- 1821
    x$catch_data[x$catch_data$species_code == 12157,"species_code"] <- 8382
    x$catch_data[x$catch_data$species_code == 10854,"species_code"] <- 500
    x$catch_data[x$catch_data$species_code == 12096,"species_code"] <- 8516
    # x$catch_data[x$catch_data$species_code == 18495,"species_code"] <- 6105
    
    theMsg <- paste0(theMsg[!is.na(theMsg)], "\tRemapping species in the specimen_data\n")  
    x$specimen_data[x$specimen_data$species_code == 18411,"species_code"] <- 6102
    x$specimen_data[x$specimen_data$species_code == 13519,"species_code"] <- 4213
    x$specimen_data[x$specimen_data$species_code == 14878,"species_code"] <- 4508
    x$specimen_data[x$specimen_data$species_code == 12161,"species_code"] <- 8216
    x$specimen_data[x$specimen_data$species_code == 19218,"species_code"] <- 9331
    x$specimen_data[x$specimen_data$species_code == 19950,"species_code"] <- 1510
    x$specimen_data[x$specimen_data$species_code == 18680,"species_code"] <- 1821
    x$specimen_data[x$specimen_data$species_code == 12157,"species_code"] <- 8382
    x$specimen_data[x$specimen_data$species_code == 10854,"species_code"] <- 500
    x$specimen_data[x$specimen_data$species_code == 12096,"species_code"] <- 8516
    # x$specimen_data[x$specimen_data$species_code == 18495,"species_code"] <- 6105
    
    theMsg <- paste0(theMsg[!is.na(theMsg)], "\tRemapping species in  the basket_data\n") 
    x$basket_data[x$basket_data$species_code == 18411,"species_code"] <- 6102
    x$basket_data[x$basket_data$species_code == 13519,"species_code"] <- 4213
    x$basket_data[x$basket_data$species_code == 14878,"species_code"] <- 4508
    x$basket_data[x$basket_data$species_code == 12161,"species_code"] <- 8216
    x$basket_data[x$basket_data$species_code == 19218,"species_code"] <- 9331
    x$basket_data[x$basket_data$species_code == 19950,"species_code"] <- 1510
    x$basket_data[x$basket_data$species_code == 18680,"species_code"] <- 1821
    x$basket_data[x$basket_data$species_code == 12157,"species_code"] <- 8382
    x$basket_data[x$basket_data$species_code == 10854,"species_code"] <- 500
    x$basket_data[x$basket_data$species_code == 12096,"species_code"] <- 8516
    # x$basket_data[x$basket_data$species_code == 18495,"species_code"] <- 6105
    
    theMsg <- paste0(theMsg[!is.na(theMsg)], "\tHandling 'spp 18495'.  Basket 1 was spp 6102; Basket 2 was spp 6126 - need an additional catch record\n") 
    x$catch_data<- rbind(x$catch_data, x$catch_data[x$catch_data$species_code==18495,])
    x$catch_data[x$catch_data$species_code==18495,"species_code"] <- c(6102,6126)
    
    x$basket_data<-x$basket_data[order(x$basket_data$id),]
    x$basket_data[x$basket_data$species_code==18495,"species_code"] <- c(6102,6126)
  }
  if (!is.na(theMsg)) message("Universal Tweaks: \n", theMsg)
  return(x)
}

#' @title tweakBaskets
#' @description This function will perform tweaks to data coming from andes before 
#' it is imported to Oracle.  
#' @param x default is \code{NULL}.  This is the ESE_BASKET data frame
#' @return a data frame for loading into 
#' @family general_use
#' @author  Pablo Vergara, \email{Pablo.Vergara@@dfo-mpo.gc.ca}
#' @export
tweakBaskets <- function(x = NULL){
  if(length(unique(x$MISSION)) > 1) stop("The object sent has more than one mission in it, abort")
  theMsg <- NA
  
  if (!is.na(theMsg)) message("Tweaking BASKET: ", theMsg)
  return(x)
}

#' @title tweakCatches
#' @description This function will perform tweaks to data coming from andes before 
#' it is imported to Oracle.  
#' @param x default is \code{NULL}.   This is the ESE_CATCHES data frame
#' @return a data frame for loading into ESE_CATCHES
#' @family general_use
#' @author  Pablo Vergara, \email{Pablo.Vergara@@dfo-mpo.gc.ca}
#' @export
tweakCatches <- function(x = NULL){
  if(length(unique(x$MISSION)) > 1) stop("The object sent has more than one mission in it, abort")
  theMsg <- NA
  
  if (!is.na(theMsg)) message("Tweaking CATCH: ", theMsg)
  return(x)
}

#' @title tweakSpecimensRaw
#' @description This function will perform tweaks to data coming from andes before 
#' it is imported to Oracle.  specimen_data gets converted into both ESE_SPECIMENS and 
#' ESE_LV1_OBSERVATIONS, so many tweaks should be performed here to ensure that changes are 
#' reflected in both output tables.  Also, changing here will ensure sequential LV1_OBSERVATION_ID 
#' values 
#' @param x default is \code{NULL}. This is the raw "specimen_data" data frame
#' @return a data frame for loading into ESE_BASKETS
#' @family general_use
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
tweakSpecimensRaw <- function(x = NULL){
  if(length(unique(x$MISSION)) > 1) stop("The object sent has more than one mission in it, abort")
  theMsg <- NA
  browser()
  # if(x$MISSION[1] == "CAR2022102"){
  #   theMsg <- paste0(theMsg[!is.na(theMsg)], "\tRemoving Fish Numbers from Mackerel records\n")
  #   x <- x[!(x$SPEC == 70 & x$LV1_OBSERVATION == "Fish Number"),]
  #   
  #   theMsg <- paste0(theMsg[!is.na(theMsg)], "\tRemoving comments created solely to generate labels\n")
  #   commentsDrop <- c(".", "1", "edrftgujik", "fdgj", "fgjh", "rdfyg", "srdtfygvuh")
  #   x <- x[-which(x$LV1_OBSERVATION == "Comments" & x$DATA_VALUE %in% commentsDrop),]
  #   
  #   
  #   warning("tweaks at this level will change the LV1_OBSERVATION_ID")
  # }
  # Set 44, spec 1191, specimen id 14496 – length entered as 0.23
  # o This appears to be an error entry and should be deleted. (Basket weight for 1191 in this set 
  #is approx. the expected weight for the 34 cm skate entered)
  # Set 26, spec 14, specimen id 9332 – weight entered as 0.005
  # o Weight in kg but weight should be in g. Change to 5
  # browser()
  # x[x$SPECIMEN_ID==14496,]
  warning("implement these!")
  if (!is.na(theMsg)) message("Tweaking specimen_data: ", theMsg)
  return(x)
}

#' @title tweakSpecimens
#' @description This function will perform tweaks to data coming from andes before 
#' it is imported to Oracle.  
#' @param x default is \code{NULL}. This is the ESE_BASKETS data frame
#' @return a data frame for loading into ESE_BASKETS
#' @family general_use
#' @author  Pablo Vergara, \email{Pablo.Vergara@@dfo-mpo.gc.ca}
#' @export
tweakSpecimens <- function(x = NULL){
  if(length(unique(x$MISSION)) > 1) stop("The object sent has more than one mission in it, abort")
  theMsg <- NA
  
  
  if (!is.na(theMsg)) message("Tweaking SPECIMENS: ", theMsg)
  return(x)
}

#' @title tweakLv1
#' @description This function will perform tweaks to data coming from andes before 
#' it is imported to Oracle.  
#' @param x default is \code{NULL}.  This is the ESE_LV1_OBSERVATIONS data frame.
#' @return a data frame for loading into ESE_LV1_OBSERVATIONS
#' @family general_use
#' @author  Pablo Vergara, \email{Pablo.Vergara@@dfo-mpo.gc.ca}
#' @export
tweakLv1 <- function(x = NULL){
  if(length(unique(x$MISSION)) > 1) stop("The object sent has more than one mission in it, abort")
  theMsg <- NA
  
  if(x$MISSION[1] == "CAR2022102"){
    theMsg <- paste0(theMsg[!is.na(theMsg)], "\tRemoving Fish Numbers from Mackerel records\n")
    x <- x[!(x$SPEC == 70 & x$LV1_OBSERVATION == "Fish Number"),]
    
    theMsg <- paste0(theMsg[!is.na(theMsg)], "\tRemoving comments created solely to generate labels\n")
    commentsDrop <- c(".", "1", "edrftgujik", "fdgj", "fgjh", "rdfyg", "srdtfygvuh")
    x <- x[-which(x$LV1_OBSERVATION == "Comments" & x$DATA_VALUE %in% commentsDrop),]
    

    warning("tweaks at this level will change the LV1_OBSERVATION_ID")
  }
  
  if (!is.na(theMsg)) message("Tweaking LV1: \n", theMsg,"\n")
  return(x)
}

#' @title tweakSets
#' @description This function will perform tweaks to data coming from andes before 
#' it is imported to Oracle.  
#' @param x default is \code{NULL}.  This is the ESE_SETS data frame.
#' @return a data frame for loading into ESE_SETS
#' @family general_use
#' @author  Pablo Vergara, \email{Pablo.Vergara@@dfo-mpo.gc.ca}
#' @export
tweakSets <- function(x = NULL){
  if(length(unique(x$MISSION)) > 1) stop("The object sent has more than one mission in it, abort")
  theMsg <- NA
  
  if(x$MISSION[1] == "CAR2022102"){
    theMsg <- paste0(theMsg[!is.na(theMsg)], "\tRecoding gear from 23 to 15\n")
    #NEST trawl is not 23, but 15 - change here; expect this will be corrected in the future
    x[x$GEAR==23,"GEAR"] <- 15
    
    theMsg <- paste0(theMsg[!is.na(theMsg)], "\tSet 16: Correct tow distance\n")
    x[x$SETNO==16,"DIST "] <- 0.978
    
    theMsg <- paste0(theMsg[!is.na(theMsg)], "\tSet 48: Correct ELONG no -ve\n")
    x[x$SETNO==48,"ELONG"] <- as.numeric(x[x$SETNO==48,"ELONG"])*-1
    
    warning("Set 4: end longitude coordinates are 0 (both DD and MMmm)")
    # Set 9 – End time shows 1741 but note indicates that it should be 1739
    # Set 41 – End time shows 0056 but note indicates that it should be 0054
    
    theMsg <- paste0(theMsg[!is.na(theMsg)], "\tSets 9 & 41: Correct ETIME\n")
    x[x$SETNO==9,"END_TIME"] <- 1739
    x[x$SETNO==41,"END_TIME"] <- 0054
  }
  
  if (!is.na(theMsg)) message("Tweaking SETS: \n", theMsg)
  return(x)
}

