#' @title tweakUniversal
#' @description This function will perform tweaks on data from Andes that exists in multiple 
#' exported csv files  
#' @param x default is \code{NULL}.  This is a list of all of the objects from the csv files
#' @return list
#' @family general_use
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
tweakUniversal <- function(x = NULL){

  # mission = x$cruise_data$MISSION[1]
  # 
  # if (x$cruise_data$MISSION[1] == "CAR2022102"){
  #     message("remapping species")
  #     x$catch_data[x$catch_data$species_code == 18411,"species_code"] <- 6102
  #     x$catch_data[x$catch_data$species_code == 13519,"species_code"] <- 4213
  #     x$catch_data[x$catch_data$species_code == 14878,"species_code"] <- 4508
  #     x$catch_data[x$catch_data$species_code == 12161,"species_code"] <- 8216
  #     x$catch_data[x$catch_data$species_code == 19218,"species_code"] <- 9331
  #     x$catch_data[x$catch_data$species_code == 19950,"species_code"] <- 1510
  #     x$catch_data[x$catch_data$species_code == 18680,"species_code"] <- 1821
  #     x$catch_data[x$catch_data$species_code == 12157,"species_code"] <- 8382
  #     x$catch_data[x$catch_data$species_code == 10854,"species_code"] <- 500
  #     # x$catch_data[x$catch_data$species_code == 18495,"species_code"] <- 6105
  #     x$catch_data[x$catch_data$species_code == 12096,"species_code"] <- 8516
  #     #get rid of text species name for overwritten spp, since they're known to be wrong and we 
  #     #should never accidentally use them
  #     x$catch_data$species <- x$catch_data$species_id <- NULL
  #     
  #     x$specimen_data[x$specimen_data$species_code == 18411,"species_code"] <- 6102
  #     x$specimen_data[x$specimen_data$species_code == 13519,"species_code"] <- 4213
  #     x$specimen_data[x$specimen_data$species_code == 14878,"species_code"] <- 4508
  #     x$specimen_data[x$specimen_data$species_code == 12161,"species_code"] <- 8216
  #     x$specimen_data[x$specimen_data$species_code == 19218,"species_code"] <- 9331
  #     x$specimen_data[x$specimen_data$species_code == 19950,"species_code"] <- 1510
  #     x$specimen_data[x$specimen_data$species_code == 18680,"species_code"] <- 1821
  #     x$specimen_data[x$specimen_data$species_code == 12157,"species_code"] <- 8382
  #     x$specimen_data[x$specimen_data$species_code == 10854,"species_code"] <- 500
  #     # x$specimen_data[x$specimen_data$species_code == 18495,"species_code"] <- 6105
  #     x$specimen_data[x$specimen_data$species_code == 12096,"species_code"] <- 8516
  # 
  #     x$basket_data[x$basket_data$species_code == 18411,"species_code"] <- 6102
  #     x$basket_data[x$basket_data$species_code == 13519,"species_code"] <- 4213
  #     x$basket_data[x$basket_data$species_code == 14878,"species_code"] <- 4508
  #     x$basket_data[x$basket_data$species_code == 12161,"species_code"] <- 8216
  #     x$basket_data[x$basket_data$species_code == 19218,"species_code"] <- 9331
  #     x$basket_data[x$basket_data$species_code == 19950,"species_code"] <- 1510
  #     x$basket_data[x$basket_data$species_code == 18680,"species_code"] <- 1821
  #     x$basket_data[x$basket_data$species_code == 12157,"species_code"] <- 8382
  #     x$basket_data[x$basket_data$species_code == 10854,"species_code"] <- 500
  #     # x$basket_data[x$basket_data$species_code == 18495,"species_code"] <- 6105
  #     x$basket_data[x$basket_data$species_code == 12096,"species_code"] <- 8516
  # }
  # 
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
  mission = x$MISSION[1]
  
  # Tweaks FOR 2021 data on the Cartier
  if(mission == "CJC2021222"){
    message("remapping species")
    index = x$SIZE_CLASS == 3 & x$SPEC == 23
    x[index,]$SIZE_CLASS = 2
  }

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
  mission = x$MISSION[1]
  
  # if(mission == "CJC2021222"){
  #   
  # }
  
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
  mission = x$MISSION[1]
  
  # if(mission == "CJC2021222"){
  #   
  # }

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
  mission = x$MISSION[1]
  
  # if(mission == "CJC2021222"){
  #   
  # }
  
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
  mission = x$MISSION[1]
  
  if(mission == "CAR2022102"){
    message("recoding gear from 23 to 15")
    #NEST trawl is not 23, but 15 - change here; expect this will be corrected in the future
    x[x$GEAR==23,"GEAR"] <- 15
  }

  return(x)
}

