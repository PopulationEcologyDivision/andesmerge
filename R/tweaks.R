#' @title basketTweaks
#' @description This function will perform tweaks to data coming from ANdes before 
#' it is imported to Oracle.  
#' @param quiet default is \code{FALSE} Determines whether or not the script should output 
#' informational messages 
#' @param x default is \code{NULL}.  Basket data frame
#' @return a basket data frame
#' @family general_use
#' @author  Pablo Vergara, \email{Pablo.Vergara@@dfo-mpo.gc.ca}
#' @export
basketTweaks <- function(x = NULL, quiet = FALSE){
  tweaksPerformedOn = NA
  if(length(unique(x$MISSION)) > 1) stop("The object sent has more than one mission in it, abort")
  mission = x$MISSION[1]
  
  # Tweaks FOR 2021 data on the Cartier
  if(mission == "CJC2021222"){
    if (!quiet) message("Attempting to perform tweaks on Basket data for mission  ", mission)
    index = x$SIZE_CLASS == 3 & x$SPEC == 23
    x[index,]$SIZE_CLASS = 2
    tweaksPerformedOn = mission
  }

  if (!quiet & !is.na(tweaksPerformedOn)) message("Tweaks were performed on Basket data for mission ", tweaksPerformedOn)
  return(x)
}

#' @title catchTweaks
#' @description This function will perform tweaks to data coming from ANdes before 
#' it is imported to Oracle.  
#' @param quiet default is \code{FALSE} Determines whether or not the script should output 
#' informational messages 
#' @param x default is \code{NULL}.  catch data frame
#' @return a catch data frame
#' @family general_use
#' @author  Pablo Vergara, \email{Pablo.Vergara@@dfo-mpo.gc.ca}
#' @export
catchTweaks <- function(x = NULL, quiet = FALSE){
  tweaksPerformedOn = NA
  if(length(unique(x$MISSION)) > 1) stop("The object sent has more than one mission in it, abort")
  mission = x$MISSION[1]
  
  # if(mission == "CJC2021222"){
  #   if (!quiet) message("Attempting to perform tweaks on Basket data for mission  ", mission)
  #   tweaksPerformedOn = mission
  # }
  
  if (!quiet & !is.na(tweaksPerformedOn)) message("Tweaks were performed on Catch data for mission ", tweaksPerformedOn)
  return(x)
}

#' @title specimenTweaks
#' @description This function will perform tweaks to data coming from ANdes before 
#' it is imported to Oracle.  
#' @param quiet default is \code{FALSE} Determines whether or not the script should output 
#' informational messages 
#' @param x default is \code{NULL}.  Specimen data frame
#' @return a Specimen data frame
#' @family general_use
#' @author  Pablo Vergara, \email{Pablo.Vergara@@dfo-mpo.gc.ca}
#' @export
specimenTweaks <- function(x = NULL, quiet = FALSE){
  tweaksPerformedOn = NA
  if(length(unique(x$MISSION)) > 1) stop("The object sent has more than one mission in it, abort")
  mission = x$MISSION[1]
  
  # if(mission == "CJC2021222"){
  #   if (!quiet) message("Attempting to perform tweaks on Specimen data for mission  ", mission)
  #   tweaksPerformedOn = mission
  # }

  if (!quiet & !is.na(tweaksPerformedOn)) message("Tweaks were performed on Specimen data for mission ", tweaksPerformedOn)
  return(x)
}

#' @title setTweaks
#' @description This function will perform tweaks to data coming from ANdes before 
#' it is imported to Oracle.  
#' @param quiet default is \code{FALSE} Determines whether or not the script should output 
#' informational messages 
#' @param x default is \code{NULL}.  Set data frame
#' @return a  Set data frame
#' @family general_use
#' @author  Pablo Vergara, \email{Pablo.Vergara@@dfo-mpo.gc.ca}
#' @export
setTweaks <- function(x = NULL, quiet = FALSE){
  tweaksPerformedOn = NA
  if(length(unique(x$MISSION)) > 1) stop("The object sent has more than one mission in it, abort")
  mission = x$MISSION[1]
  
  if(mission == "CAR2022102"){
    #NEST trawl is not 23, but 15 - change here; expect this will be corrected in the future
    x[x$GEAR==23,"GEAR"] <- 15
    tweaksPerformedOn = mission
  }

  if (!quiet & !is.na(tweaksPerformedOn)) message("Tweaks were performed on Set data for mission ", tweaksPerformedOn)
  return(x)
}

