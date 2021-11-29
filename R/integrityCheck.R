#' @title integrityCheck
#' @description This function verifies that key fields within one ESE table exist in the other 
#' tables in the ESE.  For example, if a SETNO exists in ESE_CATCHES that does not exist in ESE_SETS,
#' a message would be output.
#' @param cxnObj default is \code{NULL}.  This is a connection object from 
#' \code{Mar.utils::make_oracle_cxn()}. 
#' @param mission default is \code{NULL}.  This is a vector of one or more mission identifiers 
#' (e.g "CAR2021240") that will be used to limit the extractions to particular mission(s). 
#' \code{"ALL"} is also valid, and will return all records for all of the specified tables. 
#' Use with caution.
#' @param type1Only default is \code{FALSE}. The presence of failed tows can result in a cascade of 
#' 'missing' records in other tables. Setting this to \code{TRUE} will output which sets are not 
#' type 1, but will then drop them from all other tables.
#' @param debug default is \code{FALSE}.  When TRUE, this captures the ESE output locally, allowing 
#' multiple runs without having to re-extract the data
#' @param addErrors default is \code{TRUE}.  When \code{TRUE}, this modifies some SETNO, SPEC and 
#' SIZE_CLASS to review how such changes will be handles by the function.  It will only work if 
#' \code{debug = T}
#' @return nothing - just outputs messages
#' @family qc
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
integrityCheck <- function(cxnObj = NULL, mission = NULL, type1Only =FALSE, debug =FALSE, addErrors=FALSE){
  setTypeCheck <- function(setTable=NULL){
    typeXSets<- setTable[which(is.na(setTable$EXPERIMENT_TYPE_CODE) | setTable$EXPERIMENT_TYPE_CODE != 1),]
    typeXSets <- typeXSets[,c("MISSION", "SETNO", "START_DATE", "START_TIME", "END_DATE", "END_TIME", "STRAT", "SLAT", "SLONG", "ELAT", "ELONG", "DIST", "EXPERIMENT_TYPE_CODE", "GEAR","NOTE")]
    if(nrow(typeXSets)>0){
      res <- paste0(typeXSets$MISSION, "_",typeXSets$SETNO)
      message("ESE_SETS includes the following set(s) with EXPERIMENT_TYPE_CODE  != 1. 
To avoid false positives, other tables will NOT report when these sets are missing") 
      print(typeXSets)
    }else{
      res <- NA
    }
    return(res)
  }
  
  if (!exists("qcEnv", envir = .GlobalEnv)){
    # if debug is True, don't re-extract ese if they are available.
    qcEnv <- new.env()
    eseExtractor(cxnObj = cxnObj, mission=mission, env = qcEnv)
    if (debug) {
      .GlobalEnv$qcEnv<-rlang::env_clone(qcEnv)
      message("Saved copy of extracted ESE data for future debugging")
    }
  }else{
    if (debug){
      qcEnv<-rlang::env_clone(.GlobalEnv, env = qcEnv)
      message("Loaded old copies of ESE data")
    } 
  }
  
  toCheck <- getEseTables()
  
  if (type1Only){
    #drop all cases where non-type1 sets are referenced in other tables
    notableSets <- setTypeCheck(setTable = qcEnv$ESE_SETS)
    for (i in 1:length(toCheck)){
      if (all(c("MISSION","SETNO") %in% names(qcEnv[[toCheck[i]]]))) {
        qcEnv[[toCheck[i]]] <- qcEnv[[toCheck[i]]][which(!paste0(qcEnv[[toCheck[i]]]$MISSION,"_",qcEnv[[toCheck[i]]]$SETNO) %in% notableSets),]
      }
    }
  }
  if (debug && addErrors){
    #add errors
    qcEnv$ESE_CATCHES[qcEnv$ESE_CATCHES$SETNO %in% c(15),"SETNO"]<- 115
    qcEnv$ESE_BASKETS[qcEnv$ESE_BASKETS$SPEC %in% c(11),"SPEC"]<- 12345
    qcEnv$ESE_SPECIMENS[qcEnv$ESE_SPECIMENS$SIZE_CLASS %in% c(2),"SIZE_CLASS"]<- 18
  }
  for (p in 1:length(toCheck)){
    parentTabName <- toCheck[p]
    parentTabFields <- getKeyFields(parentTabName)
    parentTab <- qcEnv[[parentTabName]]
    message(parentTabName)
    childTabNames <- setdiff(toCheck, parentTabName)
    for (c in 1:length(childTabNames)){
      childTabName <- childTabNames[c]
      childTabFields <- getKeyFields(childTabName)
      childTab <- qcEnv[[childTabName]]
      compareFields <- intersect(childTabFields, parentTabFields)
      if ((parentTabName == "ESE_SPECIMENS" & childTabName %in% c("ESE_CATCHES","ESE_BASKETS")) |
          (parentTabName == "ESE_LV1_OBSERVATIONS" & childTabName %in% c("ESE_CATCHES","ESE_BASKETS"))
      ){
        message("\tvs ", childTabName,": (Skipping check on ",paste0(compareFields, collapse=","),")")
        next
      }
      ttt<-dplyr::setdiff(childTab[,compareFields],parentTab[,compareFields])
      if(length(ttt)>0){
        f<-merge(parentTab, ttt, by=compareFields, all.y=T)
        if (nrow(f)>0){
          message("\tvs ", childTabName,": The following records can not be linked on ",paste0(compareFields, collapse=","),": ")
          if (debug){
            print(utils::head(f,5))
          }else{
            print(f)
          }
        }else{
          message("\tvs ", childTabName,": All records can be linked on ",paste0(compareFields, collapse=","))
        }
      }
    }
  }
}  