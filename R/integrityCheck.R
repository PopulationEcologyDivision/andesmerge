#' @title integrityCheck
#' @description This function verifies that key fields within one ESE table exist in the other 
#' tables in the ESE.  For example, if a SETNO exists in ESE_CATCHES that does not exist in ESE_SETS,
#' a message would be output.
#' @return nothing - just outputs messages
#' @family general_use
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
integrityCheck <- function(cxnObj = NULL, mission = NULL, type1Only =T){
  parentlessChildren <- function(parent = NULL, parentKey = NULL, child = NULL, childKey = NULL, keyField = NULL){
    res <- setdiff(childKey,parentKey)
    if (length(res)==0){
      message("\tAll values of ",paste0(keyField, collapse=","), " that exist in ", child," can be found in ",parent)
    }else{
      message("\tPC The following combinations of ",paste0(keyField, collapse="_")," exist in ", child,", but NOT in ",parent)
      paste0("\t\t",unique(res),collapse=", ")
    }
  }
  setTypeCheck <- function(setTable=NULL){
    res<- setTable[which(setTable$EXPERIMENT_TYPE_CODE != 1),c("MISSION", "SETNO")]
    if(nrow(res)>0){
      res <- paste0(res$MISSION, "_",res$SETNO)
      message("ESE_SETS includes set(s) other than EXPERIMENT_TYPE_CODE  = 1. To avoid false positives, other tables will NOT report when these sets are missing") 
    }else{
      res <- NA
    }
    return(res)
  }
  
  qcEnv <- new.env()
  eseExtractor(cxnObj = cxnObj, mission=mission, env = qcEnv)
  toCheck <- getEseTables()
  
  if (type1Only){
    notableSets <- setTypeCheck(setTable = qcEnv$ESE_SETS)
    for (i in 1:length(toCheck)){
      if (all(c("MISSION","SETNO") %in% names(qcEnv[[toCheck[i]]]))) {
        qcEnv[[toCheck[i]]] <- qcEnv[[toCheck[i]]][which(!paste0(qcEnv[[toCheck[i]]]$MISSION,"_",qcEnv[[toCheck[i]]]$SETNO) %in% notableSets),]
      }
    }
  }
  
  for (p in 1:length(toCheck)){
    parentTabName <- toCheck[p]
    parentTabFields <- getKeyFields(parentTabName)
    parentTab <- qcEnv[[parentTabName]]
    message("\t",parentTabName)
    childTabNames <- setdiff(toCheck, parentTabName)
    for (c in 1:length(childTabNames)){
      childTabName <- childTabNames[c]
      childTabFields <- getKeyFields(childTabName)
      childTab <- qcEnv[[childTabName]]
      theMessage <- paste0("\t\t", childTabName)
      compareFields <- intersect(childTabFields, parentTabFields)
      
      ttt<-dplyr::setdiff(parentTab[,compareFields],childTab[,compareFields])
      if(length(ttt)>0){
        f<-merge(parentTab, ttt, by=compareFields, all.y=T)
        if (nrow(f)>0){
          message(theMessage)
          print(head(f,20))
        }else{
          message(theMessage, " - checked")
        }
      }
    }
  }
}  