#' @title integrityCheck
#' @description This function verifies that key fields within one ESE table exist in the other 
#' tables in the ESE.  For example, if a SETNO exists in ESE_CATCHES that does not exist in ESE_SETS,
#' a message would be output.
#' @param eseList default is \code{NULL}. This is the output object from either \code{eseExtractor()}
#' or \code{matchAndesToESE}.
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
integrityCheck <- function(eseList = NULL, type1Only =FALSE, debug =FALSE, addErrors=FALSE){
  setTypeCheck <- function(setTable=NULL){
    typeXSets<- setTable[which(is.na(setTable$EXPERIMENT_TYPE_CODE) | setTable$EXPERIMENT_TYPE_CODE != 1),]
    typeXSets <- typeXSets[,c("MISSION", "SETNO", "START_DATE", "STRAT", "SLAT", "SLONG", "DIST", "EXPERIMENT_TYPE_CODE", "GEAR","NOTE")]
    typeXSets$NOTE <- paste0(substr(typeXSets$NOTE,1, 50),"...")
    typeXSets = typeXSets[with(typeXSets, order(MISSION, SETNO)), ]
    # "START_TIME", "END_DATE", "END_TIME", "ELAT", "ELONG", 
    if(nrow(typeXSets)>0){
      res <- paste0(typeXSets$MISSION, "_",typeXSets$SETNO)
      msg_text <- "Note that ESE_SETS includes the following set(s) with EXPERIMENT_TYPE_CODE  != 1."
      if(type1Only){
        msg_text <- paste0(msg_text, "\nBecause 'type1Only = TRUE', references to these sets will be ignored by the other tables)")
      } else{
        msg_text <- paste0(msg_text, "\nYou can set 'type1Only = TRUE' to avoid seeing references to these non-type 1 sets.") 
      }
      message(msg_text)
      print(typeXSets)
    }else{
      res <- NA
    }
    return(res)
  }
  
  if (!exists("qcEnv", envir = .GlobalEnv)){
    # if debug is True, don't re-extract ese if they are available.
    qcEnv <- new.env()
    # if(localFiles){
    qcEnv$ESE_BASKETS <- eseList$ESE_BASKETS
    qcEnv$ESE_CATCHES <- eseList$ESE_CATCHES
    qcEnv$ESE_LV1_OBSERVATIONS <- eseList$ESE_LV1_OBSERVATIONS
    qcEnv$ESE_MISSIONS <- eseList$ESE_MISSIONS
    qcEnv$ESE_SETS <- eseList$ESE_SETS
    qcEnv$ESE_SPECIMENS <- eseList$ESE_SPECIMENS
    # }else{
    #   eseExtractor(cxnObj = cxnObj, mission=mission, env = qcEnv)
    # }
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
  
  
  
  notableSets <- setTypeCheck(setTable = qcEnv$ESE_SETS)
  if (type1Only){
    #drop all cases where non-type1 sets are referenced in other tables
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
        message("\t", childTabName,": (Skipping check against ",parentTabName," on ",paste0(compareFields, collapse=","),")")
        next
      }
      if(nrow(childTab[,compareFields, drop=F])<1){
        message(childTabName," had no usable data")
        browser()
        next
      }
      if(nrow(parentTab[,compareFields, drop=F])<1){
        message(parentTab," had no usable data")
        browser()
        next
      }
      ttt<-dplyr::setdiff(childTab[,compareFields, drop=F],parentTab[,compareFields, drop=F])
      if(length(ttt)>0){
        
        f<-merge(parentTab, ttt, by=compareFields, all.y=T)
        if (nrow(f)>0){
          f = f[with(f, order(MISSION, SETNO)), ]
          message("\t", childTabName,": The following records can not be linked to ",parentTabName," on ",paste0(compareFields, collapse=","),": ")
          if (debug){
            print(utils::head(f,5))
          }else{
            print(f)
          }
        }else{
          message("\t", childTabName,": All records can be linked to ",parentTabName," on ",paste0(compareFields, collapse=","))
        }
      }
    }
  }
}  