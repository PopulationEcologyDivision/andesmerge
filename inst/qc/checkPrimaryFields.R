#' @title checkPrimaryFields
#' @description This function verifies that key fields within one ESE table exist in the other 
#' tables in the ESE.  For example, if a SETNO exists in ESE_CATCHES that does not exist in ESE_SETS,
#' a message would be output.
#' @param primaryTable default is \code{NULL}.  This is a connection object from 
#' @return nothing - just outputs messages
#' @family general_use
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
checkPrimaryFields <- function(primaryTable = NULL){
  if (primaryTable == "ESE_LV1_OBSERVATIONS")return(NULL)
  message("Checking ",primaryTable)
  #ese objects must have already been loaded locally via eseExtractor()
  childlessParents <- function(parent = NULL, parentKey = NULL, child = NULL, childKey = NULL, keyField = NULL){
    res <- setdiff(parentKey, childKey)
    if (length(res)==0){
      message("\tAll values of ",paste0(keyField, collapse=","), " that exist in ", parent," can be found in ",child)
    }else{
      browser()
      message("\tCP The following combinations of ",paste0(keyField, collapse="_")," exist in ", parent,", but NOT in ",child)
      print(utils::head(res))
    }
  }
  parentlessChildren <- function(parent = NULL, parentKey = NULL, child = NULL, childKey = NULL, keyField = NULL){
    res <- setdiff(childKey,parentKey)
    if (length(res)==0){
      message("\tAll values of ",paste0(keyField, collapse=","), " that exist in ", child," can be found in ",parent)
    }else{
      
      browser()
      message("\tPC The following combinations of ",paste0(keyField, collapse="_")," exist in ", child,", but NOT in ",parent)
      paste0("\t\t",unique(res),collapse=", ")
    }
  }
  setTypeCheck <- function(setTable=NULL){
    res<- setTable[which(setTable$EXPERIMENT_TYPE_CODE != 1),c("MISSION", "SETNO")]
    if(nrow(res)>0){
      res <- paste0(res$MISSION, "_",res$SETNO)
    }else{
      res <- NA
    }
    return(res)
  }
  toCheck <- getEseTables()
  notableSets <- setTypeCheck(setTable = ESE_SETS)
  if (!all(is.na(notableSets))){
    message("ESE_SETS includes set(s) other than EXPERIMENT_TYPE_CODE  = 1. To avoid false positives, other tables will NOT report when these sets are missing") 
  }
  toCheck <- toCheck[toCheck != primaryTable]
  primary_fields <- getKeyFields(primaryTable)
  primary <- getTblKey(get(primaryTable), primary_fields)
  
  for (t in 1:length(toCheck)){
    thisTabName <- toCheck[t]
    thisTab <- get(toCheck[t])
    thisTabKeys <- getTblKey(thisTab, primary_fields)
    if (all(is.na(thisTabKeys))){
      next
    }
    message("\t",thisTabName,":")
    if (thisTabName != "ESE_MISSIONS"){
      if (!all(is.na(notableSets))) {
        thisTab <- thisTab[!paste0(thisTab$MISSION,"_",thisTab$SETNO) %in% notableSets,]
        }
    }
    parentlessChildren(parent=primaryTable, parentKey = primary, child = toCheck[t], childKey = thisTabKeys, keyField = primary_fields)
    if (!thisTabName %in% c("ESE_SPECIMENS", "ESE_LV1_OBSERVATIONS")){
      childlessParents(parent=primaryTable,parentKey = primary, child = toCheck[t], childKey = thisTabKeys, keyField = primary_fields)
    }else{
      message("\t<Records in ", toCheck[t], " do not imply equivalent records in ",primaryTable,">")
    }
  }
  message("\n")
}