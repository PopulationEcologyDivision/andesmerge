
checkPrimaryFields <- function(primaryTable = NULL){
  #ese objects must have already been loaded locally via eseExtractor()
  
  #the last ese table examined has more 
  if (primaryTable == getEseTables()[length(getEseTables())])return(NULL)
  toCheck <- getEseTables()
  toCheck <- toCheck[toCheck != primaryTable]
  primary_fields <- getKeyFields(primaryTable)
  primary <- getTblKey(get(primaryTable), primary_fields)
  if (primaryTable %in% c("ESE_CATCHES","ESE_BASKETS")) toCheck <- toCheck[!toCheck %in% c("ESE_SPECIMENS", "ESE_LV1_OBSERVATIONS")]
  message(primaryTable, ": Verifying that the other tables only have values for ", paste0(primary_fields, collapse=", ")," that exist in ",primaryTable)

  for (t in 1:length(toCheck)){
    thisTabKeys <- getTblKey(get(toCheck[t]), primary_fields)
    if (all(is.na(thisTabKeys))){
      next
    }else{
      message("\tChecking ", toCheck[t])
    }
    if (length(setdiff(thisTabKeys,primary))>0 || length(setdiff(primary, thisTabKeys))>0){
      if (length(setdiff(thisTabKeys,primary))>0) {
        message("\tThe following values do not exist in ",primaryTable,", but are found in ",toCheck[t],":\n",
        paste0("\t\t",unique(setdiff(thisTabKeys,primary)),collapse=", "))
      }
      if (length(setdiff(primary, thisTabKeys))>0) {
        message("\tThe following values do not exist in ",toCheck[t],", but are found in ",primaryTable,":\n")
        if (primaryTable == "ESE_SETS" && toCheck[t]== "ESE_CATCHES"){
          bad <- get(primaryTable)[paste0(get(primaryTable)[,primary_fields[1]], "_",get(primaryTable)[,primary_fields[2]]) %in% setdiff(primary, thisTabKeys) & "EXPERIMENT_TYPE_CODE" ==1,primary_fields]          # want to check for type 1
          if (nrow(bad)>0){
            badRes <- paste0(bad[,primary_fields[1]], "_",bad[,primary_fields[2]])
            message(paste0("\t\t",unique(badRes),collapse=", ")," (Type 1 set)")
          }
        }else{
        message(paste0("\t\t",unique(setdiff(primary,thisTabKeys)),collapse=", "))
        }
      }
    }
  }
}

# ESE_CATCHES: Verifying that the other tables only have values for MISSION, SETNO, SPEC that exist in ESE_CATCHES
# The following values do not exist in ESE_SPECIMENS, but are found in ESE_CATCHES:
# The following values do not exist in ESE_LV1_OBSERVATIONS, but are found in ESE_CATCHES:

# ESE_BASKETS: Verifying that the other tables only have values for MISSION, SETNO, SPEC, SIZE_CLASS that exist in ESE_BASKETS
# The following values do not exist in ESE_SPECIMENS, but are found in ESE_BASKETS:
# The following values do not exist in ESE_LV1_OBSERVATIONS, but are found in ESE_BASKETS: