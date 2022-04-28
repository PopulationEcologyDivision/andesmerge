eseJoiner <- function(eseList = NULL){

  sets <- eseList$ESE_SETS[,!names(eseList$ESE_SETS) %in% c("NOTE")]
  sets <- Mar.utils::drop_cols(sets, justDropNAs=T)
  sets <- Mar.utils::drop_cols(sets, justDropNAs=F, uniformFields = 'drop')
  catches <- merge(eseList$ESE_CATCHES[,!names(eseList$ESE_CATCHES) %in% c("NOTE")], 
                   eseList$ESE_BASKETS[,!names(eseList$ESE_BASKETS) %in% c("NOTE")], all.y="T")
  catches <- Mar.utils::drop_cols(catches, justDropNAs=T)
  catches <- Mar.utils::drop_cols(catches, justDropNAs=F, uniformFields = 'drop')
  #long to wide
  obs <- eseList$ESE_LV1_OBSERVATIONS[,!names(eseList$ESE_LV1_OBSERVATIONS) %in% c("DATA_DESC")]
  obs <-  tidyr::spread(obs, LV1_OBSERVATION, DATA_VALUE, fill=NA, convert=F)
  obs <- Mar.utils::drop_cols(obs, justDropNAs=T)
  obs <- Mar.utils::drop_cols(obs, justDropNAs=F, uniformFields = 'drop')

  res<-list() 
  res[["sets"]] <- sets
  res[["catches"]] <- catches
  res[["specimenObservations"]] <- obs
  return(res)
}