#' @title eseExtractor
#' @description This function extracts all of the ESE data for a specified MISSION from the 
#' GROUNDFISH schema.
#' @param cxnObj default is \code{NULL}.  This is a connection object from 
#' \code{Mar.utils::make_oracle_cxn()}. 
#' @param mission default is \code{NULL}.  This is a vector of one or more mission identifiers 
#' (e.g "CAR2021240") that will be used to limit the extractions to particular Mission(s). 
#' @param quiet default is \code{FALSE} Determines whether or not the script should output 
#' informational messages 
#' @return nothing - just loads data to environment
#' @family general_use
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
eseExtractor <- function(cxnObj = NULL, mission = NULL, quiet = FALSE){
  if (is.null(cxnObj))stop("Can't proceed without valid cxnObj")
  tabs <- c("ESE_MISSIONS", "ESE_SETS", "ESE_CATCHES", "ESE_BASKETS", "ESE_SPECIMENS", "ESE_LV1_OBSERVATIONS")
  for (t in 1:length(tabs)){
    qry <- paste0("Select * from GROUNDFISH.",tabs[t]," WHERE MISSION IN (",Mar.utils::SQL_in(mission, apos=T),")")
    assign(x = tabs[t], value = cxnObj$thecmd(cxnObj$channel, qry), envir = .GlobalEnv)
    if (!quiet) message("Loaded ",tabs[t]," for mission(s) '",paste0(mission, collapse = "', '"), "' into the local environment")
  }
}