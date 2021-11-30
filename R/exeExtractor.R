#' @title eseExtractor
#' @description This function extracts all of the ESE data for a specified MISSION from the 
#' GROUNDFISH schema.
#' @param cxnObj default is \code{NULL}.  This is a connection object from 
#' \code{Mar.utils::make_oracle_cxn()}. 
#' @param mission default is \code{NULL}.  This is a vector of one or more mission identifiers 
#' (e.g "CAR2021240") that will be used to limit the extractions to particular mission(s). 
#' \code{"ALL"} is also valid, and will return all records for all of the specified tables. 
#' Use with caution.
#' @param tabs default is \code{NULL}.  This is a vector the ESE tables that should be 
#' extracted.  By default, all will be extracted, but fewer names can be sent to extract a subset.  
#' Valid values include \code{c("ESE_MISSIONS", "ESE_SETS", "ESE_CATCHES", "ESE_BASKETS", 
#' "ESE_SPECIMENS", "ESE_LV1_OBSERVATIONS")}.
#' @param quiet default is \code{FALSE} Determines whether or not the script should output 
#' informational messages 
#' @param env default is \code{.GlobalEnv} The environment where the data should be loaded
#' @return nothing - just loads data
#' @family general_use
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
eseExtractor <- function(cxnObj = NULL, mission = NULL, tabs = NULL, quiet = FALSE, env = .GlobalEnv){

  if (is.null(tabs)){
    tabs <- getEseTables()
  }else{
    tabs <- toupper(tabs)
  }
  tabsExist <- getEseTables()
  
  mission <- toupper(mission)
  if (is.null(cxnObj)) stop("Can't proceed without valid cxnObj")
  if (is.null(mission)) stop("'mission' cannot be NULL.  Please provide a vector of 1 or more valid missions")
  
  if (length(mission) == 1 && mission == "ALL") {
    tabs <- tabsExist
    whereM <- ""
  }else{
    whereM <- paste0("WHERE MISSION IN (",Mar.utils::SQL_in(mission, apos=T),")")
  }
  #Verify that requested tables exist
  tabsValid <- intersect(tabs,tabsExist)
  if (!quiet && length(tabsValid)<length(tabs)) message("The following requested tables either don't exist, or can't be extracted by this function:\n",paste0(setdiff(tabs,tabsExist), collapse=", "))
  if (length(tabsValid)<1){
    message("No valid tables requested, stoppping")
    return(NULL)
  }

  
  x = list()
  
  for (t in 1:length(tabsValid)){
    qry <- paste0("Select * from GROUNDFISH.",tabsValid[t]," ", whereM)
    x[[tabsValid[t]]]<- cxnObj$thecmd(cxnObj$channel, qry)
    # assign(x = tabsValid[t], value = cxnObj$thecmd(cxnObj$channel, qry), envir = env)
    if (!quiet) message("Extracted ",tabsValid[t]," for mission(s) '",paste0(mission, collapse = "', '"), "'")
  }
  return(x)
}
