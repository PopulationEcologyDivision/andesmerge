#' @title toOracle
#' @description This function loads any of 6 known data frame objects into Oracle, ensuring that the 
#' records of the new objects will not replace any existing records.
#' @param cxnObj default is \code{NULL}.  This is a connection object from 
#' Mar.utils::make_oracle_cxn(). The account specified by this needs permission to write tables in 
#' the \code{target_schema}.
#' @param source_df default is \code{NULL}.  This is a data frame containing the data that should be 
#' loaded to Oracle.  It should correspond with the structure used by one of ESE_MISSIONS, ESE_SETS, 
#' ESE_CATCHES, ESE_BASKETS, ESE_SPECIMENS, ESE_LV1_OBSERVATIONS
#' @param target_schema default is \code{"groundfish"}.  This is the name of the Oracle schema where the 
#' data stored in\code{source_df} should be loaded. 
#' @param target_table default is \code{NULL}.  This is the name of the specific Oracle table within
#' \code{target_schema} where the data should be loaded 
#' @param createReplaceTarget  default is \code{FALSE}.  By default, new data is appended to tables
#' that already exist within the specified \code{target_schema}.  Setting this to \code{TRUE} will 
#' result in the creation of new tables or the replacement of existing tables within 
#' \code{target_schema}.  Use with caution.  
#' @return nothing, but data gets loaded to the selected location in Oracle.
#' @family general_use
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
toOracle <- function(cxnObj = NULL, source_df= NULL, target_schema = "groundfish", target_table = NULL, createReplaceTarget =FALSE){
  target_table <- toupper(target_table)
  target_schema <- toupper(target_schema)
  doAppend <- TRUE   
  doSafer <- TRUE
  doOverwrite <- FALSE
  
  if (cxnObj$usepkg=='roracle'){
    if (createReplaceTarget){
      doAppend = FALSE
      doOverwrite = TRUE
    }else{
      doAppend = TRUE
      doOverwrite = FALSE
    }
    this = tryCatch(
      {
        ROracle::dbWriteTable(conn = cxnObj$channel, schema=target_schema, value = source_df, 
                              name = target_table, date=TRUE,row.names = FALSE, overwrite = doOverwrite, append = doAppend)
      },
      error=function(cond){
        message(cond)
        return(-1)
      }
    )
  } else if (cxnObj$usepkg=='rodbc'){
    if (createReplaceTarget){
      doSafer = FALSE
    }else{
      doSafer = TRUE
    }
    this = tryCatch(
      {
        RODBC::sqlSave(channel = cxnObj$channel, dat = source_df, 
                       tablename = paste0(target_schema,".",target_table), 
                       safer = TRUE, verbose = T, nastring = NULL,
                       rownames = F)
      },
      error=function(cond){
        message(cond)
        return(-1)
      }
    )
  }
  if (this == -1) message("Could not write ", deparse(substitute(source_df)) ," to ",target_schema,".", target_table)
}

