#' @title toOracle
#' @description This function loads any of 6 known data frame objects into Oracle, ensuring that the 
#' records of the new objects will not replace any existing records.
#' @param cxnObj default is \code{NULL}.  This is a connection object from Mar.utils::make_oracle_cxn(). 
#' @param source_df default is \code{NULL}.  This is a data frame containing the data that should be 
#' loaded to Oracle.  It should correspond with the structure used by one of ESE_MISSIONS, ESE_SETS, 
#' ESE_CATCHES, ESE_BASKETS, ESE_SPECIMENS, ESE_LV1_OBSERVATIONS
#' @param target_schema default is \code{NULL}.  This is the name of the Oracle schema where the 
#' data stored in\code{source_df} should be loaded
#' @param target_table default is \code{NULL}.  This is the name of the specific Oracle table within
#' \code{target_schema} where the data should be loaded 
#' @param createReplaceTarget  default is \code{FALSE}.  By default, new data is appended to tables
#' that already exist within the specified \code{target_schema}.  Setting this to \code{TRUE} will 
#' result in the creation of new tables or the replacement of existing tables within 
#' \code{target_schema}.  Use with caution.  
#' @return 
#' @family general_use
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
toOracle <- function(cxnObj = NULL, source_df= NULL, target_schema = NULL, target_table = NULL, createReplaceTarget =FALSE){
  target_table <- toupper(target_table)
  target_schema <- toupper(target_schema)
  keyFields <- getKeyFields(target_table)
  doAppend <- TRUE   
  doOverwrite <- FALSE
  if (is.null(keyFields))stop("Target table not a recognized target")
  
  if (!createReplaceTarget){
    ##### Check if records already exist in target table
    #create a query that will do the same of the db values.  Don't run till we know connection type
    recsQuery <- paste0("SELECT DISTINCT ",paste0(keyFields, collapse = ", ")," FROM ",target_schema,".",target_table)
    newrecsKey <- getTblKey(df = source_df, keyFields = keyFields)
    data_target <- cxnObj$thecmd(cxnObj$channel,  recsQuery)
    if (class(data_target)== "data.frame"){
      oldrec_key <- getTblKey(df=data_target, keyFields = keyFields)
      if (any(newrecsKey %in% oldrec_key)) stop("Some submitted records already exist in the target table")
    }else if (grepl("does not exist", x = data_target[1], ignore.case = T)){
      #the table does not exist and will be created
    }
    ##### existing record check over
  }else{
    if (target_schema == "GROUNDFISH"){
      message("ALERT! You have chosen to REPLACE (i.e. not append) ",target_table," - an ESE table within the GROUNDFISH schema.
Are you certain you want to do this?\n\n")
      PROCEED = toupper(readline(prompt = paste0("Please re-type '",target_table,"' to proceed (no quotes), or anything else to cancel:  ")))
      print(PROCEED)
      if (PROCEED != target_table) {
        stop("Cancelled")
      }
    }
    doAppend = FALSE
    doOverwrite = TRUE
  }
  if (cxnObj$usepkg=='roracle'){
    this = tryCatch(
      {
        ROracle::dbWriteTable(conn = cxnObj$channel, schema=target_schema, value = source_df, 
                              name = target_table, date=TRUE,row.names = FALSE, overwrite = doOverwrite, append = doAppend)
      },
      error=function(cond){
        return(-1)
      }
    )
  } else if (cxnObj$usepkg=='rodbc'){
    target_table <- paste0(target_schema,".",target_table)
    this = tryCatch(
      {
        RODBC::sqlSave(channel = cxnObj$channel, dat = source_df, tablename = target_table, 
                       colnames = FALSE, rownames = FALSE, append = doAppend)
      },
      error=function(cond){
        return(-1)
      }
    )
  }
  if (this == -1) message("Could not write ", deparse(substitute(source_df)) ," to ",target_schema,".", target_table)
}