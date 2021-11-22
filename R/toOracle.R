#' @title toOracle
#' @description This function loads any of 6 known data frame objects into Oracle, ensuring that the 
#' records of the new objects will not replace any existing records.
#' @param cxnObj default is \code{NULL}.  This is a connection object from make_oracle_cxn(). 
#' @param source_df default is \code{NULL}.  This is a data frame containing the data that should be 
#' loaded to Oracle.  It should correspond with the structure used by one of ESE_MISSIONS, ESE_SETS, 
#' ESE_CATCHES, ESE_BASKETS, ESE_SPECIMENS, ESE_LV1_OBSERVATIONS
#' @param target_schema default is \code{NULL}.  This is the name of the Oracle schema where the 
#' data stored in\code{source_df} should be loaded
#' @param target_table default is \code{NULL}.  This is the name of the specific Oracle table within
#' \code{target_schema} where the data should be loaded 
#' @return 
#' @family general_use
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
toOracle <- function(cxnObj = NULL, source_df= NULL, target_table = NULL, target_schema = NULL){
  target_table <- toupper(target_table)
  target_schema <- toupper(target_schema)
  #for each known table, create a key from fields needed to create unique value
  #each table will need it's own group of fields to do so
  keyFields <- switch(target_table, 
                      "ESE_MISSIONS" =         c("MISSION"),
                      "ESE_SETS" =             c("MISSION", "SETNO"),
                      "ESE_CATCHES" =          c("MISSION", "SETNO", "SPEC"),
                      "ESE_BASKETS" =          c("MISSION", "SETNO", "SPEC", "SIZE_CLASS"),
                      "ESE_SPECIMENS" =        c("MISSION", "SETNO", "SPEC", "SIZE_CLASS","SPECIMEN_ID"),
                      "ESE_LV1_OBSERVATIONS" = c("MISSION", "SETNO", "SPEC", "SIZE_CLASS","SPECIMEN_ID","LV1_OBSERVATION_ID")
                      )
                      
  if (is.null(keyFields))stop("Target table not a recognized target")
  
  ##### Check if records already exist in target table
  #paste together keyFields of submitted data to create vector of unique values
  getTblKey <- function(df=NULL, keyFields = NULL){
    if (length(keyFields)>1){
      uvec <- apply( df[ , keyFields ] , 1 , paste0 , collapse = "_" )
    }else{ 
      uvec <- sort(unique(df[,keyFields]))
    }
    uvec <- gsub(pattern = " ", replacement = "", x = uvec) 
    return(uvec)
  }
  #create a query that will do the same of the db values.  Don't run till we kow connection type
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
  if (cxnObj$usepkg=='roracle'){
    this = tryCatch(
      {
        ROracle::dbWriteTable(conn = cxnObj$channel, schema=target_schema, value = source_df, 
                              name = target_table, date=TRUE,row.names = FALSE, overwrite = F, append = TRUE)
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
                       colnames = FALSE, rownames = FALSE, append = TRUE)
      },
      error=function(cond){
        return(-1)
      }
    )
  }
    if (this == -1) message("Could not write ", deparse(substitute(source_df)) ," to ",target_schema,".", target_table)
}