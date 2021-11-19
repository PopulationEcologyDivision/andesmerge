inserter <- function(cxnObj = NULL, source_df= NULL, target_table = NULL, target_schema = NULL){
  target_table <- toupper(target_table)
  target_schema <- toupper(target_schema)
  
  #for each known table, create a key from fields needed to create unique value
  #each table will need it's own group of fields to do so
  keyFields <- switch(target_table, "GSMISSIONS" = "MISSION", 
                                    "GSCAT" = c("MISSION", "SETNO"), 
                                    "GSDET" = c("MISSION", "SETNO", "SPEC"))
  if (is.null(keyFields))stop("Target table not a recognized target")
  
  ##### Check if records already exist in target table
  #paste together keyFields of submitted data to create vector of unique values
  getTblKey <- function(df=NULL, keyFields = NULL){
    df <- apply( df[ , keyFields ] , 1 , paste0 , collapse = "_" )
    df <- gsub(pattern = " ", replacement = "", x = df) 
    return(df)
  }
  #create a query that will do the same of the db values.  Don't run till we kow connection type
  recsQuery <- paste0("SELECT DISTINCT ",paste0(keyFields, collapse = ", ")," FROM ",target_schema,".",target_table)
  newrecsKey <- getTblKey(df = source_df, keyFields = keyFields)
  oldrec_key <- getTblKey(df=cxnObj$thecmd(cxnObj$channel,  recsQuery), keyFields = keyFields)
  if (any(newrecsKey %in% oldrec_key)) stop("Some submitted records already exist in the target table")
  ##### existing record check over
  
  if (cxnObj$usepkg=='roracle'){
    this = tryCatch(
      {
        ROracle::dbWriteTable(conn = cxnObj$channel, schema=target_schema, value = source_df, 
                              name = target_table, date=T,row.names = FALSE, overwrite = F, append = T)
      },
      error=function(cond){
        return(-1)
      }
    )
  } else if (cxnObj$usepkg=='rodbc'){
    this = tryCatch(
      {
        #schema=target_schema, ??
        RODBC::sqlSave(channel = cxnObj$channel, dat = source_df, 
                       tablename = target_table, 
                       colnames = FALSE, rownames = FALSE, append = T)
      },
      error=function(cond){
        return(-1)
      }
    )
  
  }
    if (this == -1) message("Could not write ", deparse(substitute(source_df)) ," to ",target_schema,".", target_table)
}