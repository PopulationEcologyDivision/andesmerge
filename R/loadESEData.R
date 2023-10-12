#' @title loadESEData
#' @description This function loads any of 6 known data frame objects into Oracle.  If an unknown 
#' \code{target_table} is specified, that table will be created (and deleted, if it already existed)
#' If the \code{target_table} is an existing ESE_* table, and \code{confirmOverwrite} is TRUE, then 
#' the new data will be appended.
#' @param cxnObj default is \code{NULL}.  This is a connection object from 
#' Mar.utils::make_oracle_cxn(). It MUST use ROracle - RODBC is not supported.  The account 
#' specified by this needs permission to write tables in the \code{target_schema}.
#' @param source_df default is \code{NULL}.  This is a data frame containing the data that should be 
#' loaded to Oracle.  It should correspond with the structure used by one of ESE_MISSIONS, ESE_SETS, 
#' ESE_CATCHES, ESE_BASKETS, ESE_SPECIMENS, ESE_LV1_OBSERVATIONS
#' @param target_table default is \code{NULL}.  This is the name of the specific Oracle table within 
#' the user schema (i.e. GROUNDFISH) where the data should go
#' @param confirmOverwrite default is \code{F}. This must be set to \code{T} to append data to the 
#' production ESE tables  
#' @return nothing, but data gets loaded to the selected location in Oracle.
#' @family general_use
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
loadESEData <- function(cxnObj = NULL, source_df= NULL, target_table = NULL, confirmOverwrite = F){
  
  if (cxnObj$usepkg=='rodbc') stop("Sorry - rodbc is not supported, please create an roracle connection")
  thisMission <- source_df$MISSION[1]
  dryRun <- T
  dfNm <-deparse(substitute(source_df))
  target_table <- toupper(target_table)
  
  delete_recs <-  paste0("DELETE FROM ",target_table," WHERE MISSION = '",thisMission,"'")
  if (grepl("^ESE_",target_table) & confirmOverwrite == T){
    dryRun <- F
  }else if (grepl("^ESE_",target_table)){
    stop("To add data to production tables, set 'confirmOverwrite' to TRUE")
  }
  
  #Just creating the SQL that would generate tables - not actually doing it yet.
  message("Working on ", dfNm)
  if (grepl(pattern = "ESE_MISSIONS", x = dfNm)){
    create_table <- paste0("create table ",target_table,"(
MISSION VARCHAR2(10),
SAMPLING_REQUIREMENT VARCHAR2(200),
NOTE VARCHAR2(2000),
DATA_VERSION NUMBER(8,0),
PROGRAM_TITLE VARCHAR2(225)
)")
  }else if (grepl(pattern = "ESE_SETS", x = dfNm)){
    create_table <- paste0("create table ",target_table,"(
MISSION VARCHAR2(10),
SETNO NUMBER(3,0),
START_DATE VARCHAR2(8),
START_TIME VARCHAR2(4),
END_DATE VARCHAR2(8),
END_TIME VARCHAR2(4),
STRAT VARCHAR2(3),
SLAT NUMBER(8,2),
SLONG NUMBER(8,2),
ELAT NUMBER(8,2),
ELONG NUMBER(8,2),
AREA VARCHAR2(3),
DIST NUMBER(6,2),
HOWD NUMBER(1,0),
SPEED NUMBER(6,2),
HOWS NUMBER(1,0),
DMIN NUMBER(4,0),
DMAX NUMBER(4,0),
START_DEPTH NUMBER(4,0),
END_DEPTH NUMBER(4,0),
WIND NUMBER(3,0),
FORCE NUMBER(1,0),
CURNT NUMBER(1,0),
EXPERIMENT_TYPE_CODE NUMBER(2,0),
GEAR NUMBER(2,0),
AUX NUMBER(1,0),
WARPOUT NUMBER(4,0),
NOTE VARCHAR2(2000),
SURFACE_TEMPERATURE NUMBER(3,1),
BOTTOM_TEMPERATURE NUMBER(3,1),
BOTTOM_SALINITY NUMBER(5,3),
HYDRO VARCHAR2(10),
STATION NUMBER(4,0),
BOTTOM_TYPE_CODE NUMBER(1,0),
BOTTOM_TEMP_DEVICE_CODE NUMBER(1,0),
WAVE_HEIGHT_CODE NUMBER(1,0),
LIGHT_LEVEL_CODE NUMBER(3,0),
GEAR_MONITOR_DEVICE_CODE NUMBER(1,0)
)")
  }else if (grepl(pattern = "ESE_BASKETS", x = dfNm)){
    create_table <- paste0("create table ",target_table,"(
MISSION VARCHAR2(10),
SETNO NUMBER(4,0),
SPEC NUMBER(8,0),
SIZE_CLASS NUMBER(1,0),
BASKET_WEIGHT NUMBER(8,4),
SAMPLED VARCHAR2(1)
)")
  }else if (grepl(pattern = "ESE_CATCHES", x = dfNm)){
    create_table <- paste0("create table ",target_table,"(
MISSION VARCHAR2(10),
SETNO NUMBER(4,0),
SPEC NUMBER(8,0),
SIZE_CLASS NUMBER(1,0),
NOTE VARCHAR2(2000),
UNWEIGHED_BASKETS NUMBER(4,0),
NUMBER_CAUGHT NUMBER(6,0)
)")
  }else if (grepl(pattern = "ESE_SPECIMENS", x = dfNm)){
    create_table <- paste0("create table ",target_table,"(
MISSION VARCHAR2(10),
SETNO NUMBER(4,0),
SPEC NUMBER(8,0),
SIZE_CLASS NUMBER(1,0),
SPECIMEN_ID NUMBER(10,0)
)")
  }else if (grepl(pattern = "ESE_LV1_OBSERVATIONS", x = dfNm)){
    create_table <- paste0("create table ",target_table,"(
MISSION VARCHAR2(10),
SETNO NUMBER(4,0),
SPEC NUMBER(8,0),
SIZE_CLASS NUMBER(1,0),
SPECIMEN_ID NUMBER(10,0),
LV1_OBSERVATION_ID NUMBER(10,0),
LV1_OBSERVATION VARCHAR2(50),
DATA_VALUE VARCHAR2(50),
DATA_DESC VARCHAR2(255)
)")
  }
  
  n_commas <- length(gregexpr("),", create_table, fixed = TRUE)[[1]])+1
  ins_str <- paste0("insert into ",target_table," values(",paste0(":",paste(seq(1:n_commas), collapse=",:"),")"))
  
  ##### if this is a dry run, we will delete the old tables, and re-create new, empty ones

  if (dryRun){
    #remove the old table
    tryDeleteTabs = tryCatch(
      {
        DBI::dbRemoveTable(cxnObj$channel, target_table)
      },
      error=function(cond){
        message(cond)
        return(-1)
      }
    )
    if (tryDeleteTabs == -1){
      message("\tCould not remove ", target_table,".  Maybe it was already deleted?")
    }else{
      message("\tDeleted old ", target_table," table")
    }
    #create a new, specifically defined, empty table
    tryCreate = tryCatch(
      {
        DBI::dbGetQuery(cxnObj$channel, create_table)
      },
      error=function(cond){
        message(cond)
        return(-1)
      }
    )
    if (tryCreate == -1){
      message("\tCould not create (empty) ", target_table)
    }else{
      message("\tCreated (empty) ", target_table)
    }
  }else{
    #about to write new andesmerge data into ESE tables - make sure no records exist for the current mission...
    tryDeleteRecs = tryCatch(
      {
        DBI::dbGetQuery(cxnObj$channel, delete_recs)
      },
      error=function(cond){
        message(cond)
        return(-1)
      }
    )
    if (tryDeleteRecs == -1){
      message("\tCould not delete existing data for this mission")
      
    }else{
      message("\tDeleted old data for this mission")
    }
  }
  ##### dry run or not, we will now load the data into the tables 
  tryLoad = tryCatch(
    {
      DBI::dbGetQuery(cxnObj$channel, ins_str, source_df)
    },
    error=function(cond){
      message(cond)
      return(-1)
    }
  )
  if (tryLoad == -1){
    message("\tCould not load data")
    
  }else{
    message("\tLoaded data")
  }
  
  tryCommit = tryCatch(
    {
      DBI::dbCommit(cxnObj$channel)
    },
    error=function(cond){
      message(cond)
      return(-1)
    }
  )
  if (tryCommit == -1){
    message("\tCould not commit")
  }else{
    message("\tCommitted")
  }
  
}

