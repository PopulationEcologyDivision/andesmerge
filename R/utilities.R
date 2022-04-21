#' @title getEseTables
#' @description This function returns the names of the ese tables.
#' @return character vector of the 6 core ESE tables
#' @family internal
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
#' 
getEseTables <- function(){
  return(c("ESE_MISSIONS", "ESE_SETS", "ESE_CATCHES", "ESE_BASKETS", "ESE_SPECIMENS", "ESE_LV1_OBSERVATIONS")) 
}

#' @title getKeyFields
#' @description This function returns the key fields for any of the ESE tables
#' @param table default is \code{NULL}.  This is the name of the table for which the key fields are 
#' desired
#' @return nothing - just loads data to environment
#' @family internal
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' 
getKeyFields <- function(table=NULL){
  #for each known table, create a key from fields needed to create unique value
  #each table will need it's own group of fields to do so
  keyFields <- switch(table, 
                      "ESE_MISSIONS" =         c("MISSION"),
                      "ESE_SETS" =             c("MISSION", "SETNO"),
                      "ESE_CATCHES" =          c("MISSION", "SETNO", "SPEC"),
                      "ESE_BASKETS" =          c("MISSION", "SETNO", "SPEC", "SIZE_CLASS"),
                      "ESE_SPECIMENS" =        c("MISSION", "SETNO", "SPEC", "SIZE_CLASS","SPECIMEN_ID"),
                      "ESE_LV1_OBSERVATIONS" = c("MISSION", "SETNO", "SPEC", "SIZE_CLASS","SPECIMEN_ID","LV1_OBSERVATION_ID")
  )
  return(keyFields)
}

#' @title getTblKey
#' @description This function takes a data frame and a vector of field names from that data frame.
#' It concatenates together all of the combinations of values from those key fields into a vector.  
#' When appropriate fields are chosen as \code{keyFields}, the result can be used to create a 
#' primary key for identifying unique rows
#' @param df default is \code{NULL}.  This is a connection object from Mar.utils::make_oracle_cxn(). 
#' @param keyFields default is \code{NULL}.  This is a connection object from Mar.utils::make_oracle_cxn(). 
#' @return nothing - just loads data to environment
#' @family internal
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
getTblKey <- function(df=NULL, keyFields = NULL){
  #paste together keyFields of submitted data to create vector of unique values
  if (!all(keyFields %in% names(df))) {
    #the specified df does not have all of the keyfields
    return(NA)
  }
  if (length(keyFields)>1){
    uvec <- apply( df[ , keyFields ] , 1 , paste0 , collapse = "_" )
  }else{ 
    uvec <- sort(unique(df[,keyFields]))
  }
  uvec <- gsub(pattern = " ", replacement = "", x = uvec) 
  return(uvec)
}

#' @title cleanEse
#' @description This function removes any ESE objects from the R environment
#' @return nothing - just remove data frames from environment
#' @family internal
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
cleanEse <- function(){
 eseObs <- getEseTables()
 rm(list = eseObs, envir = .GlobalEnv)
}


#' @title addSizeClassToCatch
#' @description This function fills in the catch with the missing size class 2 entries.   
#' Andes does not provide a way to add Size class directly, Size classes are entered via the basket form.
#' @param basket A data frame containing the basket information from Andes
#' @param catch A data frame containing the catch information from Andes
#' @param quiet default is \code{FALSE} Determines whether or not the script should output 
#' informational messages 
#' @return the merged catch card
#' @family general_use
#' @author  Pablo Vergara, \email{Pablo.Vergara@@dfo-mpo.gc.ca}
#' @export
addSizeClassToCatch <- function(basket, catch, quiet = FALSE){
  
  # find all Size class 2 entries in basket to add to catch
  index =  basket$SIZE_CLASS == 2
  
  if(length(which(index)) == 0){
    if(!quiet) message("No Size Class 2 entries in Basket, nothing to do")
    return(catch)
  } 
  
  # Create a generic catch row
  temp = catch[1,]
  
  for(i in 1:length(which(index)))
  {
    temp$SETNO  = basket[index,][i,]$SETNO
    temp$SPEC = basket[index,][i,]$SPEC
    temp$NOTE = "New entry of Size Class 2 created from Basket information"
    temp$UNWEIGHED_BASKETS = NA
    temp$SIZE_CLASS = 2
    
    # We only want to create one entry in the catch card. Sometimes we will have more than one basket with size class 2, 
    index2 = catch$SETNO == temp$SETNO & catch$SPEC == temp$SPEC & catch$SIZE_CLASS == 2
    if(length(which(index2)) > 0){
      if(!quiet) message(paste0("Class Size 2 was already entered for set#: ",temp$SETNO, " , species: ", temp$SPEC))
    }else{
      catch = rbind(catch, temp)
    }
  }
  
  # return the catch card
  return(catch)
}

#' @title cleanfields
#' @description This function takes a field and replaces various odd "smart" curly quotes and 
#' apostrophes and replaces them with normal versions
#' @param data default is \code{NULL}.  This is a field in a data frame 
#' @family internal
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
cleanfields <- function(data= NULL){
  #replace other stuff with nothing
  data <- gsub(x = data, pattern = "â€™", replacement = "'") # replace curly apostrophe with '
  data <- gsub(x = data, pattern = 'â€œ', replacement = '"') # replace curly open quote with "
  data <- gsub(x = data, pattern = 'â€', replacement = '"') # replace curly close quote with "
  return(data)
}

##### PATH DEFINITON #####

# default path in repo where CSV files are stored
.andesData = list()
.andesData$defaultCsvPath <- "inst/sampleData/"

# other paths...

