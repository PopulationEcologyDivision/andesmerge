
#' @title setExperimentType
#' @description This function sets the proper experiment type to match existing data structures in Maritime and Gulf region
#' . Based on a mix of experiment typer and set results  
#' @param quiet default is \code{FALSE} Determines whether or not the script should output 
#' informational messages 
#' @param x set object from ANdes raw data
#' @return The experiment code
#' @family general_use
#' @author  Pablo Vergara, \email{Pablo.Vergara@@dfo-mpo.gc.ca}
#' @export
setExperimentType <- function(x, quiet = FALSE){
  
  
  #exp.num =  x$experiment_type_id
  exp.num =  as.numeric(gsub("(^[0-9]{1,2})(.*$)", "\\1", x$experiment_type))
  set.res =  x$set_result_id
  
  
  # possible experiment types (from : andesdb.shared_models_experimenttype)
  #   1	Stratified random survey set		
  #   5	Comparative fishing experiment	
  #   6	Tagging set		
  #   9	Hydrography		
  #   7	Gear testing		
  #   99	Systematic		
  valid.exp.num = c(1, 5, 6, 7, 9, 99)
  if(!all(exp.num %in% valid.exp.num)) stop("Unknown Experiment type(s) found")
  x$experiment_type_id_tweaked <- NA
  x$experiment_type_id_tweaked <- as.numeric(gsub("(^[0-9]{1,2})(.*$)", "\\1", x$experiment_type))
  
  # Possible set result values (from  : andesdb.shared_models_setresult)
  # 1	NORMAL - No damage to gear	
  # 2	NORMAL - Minor damage to gear - Catch unaffected	NORMAL 
  # 3	FAILED - Major damage to gea
  # 4	FAILED - Bad depth
  # 5	FAILED - Fishing gears
  # 6	FAILED - Wrong gear operation
  # MMM handle hydrography - exp.num = 9 and is.na(set.res)
  # NA Normal - only if exp.num ==9 - Hydrography
  valid.result.num = c(1, 2, 3, 4, 5, 6, NA)
  if(!all(set.res %in% valid.result.num)) stop("Set result not in list")
  
  #NORMAL - GOOD
  index = (exp.num %in% valid.exp.num & set.res %in% c(1, 2))
  if(length(which(index)) > 0)
  {
    x[index,]$experiment_type_id_tweaked = 1
    if(!quiet){ message("Experiment type set to 1")}
  }
  
  #NORMAL - FAIL
  index = (exp.num %in% valid.exp.num & set.res %in% c(3, 4, 5, 6))
  if(length(which(index)) > 0 ){
    x[index,]$experiment_type_id_tweaked = 3
    if(!quiet){ message("Experiment type set to 3")}
  }
  
  #NORMAL - HYDROGRAPHY
  #Added by MMM - not sure about using an NA as an identifier, but maybe in combo with exp.num==9 
  index = (exp.num == 9 & is.na(set.res))
  if(length(which(index)) > 0 ){
    x[index,]$experiment_type_id_tweaked = 9
    if(!quiet){ message("Experiment type set to 9")}
  }
  
  
  return(as.numeric(x$experiment_type_id_tweaked))
}
