#' @title get_value
#' @description This function returns the  lookup value in a vector
#' @param myKey default is \code{NULL}.
#' @param mylookupvector default is \code{NULL}.
#' @return lookup value
#' @family internal
#' @author  Pablo Vergara, \email{pablo.vergara@@dfo-mpo.gc.ca}
#' @export
#' 
get_value <- function(myKey, mylookupvector){
  myvalue = mylookupvector[myKey]
  myvalue = unname(myvalue)
  return(myvalue)
}

#' @title convertFORCE
#' @description This function returns converted FORCE codes
#' @param x default is \code{NULL}. This is the string to be processed.
#' @return converted FORCE codes
#' @family internal
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
#' 
convertFORCE <- function(x){
  
  ESE.vals <- list("0" = 0,
                   "1" = 1,
                   "2" = 2,
                   "3" = 3,
                   "4" = 4,
                   "5" = 5,
                   "6" = 6,
                   "7" = 7,
                   "8" = 8,
                   "9" = NA)
  
  ESE.vals = unlist(ESE.vals)
  x = get_value(x, ESE.vals)
  
  return(x)
}

#' @title convertHOWOBT
#' @description This function returns converted obtained_code codes
#' @param x default is \code{NULL}. This is the string to be processed.
#' @return converted FORCE codes
#' @family internal
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
#' 
#' @title TF2YN
#' @description This function converts TRUE/FALSE values to Y/N
#' @param field default is \code{NULL}. This is the field that should be converted.
#' @return vector
#' @family internal
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
#' 
TF2YN <- function(x) {
  ESE.vals <- list("True" = "Y", 
                   "False" = "N" 
                   ) 
  
  ESE.vals = unlist(ESE.vals)
  x = get_value(x, ESE.vals)
  return(x)
}

#' @title convertHOWOBT
#' @description This function returns converted obtained_code codes
#' @param x default is \code{NULL}. This is the string to be processed.
#' @return converted FORCE codes
#' @family internal
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
#' 
convertHOWOBT <- function(x){
  
  ESE.vals <- list("1" = 1, # "Ships log or distance program"
                   "2" = 2, # "Radar bouy"
                   "3" = 3, # "DECCA bearings"
                   "4" = 0, # "LORAN bearings or GPS"
                   "5" = 5, # "DECCA radar"
                   "6" = 6, # "LORAN radar"
                   "7" = 7, # "DECCA and LORAN"
                   "8" = 8, # "Satelite navigation"
                   "9" = 9) # "No observation / hydrographic station"
  
  ESE.vals = unlist(ESE.vals)
  x = get_value(x, ESE.vals)

  if (any(0 %in% x)) message('All andes entries of "4 - LORAN bearings or GPS" (in either HOWS or HOWD) are interpreted as "0 - GPS" during Maritimes import')
  return(x)
}