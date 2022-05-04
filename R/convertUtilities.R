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

