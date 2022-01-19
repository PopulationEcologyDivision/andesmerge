#' @title get_value
#' @description This function returns the  lookup value in a vector
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

#' @title convertYesNo
#' @description This function returns converted "Y" or "N' to 0 or 1
#' @return converted characters
#' @family internal
#' @author  Pablo Vergara, \email{pablo.vergara@@dfo-mpo.gc.ca}
#' @export
#' 
convertYesNo <- function(x){
  
  ESE.vals <- list("Y"     = 1,
                   "N"     = 0)
  
  ESE.vals = unlist(ESE.vals)
  x = get_value(x, ESE.vals)
  
  return(x)
}

#' @title convertLengthUnits
#' @description This function returns index values used by andes for given string in ESE
#' @return numeric value representing of index
#' @family internal
#' @author  Pablo Vergara, \email{pablo.vergara@@dfo-mpo.gc.ca}
#' @export
#' 
convertLengthUnits <- function(x){
  # Codes from the data_fixtures folder in Andes.  These values are hard coded and there is no lookup tables for them.
  # Note that these could change and would need updating here
  # {"id": 1, "name": _("millimeters"), "abbrev": _("mm"), "code": None, "used_for": "length"},
  # {"id": 2, "name": _("centimeters"), "abbrev": _("cm"), "code": None, "used_for": "length"},
  
  #Names coming from ESE are not exactly written as in ANDES so we need to set manually herer for a match
  ESE.vals <- list("Centimeters"     = 2,
                   "Millimeters"     = 1)
  
  ESE.vals = unlist(ESE.vals)
  x = get_value(x, ESE.vals)
  x[is.na(x)] = 2  # there should not be empty values, place holder , to be fixed on DB side if needed after
  return(x)
}

#' @title convertLengthType
#' @description This function returns index values used by andes for given string in ESE
#' @return numeric value representing of index
#' @family internal
#' @author  Pablo Vergara, \email{pablo.vergara@@dfo-mpo.gc.ca}
#' @export
#' 
convertLengthType <- function(x){
  # Codes from the data_fixtures folder in Andes.  These values are hard coded and there is no lookup tables for them 
  # Note that these could change and would need updating here
  # {"id": 12, "name": _("Carapace"), "used_for": "length"},
  # {"id": 13, "name": _("Mantle"), "used_for": "length"},
  # {"id": 16, "name": _("Total length"), "used_for": "length"},
  # {"id": 17, "name": _("Fork length"), "used_for": "length"},
  # {"id": 18, "name": _("Standard length (SL)"), "used_for": "length"},
  # {"id": 19, "name": _("Snout length (SNL)"), "used_for": "length"},
  # {"id": 20, "name": _("Length of eye (LOE)"), "used_for": "length"},
  # {"id": 21, "name": _("Predorsal length (PDL)"), "used_for": "length"},
  # {"id": 22, "name": _("Upper jaw length (UJL)"), "used_for": "length"},
  # {"id": 23, "name": _("Head length (HL)"), "used_for": "length"},
  # {"id": 24, "name": _("Preanal length (PAL)"), "used_for": "length"},
  # {"id": 25, "name": _("Postorbital length (POL)"), "used_for": "length"},
  # {"id": 26, "name": _("Head width (HW)"), "used_for": "length"},
  # {"id": 27, "name": _("Interorbital width (IOW)"), "used_for": "length"},
  # {"id": 28, "name": _("Body depth at caudal peduncle (BDCP)"), "used_for": "length"},
  # {"id": 29, "name": _("Body depth at the origin of the first (BDD1) dorsal fin"), "used_for": "length"},
  # {"id": 30, "name": _("Body depth at the origin of the first (BDD2) dorsal fin"), "used_for": "length"},
  # {"id": 31, "name": _("Body depth at the origin of the first (BDD3) dorsal fin"), "used_for": "length"},
  
  #Names coming from ESE are not exactly written as in ANDES so we need to set manually herer for a match
  ESE.vals <- list("Carapace Length"     = 12,
                   "Carapace Width"      = 12,
                   "Mantle Length"       = 13,
                   "Total Length"        = 16, 
                   "Fork Length"         = 17,
                   "Pre Anal Fin Length" = 24,
                   "Shell Height"        = 32)
  ESE.vals = unlist(ESE.vals)
  x = get_value(x, ESE.vals)
  x[is.na(x)] = 16  # there should not be empty values, place holder , to be fixed on DB side if needed after
  
  return(x)
}

#' @title convertWeightUnits
#' @description This function returns index values used by andes for given string in ESE
#' @return numeric value representing of index
#' @family internal
#' @author  Pablo Vergara, \email{pablo.vergara@@dfo-mpo.gc.ca}
#' @export
#' 
convertWeightUnits <- function(x){
  # Codes from the data_fixtures folder in Andes.  These values are hard coded and there is no lookup tables for them 
  # {"id": 3, "name": _("grams"), "abbrev": _("g"), "code": None, "used_for": "weight"},
  # {"id": 4, "name": _("kilograms"), "abbrev": _("kg"), "code": None, "used_for": "weight"},
  # Note that these could change and would need updating here
  
  #Names coming from ESE are not exactly written as in ANDES so we need to set manually herer for a match
  ESE.vals <- list("Grams"     = 3,
                   "Kilos"     = 4,
                   " "         = NA)
  
  ESE.vals = unlist(ESE.vals)
  x = get_value(x, ESE.vals)
  
  return(x)
}

#' @title convertWeighType
#' @description This function returns index values used by andes for given string in ESE
#' @return numeric value representing of index
#' @family internal
#' @author  Pablo Vergara, \email{pablo.vergara@@dfo-mpo.gc.ca}
#' @export
#' 
convertWeighType <- function(x){
  # Codes from the data_fixtures folder in Andes.  These values are hard coded and there is no lookup tables for them 
  # Note that these could change and would need updating here
  # {"id": 5, "name": _("Somatic"), "used_for": "weight"},
  # {"id": 6, "name": _("Stomach"), "used_for": "weight"},
  # {"id": 7, "name": _("Gonad"), "used_for": "weight"},
  # {"id": 14, "name": _("Total"), "used_for": "weight"},
  # {"id": 15, "name": _("Gutted"), "used_for": "weight"},
  
  #Names coming from ESE are not exactly written as in ANDES so we need to set manually herer for a match
  ESE.vals <- list("Total Weight"   = 14,
                   " "         = NA)
  
  ESE.vals = unlist(ESE.vals)
  x = get_value(x, ESE.vals)
  
  return(x)
}

#' @title convertFieldType
#' @description This function returns index values used by andes for given string in ESE
#' @return numeric value representing of index
#' @family internal
#' @author  Pablo Vergara, \email{pablo.vergara@@dfo-mpo.gc.ca}
#' @export
#' 
convertFieldType <- function(x){
  # 5	Age Material Type			   # 27	Abdominal Width			     # 50	put the fish in the blender
  # 6	Comments			           # 28	Spermatophore Presence	 # 52	Collect otoliths
  # 7	Length			             # 29	Egg Stage			           # 53	Collect specimen (DELETE ME)
  # 8	Maturity			           # 30	Clutch Fullness Rate	   # 54	Lobster Female eggs
  # 9	Sex			                 # 31	Collect Leg Sample		   # 55	IML measurig techi
  # 10	Stomach Analysed By		 # 32	Sample Vial Number			 # 56	IML shrimp cailper
  # 11	Stomach Comments			 # 33	Pre-anal length			     # 57	Crab egg condition
  # 12	Stomach Content Weight # 34	Collect vertebrae			   # 58	Lobster Carapace Condition
  # 13	Stomach Empty Weight	 # 35	Collect gonads			     # 59	Lobster shell disease
  # 14	Stomach Fullness			 # 36	Missing legs - left			 # 60	Lobster egg condition
  # 15	Stomach Prey Species   # 37	Missing legs - right		 # 61	Release Crab
  # 16	Stomach State			     # 38	regenerated legs - left	 # 62	Latitude
  # 17	Stomach Total Weight	 # 39	Disc width			         # 63	Longitude
  # 18	Weight			           # 40	Bobtail			             # 64	Datetime
  # 19	Gonad Weight			     # 41	Maturity Halibut			   # 65	Rock crab tag number
  # 20	Liver Weight			     # 42	Maturity Skate			     # 66	Collect Specimen 2
  # 21	Viscera Weight			   # 43	Stomach Weight			     # 67	Collect Specimen 3
  # 22	Carcass Weight			   # 44	regenerated legs - right # 68	Collect Stomach
  # 22	Carcass Weight			   # 45	Crab Maturity			       # 69	Fin ray count
  # 23	Fin Clip			         # 46	Gonad Vial               				
  
  
  ESE.vals <- list("Carcass Weight"          = 22,
                   "Liver Weight"            = 20,
                   "Age Material Type"       = 5 ,
                   "Fish Number"             = 52,
                   "Length"                  = 7 ,
                   "Maturity"                = 8,
                   "Sex"                     = 9,
                   "Weight"                  = 18,
                   "Fin Clip"                = 23,
                   "Sample Vial Number"      = 32,
                   "Stomach Fullness"        = 14,
                   "Comments"                = 6,
                   "Tail Weight"             = 70,
                   "Abdominal Width"         = 27,
                   "Clutch Fullness Rate"    = 30,
                   "Egg Stage"               = 29,
                   "Molt Stage"              = 71,
                   "Shell Disease Index"     = 26,
                   "Spermatophore Presence"  = 28,
                   "Osculum Diameter"        = 72,
                   "Width"                   = 73)
  
  
  ESE.vals = unlist(ESE.vals)
  x = get_value(x, ESE.vals)
  
  return(x)
  
}
