#' @title tweakUniversal
#' @description This function will perform tweaks on data from Andes that exists in multiple 
#' exported csv files  
#' @param x default is \code{NULL}.  This is a list of all of the objects from the csv files
#' @param mission default is \code{NULL}. This is the cleaned mission id (e.g. CAR2022102)
#' @return list
#' @family general_use
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
tweakUniversal <- function(x = NULL, mission=NULL){
  theMsg <- NA
  #get rid of text species name for overwritten spp, we will really entirely on the species codes
  x$catch_data$species <- x$catch_data$species_id <- NULL
  if(mission == "CAR2022102"){  
    theMsg <- paste0(theMsg[!is.na(theMsg)], "\tRemapping species in  the catch_data\n")
    x$catch_data[x$catch_data$species_code == 18411,"species_code"] <- 6102
    x$catch_data[x$catch_data$species_code == 13519,"species_code"] <- 4213
    x$catch_data[x$catch_data$species_code == 14878,"species_code"] <- 4508
    x$catch_data[x$catch_data$species_code == 12161,"species_code"] <- 8216
    x$catch_data[x$catch_data$species_code == 19218,"species_code"] <- 9331
    x$catch_data[x$catch_data$species_code == 19950,"species_code"] <- 1510
    x$catch_data[x$catch_data$species_code == 18680,"species_code"] <- 1821
    x$catch_data[x$catch_data$species_code == 12157,"species_code"] <- 8382
    x$catch_data[x$catch_data$species_code == 10854,"species_code"] <- 500
    x$catch_data[x$catch_data$species_code == 12096,"species_code"] <- 8516
    
    x$catch_data[x$catch_data$species_code == 9003,"species_code"] <- 1224
    # x$catch_data[x$catch_data$species_code == 18495,"species_code"] <- 6105
    
    theMsg <- paste0(theMsg[!is.na(theMsg)], "\tRemapping species in the specimen_data\n")  
    x$specimen_data[x$specimen_data$species_code == 18411,"species_code"] <- 6102
    x$specimen_data[x$specimen_data$species_code == 13519,"species_code"] <- 4213
    x$specimen_data[x$specimen_data$species_code == 14878,"species_code"] <- 4508
    x$specimen_data[x$specimen_data$species_code == 12161,"species_code"] <- 8216
    x$specimen_data[x$specimen_data$species_code == 19218,"species_code"] <- 9331
    x$specimen_data[x$specimen_data$species_code == 19950,"species_code"] <- 1510
    x$specimen_data[x$specimen_data$species_code == 18680,"species_code"] <- 1821
    x$specimen_data[x$specimen_data$species_code == 12157,"species_code"] <- 8382
    x$specimen_data[x$specimen_data$species_code == 10854,"species_code"] <- 500
    x$specimen_data[x$specimen_data$species_code == 12096,"species_code"] <- 8516
    # x$specimen_data[x$specimen_data$species_code == 18495,"species_code"] <- 6105
    
    theMsg <- paste0(theMsg[!is.na(theMsg)], "\tRemapping species in  the basket_data\n") 
    x$basket_data[x$basket_data$species_code == 18411,"species_code"] <- 6102
    x$basket_data[x$basket_data$species_code == 13519,"species_code"] <- 4213
    x$basket_data[x$basket_data$species_code == 14878,"species_code"] <- 4508
    x$basket_data[x$basket_data$species_code == 12161,"species_code"] <- 8216
    x$basket_data[x$basket_data$species_code == 19218,"species_code"] <- 9331
    x$basket_data[x$basket_data$species_code == 19950,"species_code"] <- 1510
    x$basket_data[x$basket_data$species_code == 18680,"species_code"] <- 1821
    x$basket_data[x$basket_data$species_code == 12157,"species_code"] <- 8382
    x$basket_data[x$basket_data$species_code == 10854,"species_code"] <- 500
    x$basket_data[x$basket_data$species_code == 12096,"species_code"] <- 8516
    
    x$basket_data[x$basket_data$species_code == 9003,"species_code"] <- 1224
    # x$basket_data[x$basket_data$species_code == 18495,"species_code"] <- 6105
    
    theMsg <- paste0(theMsg[!is.na(theMsg)], "\tHandling 'spp 18495'.  Basket 1 was spp 6102; Basket 2 was spp 6126 - need an additional catch record\n") 
    x$catch_data<- rbind(x$catch_data, x$catch_data[x$catch_data$species_code==18495,])
    x$catch_data[x$catch_data$species_code==18495,"species_code"] <- c(6102,6126)
    
    x$basket_data<-x$basket_data[order(x$basket_data$id),]
    x$basket_data[x$basket_data$species_code==18495,"species_code"] <- c(6102,6126)
    colnames(x$specimen_data)[colnames(x$specimen_data)=="species_code"]  <- "SPEC"
    colnames(x$catch_data)[colnames(x$catch_data)=="species_code"]  <- "SPEC"
    colnames(x$basket_data)[colnames(x$basket_data)=="species_code"]  <- "SPEC"
  }
  
  # if(mission != "CAR2022102"){
  #   colnames(x$specimen_data)[colnames(x$specimen_data)=="species_aphia_id"]  <- "SPEC"
  #   colnames(x$catch_data)[colnames(x$catch_data)=="species_aphia_id"]  <- "SPEC"
  #   colnames(x$basket_data)[colnames(x$basket_data)=="species_aphia_id"]  <- "SPEC"
  # }
  
  if(mission == "TEL2022010"){
    #tiny fish apparently marked as size class 12 instead of 2
    x$basket_data[x$basket_data$set_number == 121 & x$basket_data$species_aphia_id == 126503 & x$basket_data$size_class ==12,"size_class"]<-2
    x$specimen_data[x$specimen_data$set_number == 121 & x$specimen_data$species_aphia_id  == 126503 & x$specimen_data$size_class ==12,"size_class"]<-2

    #mola mola was not in andes spp list
    x$basket_data[x$basket_data$set_number == 69 & x$basket_data$SPEC == 9991,c("SPEC","species_aphia_id","species_name")] <- as.data.frame(t(as.matrix(c(730,127405,"OCEAN SUNFISH"))))
    x$specimen_data[x$specimen_data$set_number == 69 & x$specimen_data$SPEC == 9991,c("SPEC","species_aphia_id")] <- as.data.frame(t(as.matrix(c(730,127405))))
    x$catch_data[x$catch_data$set_number == 69 & x$catch_data$SPEC == 9991,c("SPEC","species_aphia_id")] <- as.data.frame(t(as.matrix(c(730,127405))))

  }
  
  if (!is.na(theMsg)) message("Universal Tweaks: \n", theMsg)
  return(x)
}

#' @title tweakBaskets
#' @description This function will perform tweaks to data coming from andes before 
#' it is imported to Oracle.  
#' @param x default is \code{NULL}.  This is the ESE_BASKET data frame
#' @return a data frame for loading into 
#' @family general_use
#' @author  Pablo Vergara, \email{Pablo.Vergara@@dfo-mpo.gc.ca}
#' @export
tweakBaskets <- function(x = NULL){
  if(length(unique(x$MISSION)) > 1) stop("The object sent has more than one mission in it, abort")
  theMsg <- NA
  if(x$MISSION[1] == "CAR2022102"){
    theMsg <- paste0(theMsg[!is.na(theMsg)], "\tCorrecting two baskets which were incorrectly flagged as being sampled\n")
    x[x$SETNO == 13 & x$SPEC == 2100 & x$id ==  630,"SAMPLED"]<-F
    x[x$SETNO == 61 & x$SPEC == 8100 & x$id == 1918,"SAMPLED"]<-F
  }
  if (!is.na(theMsg)) message("Tweaking BASKET: \n", theMsg)
  return(x)
}

#' @title tweakCatches
#' @description This function will perform tweaks to data coming from andes before 
#' it is imported to Oracle.  
#' @param x default is \code{NULL}.   This is the ESE_CATCHES data frame
#' @return a data frame for loading into ESE_CATCHES
#' @family general_use
#' @author  Pablo Vergara, \email{Pablo.Vergara@@dfo-mpo.gc.ca}
#' @export
tweakCatches <- function(x = NULL){
  if(length(unique(x$MISSION)) > 1) stop("The object sent has more than one mission in it, abort")
  theMsg <- NA
  
  if (!is.na(theMsg)) message("Tweaking CATCH: ", theMsg)
  return(x)
}

#' @title tweakSpecimensRaw
#' @description This function will perform tweaks to data coming from andes before 
#' it is imported to Oracle.  specimen_data gets converted into both ESE_SPECIMENS and 
#' ESE_LV1_OBSERVATIONS, so many tweaks should be performed here to ensure that changes are 
#' reflected in both output tables.  Also, changing here will ensure sequential LV1_OBSERVATION_ID 
#' values 
#' @param x default is \code{NULL}. This is the raw "specimen_data" data frame
#' @return a data frame for loading into ESE_BASKETS
#' @family general_use
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
tweakSpecimensRaw <- function(x = NULL){
  if(length(unique(x$MISSION)) > 1) stop("The object sent has more than one mission in it, abort")
  theMsg <- NA
  
  theMsg <- paste0(theMsg[!is.na(theMsg)], "\tBerried female crabs and lobsters recoded to 3\n")
  x[which(x$sex==2 & (x$crab.female.eggs == 1 | x$lobster.female.eggs==1)), "sex"] <-3
  if(nrow(x[which(x$sex==1 & (x$crab.female.eggs == 1 | x$lobster.female.eggs==1)), ]))warning("Berried Males detected")
  
  if(x$MISSION[1] == "CAR2022102"){
    theMsg <- paste0(theMsg[!is.na(theMsg)], "\tRemoving Fish Numbers from Mackerel records\n")
    x[x$species_code == 70,"fish_number"] <- NA
    
    theMsg <- paste0(theMsg[!is.na(theMsg)], "\tRemoving comments created solely to generate labels\n")
    commentsDrop <- c(".", "1", "edrftgujik", "fdgj", "fgjh", "rdfyg", "srdtfygvuh")
    x[x$comment %in% commentsDrop,"comment"]<- NA
    x[x$comments %in% commentsDrop,"comments"]<- NA
    
    theMsg <- paste0(theMsg[!is.na(theMsg)], "\tSet 44/Spec 1191/Specimen id 14496: Removing incorrect length\n")
    x[x$id == 14496,"length"] <- NA
    
    theMsg <- paste0(theMsg[!is.na(theMsg)], "\tSet 26/Spec   14/Specimen id  9332: Weight corrected to grams\n")
    x[x$id == 9332,"weight"] <- x[x$id == 9332,"weight"]*1000 
    
    theMsg <- paste0(theMsg[!is.na(theMsg)], "\tSet 49/Spec   11/Specimen id  16236: No useful info entered - deleted\n")
    x <- x[-which(x$id == 16236),]
    
    theMsg <- paste0(theMsg[!is.na(theMsg)], "\tSpec 60: Several sets list weight as less than <0.5g - the smallest precision available.  Bump all of these up to 0.5g\n")
    x[which(x$species_code == 60 & x$weight <0.5),"weight"] <- 0.5
    
  }
  if (!is.na(theMsg)) message("Tweaking specimen_data: \n", theMsg)
  return(x)
}

#' @title tweakSets
#' @description This function will perform tweaks to data coming from andes before 
#' it is imported to Oracle.  
#' @param x default is \code{NULL}.  This is the ESE_SETS data frame.
#' @return a data frame for loading into ESE_SETS
#' @family general_use
#' @author  Pablo Vergara, \email{Pablo.Vergara@@dfo-mpo.gc.ca}
#' @export
tweakSets <- function(x = NULL){
  if(length(unique(x$MISSION)) > 1) stop("The object sent has more than one mission in it, abort")
  theMsg <- NA
  
  if(x$MISSION[1] == "CAR2022102"){
    theMsg <- paste0(theMsg[!is.na(theMsg)], "\tRecoded GEAR from 23 to 15\n")
    #NEST trawl is not 23, but 15 - change here; expect this will be corrected in the future
    x[x$GEAR==23,"GEAR"] <- 15
    
    theMsg <- paste0(theMsg[!is.na(theMsg)], "\tSet 16: Corrected DIST\n")
    x[x$SETNO==16,"DIST"] <- 0.978
    # Mission CAR2022102, Set 16 - DIST showing as -10.98, should be 0.98
    theMsg <- paste0(theMsg[!is.na(theMsg)], "\tSet 16: Corrected START_DEPTH\n")
    x[x$SETNO==16,"START_DEPTH"] <- 92
    
    theMsg <- paste0(theMsg[!is.na(theMsg)], "\tSet 4: Corrected missing ELONG\n")
    x[x$SETNO==4,"ELONG"] <- -6620.64599
    
    theMsg <- paste0(theMsg[!is.na(theMsg)], "\tSets 9 & 41: Corrected ETIME\n")
    x[x$SETNO==9,"END_TIME"] <- "1739"
    x[x$SETNO==41,"END_TIME"] <- "0054"
    
    theMsg <- paste0(theMsg[!is.na(theMsg)], "\tManually added area for each set\n")
    x[x$SETNO ==  "1", "AREA"] <- "523"
    x[x$SETNO ==  "2", "AREA"] <- "524"
    x[x$SETNO ==  "3", "AREA"] <- "524"
    x[x$SETNO ==  "4", "AREA"] <- "524"
    x[x$SETNO ==  "5", "AREA"] <- "524"
    x[x$SETNO ==  "6", "AREA"] <- "524"
    x[x$SETNO ==  "7", "AREA"] <- "524"
    x[x$SETNO ==  "8", "AREA"] <- "524"
    x[x$SETNO ==  "9", "AREA"] <- "524"
    x[x$SETNO == "10", "AREA"] <- "524"
    x[x$SETNO == "11", "AREA"] <- "523"
    x[x$SETNO == "12", "AREA"] <- "523"
    x[x$SETNO == "13", "AREA"] <- "523"
    x[x$SETNO == "14", "AREA"] <- "523"
    x[x$SETNO == "15", "AREA"] <- "523"
    x[x$SETNO == "16", "AREA"] <- "523"
    x[x$SETNO == "17", "AREA"] <- "523"
    x[x$SETNO == "18", "AREA"] <- "523"
    x[x$SETNO == "19", "AREA"] <- "523"
    x[x$SETNO == "20", "AREA"] <- "523"
    x[x$SETNO == "21", "AREA"] <- "523"
    x[x$SETNO == "22", "AREA"] <- "523"
    x[x$SETNO == "23", "AREA"] <- "523"
    x[x$SETNO == "24", "AREA"] <- "523"
    x[x$SETNO == "25", "AREA"] <- "523"
    x[x$SETNO == "26", "AREA"] <- "523"
    x[x$SETNO == "27", "AREA"] <- "523"
    x[x$SETNO == "28", "AREA"] <- "523"
    x[x$SETNO == "29", "AREA"] <- "523"
    x[x$SETNO == "30", "AREA"] <- "523"
    x[x$SETNO == "31", "AREA"] <- "523"
    x[x$SETNO == "32", "AREA"] <- "524"
    x[x$SETNO == "33", "AREA"] <- "524"
    x[x$SETNO == "34", "AREA"] <- "524"
    x[x$SETNO == "35", "AREA"] <- "524"
    x[x$SETNO == "36", "AREA"] <- "524"
    x[x$SETNO == "37", "AREA"] <- "524"
    x[x$SETNO == "38", "AREA"] <- "524"
    x[x$SETNO == "39", "AREA"] <- "524"
    x[x$SETNO == "40", "AREA"] <- "524"
    x[x$SETNO == "41", "AREA"] <- "524"
    x[x$SETNO == "42", "AREA"] <- "524"
    x[x$SETNO == "43", "AREA"] <- "524"
    x[x$SETNO == "44", "AREA"] <- "523"
    x[x$SETNO == "45", "AREA"] <- "524"
    x[x$SETNO == "46", "AREA"] <- "524"
    x[x$SETNO == "47", "AREA"] <- "524"
    x[x$SETNO == "48", "AREA"] <- "524"
    x[x$SETNO == "49", "AREA"] <- "524"
    x[x$SETNO == "50", "AREA"] <- "523"
    x[x$SETNO == "51", "AREA"] <- "523"
    x[x$SETNO == "52", "AREA"] <- "523"
    x[x$SETNO == "53", "AREA"] <- "523"
    x[x$SETNO == "54", "AREA"] <- "523"
    x[x$SETNO == "55", "AREA"] <- "523"
    x[x$SETNO == "56", "AREA"] <- "523"
    x[x$SETNO == "57", "AREA"] <- "524"
    x[x$SETNO == "58", "AREA"] <- "524"
    x[x$SETNO == "59", "AREA"] <- "524"
    x[x$SETNO == "60", "AREA"] <- "524"
    x[x$SETNO == "61", "AREA"] <- "524"
    x[x$SETNO == "62", "AREA"] <- "524"
    x[x$SETNO == "63", "AREA"] <- "524"
    x[x$SETNO == "64", "AREA"] <- "524"
    x[x$SETNO == "65", "AREA"] <- "524"
    x[x$SETNO == "66", "AREA"] <- "524"
    x[x$SETNO == "67", "AREA"] <- "524"
    x[x$SETNO == "68", "AREA"] <- "523"
    x[x$SETNO == "69", "AREA"] <- "523"
    x[x$SETNO == "70", "AREA"] <- "523"
    x[x$SETNO == "71", "AREA"] <- "523"
    x[x$SETNO == "72", "AREA"] <- "467"
  }
  if(x$MISSION[1] == "TEL2022010"){
    theMsg <- paste0(theMsg[!is.na(theMsg)], "\tModified exp type manually for several sets\n")
    x[x$SETNO == "1", "EXPERIMENT_TYPE_CODE"] <- "9"
    x[x$SETNO == "44", "EXPERIMENT_TYPE_CODE"] <- "9"
    x[x$SETNO == "170", "EXPERIMENT_TYPE_CODE"] <- "9"
    
    theMsg <- paste0(theMsg[!is.na(theMsg)], "\tModified dist and speed manually for a set\n")
    x[x$SETNO == "99", c("DIST", "SPEED")] <- as.data.frame(t(c(1.61,3.22)))
  }
  
  if (!is.na(theMsg)) message("Tweaking SETS: \n", theMsg)
  return(x)
}

#' @title tweakBasketsPostProcessing
#' @description This function can do final changes to the basket data once all of the various basket/
#' catch handling and re-allocation of mixed catches is complete.  This processing can include using 
#' measured weights to change basket weights, so both basket and lv1 objects are necessary as 
#' parameters.
#' @param basket default is \code{NULL}.  
#' @param lv1 default is \code{NULL}.  
#' @return an updated version of ESE_BASKETS
#' @family general_use
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
tweakBasketsPostProcessing <- function(basket=NULL, lv1 = NULL){
  if(length(unique(basket$MISSION)) > 1) stop("The object sent has more than one mission in it, abort")
  message("Tweaking final BASKET weights: \n")
  badBasksSampled   <- basket[basket$BASKET_WEIGHT<0.0005 & basket$SAMPLED,"basket_id"]
  badBasksUnsampled <- basket[basket$BASKET_WEIGHT<0.0005 & !basket$SAMPLED,"basket_id"]
  if (length(badBasksUnsampled)>0 | length(badBasksSampled)>0) message("\tMinimum measurable basket weight is 0.0005 kg\n")
  if (length(badBasksUnsampled)>0){
    message("\tUnsampled baskets reporting a weight lower than the minimum were bumped up to 0.0005 kg\n")
    basket[basket$BASKET_WEIGHT<0.0005 & !basket$SAMPLED,"BASKET_WEIGHT"] <- 0.0005
  }
  if (length(badBasksSampled)>0){
    message("\tOne or more sampled baskets reporting a weight lower than the minimum were bumped up using recorded speciment weights\n\n")
    for (i in 1:length(badBasksSampled)){
      thisBasketSampled <- basket[basket$basket_id %in% badBasksSampled[i],]
      thisBasketSampledSpecimensWeight <- as.numeric(lv1[lv1$basket_id %in% badBasksSampled[i] & lv1$LV1_OBSERVATION == "Weight","DATA_VALUE"])
      # message("\t\tHandling")
      if (length(thisBasketSampledSpecimensWeight)>0){
        #wts in lv1 are in grams, but baskets are kg, convert below
        basket[basket$basket_id %in% thisBasketSampled$basket_id,"BASKET_WEIGHT"] <- sum(thisBasketSampledSpecimensWeight/1000)
        # message("\t\t\t<done>") 
      }else{
        basket[basket$basket_id %in% thisBasketSampled$basket_id,"BASKET_WEIGHT"] <- 0.0005
        # message("\t\t\t<No sampled weights could be found - forcing 0.0005 kg>")
      }
      
    }
  }
  return(basket)
}


#' @title tweakLv1
#' @description This function will perform tweaks to the lv1_observations data coming from andes before 
#' it is imported to Oracle.  
#' @param x default is \code{NULL}.  This is the ESE_LV1_OBSERVATIONS data frame
#' @return an updated version of ESE_LV1_OBSERVATIONS
#' @family general_use
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
tweakLv1 <- function(x = NULL){
  if(length(unique(x$MISSION)) > 1) stop("The object sent has more than one mission in it, abort")
  theMsg <- NA
  
  if(x$MISSION[1] == "CAR2022102"){
    theMsg <- paste0(theMsg[!is.na(theMsg)], "\tHerring otoliths taken post-survey - manually adding that information\n")
    x[x$SPEC == 60 & x$LV1_OBSERVATION == "Collect Specimen" & x$DATA_VALUE =="1" ,c("LV1_OBSERVATION","DATA_DESC")]     <- as.data.frame(t(as.matrix(c("Age Material Type","Otolith Taken"))))
  }
  if (!is.na(theMsg)) message("Tweaking LV1_OBSERVATIONS: \n", theMsg)
  return(x)
}