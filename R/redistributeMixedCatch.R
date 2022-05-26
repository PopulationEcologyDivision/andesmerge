#' @title redistributeMixedCatch
#' @description This function does stuff
#' @param catch default is \code{NULL}.  
#' @param basket default is \code{NULL}.  
#' @family internal
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
redistributeMixedCatch <- function(catch=NULL, basket = NULL){
  x <- list()
  message("Mixed catches:")
  theMsg <- NA
  allMixedCatch        <- catch[(catch$is_parent | !is.na(catch$parent_catch_id)),]
  parentIDs            <- unique(allMixedCatch[!is.na(allMixedCatch$parent_catch_id),"parent_catch_id"])
  
  if (length(parentIDs)<1){
    message("(No mixed catches found - skipping subsampling)")
    x$basket <- basket
    x$catch <- catch
    if (!is.na(theMsg)) message(theMsg)
    return(x)
  }
  for (i in 1:length(parentIDs)){
    sampB <- basket[basket$catch_id %in% allMixedCatch[allMixedCatch$parent_catch_id %in% parentIDs[i],"id"],]
    colnames(sampB)[colnames(sampB)=="id"] <- "basket_id"
    colnames(sampB)[colnames(sampB)=="catch_id"] <- "basket_catch_id"
    sampC <- catch[catch$id %in% sampB$basket_catch_id,]
    sampled <- merge(sampC, sampB, all.y=T)
    
    unsampC <- allMixedCatch[allMixedCatch$id == parentIDs[i],]
    unsampB <- basket[basket$catch_id %in% unsampC$id & !basket$SAMPLED,]
    colnames(unsampB)[colnames(unsampB)=="id"] <- "basket_id"
    colnames(unsampB)[colnames(unsampB)=="catch_id"] <- "basket_catch_id"
    unsampled <- merge(unsampC, unsampB, all.y=T)
    if (nrow(unsampled)==0 && (abs(round(sum(sampled$BASKET_WEIGHT)-basket[basket$catch_id == parentIDs[i],"BASKET_WEIGHT"],1))<=0.5)){
      message("\tIt appears that a mixed catch was handled at sea, but the parent records were not removed.")
      message("\tDeleting this CATCH record:")
      print(unsampC)
      catch <- catch[-which(catch$id %in% unsampC$id),]
      message("\tRetaining these CATCH records:")
      print(sampC)
      message("\tDeleting this BASKET records:")
      print(basket[basket$catch_id == parentIDs[i],])
      basket <- basket[-which(basket$catch_id %in% parentIDs[i]),]
      message("\tRetaining these BASKET records:")
      print(sampB)
      next
    }else if (nrow(unsampled)==0 && (abs(round(sum(sampled$BASKET_WEIGHT)-basket[basket$catch_id == parentIDs[i],"BASKET_WEIGHT"],1))>0.5)){
      warning("\tIt appears that a mixed catch was handled at sea, but the weight of the child records is different than the parent - this should be looked at")  
    }else if (nrow(unsampled)==0){
      warning("check that reDistributeMixedCatch() handles this many mixed baskets properly")
    }
    newRecs <- sampled
    newRecs$id <- unsampled$id
    newRecs$parent_catch_id <- unsampled$parent_catch_id
    
    newRecs$PROP_WT           <- round(newRecs$BASKET_WEIGHT/sum(newRecs$BASKET_WEIGHT),5)
    newRecs$WT_EACH           <- round(newRecs$BASKET_WEIGHT/newRecs$NUMBER_CAUGHT,5)
    colnames(newRecs)[colnames(newRecs)=="NUMBER_CAUGHT"] <- "NUM_smpld"
    colnames(newRecs)[colnames(newRecs)=="BASKET_WEIGHT"] <- "BW_smpld"
    
    newRecs$Parent_unsampled <- unsampled$BASKET_WEIGHT
    newRecs$NUM <- NA
    newRecs$BW_calc       <- round(newRecs$Parent_unsampled*newRecs$PROP_WT,3)
    newRecs$NUM_calc      <- round(newRecs$BW_calc/newRecs$WT_EACH,0)
    newRecs$Parent_sampled<- basket[basket$catch_id == parentIDs[i] & basket$SAMPLED,"BASKET_WEIGHT"]
    
    oldCatch <- allMixedCatch[allMixedCatch$id %in% parentIDs[i],]
    catch  <- catch[-which(catch$id %in% parentIDs[i]),]
    oldBaskets <- basket[ which(basket$catch_id %in% parentIDs[i]),]
    basket <- basket[-which(basket$catch_id %in% parentIDs[i]),]
    
    newRecs$NUM_new <- newRecs$NUM_calc + newRecs$NUM_smpld
    evidenceTable <- newRecs[,c("MISSION", "SETNO", "SPEC", "Parent_sampled", "Parent_unsampled", "BW_smpld", "PROP_WT", "NUM_smpld", "WT_EACH", "BW_calc", "NUM_calc", "NUM_new")]
    
    
    newRecs$PROP_WT <- newRecs$WT_EACH <- newRecs$Parent_unsampled <- newRecs$Parent_sampled <- newRecs$BW_smpld <- newRecs$NUM_calc <- newRecs$NUM_smpld <- newRecs$NUM <- newRecs$id <- NULL
    
    colnames(newRecs)[colnames(newRecs)=="BW_calc"] <- "BASKET_WEIGHT"
    colnames(newRecs)[colnames(newRecs)=="NUM_new"] <- "NUMBER_CAUGHT"
    newCatch  <- newRecs[,c("MISSION", "SETNO", "SPEC", "basket_catch_id","NOTE", "UNWEIGHED_BASKETS", "NUMBER_CAUGHT", "is_parent", "parent_catch_id", "SIZE_CLASS")]
    colnames(newCatch)[colnames(newCatch)=="basket_catch_id"] <- "id"
    catch <- catch[-which(catch$id %in% newCatch$id),]
    newBaskets <- newRecs[,c("MISSION", "SETNO", "SPEC", "SIZE_CLASS", "BASKET_WEIGHT", "SAMPLED", "basket_id", "basket_catch_id")]
    colnames(newBaskets)[colnames(newBaskets)=="basket_id"] <- "id"
    colnames(newBaskets)[colnames(newBaskets)=="basket_catch_id"] <- "catch_id"
    newBaskets$id <- oldBaskets[!oldBaskets$SAMPLED,"id"]
    sampledBaskets <- sampled[,c("MISSION", "SETNO", "SPEC", "SIZE_CLASS", "BASKET_WEIGHT", "SAMPLED", "basket_id", "basket_catch_id")] 
    colnames(sampledBaskets)[colnames(sampledBaskets)=="basket_id"] <- "id"
    colnames(sampledBaskets)[colnames(sampledBaskets)=="basket_catch_id"] <- "catch_id"
    message("##########")
    message("Bumping up unsampled mixed catch data:")
    message("CATCH records:")
    message("\tDeleted records:")
    print(oldCatch)
    message("\tInitial, existing, sampled Catch records:")
    print(sampC)
    message("\tNew Catch records (NUM_CAUGHT now includes estimates from unsampled baskets):")
    print(newCatch)
    message("BASKET records:")
    message("\tDeleted Basket records:")
    print(oldBaskets)
    message("\tExisting, sampled Basket records:")
    print(sampledBaskets)
    message("\tNew Basket records:")
    print(newBaskets)
    message("\n\tThe data below can be used to check the calculation of the bumped up values")
    print(evidenceTable)
    message("##########")
    basket <- rbind(basket, newBaskets)
    catch <- rbind(catch, newCatch)
  }
  
  x$basket <- basket
  x$catch <- catch
  if (!is.na(theMsg)) message(theMsg)
  return(x)
}