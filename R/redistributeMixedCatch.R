#' @title redistributeMixedCatch
#' @description This function does stuff
#' @param catch default is \code{NULL}.  
#' @param basket default is \code{NULL}.  
#' @param quiet default is \code{F}. 
#' @family internal
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
# redistributeMixedCatch2 <- function(catch=NULL, basket = NULL, quiet=T){
#   x <- list()
#   allParentsC <- catch[catch$is_parent,]
# 
#   if (nrow(allParentsC)<1){
#     theMsg <- "(No mixed catches found - skipping subsampling)"
#     x$basket <- basket
#     x$catch <- catch
#     if (!is.na(theMsg)) message(theMsg)
#     return(x)
#   }
#   parentIDs <- sort(unique(allParentsC$catch_id))
# 
#   for (i in 1:length(parentIDs)){
#     thisParentC <- allParentsC[allParentsC$catch_id == parentIDs[i],]
#     thisParentB <- basket[basket$catch_id == parentIDs[i],]
#     
#     parentBaskets <- merge(thisParentC, thisParentB[, !names(thisParentB) %in% c("SPEC")], 
#                            by=c("MISSION", "SETNO", "SIZE_CLASS", "catch_id"))
#     unsampledParent <- parentBaskets[parentBaskets$SAMPLED == FALSE,]
#     sampledParent <- parentBaskets[parentBaskets$SAMPLED == TRUE,]
# 
#     thisChildC <- catch[catch$parent_catch_id %in% parentIDs[i],]
#     thisChildB <- basket[basket$catch_id %in% thisChildC$catch_id,]
#     childBaskets <- merge(thisChildC, thisChildB[, !names(thisChildB) %in% c("SPEC")] , 
#                           by=c("MISSION", "SETNO", "SIZE_CLASS","catch_id"))
#     browser()
#     childBaskets$PROP_WT           <- round(childBaskets$BASKET_WEIGHT/sum(childBaskets$BASKET_WEIGHT),5)
#     childBaskets$WT_EACH           <- round(childBaskets$BASKET_WEIGHT/childBaskets$NUMBER_CAUGHT,5)
#     childBaskets$SAMPLED_WT        <- sampledParent$BASKET_WEIGHT
#     colnames(childBaskets)[colnames(childBaskets)=="NUMBER_CAUGHT"] <- "NUM_smpld"
#     colnames(childBaskets)[colnames(childBaskets)=="BASKET_WEIGHT"] <- "BW_smpld"
#     childBaskets$BW_calc       <- round(childBaskets$SAMPLED_WT*childBaskets$PROP_WT,3)
#     childBaskets$NUM_calc      <- round(childBaskets$BW_calc/childBaskets$WT_EACH,0)
#     
#     newCatch  <- childBaskets[,c("MISSION", "SETNO", "SPEC", "SIZE_CLASS", "catch_id","NOTE", "UNWEIGHED_BASKETS", "NUM_calc", "is_parent", "parent_catch_id")]
#     colnames(newCatch)[colnames(newCatch)=="NUM_calc"] <- "NUMBER_CAUGHT"
#     newCatch <- newCatch[with(newCatch, order(newCatch$catch_id)), ]
#     newBaskets <- childBaskets[,c("MISSION", "SETNO", "SPEC", "SIZE_CLASS", "BW_calc", "SAMPLED", "basket_id", "catch_id")]
#     newBaskets <- newBaskets[with(newBaskets, order(newBaskets$basket_id)), ]
#     colnames(newBaskets)[colnames(newBaskets)=="BW_calc"] <- "BASKET_WEIGHT"
#     if(abs(sampledParent$BASKET_WEIGHT-sum(newBaskets$BASKET_WEIGHT))>0.5){
#       message("SUBSAMPLING:\nThis parent's basket weight differs from the combined weights of the child baskets by more than 0.5kg")
#       print(thisParentB[thisParentB$SAMPLED,])
#       print(newBaskets)
#     }
#     removedCatch <- catch[which(catch$catch_id %in% sampledParent$catch_id),]
#     removedCatch <- removedCatch[with(removedCatch, order(removedCatch$catch_id)), ]
#     removedBaskets <- basket[which(basket$catch_id %in% childBaskets$catch_id),]
#     removedBaskets <- removedBaskets[with(removedBaskets, order(removedBaskets$basket_id)), ]
#     if(!quiet){
#       message("Removing subsampled catch recs")
#       print(removedCatch)
#       message("Replacing it with this ('NUMBER_CAUGHT' bumped up via subampling)")
#       print(newCatch)
# 
#       message("Removing subsampled basket rec(s)")
#       print(removedBaskets)
#       message("Replacing with these ('BASKET_WEIGHT' bumped up via subampling)")
#       print(newBaskets)
#     }
#     #remove old, uncorrected stuff
#     catch  <- catch[-which(catch$catch_id %in% removedCatch$catch_id),]
#     basket <- basket[-which(basket$catch_id %in% removedBaskets$catch_id),]
#     #add new, corrected stuff
#     basket <- rbind.data.frame(basket, newBaskets)
#     if(ncol(catch)!=ncol(newCatch))browser()
#     catch  <- rbind.data.frame(catch, newCatch)
#   }
#   x$basket <- basket
#   x$catch <- catch
#   # if (!is.na(theMsg)) message(theMsg)
#   return(x)
# }
redistributeMixedCatch <- function(catch=NULL, basket = NULL, quiet=T){
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
    sampB <- basket[basket$catch_id %in% allMixedCatch[allMixedCatch$parent_catch_id %in% parentIDs[i],"catch_id"],]
    if(nrow(sampB)<1)next
    # colnames(sampB)[colnames(sampB)=="id"] <- "basket_id"
    colnames(sampB)[colnames(sampB)=="catch_id"] <- "basket_catch_id"
    sampC <- catch[catch$catch_id %in% sampB$basket_catch_id,]
    sampled <- merge(sampC, sampB, all.y=T)

    unsampC <- allMixedCatch[allMixedCatch$catch_id == parentIDs[i],]
    unsampB <- basket[basket$catch_id %in% unsampC$catch_id & !basket$SAMPLED,]
    # colnames(unsampB)[colnames(unsampB)=="id"] <- "basket_id"
    colnames(unsampB)[colnames(unsampB)=="catch_id"] <- "basket_catch_id"
    unsampled <- merge(unsampC, unsampB, all.y=T)
    # if (nrow(sampled)==0 & nrow(unsampled)==0)browser()
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
    # newRecs$NUM <- integer()
    newRecs$BW_calc       <- round(newRecs$Parent_unsampled*newRecs$PROP_WT,3)
    newRecs$NUM_calc      <- round(newRecs$BW_calc/newRecs$WT_EACH,0)
    newRecs$Parent_sampled<- basket[basket$catch_id == parentIDs[i] & basket$SAMPLED,"BASKET_WEIGHT"]

    oldCatch <- allMixedCatch[allMixedCatch$catch_id %in% parentIDs[i],]
    catch  <- catch[-which(catch$catch_id %in% parentIDs[i]),]
    oldBaskets <- basket[ which(basket$catch_id %in% parentIDs[i]),]
    basket <- basket[-which(basket$catch_id %in% parentIDs[i]),]

    newRecs$NUM_new <- newRecs$NUM_calc + newRecs$NUM_smpld
    evidenceTable <- newRecs[,c("MISSION", "SETNO", "SPEC", "Parent_sampled", "Parent_unsampled", "BW_smpld", "PROP_WT", "NUM_smpld", "WT_EACH", "BW_calc", "NUM_calc", "NUM_new")]


    newRecs$PROP_WT <- newRecs$WT_EACH <- newRecs$Parent_unsampled <- newRecs$Parent_sampled <- newRecs$BW_smpld <- newRecs$NUM_calc <- newRecs$NUM_smpld <- newRecs$NUM <- newRecs$id <- NULL

    colnames(newRecs)[colnames(newRecs)=="BW_calc"] <- "BASKET_WEIGHT"
    colnames(newRecs)[colnames(newRecs)=="NUM_new"] <- "NUMBER_CAUGHT"
    newCatch  <- newRecs[,c("MISSION", "SETNO", "SPEC", "basket_catch_id","NOTE", "UNWEIGHED_BASKETS", "NUMBER_CAUGHT", "is_parent", "parent_catch_id", "SIZE_CLASS")]
    colnames(newCatch)[colnames(newCatch)=="basket_catch_id"] <- "catch_id"
    catch <- catch[-which(catch$catch_id %in% newCatch$catch_id),]
    newBaskets <- newRecs[,c("MISSION", "SETNO", "SPEC", "SIZE_CLASS", "BASKET_WEIGHT", "SAMPLED", "basket_id", "basket_catch_id")]
    # colnames(newBaskets)[colnames(newBaskets)=="basket_id"] <- "id"
    colnames(newBaskets)[colnames(newBaskets)=="basket_catch_id"] <- "catch_id"
    newBaskets$basket_id <- oldBaskets[!oldBaskets$SAMPLED,"basket_id"]
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