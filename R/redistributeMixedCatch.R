#' @title redistributeMixedCatch
#' @description This function does stuff
#' @param catch default is \code{NULL}.  
#' @param basket default is \code{NULL}.  
#' @param quiet default is \code{F}. 
#' @family internal
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
redistributeMixedCatch2 <- function(catch=NULL, basket = NULL, quiet=T){
  x <- list()
  message("Mixed catches:")
  theMsg <- NA
  
  #there may be cases where we don't want to apply redistribution to certain baskets/catches
  #pull off such records, but retain them as ignored*, and then add them back on again after processing
  
  #for these, size_class 1 was picked off the belt, and only size class 2 was the child of mixed catch
  ignoredBaskets <- basket[FALSE,]
  dontRedistributeBaskets <- basket[basket$MISSION=="TEL2023010" & basket$SETNO==217 & basket$SIZE_CLASS==1 & basket$SPEC %in% c(2521, 2526, 6411),]
  ignoredBaskets <- rbind.data.frame(ignoredBaskets, dontRedistributeBaskets)
  basket <- df.diff(basket, ignoredBaskets)
  
  ignoredCatch <- catch[FALSE,]
  dontRedistributeCatch <- catch[catch$MISSION=="TEL2023010" & catch$SETNO==217 & catch$SIZE_CLASS==1 & catch$SPEC %in% c(2521, 2526, 6411),]
  ignoredCatch <- rbind.data.frame(ignoredCatch, dontRedistributeCatch)
  catch <- df.diff(catch, ignoredCatch)
  #if we've ignored a catch, then we should not use the (merged) number caught for the bum up
  catch[paste0(catch$MISSION, catch$SETNO, catch$SPEC) %in% paste0(ignoredCatch$MISSION, ignoredCatch$SETNO, ignoredCatch$SPEC),"NUMBER_CAUGHT"] <- NA
  
  
  allCatchParents <- catch[catch$is_parent==T,]
  allCatchChildren<- catch[catch$parent_catch_id %in% allCatchParents$catch_id,]
  # print(allCatchChildren[allCatchChildren$SETNO==217 & allCatchChildren$SPEC == 6211,])
  if (nrow(allCatchChildren)<1){
    if(!quiet) message("(No mixed catches found - skipping subsampling)")
    x$basket <- basket
    x$catch <- catch
    if (!is.na(theMsg)) message(theMsg)
    return(x)
  }  
  parentIDs            <- unique(allCatchChildren$parent_catch_id)
  for (i in 1:length(parentIDs)){
    
    #this is the unknown species catch record.  It will be used to identify necessary child baskets, but will then be deleted 
    thisCatchParent   <- allCatchParents[allCatchParents$catch_id %in% parentIDs[i],]
    catch <- df.diff(catch, thisCatchParent)
    
    #these are records that were put in place once all of the species were identified from above
    #these will be updated during this loop, and the original values will be deleted
    thisCatchChildren <- allCatchChildren[allCatchChildren$parent_catch_id %in% parentIDs[i],]
    catch <- df.diff(catch, thisCatchChildren)
    
    #these baskets were all weighed as a mix.  These will be used to assess the ratios of species amongest the mixed catch
    #new records will be generated and these should be deleted
    mixedBaskets      <- basket[basket$catch_id %in% parentIDs[i],]
    #remove mixed baskets from known baskets - these will be broken up by their consitituent species
    basket <- df.diff(basket, mixedBaskets) 
    
    # this is the basket for which we have a breakdown of the species.  We'll use this on to make proportions
    sampledBasket        <-  mixedBaskets[mixedBaskets$SAMPLED,]
    unsampledBaskets     <-  mixedBaskets[!mixedBaskets$SAMPLED,]
    
    
    sampledBasketFindings <- basket[basket$catch_id %in% thisCatchChildren$catch_id,]
    basket <- df.diff(basket, sampledBasketFindings)
    
    #dplyr below handles cases where several baskets of the mixed was identified as the same species
    #since we want the ratio of species weights in our mixed catch, we must roll up all wts by species
    sampledBasketFindings <- sampledBasketFindings %>%
      group_by(MISSION, SETNO, SPEC, SIZE_CLASS) %>%
      summarise(BASKET_WEIGHT = sum(BASKET_WEIGHT),
                basket_id = min(basket_id),
                catch_id= min(catch_id), .groups = "keep") %>% 
      as.data.frame() 
    sampledBasketFindings$SAMPLED <- T

    if (nrow(sampledBasketFindings[is.na(sampledBasketFindings$BASKET_WEIGHT),])==1){
      #we know the parent basket weight and only a single species is missing a weight, so we can calculate the weight of the missing basket  
      catch_wt <- sampledBasket$BASKET_WEIGHT
      known_wt <- sum(sampledBasketFindings$BASKET_WEIGHT, na.rm = T)
      derived_basket_wt <- catch_wt- known_wt
      missing_dets <- sampledBasketFindings[is.na(sampledBasketFindings$BASKET_WEIGHT),c("MISSION","SETNO", "SPEC")] 
      sampledBasketFindings[is.na(sampledBasketFindings$BASKET_WEIGHT),"BASKET_WEIGHT"] <- derived_basket_wt
      message(paste0(missing_dets$MISSION,":",missing_dets$SETNO), ", SPEC:", missing_dets$SPEC, "; the reported basket weight (i.e. ",derived_basket_wt,") was derived from the weight of the parent basket \n(and the knowledge that this was the only species with an NA weight)")
    }
    

    forRatio               <- sampledBasketFindings 
    forRatio$SUM           <- sum(forRatio$BASKET_WEIGHT, na.rm = T)
    forRatio               <- merge(forRatio,thisCatchChildren[,c("catch_id","SIZE_CLASS", "NUMBER_CAUGHT")])
    forRatio$RATIO         <- round(forRatio$BASKET_WEIGHT/forRatio$SUM,6)
    forRatio$WT_EACH       <- NA
    forRatio[!is.na(forRatio$NUMBER_CAUGHT),"WT_EACH"] <- round(forRatio[!is.na(forRatio$NUMBER_CAUGHT),"BASKET_WEIGHT"]/as.numeric(forRatio[!is.na(forRatio$NUMBER_CAUGHT),"NUMBER_CAUGHT"]),5)
    forRatio$BASKET_WEIGHT <- NULL
    #add wt_each back onto sampledBasketFindings to estimate number of indivs
    sampledBasketFindings  <- merge(sampledBasketFindings, forRatio[,c("MISSION", "SETNO", "SPEC", "SIZE_CLASS", "catch_id", "basket_id", "WT_EACH")])
    sampledBasketFindings$NUMBER_CAUGHT <- round(sampledBasketFindings$BASKET_WEIGHT / sampledBasketFindings$WT_EACH,0)
    sampledBasketFindings$WT_EACH <- NULL
    
    if (nrow(unsampledBaskets)==0 && (abs(round(forRatio$SUM[1]-sampledBasket$BASKET_WEIGHT,1)) <=0.5 )){
      message("\tMore processing necessary: It appears that a mixed catch was handled at sea, but the parent records were not removed.")
      next
    }else if (nrow(unsampledBaskets)==0 && (abs(round(forRatio$SUM[1]-sampledBasket$BASKET_WEIGHT,1)) >0.5 )){
      warning("\tIt appears that a mixed catch was handled at sea, but the weight of the child records is different than the parent - this should be looked at")
    }else if (nrow(unsampledBaskets)==0){
      warning("check that reDistributeMixedCatch() handles this many mixed baskets properly")
    }

    #use what we found about the sampled basket to add all of the species and the ratios to the other mixed baskets
    unsampledBaskets <-  merge(unsampledBaskets[,c("MISSION", "SETNO", "BASKET_WEIGHT", "basket_id", "catch_id")], 
                               forRatio[,c("MISSION", "SETNO", "SPEC", "SIZE_CLASS", "RATIO", "WT_EACH")], #all.y = T,
                               by=c("MISSION", "SETNO"))
    unsampledBaskets$BASKET_WEIGHT <- round(unsampledBaskets$BASKET_WEIGHT * unsampledBaskets$RATIO,3)
    unsampledBaskets$NUMBER_CAUGHT <- round(unsampledBaskets$BASKET_WEIGHT / unsampledBaskets$WT_EACH,0)
    unsampledBaskets$WT_EACH <- unsampledBaskets$RATIO <- NULL
    unsampledBaskets$SAMPLED <- F
    updatedBaskets <- rbind.data.frame(sampledBasketFindings,unsampledBaskets)
    basket <- rbind.data.frame(basket, updatedBaskets[,!names(updatedBaskets) %in% c("NUMBER_CAUGHT")])
    
    forCatch <- updatedBaskets %>%
      group_by(MISSION, SETNO, SPEC, SIZE_CLASS) %>%
      summarise(NUMBER_CAUGHT = sum(NUMBER_CAUGHT), .groups = "keep") %>% 
      as.data.frame()
    
    updatedCatch <- thisCatchChildren
    updatedCatch$NUMBER_CAUGHT <- NULL
    
    updatedCatch<-merge(updatedCatch, forCatch)
    catch <- rbind.data.frame(catch, updatedCatch)
    
  }
  basket <- rbind.data.frame(basket, ignoredBaskets)
  catch <- rbind.data.frame(catch, ignoredCatch)
  x$basket <- basket
  x$catch <- catch
  if (!is.na(theMsg)) message(theMsg)
  return(x)
}

# redistributeMixedCatch <- function(catch=NULL, basket = NULL, quiet=T){
#   x <- list()
#   message("Mixed catches:")
#   theMsg <- NA
#   allMixedCatch        <- catch[(catch$is_parent | !is.na(catch$parent_catch_id)),]
#   parentIDs            <- unique(allMixedCatch[!is.na(allMixedCatch$parent_catch_id),"parent_catch_id"])
#   
#   if (length(parentIDs)<1){
#     if(!quiet) message("(No mixed catches found - skipping subsampling)")
#     x$basket <- basket
#     x$catch <- catch
#     if (!is.na(theMsg)) message(theMsg)
#     return(x)
#   }
#   for (i in 1:length(parentIDs)){
#     print(parentIDs[i])
#     browser()
#     sampB <- basket[basket$catch_id %in% allMixedCatch[allMixedCatch$parent_catch_id %in% parentIDs[i],"catch_id"],]
#     if(nrow(sampB)<1)next
#     
#     colnames(sampB)[colnames(sampB)=="catch_id"] <- "basket_catch_id"
#     sampC <- catch[catch$catch_id %in% sampB$basket_catch_id,]
#     sampled <- merge(sampC, sampB, all.y=T)
#     
#     unsampC <- allMixedCatch[allMixedCatch$catch_id == parentIDs[i],]
#     unsampB <- basket[basket$catch_id %in% unsampC$catch_id & !basket$SAMPLED,]
#     colnames(unsampB)[colnames(unsampB)=="catch_id"] <- "basket_catch_id"
#     unsampled <- merge(unsampC, unsampB, all.y=T)
#     
#     # print("unsampB")
#     # print(unsampB)
#     # print("Sampled")
#     # print(basket[basket$catch_id == parentIDs[i] & basket$SAMPLED,])
#     # 
#     # if (nrow(sampled)==0 & nrow(unsampled)==0)browser()
#     if (nrow(unsampled)==0 && (abs(round(sum(sampled$BASKET_WEIGHT)-basket[basket$catch_id == parentIDs[i],"BASKET_WEIGHT"],1))<=0.5)){
#       if(!quiet) message("\tIt appears that a mixed catch was handled at sea, but the parent records were not removed.")
#       if(!quiet) message("\tDeleting this CATCH record:")
#       if(!quiet) print(unsampC)
#       catch <- catch[-which(catch$id %in% unsampC$id),]
#       if(!quiet) message("\tRetaining these CATCH records:")
#       if(!quiet) print(sampC)
#       if(!quiet) message("\tDeleting this BASKET records:")
#       if(!quiet) print(basket[basket$catch_id == parentIDs[i],])
#       basket <- basket[-which(basket$catch_id %in% parentIDs[i]),]
#       if(!quiet) message("\tRetaining these BASKET records:")
#       if(!quiet) print(sampB)
#       next
#     }else if (nrow(unsampled)==0 && (abs(round(sum(sampled$BASKET_WEIGHT)-basket[basket$catch_id == parentIDs[i],"BASKET_WEIGHT"],1))>0.5)){
#       warning("\tIt appears that a mixed catch was handled at sea, but the weight of the child records is different than the parent - this should be looked at")
#     }else if (nrow(unsampled)==0){
#       warning("check that reDistributeMixedCatch() handles this many mixed baskets properly")
#     }
#     
#     newRecs <- sampled
#     
#     newRecs$PROP_WT           <- round(newRecs$BASKET_WEIGHT/sum(newRecs$BASKET_WEIGHT),5)
#     newRecs$WT_EACH           <- round(newRecs$BASKET_WEIGHT/newRecs$NUMBER_CAUGHT,5)
#     colnames(newRecs)[colnames(newRecs)=="NUMBER_CAUGHT"] <- "NUM_smpld"
#     colnames(newRecs)[colnames(newRecs)=="BASKET_WEIGHT"] <- "BW_smpld"
#     if(!quiet) cat("$$$$",newRecs$Parent_unsampled,"$$$$", unsampled$BASKET_WEIGHT)
#     
#     # print("newRecs =~ sampled")
#     # print(newRecs)
#     if (parentIDs[i]==4903)browser()
#     newRecs$Parent_unsampled <- unsampled$BASKET_WEIGHT
#     # newRecs$NUM <- integer()
#     newRecs$BW_calc       <- round(newRecs$Parent_unsampled*newRecs$PROP_WT,3)
#     newRecs$NUM_calc      <- round(newRecs$BW_calc/newRecs$WT_EACH,0)
#     newRecs$Parent_sampled<- basket[basket$catch_id == parentIDs[i] & basket$SAMPLED,"BASKET_WEIGHT"]
#     
#     oldCatch <- allMixedCatch[allMixedCatch$catch_id %in% parentIDs[i],]
#     catch  <- catch[-which(catch$catch_id %in% parentIDs[i]),]
#     oldBaskets <- basket[ which(basket$catch_id %in% parentIDs[i]),]
#     basket <- basket[-which(basket$catch_id %in% parentIDs[i]),]
#     
#     newRecs$NUM_new <- newRecs$NUM_calc + newRecs$NUM_smpld
#     evidenceTable <- newRecs[,c("MISSION", "SETNO", "SPEC", "Parent_sampled", "Parent_unsampled", "BW_smpld", "PROP_WT", "NUM_smpld", "WT_EACH", "BW_calc", "NUM_calc", "NUM_new")]
#     
#     
#     newRecs$PROP_WT <- newRecs$WT_EACH <- newRecs$Parent_unsampled <- newRecs$Parent_sampled <- newRecs$BW_smpld <- newRecs$NUM_calc <- newRecs$NUM_smpld <- newRecs$NUM <- newRecs$id <- NULL
#     
#     colnames(newRecs)[colnames(newRecs)=="BW_calc"] <- "BASKET_WEIGHT"
#     colnames(newRecs)[colnames(newRecs)=="NUM_new"] <- "NUMBER_CAUGHT"
#     newCatch  <- newRecs[,c("MISSION", "SETNO", "SPEC", "basket_catch_id","NOTE", "UNWEIGHED_BASKETS", "NUMBER_CAUGHT", "is_parent", "parent_catch_id", "SIZE_CLASS")]
#     colnames(newCatch)[colnames(newCatch)=="basket_catch_id"] <- "catch_id"
#     catch <- catch[-which(catch$catch_id %in% newCatch$catch_id),]
#     newBaskets <- newRecs[,c("MISSION", "SETNO", "SPEC", "SIZE_CLASS", "BASKET_WEIGHT", "SAMPLED", "basket_id", "basket_catch_id")]
#     # colnames(newBaskets)[colnames(newBaskets)=="basket_id"] <- "id"
#     colnames(newBaskets)[colnames(newBaskets)=="basket_catch_id"] <- "catch_id"
#     newBaskets$basket_id <- oldBaskets[!oldBaskets$SAMPLED,"basket_id"]
#     sampledBaskets <- sampled[,c("MISSION", "SETNO", "SPEC", "SIZE_CLASS", "BASKET_WEIGHT", "SAMPLED", "basket_id", "basket_catch_id")]
#     colnames(sampledBaskets)[colnames(sampledBaskets)=="basket_id"] <- "id"
#     colnames(sampledBaskets)[colnames(sampledBaskets)=="basket_catch_id"] <- "catch_id"
#     if(!quiet) {
#       message("##########")
#       message("Bumping up unsampled mixed catch data:")
#       message("CATCH records:")
#       message("\tDeleted records:")
#       print(oldCatch)
#       message("\tInitial, existing, sampled Catch records:")
#       print(sampC)
#       message("\tNew Catch records (NUM_CAUGHT now includes estimates from unsampled baskets):")
#       print(newCatch)
#       message("BASKET records:")
#       message("\tDeleted Basket records:")
#       print(oldBaskets)
#       message("\tExisting, sampled Basket records:")
#       print(sampledBaskets)
#       message("\tNew Basket records:")
#       print(newBaskets)
#       message("\n\tThe data below can be used to check the calculation of the bumped up values")
#       print(evidenceTable)
#       message("##########")
#     }
#     basket <- rbind(basket, newBaskets)
#     catch <- rbind(catch, newCatch)
#   }
#   # browser()
#   x$basket <- basket
#   x$catch <- catch
#   if (!is.na(theMsg)) message(theMsg)
#   return(x)
# }