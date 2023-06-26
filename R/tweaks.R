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
    
    x$specimen_data[x$specimen_data$species_code == 9003,"species_code"] <- 1224
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
  
  if(mission == "TEL2022010"){ 
    #changing time and coords here so they get appropriately converted by transmogrified
    x$set_data[x$set_data$set_number == 148, c("start_latitude_DD", "start_latitude_MMmm", "start_longitude_DD", "start_longitude_MMmm")]<- as.data.frame(t(c(42,27.707,-64,51.821)))
    x$set_data[x$set_data$set_number == 148, c("start_date")] <- "2022-08-03 13:43:00.000000+00:00"
    theMsg <- paste0(theMsg[!is.na(theMsg)], "\tConverting andes codes to Maritimes spp\n") 
    #mola mola was not in andes spp list
    x$basket_data[x$basket_data$set_number == 69 & x$basket_data$SPEC == 9991,c("SPEC")] <- c(730)
    x$specimen_data[x$specimen_data$set_number == 69 & x$specimen_data$SPEC == 9991,c("SPEC")] <- c(730)
    x$catch_data[x$catch_data$set_number == 69 & x$catch_data$SPEC == 9991,c("SPEC")] <- c(730)
    
    #andes used some codes not held by Gulf, so they get changed
    x$basket_data[x$basket_data$SPEC==12161,"SPEC"] <- c(8216)
    x$basket_data[x$basket_data$SPEC==12174,"SPEC"] <- c(8209)
    x$basket_data[x$basket_data$SPEC==14878,"SPEC"] <- c(4508)
    x$basket_data[x$basket_data$SPEC==16593,"SPEC"] <- c(2062)
    x$basket_data[x$basket_data$SPEC==18411,"SPEC"] <- c(6126)
    x$basket_data[x$basket_data$SPEC==18495,"SPEC"] <- c(6105)
    x$basket_data[x$basket_data$SPEC==18515,"SPEC"] <- c(6107)
    x$basket_data[x$basket_data$SPEC==18575,"SPEC"] <- c(6202)
    x$basket_data[x$basket_data$SPEC==18792,"SPEC"] <- c(1812)
    
    x$catch_data[x$catch_data$SPEC==12161,"SPEC"] <- c(8216)
    x$catch_data[x$catch_data$SPEC==12174,"SPEC"] <- c(8209)
    x$catch_data[x$catch_data$SPEC==14878,"SPEC"] <- c(4508)
    x$catch_data[x$catch_data$SPEC==16593,"SPEC"] <- c(2062)
    x$catch_data[x$catch_data$SPEC==18411,"SPEC"] <- c(6126)
    x$catch_data[x$catch_data$SPEC==18495,"SPEC"] <- c(6105)
    x$catch_data[x$catch_data$SPEC==18515,"SPEC"] <- c(6107)
    x$catch_data[x$catch_data$SPEC==18575,"SPEC"] <- c(6202)
    x$catch_data[x$catch_data$SPEC==18792,"SPEC"] <- c(1812)
    
    x$specimen_data[x$specimen_data$SPEC==12161,"SPEC"] <- c(8216)
    x$specimen_data[x$specimen_data$SPEC==12174,"SPEC"] <- c(8209)
    x$specimen_data[x$specimen_data$SPEC==14878,"SPEC"] <- c(4508)
    x$specimen_data[x$specimen_data$SPEC==16593,"SPEC"] <- c(2062)
    x$specimen_data[x$specimen_data$SPEC==18411,"SPEC"] <- c(6126)
    x$specimen_data[x$specimen_data$SPEC==18495,"SPEC"] <- c(6105)
    x$specimen_data[x$specimen_data$SPEC==18515,"SPEC"] <- c(6107)
    x$specimen_data[x$specimen_data$SPEC==18575,"SPEC"] <- c(6202)
    x$specimen_data[x$specimen_data$SPEC==18792,"SPEC"] <- c(1812)
    
    #all instances of mud star were actually pulvillus
    x$basket_data[x$basket_data$SPEC==6115,"SPEC"] <- c(6126)
    x$catch_data[x$catch_data$SPEC==6115,"SPEC"] <- c(6126)
    x$specimen_data[x$specimen_data$SPEC==6115,"SPEC"] <- c(6126)
    
    #andes used specified a level of detail different than what Mar has codes for
    #J Emberley made these suggestions
    x$basket_data[x$basket_data$SPEC==10244,"SPEC"] <- c(942)
    x$basket_data[x$basket_data$SPEC==12676,"SPEC"] <- c(1900)
    x$basket_data[x$basket_data$SPEC==12679,"SPEC"] <- c(1901)
    x$basket_data[x$basket_data$SPEC==13090,"SPEC"] <- c(1930) 
    x$basket_data[x$basket_data$SPEC==14230,"SPEC"] <- c(4316)
    x$basket_data[x$basket_data$SPEC==16592,"SPEC"] <- c(2990)
    x$basket_data[x$basket_data$SPEC==18800,"SPEC"] <- c(7733)
    x$basket_data[x$basket_data$SPEC==19220,"SPEC"] <- c(9303)
    x$basket_data[x$basket_data$SPEC==13891,"SPEC"] <- c(4400)
    x$basket_data[x$basket_data$SPEC==13908,"SPEC"] <- c(4400)
    
    x$catch_data[x$catch_data$SPEC==10244,"SPEC"] <- c(942)
    x$catch_data[x$catch_data$SPEC==12676,"SPEC"] <- c(1900)
    x$catch_data[x$catch_data$SPEC==12679,"SPEC"] <- c(1901)
    x$catch_data[x$catch_data$SPEC==13090,"SPEC"] <- c(1930) 
    x$catch_data[x$catch_data$SPEC==14230,"SPEC"] <- c(4316)
    x$catch_data[x$catch_data$SPEC==16592,"SPEC"] <- c(2990)
    x$catch_data[x$catch_data$SPEC==18800,"SPEC"] <- c(7733)
    x$catch_data[x$catch_data$SPEC==19220,"SPEC"] <- c(9303)
    x$catch_data[x$catch_data$SPEC==13891,"SPEC"] <- c(4400)
    x$catch_data[x$catch_data$SPEC==13908,"SPEC"] <- c(4400)
    
    x$specimen_data[x$specimen_data$SPEC==10244,"SPEC"] <- c(942)
    x$specimen_data[x$specimen_data$SPEC==12676,"SPEC"] <- c(1900)
    x$specimen_data[x$specimen_data$SPEC==12679,"SPEC"] <- c(1901)
    x$specimen_data[x$specimen_data$SPEC==13090,"SPEC"] <- c(1930) 
    x$specimen_data[x$specimen_data$SPEC==14230,"SPEC"] <- c(4316)
    x$specimen_data[x$specimen_data$SPEC==16592,"SPEC"] <- c(2990)
    x$specimen_data[x$specimen_data$SPEC==18800,"SPEC"] <- c(7733)
    x$specimen_data[x$specimen_data$SPEC==19220,"SPEC"] <- c(9303)
    x$specimen_data[x$specimen_data$SPEC==13891,"SPEC"] <- c(4400)
    x$specimen_data[x$specimen_data$SPEC==13908,"SPEC"] <- c(4400)
    #Mar varies from Gulf
    x$basket_data[x$basket_data$SPEC==7706,"SPEC"] <- c(8382)
    x$basket_data[x$basket_data$SPEC==7712,"SPEC"] <- c(8367)
    x$basket_data[x$basket_data$SPEC==7725,"SPEC"] <- c(3215)
    x$basket_data[x$basket_data$SPEC==7733,"SPEC"] <- c(1811)
    x$basket_data[x$basket_data$SPEC==7737,"SPEC"] <- c(708)
    
    x$catch_data[x$catch_data$SPEC==7706,"SPEC"] <- c(8382)
    x$catch_data[x$catch_data$SPEC==7712,"SPEC"] <- c(8367)
    x$catch_data[x$catch_data$SPEC==7725,"SPEC"] <- c(3215)
    x$catch_data[x$catch_data$SPEC==7733,"SPEC"] <- c(1811)
    x$catch_data[x$catch_data$SPEC==7737,"SPEC"] <- c(708)
    
    x$specimen_data[x$specimen_data$SPEC==7706,"SPEC"] <- c(8382)
    x$specimen_data[x$specimen_data$SPEC==7712,"SPEC"] <- c(8367)
    x$specimen_data[x$specimen_data$SPEC==7725,"SPEC"] <- c(3215)
    x$specimen_data[x$specimen_data$SPEC==7733,"SPEC"] <- c(1811)
    x$specimen_data[x$specimen_data$SPEC==7737,"SPEC"] <- c(708)
    
    #tiny fish apparently marked as size class 12 instead of 2
    x$basket_data[x$basket_data$set_number == 121 & x$basket_data$SPEC == 13 & x$basket_data$size_class ==12,"size_class"]<-2
    x$specimen_data[x$specimen_data$set_number == 121 & x$specimen_data$SPEC == 13 & x$specimen_data$size_class ==12,"size_class"]<-2
  }
  
  if(mission == "CAB2022010"){  
    theMsg <- paste0(theMsg[!is.na(theMsg)], "\tRemapping species in  the catch_, basket and speciment_ data\n")
    #Mar varies from Gulf
    x$basket_data[x$basket_data$SPEC==7706,"SPEC"] <- c(8382)
    x$basket_data[x$basket_data$SPEC==7712,"SPEC"] <- c(8367)
    x$basket_data[x$basket_data$SPEC==7725,"SPEC"] <- c(3215)
    x$basket_data[x$basket_data$SPEC==7733,"SPEC"] <- c(1811)
    
    x$catch_data[x$catch_data$SPEC==7706,"SPEC"] <- c(8382)
    x$catch_data[x$catch_data$SPEC==7712,"SPEC"] <- c(8367)
    x$catch_data[x$catch_data$SPEC==7725,"SPEC"] <- c(3215)
    x$catch_data[x$catch_data$SPEC==7733,"SPEC"] <- c(1811)
    
    x$specimen_data[x$specimen_data$SPEC==7706,"SPEC"] <- c(8382)
    x$specimen_data[x$specimen_data$SPEC==7712,"SPEC"] <- c(8367)
    x$specimen_data[x$specimen_data$SPEC==7725,"SPEC"] <- c(3215)
    x$specimen_data[x$specimen_data$SPEC==7733,"SPEC"] <- c(1811)
    
    #andes used some codes not held by Gulf, so they get changed
    x$basket_data[x$basket_data$SPEC==12161,"SPEC"] <- c(8216)
    x$basket_data[x$basket_data$SPEC==14878,"SPEC"] <- c(4508)
    x$basket_data[x$basket_data$SPEC==16593,"SPEC"] <- c(2062)
    x$basket_data[x$basket_data$SPEC==18114,"SPEC"] <- c(2217)
    x$basket_data[x$basket_data$SPEC==18411,"SPEC"] <- c(6126)
    x$basket_data[x$basket_data$SPEC==18495,"SPEC"] <- c(6105)
    
    x$catch_data[x$catch_data$SPEC==12161,"SPEC"] <- c(8216)
    x$catch_data[x$catch_data$SPEC==14878,"SPEC"] <- c(4508)
    x$catch_data[x$catch_data$SPEC==16593,"SPEC"] <- c(2062)
    x$catch_data[x$catch_data$SPEC==18114,"SPEC"] <- c(2217)
    x$catch_data[x$catch_data$SPEC==18411,"SPEC"] <- c(6126)
    x$catch_data[x$catch_data$SPEC==18495,"SPEC"] <- c(6105)
    
    x$specimen_data[x$specimen_data$SPEC==12161,"SPEC"] <- c(8216)
    x$specimen_data[x$specimen_data$SPEC==14878,"SPEC"] <- c(4508)
    x$specimen_data[x$specimen_data$SPEC==16593,"SPEC"] <- c(2062)
    x$specimen_data[x$specimen_data$SPEC==18114,"SPEC"] <- c(2217)
    x$specimen_data[x$specimen_data$SPEC==18411,"SPEC"] <- c(6126)
    x$specimen_data[x$specimen_data$SPEC==18495,"SPEC"] <- c(6105)
    
    x$basket_data[x$basket_data$SPEC==11352,"SPEC"] <- c(8200) #Cnidarians
    x$catch_data[x$catch_data$SPEC==11352,"SPEC"] <- c(8200) #Cnidarians
    x$specimen_data[x$specimen_data$SPEC==11352,"SPEC"] <- c(8200) #Cnidarians
    
    x$basket_data[x$basket_data$SPEC==13860,"SPEC"] <- c(4400) #Nudibranchs
    x$catch_data[x$catch_data$SPEC==13860,"SPEC"] <- c(4400) #Nudibranchs
    x$specimen_data[x$specimen_data$SPEC==13860,"SPEC"] <- c(4400) #Nudibranchs
    
    x$basket_data[x$basket_data$SPEC==15053,"SPEC"] <- c(3100) #Polychaete
    x$catch_data[x$catch_data$SPEC==15053,"SPEC"] <- c(3100) #Polychaete
    x$specimen_data[x$specimen_data$SPEC==15053,"SPEC"] <- c(3100) #Polychaete
    
    x$basket_data[x$basket_data$SPEC==19218,"SPEC"] <- c(9303) #Brown Seaweed
    x$catch_data[x$catch_data$SPEC==19218,"SPEC"] <- c(9303) #Brown Seaweed
    x$specimen_data[x$specimen_data$SPEC==19218,"SPEC"] <- c(9303) #Brown Seaweed   
    
    #these weren't necessarily global, but for specific sets  
    x$basket_data[x$basket_data$set_number ==  8 & x$basket_data$SPEC == 6600,"SPEC"] <- 6615 #(stereoderma unisemita )
    x$catch_data[x$catch_data$set_number ==  8 & x$catch_data$SPEC == 6600,"SPEC"] <- 6615
    x$specimen_data[x$specimen_data$set_number ==  8 & x$specimen_data$SPEC == 6600,"SPEC"] <- 6615 
    
    x$basket_data[x$basket_data$set_number == 23 & x$basket_data$SPEC == 9300,"SPEC"] <- 1901 #("lemonweed")
    x$catch_data[x$catch_data$set_number == 23 & x$catch_data$SPEC == 9300,"SPEC"] <- 1901
    x$specimen_data[x$specimen_data$set_number == 23 & x$specimen_data$SPEC == 9300,"SPEC"] <- 1901 
    
    x$basket_data[x$basket_data$set_number == 30 & x$basket_data$SPEC == 1901,"SPEC"] <- 1901 #("lemonweed")
    x$catch_data[x$catch_data$set_number == 30 & x$catch_data$SPEC == 1901,"SPEC"] <- 1901
    x$specimen_data[x$specimen_data$set_number == 30 & x$specimen_data$SPEC == 1901,"SPEC"] <- 1901 
    
    x$basket_data[x$basket_data$set_number == 32 & x$basket_data$SPEC == 1901,"SPEC"] <- 1901 #("lemonweed")
    x$catch_data[x$catch_data$set_number == 32 & x$catch_data$SPEC == 1901,"SPEC"] <- 1901
    x$specimen_data[x$specimen_data$set_number == 32 & x$specimen_data$SPEC == 1901,"SPEC"] <- 1901 
    
    x$basket_data[x$basket_data$set_number == 44 & x$basket_data$SPEC == 1013,"SPEC"] <- 1270 #(Nezumia Aqualis)
    x$catch_data[x$catch_data$set_number == 44 & x$catch_data$SPEC == 1013,"SPEC"] <- 1270
    x$specimen_data[x$specimen_data$set_number == 44 & x$specimen_data$SPEC == 1013,"SPEC"] <- 1270 
    
    x$basket_data[x$basket_data$set_number == 46 & x$basket_data$SPEC ==  410,"SPEC"] <-  410  #(?) (marlin spike grenadier)
    x$catch_data[x$catch_data$set_number == 46 & x$catch_data$SPEC ==  410,"SPEC"] <-  410 
    x$specimen_data[x$specimen_data$set_number == 46 & x$specimen_data$SPEC ==  410,"SPEC"] <-  410 
    
    x$basket_data[x$basket_data$set_number == 47 & x$basket_data$SPEC == 6100,"SPEC"] <- 6139 #(Plutonaster agassizi)
    x$catch_data[x$catch_data$set_number == 47 & x$catch_data$SPEC == 6100,"SPEC"] <- 6139 
    x$specimen_data[x$specimen_data$set_number == 47 & x$specimen_data$SPEC == 6100,"SPEC"] <- 6139 
    
    x$basket_data[x$basket_data$set_number == 47 & x$basket_data$SPEC == 8354,"SPEC"] <- 1283 #("Sergia robustus")
    x$catch_data[x$catch_data$set_number == 47 & x$catch_data$SPEC == 8354,"SPEC"] <- 1283 
    x$specimen_data[x$specimen_data$set_number == 47 & x$specimen_data$SPEC == 8354,"SPEC"] <- 1283 
    
    x$basket_data[x$basket_data$set_number == 64 & x$basket_data$SPEC ==  983,"SPEC"] <- 1071 #("chain cat shark")
    x$catch_data[x$catch_data$set_number == 64 & x$catch_data$SPEC ==  983,"SPEC"] <- 1071 
    x$specimen_data[x$specimen_data$set_number == 64 & x$specimen_data$SPEC ==  983,"SPEC"] <- 1071 
    
    x$basket_data[x$basket_data$set_number == 73 & x$basket_data$SPEC == 8208,"SPEC"] <- 8383 #(Epizoanthus paguriphilus)
    x$catch_data[x$catch_data$set_number == 73 & x$catch_data$SPEC == 8208,"SPEC"] <- 8383 
    x$specimen_data[x$specimen_data$set_number == 73 & x$specimen_data$SPEC == 8208,"SPEC"] <- 8383 
    
    x$basket_data[x$basket_data$set_number == 97 & x$basket_data$SPEC ==   49,"SPEC"] <-  386  #( Etropus microstomus)
    x$catch_data[x$catch_data$set_number == 97 & x$catch_data$SPEC ==   49,"SPEC"] <-  386
    x$specimen_data[x$specimen_data$set_number == 97 & x$specimen_data$SPEC ==   49,"SPEC"] <-  386 
    
    
    x$basket_data[x$basket_data$set_number ==  96 & x$basket_data$SPEC == 9992,"SPEC"] <- 785 #(silver-rag )
    x$catch_data[x$catch_data$set_number ==  96 & x$catch_data$SPEC == 9992,"SPEC"] <- 785
    x$specimen_data[x$specimen_data$set_number ==  96 & x$specimen_data$SPEC == 9992,"SPEC"] <- 785 
    
    x$basket_data[x$basket_data$set_number ==  96 & x$basket_data$SPEC == 9991,"SPEC"] <- 530 #(red eyed gaper)
    x$catch_data[x$catch_data$set_number ==  96 & x$catch_data$SPEC == 9991,c("SPEC", "notes")] <- as.data.frame(t(c(530,"Red eye gaper (Chaunax stigmaeus)")))
    x$specimen_data[x$specimen_data$set_number ==  96 & x$specimen_data$SPEC == 9991,"SPEC"] <- 530 
    
    x$basket_data[x$basket_data$set_number ==  105 & x$basket_data$SPEC == 9991,"SPEC"] <- 785 #(silver-rag )
    x$catch_data[x$catch_data$set_number ==  105 & x$catch_data$SPEC == 9991,"SPEC"] <- 785
    x$specimen_data[x$specimen_data$set_number ==  105 & x$specimen_data$SPEC == 9991,"SPEC"] <- 785
    
    x$basket_data[x$basket_data$set_number ==  64 & x$basket_data$SPEC == 9991,"SPEC"] <- 498 #(yellow fin bass )
    x$catch_data[x$catch_data$set_number ==  64 & x$catch_data$SPEC == 9991,"SPEC"] <- 498
    x$specimen_data[x$specimen_data$set_number ==  64 & x$specimen_data$SPEC == 9991,"SPEC"] <- 498
    
    x$basket_data[x$basket_data$set_number ==  85 & x$basket_data$SPEC == 9991,"SPEC"] <- 498 #(yellow fin bass )
    x$catch_data[x$catch_data$set_number ==  85 & x$catch_data$SPEC == 9991,"SPEC"] <- 498
    x$specimen_data[x$specimen_data$set_number ==  85 & x$specimen_data$SPEC == 9991,"SPEC"] <- 498
    
    #note indicated that set 28, spec 9990 was actually all spec  2214 and was not sampled
    #increase specimen_count with knowledge that 0.103kg was 67 shrimp, so .4535 was ~295 shrimp
    # 67+295=362
    x$catch_data[x$catch_data$set_number == 28 & x$catch_data$SPEC == 2214,c("specimen_count")] <- 362
    x$basket_data[x$basket_data$set_number == 28 & x$basket_data$SPEC %in% c(2214),"basket_wt_kg"] <- 0.5565
    #remove the catch and set recs for 9990 for this set, since they turned out to all be 2214 and are captured above)
    x$catch_data <- x$catch_data[!(x$catch_data$set_number == 28 & x$catch_data$SPEC == 9990),]
    x$basket_data <- x$basket_data[!(x$basket_data$set_number == 28 & x$basket_data$SPEC == 9990 & x$basket_data$basket_wt_kg == 0.1030),]
    
    x$basket_data[x$basket_data$SPEC==400 & x$basket_data$set_number==40,"size_class"]<-1
    x$specimen_data[x$specimen_data$SPEC==400 & x$specimen_data$set_number==40,"size_class"]<-1
    
    x$basket_data[x$basket_data$SPEC==11 & x$basket_data$set_number==56,"size_class"]<-1
    x$specimen_data[x$specimen_data$SPEC==11 & x$specimen_data$set_number==56,"size_class"]<-1
    
    x$basket_data[x$basket_data$SPEC==11 & x$basket_data$set_number==57,"size_class"]<-1
    x$specimen_data[x$specimen_data$SPEC==11 & x$specimen_data$set_number==57,"size_class"]<-1
    #two brown algaes were entered with different andes codes:
    #combine specimen count, delete one catch record, and reassign child basket to the retained catch record
    x$catch_data[x$catch_data$id==1207,"specimen_count" ]<- sum(x$catch_data[x$catch_data$id==1207,"specimen_count" ], x$catch_data[x$catch_data$id==1223,"specimen_count" ])
    x$catch_data<- x$catch_data[!(x$catch_data$id==1223),]
    x$basket_data[x$basket_data$catch_id==1223,"catch_id"]<-1207
    #two species of nudibranchs were entered with different andes codes but we only capture them as 
    #4400 - deleting one catch record, and reassigning its child basket to the retained catch record
    
    x$catch_data[x$catch_data$id==2254,"specimen_count" ]<- sum(x$catch_data[x$catch_data$id==2254,"specimen_count" ], x$catch_data[x$catch_data$id==2255,"specimen_count" ])
    x$catch_data<- x$catch_data[!(x$catch_data$id==2255),]
    x$basket_data[x$basket_data$catch_id==2255,"catch_id"]<- 2254

    x$set_data[x$set_data$set_number=="59",c("start_date")]<-"2022-07-31 07:07:00+00:00"
    x$set_data[x$set_data$set_number=="59",c("start_latitude_DD")]<- 41
    x$set_data[x$set_data$set_number=="59",c("start_latitude_MMmm")]<- 36.90
    x$set_data[x$set_data$set_number=="59",c("start_longitude_DD")]<- -66                                                 
    x$set_data[x$set_data$set_number=="59",c("start_longitude_MMmm")] <- 36.59
    
    x$set_data[x$set_data$set_number=="84",c("start_date")]<-"2022-08-03 13:41:00.000000+00:00"
    x$set_data[x$set_data$set_number=="84",c("start_latitude_DD")]<- 42
    x$set_data[x$set_data$set_number=="84",c("start_latitude_MMmm")]<- 27.9
    x$set_data[x$set_data$set_number=="84",c("start_longitude_DD")]<- -64                                                 
    x$set_data[x$set_data$set_number=="84",c("start_longitude_MMmm")] <- 52.4
  }
  if(mission == "VEN2023001"){
    #changes requested following Jamie Emberley's initial QC
    x$basket_data[x$basket_data$SPEC == 314,"SPEC"] <- 303 #(Spatulate sculpin entries changed to Grubby Sculpins)
    x$catch_data[x$catch_data$SPEC == 314,"SPEC"] <- 303
    x$specimen_data[x$specimen_data$SPEC == 314,"SPEC"] <- 303
    x$specimensRaw[x$specimensRaw$SPEC == 314,"SPEC"] <- 303
  
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
  if(x$MISSION[1] == "TEL2022010"){  
    #nudibranchs had been broken into sp we don't track
    x<- x[!(x$SETNO == 151 & x$SPEC == 4400),]
    x<-rbind.data.frame(x,list("TEL2022010", 151, 4400, 1, 0.003, FALSE, NA, NA))
  }
  if(x$MISSION[1] == "CAB2022010"){
    x[x$SETNO == 18 & x$SPEC == 150,"SAMPLED"]<-F
    #remove a basket weight that was estimated from the unweighed baskets
    x <- x[!(x$SETNO == 55 & x$SPEC == 2550 & x$basket_id == 2475),] 
  } 
  if(x$MISSION[1] == "CAR2023002"){

    x[which(x$SETNO == 164 & x$SPEC == 4500),"SAMPLED"] <- FALSE 
    # SETNO 7, SPEC 2541
    x[which(x$SETNO == 7 & x$SPEC == 2541),"SAMPLED"] <- FALSE 
    
    x[x$basket_id == 4484,"SIZE_CLASS"]<-1
    x[x$basket_id == 4482,"SIZE_CLASS"]<-1
    x[x$basket_id == 4480,"SIZE_CLASS"]<-2
    
    x[x$basket_id == 1747,"SAMPLED"]<-TRUE
  }
  if(x$MISSION[1] == "VEN2023001"){
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
  if(x$MISSION[1] == "TEL2022010"){  
    theMsg <- paste0(theMsg[!is.na(theMsg)], "\tApplying fixes on the catch records\n")
    
    x[x$SETNO==76 & x$SPEC == 2213,c("UNWEIGHED_BASKETS", "NUMBER_CAUGHT")] <- as.data.frame(t(c(NA,62)))
    x[x$SETNO==133 & x$SPEC == 730,"NOTE"] <- "Caught dead. Hagfish inside. Weight was estimated. Fin to Fin measurement: 167cm. Weight estimated, not weighed. Measured on deck."
    
    #nudibranchs had been broken into sp we don't track
    x<- x[!(x$SETNO == 151 & x$SPEC == 4400),]
    x<-rbind.data.frame(x,list("TEL2022010", 151, 4400, NA, NA, 6, NA, FALSE, NA))
  }
  if(x$MISSION[1] == "CAB2022010"){
    x[x$SETNO==34 & x$SPEC == 243,"NOTE"] <- "basket weight is estimate only"
    x[x$SETNO==50 & x$SPEC == 220,"NOTE"] <- "the large basket weight (858kg) was calculated at sea from a count of unweighed baskets"
    x[x$SETNO==50 & x$SPEC == 204,"NOTE"] <- "the large basket weight (507kg) was calculated at sea from a count of unweighed baskets"
  }
  
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
  x$species_aphia_id <- NULL
  # theMsg <- paste0(theMsg[!is.na(theMsg)], "\tBerried female crabs and lobsters recoded to 3\n")
  x[which(x$sex==2 & (x$crab.female.eggs == 1 | x$lobster.female.eggs==1)), "sex"] <-3
  if(nrow(x[which(x$sex==1 & (x$crab.female.eggs == 1 | x$lobster.female.eggs==1)), ]))warning("Berried Males detected")
 
  if(x$MISSION[1] == "CAR2023002"){
    theMsg <- paste0(theMsg[!is.na(theMsg)], "\tRemoving Fish Numbers from Mackerel records\n") 
  # •	Set 171, spec 11, specimen id 71795 – length entered as 0.30.  Update length to 30
    x[x$id==71795,"length"]<-30
  # •	Set 17 – spec 11, specimen id 5022, change weight to 217
    x[x$id==5022,"weight"]<-217
  # •	Set 11 – spec 4511, specimen id 3709, remove the weight observation
    x[x$id==3709,"weight"]<-NA
  # •	Set 104 – spec 303, specimen id 37264, change weight to 1.5
    x[x$id==37264,"weight"]<-1.5
  # •	Set 65 – spec 303, specimen id 19640, Change weight to 0.5
    x[x$id==19640,"weight"]<-0.5
  # •	Set 153 – spec 14, specimen id 63547, Change weight to 0.5
    x[x$id==63547,"weight"]<-0.5
  # •	Set 69 – spec 150, specimen id 22031, change weight to 4
    x[x$id==22031,"weight"]<-4
  # •	Set 189 – spec 340, specimen id 30865, change weight to 1.5
    x[x$id==30865,"weight"]<-1.5
  # •	Set 167, spec 410, specimen id 70480, Change weight to 0.5
    x[x$id==70480,"weight"]<-0.5
  # •	Set 166, spec 410, specimen id 70267, Change weight to 0.5
    x[x$id==70267,"weight"]<-0.5
  # •	Set 166, spec 410, specimen id 70272, Change weight to 0.5
    x[x$id==70272,"weight"]<-0.5
  # •	Set 166, spec 410, specimen id	70277, Change weight to 0.5
    x[x$id==70277,"weight"]<-0.5
  # •	Set 166, spec 410, specimen id	70283, Change weight to 0.5
    x[x$id==70283,"weight"]<-0.5
  # •	Set 167, spec 604, specimen id	70566, Change weight to 0.5
    x[x$id==70566,"weight"]<-0.5
  # •	Set 7, spec 44, specimen id 1231, change weight from .5 to 0.5
    x[x$id==1231,"weight"]<-0.5
  # •	Set 24, spec 143, specimen id 6483, change weight from .5 to 0.5
    x[x$id==6483,"weight"]<-0.5
  # •	Set 47, spec 60, specimen id 11696, change length from 376 to 276
    x[x$id==11696,"length"]<-276
    
    #SET 29. SPEC 4321, SPECIMEN ID 7713
    x <- x[-which(x$id == 7713),]
    #SETNO 7, SPEC 2541, SPECIMEN ID 1094,
    x <- x[-which(x$id == 1094),]
  }
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
  if(x$MISSION[1] == "TEL2022010"){
    theMsg <- paste0(theMsg[!is.na(theMsg)], "\tSet 133 - removed a duplicate sunfish record")
    x <- x[!x$id %in% 49411,]
    
    theMsg <- paste0(theMsg[!is.na(theMsg)], "\tRemoving some bad records")
    x <- x[!x$id %in% 49475,]
    x <- x[!x$id %in% 20427,]
    x <- x[!x$id %in% 14656,]
  }
  
  if(x$MISSION[1] == "CAB2022010"){
    theMsg <- paste0(theMsg[!is.na(theMsg)], "\tSet 46 - The first 30 'silver hake' records are actually dogfish (fixed)")
    x<- x[with(x,order(id)),]
    x[x$set_number == 46 & x$SPEC == 14,][1:30,"SPEC"]<- 220
    
    #remove specimens with length = 0
    x <- x[!x$id==34724,]
    x <- x[!x$id==34744,]
    #certain 0cm lengths should be 10cm
    x[x$id==35107,"length"]<-10
    x[x$id==31275,"length"]<-10
    x[x$id==47775,"length"]<-10
    x[x$id==32839,"length"]<-10
    x[x$id==27596,"length"]<-10
    #likely typo - length should be 250 (not 25)
    x[x$id==17577,"length"]<-250
    
    #lobsters measured as less than 3mm are likely typos
    x<-x[!(x$SPEC == 2550 & x$length <3),]
    #some entries for the following spp had tiny lengths - almost certainly typos
    x<-x[!(x$SPEC %in% c(11,13,14,23, 1191) & x$length <3),]
    
    
  }
  if(x$MISSION[1] == "VEN2023001"){
    #changes requested following Jamie Emberley's initial QC
    x <- x[!x$id==4098,]
    x <- x[!x$id==14617,]
    x <- x[!x$id==17226,]
    x[x$id==3315,"weight"]<- 2 #0.002 to 2
    x[x$id==9641,"weight"]<- 5 #0.0005 to 5
    x[x$id==14872,"sex"] <- 0 #too small to have been sexed
    x[x$id==20014,"weight"] <- 264 #likely typo in wgt
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
  if(x$MISSION[1] == "CAR2023002"){
    theMsg <- paste0(theMsg[!is.na(theMsg)], "\tRecoded GEAR from 23 to 15\n")
    #NEST trawl is not 23, but 15 - change here; expect this will be corrected in the future
    # Jamie created aux = 6 for the nest
    x[x$GEAR==23,"GEAR"] <- 15
    x[x$AUX==2,"AUX"] <- 6

    #set 1
    # •	Set 1 – update Experiment type to 9
    # •	Set 1 – delete station 100, leave this field blank
    # •	Set 1 – delete Warpout, leave blank
    # •	Set 1 – add a Note for the set – HFX HYD
    x[x$SETNO==1,"EXPERIMENT_TYPE_CODE"]<- 9
    x[x$SETNO==1,"STATION"]<- NA
    x[x$SETNO==1,"WARPOUT"]<- NA
    x[x$SETNO==1,"NOTE"]<- "HFX HYD"
    # set 129
  
    # •	Set 129 - update Experiment type to 9
    # •	Set 129 - delete station 100, leave this field blank
    # •	Set 129 - add a Note for the set – HFX HYD
    x[x$SETNO==129,"EXPERIMENT_TYPE_CODE"]<- 9
    x[x$SETNO==129,"STATION"]<- NA
    x[x$SETNO==129,"NOTE"]<- "HFX HYD"
  }
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
  if (x$MISSION[1] == "CAB2022010"){
    x[x$SETNO == "3", "DIST"] <- 0.95
    #NEST trawl is not 23, but 15 - change here; expect this will be corrected in the future
    x[x$GEAR==23,"GEAR"] <- 15
    x[x$EXPERIMENT_TYPE_CODE==1, "EXPERIMENT_TYPE_CODE"]<- 5
  }
  if (x$MISSION[1] == "VEN2023001"){
    x[x$SETNO == 32, "end_date"] <- "2023-02-11 23:13:04+00:00"
    x[x$SETNO == 32, "END_TIME"] <- 1913
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
  if(x$MISSION[1] == "TEL2022010"){
    theMsg <- paste0(theMsg[!is.na(theMsg)], "\tSome records were recorded in kg instead of g (fixed)\n")
    x[x$SPECIMEN_ID %in% c(24705,24707) & x$LV1_OBSERVATION == "Weight" & x$DATA_VALUE =="0.001" ,c("DATA_VALUE")]     <- 1
    x[x$SPECIMEN_ID == 178 & x$LV1_OBSERVATION == "Weight","DATA_VALUE"]<-1   
    theMsg <- paste0(theMsg[!is.na(theMsg)], "\tSome records had inncorrect lengths")
    x[x$SPECIMEN_ID == 42542 & x$LV1_OBSERVATION == "Length","DATA_VALUE"]<-14
    x[x$SPECIMEN_ID == 46991 & x$LV1_OBSERVATION == "Length","DATA_VALUE"]<-10
    x[x$SPECIMEN_ID == 54423 & x$LV1_OBSERVATION == "Length","DATA_VALUE"]<-20
    x[x$SPECIMEN_ID == 27478 & x$LV1_OBSERVATION == "Length","DATA_VALUE"]<-26
  }
  if (!is.na(theMsg)) message("Tweaking LV1_OBSERVATIONS: \n", theMsg)
  return(x)
}