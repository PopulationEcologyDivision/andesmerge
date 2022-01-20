# # GET ALL ID's generated for that protocol (we have 20 and 22) 
# #To delete these collections we need this
# 
 
# 
# DELETE in this order. To test before deleting replace the "DELETE from" to "SELECT * from"
#  change the ID of the protocol for the one you need (ex : sampling_protocol_id = 20)
# shared_models_observationgroup_sexes
#   DELETE from shared_models_observationgroup_sexes
#          where observationgroup_id IN (select id from shared_models_observationgroup smo 
#                                                 where sampling_requirement_id IN (select id from shared_models_samplingrequirement sms 
#                                                                                             where sampling_protocol_id = 20))

# shared_models_observationgroup_strata
#   DELETE from shared_models_observationgroup_strata
#          where observationgroup_id IN (select id from shared_models_observationgroup smo 
#                                                 where sampling_requirement_id IN (select id from shared_models_samplingrequirement sms 
#                                                                                             where sampling_protocol_id = 20))
  

# shared_models_observationgroupfield
#   DELETE from shared_models_observationgroupfield 
#          where observation_group_id IN (select id from shared_models_observationgroup smo 
#                                                 where sampling_requirement_id IN (select id from shared_models_samplingrequirement sms 
#                                                                                             where sampling_protocol_id = 20))

# shared_models_observationgroup
#   DELETE from shared_models_observationgroup 
#          where   sampling_requirement_id IN (select id from shared_models_samplingrequirement sms where sampling_protocol_id = 20)


# shared_models_samplingrequirement
#   DELETE from shared_models_samplingrequirement sms where sampling_protocol_id = 20




#' @title createInsertIntoSampReq
#' @description This function returns the SQL queries for the shared_models_samplingrequirement in ANDES using data coming from ESE
#' @return SQL queries
#' @family internal
#' @author  Pablo Vergara, \email{pablo.vergara@@dfo-mpo.gc.ca}
#' @export
#' 
createInsertIntoSampReq <- function(protocol, rawFile){
  
  x = read.csv(file=paste0(base,rawFile))
  
  x$LENGTH_TYPE = as.numeric(convertLengthType(x$LENGTH_TYPE))
  x$LENGTH_UNITS = as.numeric(convertLengthUnits(x$LENGTH_UNITS))
  x$WEIGHT_TYPE  = as.numeric(convertWeighType(x$WEIGHT_TYPE))
  x$WEIGHT_UNITS = as.numeric(convertWeightUnits(x$WEIGHT_UNITS))
  
  df <- data.frame(matrix(ncol = 1, nrow = 0))
  str.insert = "insert into shared_models_samplingrequirement (sampling_protocol_id,sample_by_default,rounding_rule, species_id ,a_male ,b_male, a_female, b_female, a_unk, b_unk, max_length,mature_length_unk,length_type,length_unit,weight_type,weight_unit,wait_for_sex, tolerance_threshold,immature_code,mature_length_male,mature_length_female, prompt_for_specimen_count,created_at, updated_at, created_by_id, last_modified_by_id,maturity_observation_type_id,min_length, always_collect_sex, max_basket_weight) values "
  
  for(i in 1:dim(x)[1]){
    str1 = "("
    str2  = paste(protocol, 1 ,1, x[i,]$SPECIES_CODE, x[i,]$LENGTH_WEIGHT_A_MALE, x[i,]$LENGTH_WEIGHT_B_MALE, x[i,]$LENGTH_WEIGHT_A_FEMALE, x[i,]$LENGTH_WEIGHT_B_FEMALE, x[i,]$LENGTH_WEIGHT_A_UNSPECIFIED, x[i,]$LENGTH_WEIGHT_B_UNSPECIFIED, x[i,]$MAXIMUM_LENGTH, x[i,]$MATURE_LENGTH, x[i,]$LENGTH_TYPE, x[i,]$LENGTH_UNITS, x[i,]$WEIGHT_TYPE, x[i,]$WEIGHT_UNITS,0, 25,"NULL","NULL","NULL",0,"'2021-10-08 18:00:43.364000000'","'2021-10-08 18:00:43.364000000'","NULL","NULL","NULL",'NULL',0,"NULL",  sep=",")
    if(i == dim(x)[1]){
      str3 = ")"  
    }else{
      str3 = "),"  
    }
    str = paste0(str1,str2,str3)
    str=gsub("NA", "NULL", str)  
    
    if(i==1){
      df = str
    }else{
      df = rbind(df,str)
    }
  }
  
  df = rbind(str.insert,df)
  
  return(df)
  
}



#' @title createInsertIntoObsGrp
#' @description This function returns the SQL queries for the shared_models_observationgroup in ANDES using data coming from ESE
#' @return SQL queries
#' @family internal
#' @author  Pablo Vergara, \email{pablo.vergara@@dfo-mpo.gc.ca}
#' @export
#' 
createInsertIntoObsGrp <- function(protocol, rawFile){
  x = read.csv(file=paste0(base,rawFile))
  
  x$ACTIVE = convertYesNo(x$ACTIVE)
  x$STRATIFY_BY_SEX = convertYesNo(x$STRATIFY_BY_SEX)
  x$WHEN_FISH_NUMBER = 1  # need to ask Jaies what this is supposed to  be
  
  
  str.insert = "insert INTO shared_models_observationgroup (name,nom,when_fish_number,process_order,is_active,minimum_length,maximum_length,stratified_by_sex,length_bin_size,quota_type,quota,opano_regions,created_at,updated_at,created_by_id,last_modified_by_id, mission_cap, sampling_requirement_id) value "
  
  for(i in 1:dim(x)[1]){
    str1 = "("
    #need to fix this, add commas
    str2  = paste0("'",x[i,]$OBSERVATION_GROUP,"', ","'",x[i,]$OBSERVATION_GROUP,"',",x[i,]$WHEN_FISH_NUMBER,",", x[i,]$PROCESS_ORDER,",", x[i,]$ACTIVE,",", x[i,]$MIN_LENGTH,",", x[i,]$MAX_LENGTH,",", x[i,]$STRATIFY_BY_SEX,",", x[i,]$LENGTH_GROUP,",", 1,",", x[i,]$NUM_OBS_LENGTH_GROUP,","," NULL,'2022-01-15 01:46:31.021323000'",",'2022-01-15 01:46:31.021323000'",",",10,",",10,",",x[i,]$NUM_OBS_LGRP_STRATA_GROUP,",", "(select id from shared_models_samplingrequirement sms where sampling_protocol_id = ",protocol," AND species_id = ",x[i,]$SPECIES_CODE,")")
    
    if(i == dim(x)[1]){
      str3 = ")"  
    }else{
      str3 = "),"  
    }
    str = paste0(str1,str2,str3)
    str=gsub("NA", "NULL", str)  
    
    if(i==1){
      df = str
    }else{
      df = rbind(df,str)
    }
  }
  
  df = rbind(str.insert,df)
  
  return(df)
  
}



#' @title createInsertIntoObsFields
#' @description This function returns the SQL queries for the shared_models_observationgroupfield in ANDES using data coming from ESE
#' @return SQL queries
#' @family internal
#' @author  Pablo Vergara, \email{pablo.vergara@@dfo-mpo.gc.ca}
#' @export
#' 
createInsertIntoObsFields <- function(protocol, rawFile){
  
  x = read.csv(file=paste0(base,rawFile))
  
  x$LV1_OBSERVATION = convertFieldType(x$LV1_OBSERVATION)
  x$ACTIVE  = convertYesNo(x$ACTIVE)
  
  str.insert = "insert INTO shared_models_observationgroupfield  (is_active,observation_type_id,process_order,observation_group_id) value "
  
  for(i in 1:dim(x)[1]){
    str1 = "("
    #need to fix this, add commas
    
    str2  = paste0(x[i,]$ACTIVE,",",x[i,]$LV1_OBSERVATION,",",x[i,]$PROCESS_ORDER,
                   ", (select id from shared_models_observationgroup smo where name = '", x[i,]$OBSERVATION_GROUP, "'  AND sampling_requirement_id = (select id from shared_models_samplingrequirement sms where sampling_protocol_id = ",
                   protocol, " AND species_id = " ,x[i,]$SPECIES_CODE,"))")
    if(i == dim(x)[1]){
      str3 = ")"  
    }else{
      str3 = "),"  
    }
    str = paste0(str1,str2,str3)
    str=gsub("NA", "NULL", str)  
    
    if(i==1){
      df = str
    }else{
      df = rbind(df,str)
    }
  }
  
  df = rbind(str.insert,df)
  
  return(df)
  
}

#' @title createInsertIntoSex
#' @description This function returns the SQL queries for the shared_models_observationgroupfield in ANDES using data coming from ESE
#' @return SQL queries
#' @family internal
#' @author  Pablo Vergara, \email{pablo.vergara@@dfo-mpo.gc.ca}
#' @export
#' 

createInsertIntoSex <- function(protocol, rawFile){
  
  x = read.csv(file=paste0(base,rawFile))
  
  index = is.na(x$SEX_CODE)
  x = x[!index,]
  x$SEX_CODE = convertSex(x$SEX_CODE)

  str.insert = "insert into shared_models_observationgroup_sexes (observationgroup_id, observationtypecategory_id) values "
  
  for(i in 1:dim(x)[1]){
    str1 = "("
    #need to fix this, add commas
    
    str2  = paste0("(select id from shared_models_observationgroup where sampling_requirement_id = (select id from shared_models_samplingrequirement sms where sampling_protocol_id =", protocol," and species_id = ", x[i,]$SPECIES_CODE, ") AND name = '",x[i,]$OBSERVATION_GROUP, "' ),", x[i,]$SEX_CODE)
    if(i == dim(x)[1]){
      str3 = ")"  
    }else{
      str3 = "),"  
    }
    str = paste0(str1,str2,str3)
    str=gsub("NA", "NULL", str)  
    
    if(i==1){
      df = str
    }else{
      df = rbind(df,str)
    }
  }
  
  df = rbind(str.insert,df)
  
  return(df)
  
}


#' @title createInsertIntoStrata
#' @description This function returns the SQL queries for the shared_models_stratum in ANDES using data coming from ESE
#' @return SQL queries
#' @family internal
#' @author  Pablo Vergara, \email{pablo.vergara@@dfo-mpo.gc.ca}
#' @export
#' 

createInsertIntoStrata <- function(protocol, rawFile, stratumFile){

  # 
  x = read.csv(file=paste0(base,rawFile))
  st = read.csv(file=paste0(base,stratumFile))
  # HAck here since areas are defined with more precision on ANdes. convert the data frame before processing
  
  st = convertStrata(st)
  
  #keep only ones tha have strata gorup defined
  index  = nchar(x$STRATA_GROUP) > 0
  x = x[which(index),]
  
  str.insert = "insert into shared_models_observationgroup_strata (observationgroup_id, stratum_id) values "
  start = 1
  for(i in 1:dim(x)[1]){
    
    index = st$STRATA_GROUP == x[i,]$STRATA_GROUP
    tmp = st[which(index), ]
    if(dim(tmp)[1]){
    for(j in 1:dim(tmp)[1]){
      str1 = "("
      #need to fix this, add commas
    
      str2  = paste0("(select id from shared_models_observationgroup where sampling_requirement_id = (select id from shared_models_samplingrequirement sms where sampling_protocol_id =", protocol," and species_id = ", x[i,]$SPECIES_CODE, ") AND name = '",x[i,]$OBSERVATION_GROUP, "' ),", "(select id from shared_models_stratum where name = '",tmp[j,]$STRATUM ,"')" )
      if(i == dim(x)[1] & j == dim(tmp)[1]){
        str3 = ")"  
      }else{
        str3 = "),"  
      }
      str = paste0(str1,str2,str3)
      str=gsub("NA", "NULL", str)  
    
      if(start==1){
        start =2
        df = str
      }else{
        df = rbind(df,str)
      }
    }#end j
    }
  }#end i
  
  df = rbind(str.insert,df)
  
  return(df)
  
}

# index = st$STRATUM == "5Z3"
# tmp = st[index,]
# # create new lines
# 
# st = convertStrata(st)
# for(i in 1:dim(tmp)[1]){
#   rep1 = tmp[i,]
#   rep2 = tmp[i,]
#   rep3 = tmp[i,]
#   rep4 = tmp[i,]
#   rep1$STRATUM = "5Z31"
#   rep2$STRATUM = "5Z32"
#   rep3$STRATUM = "5Z33"
#   rep4$STRATUM = "5Z34"
#   
#   st = rbind(st,rep1, rep2, rep3, rep4)
# }
# #remove original generic entry
# st = st[!index,]
# 
# index = st$STRATUM == "5Z4"
# tmp = st[index,]
# for(i in 1:dim(tmp)[1]){
#   rep1 = tmp[i,]
#   rep2 = tmp[i,]
#   rep1$STRATUM = "5Z41"
#   rep2$STRATUM = "5Z42"
#   st = rbind(st,rep1, rep2 )
# }
# st = st[!index,]

