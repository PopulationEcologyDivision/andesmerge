sourcery()
library(dplyr)
library(RVSurveyData)
teleost = "c:/Users/McMahonM/OneDrive - DFO-MPO/Support/Groundfish/Andes/summer2022Data/TELEOST/ecosystem_survey reports/"
# old = "c:/Users/McMahonM/OneDrive - DFO-MPO/Support/Groundfish/Andes/spring2022Data/"
#run table refresher
tt<-matchAndesToESE(dataPath = teleost, groundfish.username = groundfish.username, groundfish.password = groundfish.password)
  
# old<-matchAndesToESE(dataPath = old)
# con <- ROracle::dbConnect(DBI::dbDriver("Oracle"), groundfish.username, groundfish.password, "PTRAN")
# sqlStatement <- paste0("select APHIAID, CODE AS CODE_MAR, SPEC, COMM, GULF,
#                        APHIAID_SUGG, SPEC_SUGG, TSN
#                        from GROUNDFISH.GSSPECIES_APHIAS")
# MAR_ORA <- ROracle::dbGetQuery(con, sqlStatement)
# glf_Bask <- merge(tt$ESE_BASKETS, MAR_ORA[,c("CODE_MAR", "GULF")], by.x="SPEC", by.y="GULF", all.x=T)                                      
# glf_Spec <- merge(tt$ESE_SPECIMENS, MAR_ORA[,c("CODE_MAR", "GULF")], by.x="SPEC", by.y="GULF", all.x=T)                                      

# View(glf_Bask[which(glf_Bask$SPEC != glf_Bask$CODE_MAR),])
 cxn <- Mar.utils::make_oracle_cxn(fn.oracle.username = groundfish.username,fn.oracle.password = groundfish.password, fn.oracle.dsn = "PTRAN", usepkg = 'roracle')
# tmp                    <- loadData(dataPath = teleost)

loadESEData(cxnObj = cxn, source_df = tt$ESE_MISSIONS, target_table = "ANDESE_MISSIONS")
loadESEData(cxnObj = cxn, source_df = tt$ESE_SETS, target_table = "ANDESE_SETS")
loadESEData(cxnObj = cxn, source_df = tt$ESE_BASKETS, target_table = "ANDESE_BASKETS")
loadESEData(cxnObj = cxn, source_df = tt$ESE_CATCHES, target_table = "ANDESE_CATCHES")
loadESEData(cxnObj = cxn, source_df = tt$ESE_SPECIMENS, target_table = "ANDESE_SPECIMENS")
loadESEData(cxnObj = cxn, source_df = tt$ESE_LV1_OBSERVATIONS, target_table = "ANDESE_LV1_OBSERVATIONS")

cc<-matchAndesToESE(dataPath = "c:/Users/McMahonM/OneDrive - DFO-MPO/Support/Groundfish/Andes/summer2022Data/CABOT/reports/")
loadESEData(cxnObj = cxn, source_df = cc$ESE_MISSIONS, target_table = "ANDESE_CAB_MISSIONS")
loadESEData(cxnObj = cxn, source_df = cc$ESE_SETS, target_table = "ANDESE_CAB_SETS")
loadESEData(cxnObj = cxn, source_df = cc$ESE_BASKETS, target_table = "ANDESE_CAB_BASKETS")
loadESEData(cxnObj = cxn, source_df = cc$ESE_CATCHES, target_table = "ANDESE_CAB_CATCHES")
loadESEData(cxnObj = cxn, source_df = cc$ESE_SPECIMENS, target_table = "ANDESE_CAB_SPECIMENS")
loadESEData(cxnObj = cxn, source_df = cc$ESE_LV1_OBSERVATIONS, target_table = "ANDESE_CAB_LV1_OBSERVATIONS")





if (F){
loadESEData(cxnObj = cxn, source_df = tt$ESE_MISSIONS, target_table = "ESE_MISSIONS", confirmOverwrite = T)
loadESEData(cxnObj = cxn, source_df = tt$ESE_SETS, target_table = "ESE_SETS", confirmOverwrite = T)
loadESEData(cxnObj = cxn, source_df = tt$ESE_BASKETS, target_table = "ESE_BASKETS", confirmOverwrite = T)
loadESEData(cxnObj = cxn, source_df = tt$ESE_CATCHES, target_table = "ESE_CATCHES", confirmOverwrite = T)
loadESEData(cxnObj = cxn, source_df = tt$ESE_SPECIMENS, target_table = "ESE_SPECIMENS", confirmOverwrite = T)
loadESEData(cxnObj = cxn, source_df = tt$ESE_LV1_OBSERVATIONS, target_table = "ESE_LV1_OBSERVATIONS", confirmOverwrite = T)
}

ROracle::dbDisconnect(cxn$channel)

# ESE_MISSION               26 +      1 =      27   
# ESE_SETS                2665 +     72 =    2737
# ESE_BASKETS            75190 +   1895 =   77085  None?
# ESE_CATCHES            56880 +   1603 =   58483 
# ESE_SPECIMENS         797601 +  24229 =  821830
# ESE_LV1_OBSERVATIONS 1549386 +  39512 = 1588898
