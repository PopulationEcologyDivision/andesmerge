sourcery()
library(dplyr)
library(RVSurveyData)
MV = "c:/Users/McMahonM/OneDrive - DFO-MPO/Support/Groundfish/Andes/spring2023Data/"
# old = "c:/Users/McMahonM/OneDrive - DFO-MPO/Support/Groundfish/Andes/spring2022Data/"
#run table refresher
tt <-matchAndesToESE(dataPath = MV, gulfCodes=F)
  
sqlStatement <- paste0("select APHIAID, CODE AS CODE_MAR, SPEC, COMM, GULF,
                       APHIAID_SUGG, SPEC_SUGG, TSN
                       from GROUNDFISH.GSSPECIES_APHIAS")

cxn <- Mar.utils::make_oracle_cxn(fn.oracle.username = groundfish.username,fn.oracle.password = groundfish.password, fn.oracle.dsn = "PTRAN", usepkg = 'roracle')

cc<-matchAndesToESE(dataPath = "c:/Users/McMahonM/OneDrive - DFO-MPO/Support/Groundfish/Andes/summer2022Data/CABOT/reports/")
loadESEData(cxnObj = cxn, source_df = tt$ESE_MISSIONS,         target_table = "ANDESE_MISSIONS", confirmOverwrite = T)
loadESEData(cxnObj = cxn, source_df = tt$ESE_SETS,             target_table = "ANDESE_SETS", confirmOverwrite = T)
loadESEData(cxnObj = cxn, source_df = tt$ESE_BASKETS,          target_table = "ANDESE_BASKETS", confirmOverwrite = T)
loadESEData(cxnObj = cxn, source_df = tt$ESE_CATCHES,          target_table = "ANDESE_CATCHES", confirmOverwrite = T)
loadESEData(cxnObj = cxn, source_df = tt$ESE_SPECIMENS,        target_table = "ANDESE_SPECIMENS", confirmOverwrite = T)
loadESEData(cxnObj = cxn, source_df = tt$ESE_LV1_OBSERVATIONS, target_table = "ANDESE_LV1_OBSERVATIONS", confirmOverwrite = T)

CAR <- "C:/Users/McMahonM/OneDrive - DFO-MPO/Support/Groundfish/Andes/spring2023Data/dave/"
cc <-matchAndesToESE(dataPath = CAR, gulfCodes=F)


if (F){
loadESEData(cxnObj = cxn, source_df = cc$ESE_MISSIONS, target_table = "ESE_MISSIONS", confirmOverwrite = T)
loadESEData(cxnObj = cxn, source_df = cc$ESE_SETS, target_table = "ESE_SETS", confirmOverwrite = T)
loadESEData(cxnObj = cxn, source_df = cc$ESE_BASKETS, target_table = "ESE_BASKETS", confirmOverwrite = T)
loadESEData(cxnObj = cxn, source_df = cc$ESE_CATCHES, target_table = "ESE_CATCHES", confirmOverwrite = T)
loadESEData(cxnObj = cxn, source_df = cc$ESE_SPECIMENS, target_table = "ESE_SPECIMENS", confirmOverwrite = T)
loadESEData(cxnObj = cxn, source_df = cc$ESE_LV1_OBSERVATIONS, target_table = "ESE_LV1_OBSERVATIONS", confirmOverwrite = T)
}

ROracle::dbDisconnect(cxn$channel)

