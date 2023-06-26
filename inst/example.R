sourcery()
library(dplyr)
library(RVSurveyData)
theData = "c:/Users/McMahonM/OneDrive - DFO-MPO/Support/Groundfish/Andes/CAR2023002/"
#run table refresher
results <-matchAndesToESE(dataPath = theData, gulfCodes=F)

#

sqlStatement <- paste0("select APHIAID, CODE AS CODE_MAR, SPEC, COMM, GULF,
                       APHIAID_SUGG, SPEC_SUGG, TSN
                       from GROUNDFISH.GSSPECIES_APHIAS")

cxn <- Mar.utils::make_oracle_cxn(fn.oracle.username = groundfish.username,fn.oracle.password = groundfish.password, fn.oracle.dsn = "PTRAN", usepkg = 'roracle')

loadESEData(cxnObj = cxn, source_df = results$ESE_MISSIONS,         target_table = "ESE_MISSIONS", confirmOverwrite = T)
loadESEData(cxnObj = cxn, source_df = results$ESE_SETS,             target_table = "ESE_SETS", confirmOverwrite = T)
loadESEData(cxnObj = cxn, source_df = results$ESE_BASKETS,          target_table = "ESE_BASKETS", confirmOverwrite = T)
loadESEData(cxnObj = cxn, source_df = results$ESE_CATCHES,          target_table = "ESE_CATCHES", confirmOverwrite = T)
loadESEData(cxnObj = cxn, source_df = results$ESE_SPECIMENS,        target_table = "ESE_SPECIMENS", confirmOverwrite = T)
loadESEData(cxnObj = cxn, source_df = results$ESE_LV1_OBSERVATIONS, target_table = "ESE_LV1_OBSERVATIONS", confirmOverwrite = T)

CAR <- "C:/Users/McMahonM/OneDrive - DFO-MPO/Support/Groundfish/Andes/spring2023Data/dave/"
cc <-matchAndesToESE(dataPath = CAR, gulfCodes=F)


if (F){
loadESEData(cxnObj = cxn, source_df = TestMV$ESE_MISSIONS, target_table = "ESE_MISSIONS", confirmOverwrite = T)
loadESEData(cxnObj = cxn, source_df = TestMV$ESE_SETS, target_table = "ESE_SETS", confirmOverwrite = T)
loadESEData(cxnObj = cxn, source_df = TestMV$ESE_BASKETS, target_table = "ESE_BASKETS", confirmOverwrite = T)
loadESEData(cxnObj = cxn, source_df = TestMV$ESE_CATCHES, target_table = "ESE_CATCHES", confirmOverwrite = T)
loadESEData(cxnObj = cxn, source_df = TestMV$ESE_SPECIMENS, target_table = "ESE_SPECIMENS", confirmOverwrite = T)
loadESEData(cxnObj = cxn, source_df = TestMV$ESE_LV1_OBSERVATIONS, target_table = "ESE_LV1_OBSERVATIONS", confirmOverwrite = T)
}

ROracle::dbDisconnect(cxn$channel)

