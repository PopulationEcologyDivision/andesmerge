sourcery()
library(dplyr)
library(RVSurveyData)


theData_cab = "c:/Users/McMahonM/OneDrive - DFO-MPO/Support/Groups/Groundfish/ANDES_Winter_2024/"
results_cab <-matchAndesToESE(dataPath = theData_cab, gulfCodes=F)



cxn <- Mar.utils::make_oracle_cxn(fn.oracle.username = groundfish.username,fn.oracle.password = groundfish.password, fn.oracle.dsn = "PTRAN", usepkg = 'roracle')

loadESEData(cxnObj = cxn, source_df = results_cab$ESE_MISSIONS,         target_table = "CAB_ESE_MISSIONS", confirmOverwrite = T)
loadESEData(cxnObj = cxn, source_df = results_cab$ESE_SETS,             target_table = "CAB_ESE_SETS", confirmOverwrite = T)
loadESEData(cxnObj = cxn, source_df = results_cab$ESE_BASKETS,          target_table = "CAB_ESE_BASKETS", confirmOverwrite = T)
loadESEData(cxnObj = cxn, source_df = results_cab$ESE_CATCHES,          target_table = "CAB_ESE_CATCHES", confirmOverwrite = T)
loadESEData(cxnObj = cxn, source_df = results_cab$ESE_SPECIMENS,        target_table = "CAB_ESE_SPECIMENS", confirmOverwrite = T)
loadESEData(cxnObj = cxn, source_df = results_cab$ESE_LV1_OBSERVATIONS, target_table = "CAB_ESE_LV1_OBSERVATIONS", confirmOverwrite = T)

theData_car = "c:/Users/McMahonM/OneDrive - DFO-MPO/Support/Groups/Groundfish/ANDES_Summer_2023/CARTIER"
results_car <-matchAndesToESE(dataPath = theData_car, gulfCodes=F)

loadESEData(cxnObj = cxn, source_df = results_car$ESE_MISSIONS,         target_table = "CAR_ESE_MISSIONS", confirmOverwrite = T)
loadESEData(cxnObj = cxn, source_df = results_car$ESE_SETS,             target_table = "CAR_ESE_SETS", confirmOverwrite = T)
loadESEData(cxnObj = cxn, source_df = results_car$ESE_BASKETS,          target_table = "CAR_ESE_BASKETS", confirmOverwrite = T)
loadESEData(cxnObj = cxn, source_df = results_car$ESE_CATCHES,          target_table = "CAR_ESE_CATCHES", confirmOverwrite = T)
loadESEData(cxnObj = cxn, source_df = results_car$ESE_SPECIMENS,        target_table = "CAR_ESE_SPECIMENS", confirmOverwrite = T)
loadESEData(cxnObj = cxn, source_df = results_car$ESE_LV1_OBSERVATIONS, target_table = "CAR_ESE_LV1_OBSERVATIONS", confirmOverwrite = T)



if (F){
loadESEData(cxnObj = cxn, source_df = results_car$ESE_MISSIONS, target_table = "ESE_MISSIONS", confirmOverwrite = T)
loadESEData(cxnObj = cxn, source_df = results_car$ESE_SETS, target_table = "ESE_SETS", confirmOverwrite = T)
loadESEData(cxnObj = cxn, source_df = results_car$ESE_BASKETS, target_table = "ESE_BASKETS", confirmOverwrite = T)
loadESEData(cxnObj = cxn, source_df = results_car$ESE_CATCHES, target_table = "ESE_CATCHES", confirmOverwrite = T)
loadESEData(cxnObj = cxn, source_df = results_car$ESE_SPECIMENS, target_table = "ESE_SPECIMENS", confirmOverwrite = T)
loadESEData(cxnObj = cxn, source_df = results_car$ESE_LV1_OBSERVATIONS, target_table = "ESE_LV1_OBSERVATIONS", confirmOverwrite = T)
}
#

sqlStatement <- paste0("select APHIAID, CODE AS CODE_MAR, SPEC, COMM, GULF,
                       APHIAID_SUGG, SPEC_SUGG, TSN
                       from GROUNDFISH.GSSPECIES_APHIAS")
ROracle::dbDisconnect(cxn$channel)

