sourcery()
library(dplyr)
#run table refresher
tt<-matchAndesToESE(dataPath = "c:/Users/McMahonM/OneDrive - DFO-MPO/Support/Groundfish/Andes/summer2022Data/TELEOST/")

cxn <- Mar.utils::make_oracle_cxn(fn.oracle.username = groundfish.username,fn.oracle.password = groundfish.password, fn.oracle.dsn = "PTRAN", usepkg = 'roracle')

loadESEData(cxnObj = cxn, source_df = tt$ESE_MISSIONS, target_table = "ANDESE_MISSIONS")
loadESEData(cxnObj = cxn, source_df = tt$ESE_SETS, target_table = "ANDESE_SETS")
loadESEData(cxnObj = cxn, source_df = tt$ESE_BASKETS, target_table = "ANDESE_BASKETS")
loadESEData(cxnObj = cxn, source_df = tt$ESE_CATCHES, target_table = "ANDESE_CATCHES")
loadESEData(cxnObj = cxn, source_df = tt$ESE_SPECIMENS, target_table = "ANDESE_SPECIMENS")
loadESEData(cxnObj = cxn, source_df = tt$ESE_LV1_OBSERVATIONS, target_table = "ANDESE_LV1_OBSERVATIONS")

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
