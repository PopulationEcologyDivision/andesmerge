library(devtools)
library(dplyr)
library(Mar.utils)
library(ROracle)
rm(list = c("matchAndesToESE", "tweakBaskets", "tweakBasketsPostProcessing", "tweakCatches", "tweakLv1", "tweakSets",
            "tweakSpecimensRaw", "tweakUniversal"))

devtools::load_all()
cxn <- ROracle::dbConnect( DBI::dbDriver("Oracle"), groundfish.username,groundfish.password,oracle.dsn)
# cxn <- make_oracle_cxn(fn.oracle.username = groundfish.username,fn.oracle.password = groundfish.password, fn.oracle.dsn = "PTRAN", usepkg = 'roracle')
Mar.utils::get_data_tables("GROUNDFISH", cxn = cxn,  data.dir = "c:/DFO-MPO/PESDData/MarDatawrangling/", tables = "GSSPECIES_ANDES")
# GSSPECIES_ANDES <- "mork"
theData = "C:/Users/mcmahonm/OneDrive - DFO-MPO/Emberley, Jamie (DFO_MPO)'s files - CAB2026002_Mike"
# options(warn = 1)
results <-matchAndesToESE(dataPath = theData, gulfCodes=F, spCodes= GSSPECIES_ANDES)
 
loadESEData(cxnObj = cxn, source_df = results$ESE_MISSIONS,         target_table = "ESE2026_MISSIONS", confirmOverwrite = T)
loadESEData(cxnObj = cxn, source_df = results$ESE_SETS,             target_table = "ESE2026_SETS", confirmOverwrite = T)
loadESEData(cxnObj = cxn, source_df = results$ESE_BASKETS,          target_table = "ESE2026_BASKETS", confirmOverwrite = T)
loadESEData(cxnObj = cxn, source_df = results$ESE_CATCHES,          target_table = "ESE2026_CATCHES", confirmOverwrite = T)
loadESEData(cxnObj = cxn, source_df = results$ESE_SPECIMENS,        target_table = "ESE2026_SPECIMENS", confirmOverwrite = T)
loadESEData(cxnObj = cxn, source_df = results$ESE_LV1_OBSERVATIONS, target_table = "ESE2026_LV1_OBSERVATIONS", confirmOverwrite = T)

theData_tel = "C:/Users/McMahonM/OneDrive - DFO-MPO/TEL2024002_ANDES_CSV/"
results_tel <-matchAndesToESE(dataPath = theData_tel, gulfCodes=F)
cxn <- Mar.utils::make_oracle_cxn(fn.oracle.username = groundfish.username,fn.oracle.password = groundfish.password, fn.oracle.dsn = "PTRAN", usepkg = 'roracle')
loadESEData(cxnObj = cxn, source_df = results_tel$ESE_MISSIONS,         target_table = "TEL_ESE_MISSIONS", confirmOverwrite = T)
loadESEData(cxnObj = cxn, source_df = results_tel$ESE_SETS,             target_table = "TEL_ESE_SETS", confirmOverwrite = T)
loadESEData(cxnObj = cxn, source_df = results_tel$ESE_BASKETS,          target_table = "TEL_ESE_BASKETS", confirmOverwrite = T)
loadESEData(cxnObj = cxn, source_df = results_tel$ESE_CATCHES,          target_table = "TEL_ESE_CATCHES", confirmOverwrite = T)
loadESEData(cxnObj = cxn, source_df = results_tel$ESE_SPECIMENS,        target_table = "TEL_ESE_SPECIMENS", confirmOverwrite = T)
loadESEData(cxnObj = cxn, source_df = results_tel$ESE_LV1_OBSERVATIONS, target_table = "TEL_ESE_LV1_OBSERVATIONS", confirmOverwrite = T)



if (F){
loadESEData(cxnObj = cxn, source_df = results$ESE_MISSIONS, target_table = "ESE_MISSIONS", confirmOverwrite = T)
loadESEData(cxnObj = cxn, source_df = results$ESE_SETS, target_table = "ESE_SETS", confirmOverwrite = T)
loadESEData(cxnObj = cxn, source_df = results$ESE_BASKETS, target_table = "ESE_BASKETS", confirmOverwrite = T)
loadESEData(cxnObj = cxn, source_df = results$ESE_CATCHES, target_table = "ESE_CATCHES", confirmOverwrite = T)
loadESEData(cxnObj = cxn, source_df = results$ESE_SPECIMENS, target_table = "ESE_SPECIMENS", confirmOverwrite = T)
loadESEData(cxnObj = cxn, source_df = results$ESE_LV1_OBSERVATIONS, target_table = "ESE_LV1_OBSERVATIONS", confirmOverwrite = T)
}
#

sqlStatement <- paste0("select APHIAID, CODE AS CODE_MAR, SPEC, COMM, GULF,
                       APHIAID_SUGG, SPEC_SUGG, TSN
                       from GROUNDFISH.GSSPECIES_APHIAS")
ROracle::dbDisconnect(cxn$channel)

