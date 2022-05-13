#run table refresher
tt<-matchAndesToESE(dataPath = "c:/Users/McMahonM/OneDrive - DFO-MPO/Support/Groundfish/Andes/spring2022Data/")


cxn <- Mar.utils::make_oracle_cxn(fn.oracle.username = groundfish.username,fn.oracle.password = groundfish.password, fn.oracle.dsn = "PTRAN", usepkg = 'roracle')
replaceESEData(cxnObj = cxn, source_df = tt$ESE_MISSIONS, target_schema = "groundfish", target_table = "ANDESE_MISSIONS")
replaceESEData(cxnObj = cxn, source_df = tt$ESE_SETS, target_schema = "groundfish", target_table = "ANDESE_SETS")
replaceESEData(cxnObj = cxn, source_df = tt$ESE_BASKETS, target_schema = "groundfish", target_table = "ANDESE_BASKETS")
replaceESEData(cxnObj = cxn, source_df = tt$ESE_CATCHES, target_schema = "groundfish", target_table = "ANDESE_CATCHES")
replaceESEData(cxnObj = cxn, source_df = tt$ESE_SPECIMENS, target_schema = "groundfish", target_table = "ANDESE_SPECIMENS")
replaceESEData(cxnObj = cxn, source_df = tt$ESE_LV1_OBSERVATIONS, target_schema = "groundfish", target_table = "ANDESE_LV1_OBSERVATIONS")

