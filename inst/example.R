#run table refresher
tt<-matchAndesToESE(dataPath = "c:/Users/McMahonM/OneDrive - DFO-MPO/Support/Groundfish/Andes/spring2022Data/")


cxn <- Mar.utils::make_oracle_cxn(fn.oracle.username = groundfish.username,fn.oracle.password = groundfish.password, fn.oracle.dsn = "PTRAN", usepkg = 'roracle')
toOracle(cxnObj = cxn, source_df = tt$ESE_MISSIONS, target_schema = "groundfish", target_table = "ANDESE_MISSIONS", createReplaceTarget = F)
toOracle(cxnObj = cxn, source_df = tt$ESE_SETS, target_schema = "groundfish", target_table = "ANDESE_SETS", createReplaceTarget = F)
toOracle(cxnObj = cxn, source_df = tt$ESE_BASKETS, target_schema = "groundfish", target_table = "ANDESE_BASKETS", createReplaceTarget = F)
toOracle(cxnObj = cxn, source_df = tt$ESE_CATCHES, target_schema = "groundfish", target_table = "ANDESE_CATCHES", createReplaceTarget = F)
toOracle(cxnObj = cxn, source_df = tt$ESE_SPECIMENS, target_schema = "groundfish", target_table = "ANDESE_SPECIMENS", createReplaceTarget = F)
toOracle(cxnObj = cxn, source_df = tt$ESE_LV1_OBSERVATIONS, target_schema = "groundfish", target_table = "ANDESE_LV1_OBSERVATIONS", createReplaceTarget = F)


