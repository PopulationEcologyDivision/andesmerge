R.utils::sourceDirectory("c:/git/MMMcMahon/andesmerge/R/", modifiedOnly=F)
# loadData()
# Mar.datawrangling::get_data_custom(schema = 'groundfish', tables = "ESE_MISSIONS", data.dir = data.dir, env = .GlobalEnv)

oraCxn <- Mar.utils::make_oracle_cxn('rodbc', fn.oracle.dsn = "PTRAN_64")
# library(andesmerge)
eseExtractor(cxnObj = oraCxn, mission = "CAR2021240")
#QC
integrityCheck(cxnObj = oraCxn, mission = "CAR2021240")


# oraCxn2 <- Mar.utils::make_oracle_cxn('roracle', fn.oracle.dsn = "PTRAN")

toOracle(cxnObj = oraCxn, source_df = ESE_BASKETS, target_table = "ESE_BASKETS", target_schema = "mcmahonm", createReplaceTarget = F)


