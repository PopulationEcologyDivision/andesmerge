R.utils::sourceDirectory("c:/git/MMMcMahon/andesmerge/R/", modifiedOnly=F)
setwd("c:/git/MMMcMahon/andesmerge/")
# loadData()
Mar.datawrangling::get_data_custom(schema = 'groundfish', tables = "ESE_MISSIONS", data.dir = data.dir, env = .GlobalEnv)

library(andesmerge)
andesmerge::eseExtractor(cxnObj = oraCxn, mission = "CAR2021240")
tt<-sapply(X=getEseTables(), FUN=checkPrimaryFields)


oraCxn <- Mar.utils::make_oracle_cxn('<stuff>')
toOracle(cxnObj = oraCxn, source_df = ESE_BASKETS, target_table = "ESE_BASKETS", target_schema = "mcmahonm", createReplaceTarget = F)


