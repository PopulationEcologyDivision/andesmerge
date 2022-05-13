R.utils::sourceDirectory("c:/git/MMMcMahon/andesmerge/R/", modifiedOnly=F)
rm(list=ls(pattern="tmp_"))
rm(list=ls(pattern="ESE_"))

# This works for pulling csv files into, but the sampledata is gulf
Pablo <- matchAndesToESE(quiet = F)
list2env(Pablo,globalenv())

# extract data for a survey locally
oraCxn <- Mar.utils::make_oracle_cxn('rodbc', fn.oracle.dsn = "PTRAN_64")
testMike = eseExtractor(cxnObj = oraCxn, mission = "NED2020025")# TEL2021002# NED2020025
sketchyRecs <- integrityCheck(eseList = testMike)

mike = eseJoiner(testMike)

# toOracle(cxnObj=oraCxn, source_df = testmike$ESE_MISSIONS, target_schema="mcmahonm", target_table = "ESE_MISSIONS", createReplaceTarget=T)
# toOracle(cxnObj=oraCxn, source_df = testmike$ESE_BASKETS, target_schema="mcmahonm", target_table = "ESE_BASKETS", createReplaceTarget=T)
# toOracle(cxnObj=oraCxn, source_df = testmike$ESE_SETS, target_schema="mcmahonm", target_table = "ESE_SETS", createReplaceTarget=T)
# toOracle(cxnObj=oraCxn, source_df = testmike$ESE_CATCHES, target_schema="mcmahonm", target_table = "ESE_CATCHES", createReplaceTarget=T)
# toOracle(cxnObj=oraCxn, source_df = testmike$ESE_SPECIMENS, target_schema="mcmahonm", target_table = "ESE_SPECIMENS", createReplaceTarget=T)
# toOracle(cxnObj=oraCxn, source_df = testmike$ESE_LV1_OBSERVATIONS, target_schema="mcmahonm", target_table = "ESE_LV1_OBSERVATIONS", createReplaceTarget=T)
#OK - imagine all is good
lapply()
library(Mar.qcdata)

test=Mar.qcdata::qc_outlierCheck(df=testmike$ESE_CATCHES, field = testmike$ESE_CATCHES$NUMBER_CAUGHT)


head(qcEnv$ESE_MISSIONS)
head(qcEnv$ESE_SETS[qcEnv$ESE_SETS$SETNO==1,1:10])
head(qcEnv$ESE_CATCHES[qcEnv$ESE_CATCHES$SETNO==1 & qcEnv$ESE_CATCHES$SPEC==2511,])
head(qcEnv$ESE_BASKETS[qcEnv$ESE_BASKETS$SETNO==1 & qcEnv$ESE_BASKETS$SPEC==2511,])
head(qcEnv$ESE_SPECIMENS[qcEnv$ESE_SPECIMENS$SETNO==1 & qcEnv$ESE_SPECIMENS$SPEC==2511,])
head(qcEnv$ESE_LV1_OBSERVATIONS[qcEnv$ESE_LV1_OBSERVATIONS$SETNO==1 & qcEnv$ESE_LV1_OBSERVATIONS$SPEC==2511,])

# oraCxn2 <- Mar.utils::make_oracle_cxn('roracle', fn.oracle.dsn = "PTRAN")

toOracle(cxnObj = oraCxn, source_df = ESE_BASKETS, target_table = "ESE_BASKETS", target_schema = "mcmahonm", createReplaceTarget = F)


