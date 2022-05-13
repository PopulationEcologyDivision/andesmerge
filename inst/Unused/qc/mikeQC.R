# Mar.qcdata tests
R.utils::sourceDirectory("c:/git/MMMcMahon/andesmerge/R/", modifiedOnly=F)
# extract data for a survey locally
oraCxn <- Mar.utils::make_oracle_cxn('rodbc', fn.oracle.dsn = "PTRAN_64")
testMike = eseExtractor(cxnObj = oraCxn, mission = "NED2020025")# TEL2021002# NED2020025

test=Mar.qcdata::qc_outlierCheck(df=testmike$ESE_CATCHES, field = "NUMBER_CAUGHT")