setwd("C:/git/PopulationEcologyDivision/andesmerge/inst/dataRaw/")

SPRING_4VW  = utils::read.csv(file = "Spring_4VW.csv")
SPRING_4X   = utils::read.csv(file = "Spring_4X.csv")
SUMMER_4VWX = utils::read.csv(file = "Summer_4VWX.csv")
GEORGES_5Z  = utils::read.csv(file = "Georges_5Z.csv")

usethis::use_data(SPRING_4VW, SPRING_4VW, overwrite = T)
usethis::use_data(SPRING_4X, SPRING_4X, overwrite = T)
usethis::use_data(SUMMER_4VWX, SUMMER_4VWX, overwrite = T)
usethis::use_data(GEORGES_5Z, GEORGES_5Z, overwrite = T)