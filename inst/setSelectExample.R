# devtools::install_github('PopulationEcologyDivision\andesmerge')
library(Mar.utils)
library(andesmerge)
Georges_2024<- setSelect(MaritimesSurvey = "custom", stationData = "C:/Users/McMahonM/Downloads/Georges_5Z_Comp.csv", 
                         oceansAreas = "C:/git/PopulationEcologyDivision/andesmerge/inst/oceansAreas/oceansAreas.shp",
                         avoidShp = "C:/Users/McMahonM/OneDrive - DFO-MPO/Support/Groundfish/SetSelector/exclusionAreas/20230626/avoidAreas_0Buff_Dissolve.shp")