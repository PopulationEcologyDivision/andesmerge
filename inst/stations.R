Summer2023Stations <- xlsx::read.xlsx("C:/Users/McMahonM/Downloads/2023_Summer_Station_Selection_Count.xlsx", sheetIndex = 1)
write.csv(x = Summer2023Stations, file = "C:/Users/McMahonM/Downloads/2023_Summer_Station_Selection_Count.csv", row.names = F)
library(andesmerge)
setSelect(MaritimesSurvey = "custom", stationData = "C:/Users/McMahonM/Downloads/2023_Summer_Station_Selection_Count.csv")
