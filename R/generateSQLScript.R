#' @title script
#' @description This scripts generates the SQL queries needed to populate ANdes DB with data coming from ESE
#' @return nothing
#' @family internal
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
#' 

base = here::here()
dataDir = "/data/maritimes/"
# IMPORTANT.  THis ID needs to be triple checked as it will be used to create INERT statements in ANDES DB

# LET's create the SQL queries for the comparative survey
sampling.id  = 20 # comparative survey ID on ANDES DB

df = createInsertIntoSampReq(protocol = sampling.id,rawFile = "/data/maritimes/ESE_MSTR_SAMPREQ_SPECIES(combined).csv")
  write(df, file=paste0(base,dataDir,"SampReq", sampling.id, ",NEW.txt"))

df = createInsertIntoObsGrp(protocol = sampling.id,rawFile =  "/data/maritimes/ESE_SAMPREQ_OBS_GRPS_GB_NED2022.csv")
  write(df, file=paste0(base,dataDir,"Collections", sampling.id, ",NEW.txt"))

df = createInsertIntoObsFields(protocol = sampling.id,rawFile = "/data/maritimes/ESE_SAMPREQ_LV1_OBS_NED2022.csv" )
  write(df, file=paste0(base,dataDir,"Fields", sampling.id, ",NEW.txt"))


  # LET's create the SQL queries for the main survey
sampling.id  = 22 #  survey ID on ANDES DB

df = createInsertIntoSampReq(protocol = sampling.id,rawFile = "/data/maritimes/ESE_MSTR_SAMPREQ_SPECIES(combined).csv")
  write(df, file=paste0(base,dataDir,"SampReq", sampling.id, ",NEW.txt"))

df = createInsertIntoObsGrp(protocol = sampling.id,rawFile =  "/data/maritimes/COMP_ESE_MSTR_SAMPREQ_OBS_GRPS.csv")
  write(df, file=paste0(base,dataDir,"Collections", sampling.id, ",NEW.txt"))

df = createInsertIntoObsFields(protocol = sampling.id,rawFile = "/data/maritimes/COMP_ESE_MSTR_SAMPREQ_LV1_OBS.csv" )
  write(df, file=paste0(base,dataDir,"Fields", sampling.id, ",NEW.txt"))
  