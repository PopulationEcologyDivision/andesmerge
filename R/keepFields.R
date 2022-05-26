#' @title keepFieldsMissions
#' @description This function subsets the andes cruise_data, retaining only the fields usable by the 
#' ESE_MISSIONS table
#' @param df default is \code{NULL}.  This is the andes cruise_data data frame  
#' @family internal
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
keepFieldsMissions <- function(df = NULL){
  df <- df[,c("mission_number", 
              "area_of_operation", 
              "description")]
  return(df)
}
#' @title keepFieldsSets
#' @description This function subsets the andes sets_data, retaining only the fields usable by the 
#' ESE_SETS table
#' @param df default is \code{NULL}.  This is the andes set_data data frame  
#' @param mission default is \code{NULL}. This is the cleaned mission id (e.g. CAR2022102)
#' @family internal
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
keepFieldsSets <- function(df = NULL, mission=NULL){
  df <- df[,c("set_number",
              "start_date",
              "end_date",
              "stratum",
              "start_latitude_DD",
              "start_latitude_MMmm",
              "start_longitude_DD",
              "start_longitude_MMmm",
              "end_latitude_DD",
              "end_latitude_MMmm",
              "end_longitude_DD",
              "end_longitude_MMmm",
              "distance_towed_obtained_code",
              "distance_towed",
              "ship_speed_obtained_code",
              "ship_speed",
              "start_depth_m",
              "end_depth_m",
              "wind_direction_degree",
              "wind_force_code",
              "tide_direction_code",
              "experiment_type",
              "set_result_id",
              "gear_type",
              "auxiliary_equipment",
              "port_warp",
              "remarks",
              "new_station")]
  
  df <- data.frame(MISSION = mission, df)
  return(df)
}
#' @title keepFieldsCatches
#' @description This function subsets the andes catches_data, retaining only the fields usable by 
#' the ESE_CATCHES table
#' @param df default is \code{NULL}.  This is the andes catch_data data frame  
#' @param mission default is \code{NULL}. This is the cleaned mission id (e.g. CAR2022102)
#' @family internal
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
keepFieldsCatches <- function(df = NULL, mission=NULL){
  df <- df[,c("set_number",
              "species_code",
              "notes",
              "unweighed_baskets",
              "specimen_count",
              "id",
              "is_parent",
              "parent_catch_id")]
  df <- data.frame(MISSION = mission, df)
  return(df)
}
#' @title keepFieldsBaskets
#' @description This function subsets the andes baskets_data, retaining only the fields usable by 
#' the ESE_BASKETS table
#' @param df default is \code{NULL}.  This is the andes basket_data data frame 
#' @param mission default is \code{NULL}. This is the cleaned mission id (e.g. CAR2022102) 
#' @family internal
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
keepFieldsBaskets <- function(df = NULL, mission=NULL){
  df <- df[,c("set_number",
              "species_code",
              "size_class",
              "basket_wt_kg",
              "id",
              "catch_id",
              "sampled")]
  df <- data.frame(MISSION = mission, df)
  return(df)
}
#' @title keepFieldsSpecimens
#' @description This function subsets the andes specimen_data, retaining only the fields usable by 
#' both the ESE_SPECIMENS and ESE_LV1_OBSERVATIONS table
#' @param df default is \code{NULL}.  This is the andes specimen_data data frame  
#' @param mission default is \code{NULL}. This is the cleaned mission id (e.g. CAR2022102)
#' @family internal
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
keepFieldsSpecimens <- function(df = NULL, mission=NULL){
  df <- df[,!names(df) %in% c("mission_number",
                              "size_class_display", 
                              "creation_date", 
                              "guessed_weight")]
  df <- data.frame(MISSION = mission, df)
  return(df)
}