#' @title keepFieldsMissions
#' @description This function subsets the andes cruise_data, retaining only the fields usable by the 
#' ESE_MISSIONS table
#' @param df default is \code{NULL}.  This is the andes cruise_data data frame  
#' @family internal
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
keepFieldsMissions <- function(df = NULL, new=T){
  if (!new){
  df <- df[,c("mission_number", 
              "area_of_operation", 
              "description")]
  }else{
    mission_number <- df[df$ï..KEY== "mission_number", "VALUE"]
    area_of_operation <- df[df$ï..KEY== "area_of_operation", "VALUE"]
    description <- df[df$ï..KEY== "description", "VALUE"]
    df <- data.frame("mission_number"=c(mission_number),"area_of_operation"=c(area_of_operation),"description"=c(description))
  }
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
                "start_latitude_dd",
                "start_longitude_dd",
              "end_latitude_dd",
              "end_longitude_dd",
                "distance_towed_obtained_code",
                "distance_towed", #was "distance_towed", #left this field in in case we need it for tweaks
                "calculated_distance_nm_xy", #was "crow_distance",
                "ship_speed",#was  "ship_speed",
                 "ship_speed_obtained_code",
                "start_depth_m",
                "end_depth_m",
                "wind_direction_degree",
                "wind_force_kts",
                "tide_direction_code",
                "experiment_type",
                "set_result",
              "valid_outcome",
                "gear_type",
                "auxiliary_equipment",
                "port_warp",
                "remarks",
                "station_number",
              "max_depth_m",
              "min_depth_m"
              )]
 
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
              "catch_code",
              "catch_uuid",
              "is_mixed_catch",
              "notes",
              "total_adjusted_basket_weight_kg",
              "unmeasured_specimen_count"
              ,"unweighed_baskets"
              )] 
  
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
  #ship_speed_obtained_code
  df <- df[,c("set_number",
              "catch_code",
              "basket_uuid",
              "basket_id",
              "catch_id",
              "was_sampled",
              "sample_class",
              "adjusted_basket_weight_kg",
              "measured_specimen_count",
              "adjusted_unmeasured_specimen_count"
              )] 

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
                              "guessed_weight",
                              "species_uuid",
                              "print.label",
                              "collect.specimen.w.fish.number")]
  df <- data.frame(MISSION = mission, df)
  return(df)
}