keepFieldsMissions <- function(df = NULL){
  df <- df[,c("mission_number", 
              "area_of_operation", 
              "description")]
  return(df)
}
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
              "gear_type",
              "auxiliary_equipment",
              "port_warp",
              "remarks",
              "new_station")]
  
  df <- data.frame(MISSION = mission, df)
  return(df)
}
keepFieldsCatches <- function(df = NULL, mission=NULL){
  df <- df[,c("set_number",
              "species_code",
              "notes",
              "unweighed_baskets",
              "specimen_count",
              "is_parent",
              "parent_catch_id")]
  df <- data.frame(MISSION = mission, df)
  return(df)
}
keepFieldsBaskets <- function(df = NULL, mission=NULL){
  df <- df[,c("set_number",
              "species_code",
              "size_class",
              "basket_wt_kg",
              "catch_id",
              "sampled")]
  df <- data.frame(MISSION = mission, df)
  return(df)
}
keepFieldsSpecimens <- function(df = NULL, mission=NULL){
  df <- df[,!names(df) %in% c("mission_number",
                              "size_class_display", 
                              "creation_date", 
                              "guessed_weight",
                              "basket")]
  df <- data.frame(MISSION = mission, df)
  return(df)
}