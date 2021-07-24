# -------------------- Applies SMP framework function ---------------------
apply_SMP_framework <- function(my_list, velocity_thresholds, threshold_value, 
                                movement_unit_subsequence_duration, movement_unit_subsequence_length, number_of_clusters) {
  
  output_list <- list()
  dictionary_df <- movement_unit_dictionary(default = TRUE)
  
  progress_bar <- txtProgressBar(min = 0, max = length(my_list), initial = 1, style = 3)
  
  # Applies SMP framework function to each df in list
  for(i in seq_along(my_list)) {
    
    player_df <- my_list[[i]] %>%
      data.frame()
    
    player_id <- unique(player_df$Player_name)
    acceleration_thresholds <- c(min(player_df$Acceleration), -0.20, 0.20)
    
    SMP_output <- SMP_framework(player_df = player_df,
                                dictionary_df = dictionary_df,
                                velocity_thresholds = velocity_thresholds, 
                                acceleration_thresholds = acceleration_thresholds,
                                threshold_value = threshold_value,
                                movement_unit_subsequence_duration = movement_unit_subsequence_duration,
                                movement_unit_subsequence_length = movement_unit_subsequence_length,
                                number_of_clusters = number_of_clusters)
    
    output_list[[player_id]] <- SMP_output
    setTxtProgressBar(progress_bar, i)
  }
  
  return(output_list)
  
}