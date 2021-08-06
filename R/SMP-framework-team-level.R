#' Team level application of the SMP framework
#' 
#' Applies the SMP framework to a list of Catapult 10Hz GPS dataframes.
#' @param my_list A list of Catapult 10Hz GPS dataframes of multiple players for a single match or training drill of any length.
#' @param velocity_thresholds A numeric vector representing the velocity time series threshold values which
#' correspond to the velocity descriptors supplied to the movement unit dictionary.
#' @param threshold_value A numeric value representing the change point velocity.
#' @param movement_unit_subsequence_duration A numeric value representing the minimum number of movement units required for
#' a movement subsequence to be included in the analysis.
#' @param movement_unit_subsequence_length A character string, either "long" or "short" which pertains to the repeated 
#' movement units within a movement unit subsequence.
#' @param number_of_clusters A numeric value with the initial number of clusters the user wants for the Hierarchical cluster
#' analysis.
#' @return A list of outputs including the original data, the individuals movement unit dictionary with summary stats, 
#' the frequent SMP and movement unit subsequences with summary stats for all players.
#' @examples 
#' SMP_data <- apply_SMP_framework(my_list = gps_data_list, velocity_thresholds = c(0.00, 1.69, 3.90, 4.99),
#'                                 threshold_value = 1.20, locomotive_event_subsequence_duration = 50, 
#'                                 locomotive_event_subsequence_length = "long", number_of_clusters = 25)
#' @export
# -------------------- Applies SMP framework function ---------------------
SMP_framework_team_level <- function(my_list, velocity_thresholds, threshold_value, 
                                     movement_unit_subsequence_duration, movement_unit_subsequence_length, number_of_clusters) {
  
  output_list <- list()
  dictionary_df <- create_movement_unit_dictionary(default = TRUE)
  
  progress_bar <- txtProgressBar(min = 0, max = length(my_list), initial = 1, style = 3)
  
  # Applies SMP framework function to each df in list
  for(i in seq_along(my_list)) {
    
    player_df <- my_list[[i]] %>%
      data.frame()
    
    player_id <- unique(player_df$Player_name)
    acceleration_thresholds <- c(min(player_df$Acceleration), -0.20, 0.20)
    
    SMP_output <- SMP_framework_player_level(player_df = player_df,
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