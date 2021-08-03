#' Player level application of SMP framework
#' 
#' Applies the SMP framework on an individual basis.
#' @param player_df A Catapult 10Hz GPS dataframe of a singular player for a singular period of any length.
#' @param dictionary_df A standardised movement unit dictionary; built using the create_movement_unit_dictionary() function. 
#' @param velocity_thresholds A numeric vector representing the velocity time series threshold values which
#' correspond to the velocity descriptors supplied to the movement unit dictionary.
#' @param acceleration_thresholds A numeric vector representing the acceleration time series threshold values which
#' correspond to the acceleration descriptors supplied to the movement unit dictionary. Use c(min(player_df$Acceleration))
#' for the first observation to avoid errors - see example.
#' @param threshold_value A numeric value representing the change point velocity.
#' @param number_of_clusters A numeric value with the initial number of clusters the user wants for the Hierarchical cluster
#' analysis.
#' @param movement_unit_subsequence_duration A numeric value representing the minimum number of movement units required for
#' a movement subsequence to be included in the analysis.
#' @param movement_unit_subsequence_length A character string, either "long" or "short" which pertains to the repeated 
#' movement units within a movement unit subsequence.
#' @return A list of outputs including the original data, the individuals movement unit dictionary with summary stats, 
#' the frequent SMP and movement unit subsequences with summary stats.
#' @examples 
#' my_SMP_data <- SMP_framework_player_level(player_df = my_df, dictionary_df = my_dictionary,
#'                                           velocity_thresholds = c(0.00, 1.69, 3.90, 4.99), acceleration_thresholds = c(min(player_df$Acceleration), -0.20, 0.20),
#'                                           threshold_value = 1.20, number_of_clusters = 25, movement_unit_subsequence_duration = 50,
#'                                           movement_unit_subsequence_length = "long")
#' @export
# -------------------- SMP framework player level application ------------- 
SMP_framework_player_level <- function(player_df, dictionary_df, 
                                       velocity_thresholds, acceleration_thresholds, 
                                       threshold_value, number_of_clusters, 
                                       movement_unit_subsequence_duration, movement_unit_subsequence_length) {
  
  # Data quality check ----------------------
  threshold_value <- as.numeric(threshold_value)
  number_of_clusters <- as.numeric(number_of_clusters)
  movement_unit_subsequence_duration <- as.numeric(movement_unit_subsequence_duration)
  
  # Formation of player df ------------------
  player_df %<>%
    select(Long_1 = Longitude, Lat_1 = Latitude, Time = Seconds, Velocity, Acceleration) %>%
    distinct(Long_1, Lat_1, Time, Velocity, Acceleration, .keep_all =  TRUE) %>%
    mutate(Long_2 = lead(Long_1), Lat_2 = lead(Lat_1)) %>%
    filter(complete.cases(.))
  
  # Calculation of bearing ------------------
  player_df %<>%
    mutate(Bearing_1 = compute_bearing(lat_1 = Lat_1, lon_1 = Long_1, 
                                       lat_2 = Lat_2, lon_2 = Long_2),
           Bearing_2 = lead(Bearing_1)) %>%
    filter(complete.cases(.))
  
  # Calculation of turning angle ------------
  player_df %<>% 
    mutate(Turning_angle = compute_turning_angle(bearing_vec_1 = Bearing_1, 
                                                 bearing_vec_2 = Bearing_2)) %>%
    select(Time, Velocity, Acceleration, Turning_angle)
  
  # Setting movement descriptor thresholds --
  velocity_thresholds <- append(velocity_thresholds, max(player_df$Velocity), after = 4)
  acceleration_thresholds <- append(acceleration_thresholds, max(player_df$Acceleration), after = 3)
  turning_angle_thresholds <- c(0, 10.00, 45.00, 90.00, 180.00)
  
  # Formation of movement_units -------------
  player_df %<>%
    mutate(Velocity_descriptors = cut(Velocity, velocity_thresholds, 
                                      labels = c("Walk", "Jog", "Run", "Sprint"), include.lowest = T),
           Acceleration_descriptors = cut(Acceleration, acceleration_thresholds, 
                                          labels = c("Deceleration", "Neutral", "Acceleration"), include.lowest = T),
           Turning_angle_descriptors = cut(Turning_angle, turning_angle_thresholds, 
                                           labels = c("Straight", "Acute-Change", "Large-Change", "Backwards"), include.lowest = T),
           Movement_units = paste(Velocity_descriptors, Acceleration_descriptors, Turning_angle_descriptors, sep = "")) %>%
    left_join(., dictionary_df, "Movement_units") %>% 
    arrange(Time)
  
  # Formation of sub-sequences of MU --------
  movement_unit_subsequence_index_data <- data.frame(identify_index_ranges(player_df$Velocity, 
                                                                           threshold_value = threshold_value))
  
  movement_unit_subsequence_list <- list()
  for (i in 1:length(movement_unit_subsequence_index_data$starts)) {
    string_vector <- c(player_df$Movement_unit_characters[movement_unit_subsequence_index_data$starts[i]:movement_unit_subsequence_index_data$ends[i]])
    string_name <- paste("V", i)
    movement_unit_subsequence_list[[string_name]] <- string_vector
  }
  
  movement_unit_subsequence_list <- movement_unit_subsequence_list[sapply(movement_unit_subsequence_list, length) >= movement_unit_subsequence_duration]
  
  for (i in 1:length(movement_unit_subsequence_list)) {
    movement_unit_subsequence_list[[i]] <- str_c(movement_unit_subsequence_list[[i]], sep = "", collapse = "")
  }
  
  movement_unit_subsequence_df <- t(bind_rows(movement_unit_subsequence_list))
  
  # Controls for MU sub-sequence length -----
  if (movement_unit_subsequence_length == 'long') {
    movement_unit_subsequence_df <- movement_unit_subsequence_df
  } else {
    movement_unit_subsequence_df <- gsub("(.)\\1{1,}", "\\1", movement_unit_subsequence_df)
  }
  
  # Creates Levenshtein distance matrix -----
  movement_unit_subsequence_matrix <- adist(movement_unit_subsequence_df)
  movement_unit_subsequence_matrix  <- as.matrix(movement_unit_subsequence_matrix, labels = TRUE)
  colnames(movement_unit_subsequence_matrix) <- rownames(movement_unit_subsequence_matrix) <- movement_unit_subsequence_df
  
  # Creates hierarchical cluster model ------
  hierarchical_cluster_model <- hclust(as.dist(movement_unit_subsequence_matrix), method = "ward.D2")
  hierarchical_cluster_groups <- cutree(hierarchical_cluster_model, k = number_of_clusters)
  cluster_groups <- as.data.frame(unlist(hierarchical_cluster_groups))
  hierarchical_cluster_assignment_df <- as.data.frame(movement_unit_subsequence_df) %>%
    mutate(Group = cluster_groups[,1])
  
  # Reassigns single element hc groups ------
  hierarchical_cluster_reassignment_df <- hierarchical_cluster_reassignment(movement_unit_subsequence_matrix = movement_unit_subsequence_matrix, 
                                                                            hierarchical_cluster_assignment_df = hierarchical_cluster_assignment_df)
  
  # Implements the lcs algorithm ------------
  number_of_clusters <- length(unique(hierarchical_cluster_reassignment_df$Group))
  frequent_smp_df <- identify_frequent_SMP(movement_unit_subsequences = hierarchical_cluster_reassignment_df,
                                           number_of_clusters = number_of_clusters)
  
  # Creates dictionary df stats summary -----
  movement_unit_summary_df <- data.frame(Movement_unit_characters = player_df$Movement_unit_characters) %>%
    group_by(Movement_unit_characters) %>%
    summarise(Count = length(Movement_unit_characters)) %>%
    mutate(Percentage_contribution = round(((Count/sum(Count))*100), 2),
           Movement_unit_characters = as.character(Movement_unit_characters))
  
  dictionary_df <- merge(x = dictionary_df, y = movement_unit_summary_df, by = "Movement_unit_characters", all.x = TRUE)
  
  # Creates final list of outputs -----------
  frequent_smp_data <- list(player_df, dictionary_df, frequent_smp_df, data.frame(Movement_unit_subsequences = movement_unit_subsequence_df))
  names(frequent_smp_data) <- c("_Alpha_data", "_Movement_dictionary", "_Frequent_SMP", "_Movement_unit_subsequences")
  
  # Returns list of outputs to user ---------
  return(frequent_smp_data)
  
}