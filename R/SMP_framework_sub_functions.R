#' Removes string elements
#' 
#' Removes chosen string element or pattern from a list of file names.
#' @param file_names A character string which represents a file name.
#' @param pattern_to_remove A character string that the user wants removed. 
#' @return A character string with chosen elements removed.
#' @examples 
#' my_string <- remove_string_elements(file_names = "participant_one.csv", pattern_to_remove = ".csv")
#' @export
# -------------------- Remove chosen string elements from file name -------
remove_string_elements <- function(file_names, pattern) {
  
  if (pattern == "") {
    
    # Checks for null entry -------------------
    file_names_updated <- file_names
    return(file_names)
  } else {
    # Evaluates the file names & removes the pattern
    file_names_updated <- c()
    for (i in 1:length(file_names)) {
      filtered_file_name <- str_remove(file_names[i], pattern)
      filtered_file_name <- gsub('[0-9]+', '', filtered_file_name)
      filtered_file_name <- str_replace(filtered_file_name, " .csv", ".csv")
      file_names_updated <- c(file_names_updated, filtered_file_name)
    }
    
    return(file_names_updated)
    
  }
  
}

#' Read GPS files function
#' 
#' Reads in Catapult 10Hz GPS files from a source directory and removes file type extensions to create a list of player/ position names.
#' @param gps_file_names A character vector of the names of GPS files in the named directory.
#' @param pattern_to_remove A character string that the user wants removed from the list of file names. 
#' @return A list of Catapult 10Hz GPS dataframes.
#' @examples 
#' my_gps_data_list <- read_gps_files(gps_file_names = list.files(pattern = "*.csv"), pattern_to_remove = "")
#' @export
# -------------------- Filter + read GPS file names -----------------------
read_gps_files <- function(gps_file_names, pattern_to_remove) {
  
  gps_data_list <- list()
  
  for(i in seq_along(gps_file_names)) {
    file_name <- gps_file_names[i]
    
    # Removes user selected pattern from files
    filtered_file_name <- remove_string_elements(file_name,
                                                 pattern = pattern_to_remove)
    # Creates player name column and renames dataframes
    gps_dataframe <- data.table::fread(file_name, skip = 8) %>%
      dplyr::mutate(File_name = gsub(".csv", "", basename(filtered_file_name))) %>%
      tidyr::separate(File_name, c('z5', 'z6')," ") %>%
      dplyr::mutate(Player_name = paste(z5, z6))
    
    player_name <- unique(gps_dataframe$Player_name)
    gps_data_list[[player_name]] <- data.frame(gps_dataframe)
  }
  
  return(gps_data_list)
  
}

#' Creates movement unit dictionary
#' 
#' Creates a standardised movement unit dictionary for use in the SMP framework.
#' @param default If default is = TRUE the function builds default movement unit dictionary.
#' @param velocity_label A character string vector containing velocity descriptors. 
#' The default argument is c("Walk", "Jog", "Run", "Sprint").
#' @param acceleration_label A character string vector containing acceleration descriptors.
#' The default argument is c("Deceleration", "Neutral", "Acceleration").
#' @param turning_angle_label A character string vector containing turning angle descriptors.
#' The default argument is c("Straight", "Acute-Change", "Large-Change", "Backwards").
#' @return A standardised movement unit dictionary dataframe for use in the SMP framework.
#' @examples 
#' my_movement_dictionary <- create_movement_unit_dictionary(default = TRUE)
#' @export
# -------------------- Movement unit dictionary formation function --------
create_movement_unit_dictionary <- function(default, velocity_label, acceleration_label, turning_angle_label) {
  
  # Default check & vector assignment -------
  if (default == TRUE) {
    velocity_label <- c("Walk", "Jog", "Run", "Sprint")
    acceleration_label <- c("Deceleration", "Neutral", "Acceleration")
    turning_angle_label <- c("Straight", "Acute-Change", "Large-Change", "Backwards")
  }
  
  # Formation of mu dictionary --------------
  var_1 <- velocity_label
  var_2 <- acceleration_label
  var_3 <- turning_angle_label
  
  dictionary_df <- expand.grid(Velocity_descritptors = var_1, 
                               Acceleration_descritptors = var_2, 
                               Turning_angle_descritptors = var_3)
  
  n <- as.numeric(tally(dictionary_df) - length(letters))
  
  dictionary_df %<>%
    arrange(factor(Velocity_descritptors, levels = var_1), 
            factor(Acceleration_descritptors, levels = var_2),
            factor(Turning_angle_descritptors, levels = var_3)) %>%
    within(Locomotive_events <- paste(Velocity_descritptors, 
                                      Acceleration_descritptors, 
                                      Turning_angle_descritptors, sep = "")) %>%
    mutate(Locomotive_event_characters = as.character(c(letters, LETTERS[1:n]))) %>%
    select(Locomotive_events, Locomotive_event_characters) %>%
    as.data.frame()
  
  # Error message checks --------------------
  error_check_1 <- if_else(as.numeric(length(var_1)) != 4.00, TRUE, FALSE)
  error_check_2 <- if_else(as.numeric(length(var_2)) != 3.00, TRUE, FALSE)
  error_check_3 <- if_else(as.numeric(length(var_3)) != 4.00, TRUE, FALSE)
  error_checker <- c(error_check_1, error_check_2, error_check_3)
  
  error_warning <- c()
  for (i in seq_along(error_checker)) {
    element_logical <- error_checker[i]
    if (element_logical == TRUE) {
      error_indicator <- 1.00
    } else {
      error_indicator <- 0.00
    }
    error_warning <- c(error_warning, error_indicator)
  }
  
  # Function output control statement -------
  if (sum(error_warning) > 0) {
    return(print("Error! Too many vector elements provided in descriptor labels"))
  } else {
    return(dictionary_df)
  }
  
}

#' Bearing computation
#' 
#' Computes the bearing or heading angle between a pair of decimal degree geospatial coordinates.
#' @param lat_1 The decimal degree latitude value of coordinate one.
#' @param lon_1 The decimal degree longitude value of coordinate one.
#' @param lat_2 The decimal degree latitude value of coordinate two.
#' @param lon_2 The decimal degree longitude value of coordinate two.
#' @return The absolute bearing or heading angle from coordinate one to coordinate two in degrees.
#' @examples 
#' my_bearing <- compute_bearing(lat_1 = 8.46696, lon_1 = -17.03663, lat_2 = 32.918481, lon_2 = 11.96337)
#' @export
# -------------------- Bearing computation function -----------------------
compute_bearing <- function(lat_1, lon_1, lat_2, lon_2) {
  
  # Conversion of degrees to radians --------
  lat_1 <- lat_1 * pi / 180
  lat_2 <- lat_2 * pi / 180
  lon_1 <- lon_1 * pi / 180
  lon_2 <- lon_2 * pi / 180
  
  # Bearing computation ---------------------
  x <- cos(lat_2) * sin(lon_2 - (lon_1))
  y <- cos(lat_1) * sin(lat_2) - sin(lat_1) * cos(lat_2) * cos(lon_2 - (lon_1))
  bearing <- atan2(x, y) * (180 / pi)
  
  return(abs(bearing))
  
}

#' Turning angle computation
#' 
#' Computes the turning angle of an object.
#' @param bearing_vec_1 A vector or numeric value of bearing angles.
#' @param bearing_vec_2 A vector or numeric value of bearing angles, one observation ahead of bear_vec_1.
#' @return The absolute turning angle or the change in bearing angles of an object in degrees.
#' @examples 
#' my_turning_angle <- compute_turning_angle(bearing_vec_1 = 90, bearing_vec_2 = 60)
#' @export
# -------------------- Turning angle computation function -----------------
compute_turning_angle <- function(bearing_vec_1, bearing_vec_2) {
  
  # Turning angle computation ---------------
  turning_angle <- (bearing_vec_2) - (bearing_vec_1)
  turning_angle <- abs(round(turning_angle, 2))
  
  return(turning_angle)
  
}

#' Identify index ranges
#' 
#' Identifies the movement unit subsequence index's by delineating
#' the velocity vector time-series using the user selected threshold value.
#' @param velocity_vector A participants velocity vector time-series.
#' @param threshold_value A numeric value representing the change point velocity.
#' @return The start and end index values of the movement unit subsequences.
#' @examples 
#' my_index_data <- identify_index_ranges(velocity_vector = player_df$Velocity, 
#'                                        threshold_value = 1.20)
#' @export
# -------------------- Sub-sequence index identifier function -------------
identify_index_ranges <- function(velocity_vector, threshold_value) {
  
  index_range <- which(velocity_vector > threshold_value)
  n <- length(index_range)
  
  ind <- which(index_range[-1] - index_range[-n] > 1)
  
  starts <- c(index_range[1], index_range[ind + 1])
  ends <- c(index_range[ind], index_range[n])
  
  cbind(starts,ends)
  
}

#' Hierarchical cluster reassignment function
#' 
#' Reprocesses hierarchical cluster results by identifying single element clusters and
#' reassigning the single elements to the next most similar cluster.
#' @param movement_unit_subsequence_matrix A Levenshtein distance matrix of a participants movement unit subsequences.
#' @param hierarchical_cluster_assignment_df A dataframe composed of a participants movement unit subsequences & hierarchical cluster groups.
#' @return A dataframe containing the supplied movement unit subsequences & their newly associated cluster groups.
#' @examples 
#' my_reassigned_clusters <- hierarchical_cluster_reassignment(movement_unit_subsequence_matrix = player_matrix,
#'                                                             hierarchical_cluster_assignment_df = player_hclust_df)
#' @export
# -------------------- Hierarchical cluster reassignment function  --------
hierarchical_cluster_reassignment <- function(movement_unit_subsequence_matrix, hierarchical_cluster_assignment_df) {
  
  cluster_ids <- which(table(hierarchical_cluster_assignment_df$Group) != 1)
  single_cluster_items <- as.character(hierarchical_cluster_assignment_df[!(hierarchical_cluster_assignment_df$Group %in% cluster_ids), 1])
  
  selection_statistics_list <- list()
  
  for(i in 1:length(single_cluster_items)) {
    
    cluster_stats <- NULL
    
    for(cluster in cluster_ids) {
      multiple_cluster_items <- as.character(hierarchical_cluster_assignment_df[hierarchical_cluster_assignment_df$Group %in% cluster, 1])
      distance_matrix <- movement_unit_subsequence_matrix[single_cluster_items[i], multiple_cluster_items]
      cluster_stats <- rbind(cluster_stats, c(cluster, mean(distance_matrix), min(distance_matrix), max(distance_matrix)))
    }
    
    selection_statistics_list[[single_cluster_items[i]]] <- cluster_stats
    
  }
  
  new_cluster <- c()
  
  for (i in seq_along(selection_statistics_list)) {
    cluster_stats <- selection_statistics_list[[i]]
    new_cluster <- c(new_cluster, cluster_stats[which.min(cluster_stats[,2]), 1])
  }
  
  new_clusters_assigment <- cbind(names(selection_statistics_list), as.numeric(new_cluster))
  ids <- which(hierarchical_cluster_assignment_df$V1 %in% single_cluster_items)
  hierarchical_cluster_assignment_df$Group[ids] <- as.numeric(new_cluster)
  
  hierarchical_cluster_reassignment_df <- hierarchical_cluster_assignment_df
  
  return(hierarchical_cluster_reassignment_df)
  
}

#' Identify frequent SMP
#' 
#' Applies the longest common subsequence algorithm to the movement unit subsequences to identify
#' the frequent SMP within each cluster group.
#' @param movement_unit_subsequences A dataframe containing the movement unit subsequences and corresponding hcluster group number.
#' @param number_of_clusters The number of unique cluster groups contained in the movement unit subsequences dataframe.
#' @return A dataframe containing frequent SMP identified by the LCS algorithm and summary statistics.
#' @examples 
#' number_of_clusters <- length(unique(hierarchical_cluster_reassignment_df$Group))
#' frequent_smp_df <- identify_frequent_SMP(movement_unit_subsequences = hierarchical_cluster_reassignment_df, 
#'                                          number_of_clusters = number_of_clusters)
#' @export
# -------------------- Frequent SMP identifier function -------------------
identify_frequent_SMP <- function(movement_unit_subsequences, number_of_clusters) { 
  
  # Formation of vector objects -------------
  frequent_smp <- c()
  hierarchical_cluster_group <- c()
  fsmp_count <- c()
  percentage_contribution <- c()
  group_number <- unique(movement_unit_subsequences$Group)
  
  # Application of lcs algorithm ------------
  for(i in 1:number_of_clusters) {
    n <- group_number[i]
    temp_df <- movement_unit_subsequences[movement_unit_subsequences$Group == n, 1]
    temp_df <- as.character(temp_df) 
    lcs <- LCSn(temp_df)
    count <- length(temp_df)
    
    if (lcs == "") {
      lcs <- NA
      count <- NA
    }
    
    fsmp_count <- c(fsmp_count, count)
    frequent_smp <- c(frequent_smp, lcs)
    hierarchical_cluster_group <- c(hierarchical_cluster_group, n)
  }
  
  for (i in seq_along(frequent_smp)) {
    lep <- frequent_smp[i]
    
    if (is.na(lep) == TRUE) {
      n <- group_number[i]
      lep_df <- movement_unit_subsequences[movement_unit_subsequences$Group == n, 1]
      lep_df <- as.character(lep_df)
      
      for (x in seq_along(lep_df)) {
        lcs <- LCSn(lep_df[1:x])
        print(lcs)
        if (lcs == "") {
          lcs_temp1 <- LCSn(lep_df[1:(x - 1)])
          frequent_smp <- replace_na(frequent_smp, lcs_temp1)
          count_temp <- length(lep_df[1:(x - 1)])
          fsmp_count <- replace_na(fsmp_count, as.numeric(count_temp))
          lcs_temp2 <- LCSn(lep_df[x:length(lep_df)])
          count_temp2 <- length(lep_df[x:length(lep_df)])
          lcs_temp2_group <- (max(group_number) + 1)
          frequent_smp <- append(frequent_smp, lcs_temp2, after = length(frequent_smp))
          hierarchical_cluster_group <- append(hierarchical_cluster_group, lcs_temp2_group, after = length(hierarchical_cluster_group))
          fsmp_count <- append(fsmp_count, count_temp2, after = length(fsmp_count))
          break
        }
        
      }
      
    }
    
  }
  
  # Formation of output ---------------------
  frequent_smp <- data.frame(frequent_smps = frequent_smp) 
  frequent_smp <- mutate(frequent_smp, 
                         Hierarchical_cluster_groups = hierarchical_cluster_group,
                         Count = fsmp_count,
                         Percentage_contribution = ((Count/sum(Count))*100))
  return(frequent_smp)
  
}