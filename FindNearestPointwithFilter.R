# Author: Kwan Yu Chuk

library(readr) 
library(dplyr) 

# This code is to find the 3 nearest reserve points using 2 filters 
# Filter 1: same CEA
# Filter 2: same Strata

# Load file
df <- read.csv("path/mycsv.csv", stringsAsFactors = T)

df <- df %>%
  mutate(
    point_type = as.character(point_type),
    strata = as.character(Strata),
    CEA_No = as.character(CEA_No),
    x = as.numeric(x),
    y = as.numeric(y),
    point_id = as.character(Unique_Sam)
  )

# Initialize columns for 3 nearest points
df$nearest_neighbor_id_1 <- character(nrow(df))
df$nearest_distance_1 <- numeric(nrow(df))
df$nearest_neighbor_id_2 <- character(nrow(df))
df$nearest_distance_2 <- numeric(nrow(df))
df$nearest_neighbor_id_3 <- character(nrow(df))
df$nearest_distance_3 <- numeric(nrow(df))

for (i in 1:nrow(df)) {
  current_point <- df[i, ]
  current_type <- current_point$point_type
  current_strata <- current_point$strata
  current_cea <- current_point$CEA_No
  current_x <- current_point$x
  current_y <- current_point$y
  current_id <- current_point$point_id
  
  # Filter for points with the same strata and CEA_No but different point type
  potential_neighbors <- df %>%
    filter(strata == current_strata,
           CEA_No == current_cea,
           point_type != current_type,
           point_id != current_id) # Exclude the current point
  
  if (nrow(potential_neighbors) > 0) {
    
    # Using Easting/Northing, not latitude/longitude
    # Calculate distances using Euclidean distance
    distances <- apply(potential_neighbors, 1, function(row) {
      row_x <- as.numeric(row["x"]) 
      row_y <- as.numeric(row["y"])
      if(!is.na(row_x) && !is.na(row_y) && !is.na(current_x) && !is.na(current_y)){
        sqrt((current_x - row_x)^2 + (current_y - row_y)^2)
      }
      else{
        return(NA)
      }
    })
    
    # Find the  nearest neighbors
    nearest_indices <- order(distances)[1:min(3, length(distances))] 
    nearest_neighbors <- potential_neighbors[nearest_indices, ]
    nearest_distances <- distances[nearest_indices]
    
    
    if (length(nearest_indices) >= 1) {
      df[i, "nearest_neighbor_id_1"] <- nearest_neighbors$point_id[1]
      df[i, "nearest_distance_1"] <- nearest_distances[1]
    }
    if (length(nearest_indices) >= 2) {
      df[i, "nearest_neighbor_id_2"] <- nearest_neighbors$point_id[2]
      df[i, "nearest_distance_2"] <- nearest_distances[2]
    }
    if (length(nearest_indices) >= 3) {
      df[i, "nearest_neighbor_id_3"] <- nearest_neighbors$point_id[3]
      df[i, "nearest_distance_3"] <- nearest_distances[3]
    }
    
  } else {
    df[i, "nearest_neighbor_id_1"] <- NA_character_
    df[i, "nearest_distance_1"] <- NA_real_
    df[i, "nearest_neighbor_id_2"] <- NA_character_
    df[i, "nearest_distance_2"] <- NA_real_
    df[i, "nearest_neighbor_id_3"] <- NA_character_
    df[i, "nearest_distance_3"] <- NA_real_
  }
}


View(df)
write.csv(df, "path/mycsvoutput.csv")
