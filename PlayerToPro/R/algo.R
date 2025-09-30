# Function to standardize specific columns in a data frame
scale_stats <- function(df, cols) {
  # across applies the scale function to each column in cols
  # scale subtracts the column's mean, divides by the column's standard deviation
  df %>% mutate(across(all_of(cols), scale))
}

# Map for similar positions - maybe need to keep updating this
similar_positions_map <- list(
  G  = c("G", "PG", "SG"),
  "F"  = c("F", "SF", "PF"),
  C  = c("C", "F/C", "C/F"),
  "G/F" = c("G", "F", "SF", "SG", "G/F"),
  PG = c("PG", "G"),
  SG = c("SG", "G"),
  SF = c("SF", "F"),
  PF = c("PF", "F")
)

# Function to determine similarity score between current usport player and pro player
get_similar_players <- function(usports_player, filter_by_similar_position = FALSE, k = 10) {
  usports_player <- trimws(usports_player)
  
  # Columns to compare
  stat_cols <- c("PPG", "REB", "AST", "STL", "BLK", "FG%", "3P%", "FT%")
  
  # Grab the USPORTS player's Total row
  u_row <- current_players %>%
    filter(trimws(Player) == usports_player, Season == "Total") %>%
    select(Player, Position, all_of(stat_cols))
  
  # Grab pro players with Total row
  pro_raw <- pro_players_usports_data %>%
    filter(Season == "Total") %>%
    select(Player, Position, all_of(stat_cols), `All USports Teams Played For`) %>%
    drop_na()
  
  # Bind together for consistent scaling, current usports players tagged with "u" and pro player tagged with "p"
  all_data <- bind_rows(
    u_row %>% mutate(source = "u"),
    pro_raw %>% select(all_of(stat_cols)) %>% mutate(source = "p")
  )
  
  # Scale selected stats (drop = FALSE ensures it's still a data frame)
  # Standardizes by subtracting the column mean then dividing by standard deviation for that column
  scaled_mat <- scale(as.matrix(all_data[, stat_cols, drop = FALSE]))
  
  # Separate back out
  u_scaled_vec <- scaled_mat[all_data$source == "u", , drop = FALSE]
  pro_scaled_mat <- scaled_mat[all_data$source == "p", , drop = FALSE]
  
  # Compute squared Euclidean distances
  dists <- rowSums((pro_scaled_mat - matrix(u_scaled_vec,
                                            nrow = nrow(pro_scaled_mat),
                                            ncol = ncol(pro_scaled_mat),
                                            byrow = TRUE))^2)
  
  # Add distances back to the raw pro dataset
  pro_raw$dist <- dists
  
  if (filter_by_similar_position) {
    # Also filter by position
    u_position <- u_row$Position[1]
    valid_positions <- similar_positions_map[[u_position]]
    if (!is.null(valid_positions)) {
      pro_raw <- pro_raw %>% filter(Position %in% valid_positions)
    }
  } 
  
  # Return top-k
  pro_raw %>%
    arrange(dist) %>% # arrange sorts players by increasing distance (smallest distance first)
    head(k) %>%
    mutate(across(all_of(stat_cols), ~ round(.x, 1))) %>%
    select(Player, `All USports Teams Played For`, dist, all_of(stat_cols))
}