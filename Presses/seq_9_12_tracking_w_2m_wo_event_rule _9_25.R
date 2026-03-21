library(dplyr)
library(purrr)
library(stringr)
library(tidyr)

 
tracking_folder <- "C:/Users/pms15/Desktop/9_25/Tracking"
events_folder   <- "C:/Users/pms15/Desktop/9_25/Events"
player_folder   <- "C:/Users/pms15/Desktop/9_25/Player"
presses_file    <- "C:/Users/pms15/Desktop/9_25/Presses_filtered_nonNA.csv"

presses <- read.csv(presses_file)
 
read_tracking <- function(file) {
  df <- readRDS(file)
  df <- as.data.frame(df)
  df$T <- as.numeric(df$T)
  df$X <- as.numeric(df$X)
  df$Y <- as.numeric(df$Y)
  df
}

 
 
read_events <- function(MatchId) {
  file <- list.files(
    events_folder,
    pattern = paste0("^events_", MatchId, "\\.csv$"),
    full.names = TRUE
  )
  if (length(file) != 1) return(NULL)
  
  df <- read.csv(file)
  if ("Time" %in% names(df)) {
    df <- df %>% rename(T = Time)
  }
  df$T <- as.numeric(df$T)
  df
}
 

read_players <- function(MatchId) {
  file <- list.files(
    player_folder,
    pattern = paste0("^player_", MatchId, "\\.csv$"),
    full.names = TRUE
  )
  if (length(file) != 1) return(NULL)
  read.csv(file)
}
 
 
get_ball_cross_info <- function(MatchId, IdHalf_val, start_time) {
  file <- list.files(
    tracking_folder, 
    pattern = paste0("^tracking_df_", MatchId, "\\.rds$"), 
    full.names = TRUE
  )
  if (length(file) != 1) return(list(time = NA_real_, x = NA_real_, y = NA_real_))
  
  tracking <- read_tracking(file)
  
  ball_positions <- tracking %>%
    filter(IsBall %in% c(TRUE, "True"),
           IdHalf == IdHalf_val,
           T >= start_time) %>%
    select(X, Y, T)
  
  if (nrow(ball_positions) == 0) return(list(time = NA_real_, x = NA_real_, y = NA_real_))
  
  initial_X <- ball_positions$X[1]
  ball_side <- ifelse(initial_X < 0, "left", "right")
  
  if (ball_side == "left") {
    idx <- which(ball_positions$X > -1750)[1]
  } else {
    idx <- which(ball_positions$X < 1750)[1]
  }
  
  if (is.na(idx)) return(list(time = NA_real_, x = NA_real_, y = NA_real_))
  
  list(
    time = as.numeric(ball_positions$T[idx]),
    x    = as.numeric(ball_positions$X[idx]),
    y    = as.numeric(ball_positions$Y[idx])
  )
}

  
Presses_with_ball_cross <- presses %>%
  mutate(
    press_time = Time,
    cross_info = pmap(
      list(MatchId    = match_id,
           IdHalf_val = IdHalf,
           start_time = Time),
      get_ball_cross_info
    )
  ) %>%
  mutate(
    ball_cross_time = map_dbl(cross_info, "time"),
    ball_cross_x    = map_dbl(cross_info, "x"),
    ball_cross_y    = map_dbl(cross_info, "y")
  ) %>%
  select(-cross_info)

  
out_of_bound_events <- c("Out for throw-in", "Out for goal kick", "Out for corner")

 
Presses_with_events_teams <- Presses_with_ball_cross %>%
  mutate(
    events_between = pmap_chr(
      list(MatchId = match_id,
           IdHalf_val = IdHalf,
           press_time = press_time,
           cross_time = ball_cross_time),
      function(MatchId, IdHalf_val, press_time, cross_time) {
        ev <- read_events(MatchId)
        if (is.null(ev)) return(NA_character_)
        ev_sub <- ev %>% filter(IdHalf == IdHalf_val,
                                T >= press_time,
                                T <= cross_time)
        if (nrow(ev_sub) == 0) return(NA_character_)
        paste(ev_sub$EventName, collapse = ", ")
      }
    ),
    times_between = pmap_chr(
      list(MatchId = match_id,
           IdHalf_val = IdHalf,
           press_time = press_time,
           cross_time = ball_cross_time),
      function(MatchId, IdHalf_val, press_time, cross_time) {
        ev <- read_events(MatchId)
        if (is.null(ev)) return(NA_character_)
        ev_sub <- ev %>% filter(IdHalf == IdHalf_val,
                                T >= press_time,
                                T <= cross_time)
        if (nrow(ev_sub) == 0) return(NA_character_)
        paste(ev_sub$T, collapse = ", ")
      }
    ),
    idactors_between = pmap_chr(
      list(MatchId = match_id,
           IdHalf_val = IdHalf,
           press_time = press_time,
           cross_time = ball_cross_time),
      function(MatchId, IdHalf_val, press_time, cross_time) {
        ev <- read_events(MatchId)
        if (is.null(ev)) return(NA_character_)
        ev_sub <- ev %>% filter(IdHalf == IdHalf_val,
                                T >= press_time,
                                T <= cross_time)
        if (nrow(ev_sub) == 0) return(NA_character_)
        paste(ev_sub$IdActor1, collapse = ", ")
      }
    ),
    idteams_between = pmap_chr(
      list(MatchId = match_id,
           IdHalf_val = IdHalf,
           press_time = press_time,
           cross_time = ball_cross_time),
      function(MatchId, IdHalf_val, press_time, cross_time) {
        ev <- read_events(MatchId)
        players <- read_players(MatchId)
        if (is.null(ev) | is.null(players)) return(NA_character_)
        ev_sub <- ev %>% filter(IdHalf == IdHalf_val,
                                T >= press_time,
                                T <= cross_time)
        if (nrow(ev_sub) == 0) return(NA_character_)
        ev_join <- ev_sub %>%
          left_join(players %>% select(IdActor, IdTeam),
                    by = c("IdActor1" = "IdActor"))
        paste(ev_join$IdTeam, collapse = ", ")
      }
    ), 
    ob_index = map_int(
      events_between,
      function(ev_str) {
        if (is.na(ev_str)) return(NA_integer_)
        ev_list <- str_split(ev_str, ",\\s*")[[1]]
        match_idx <- which(ev_list %in% out_of_bound_events)
        if (length(match_idx) == 0) NA_integer_ else match_idx[1]
      }
    ), 
    first_turnover_index = map_int(
      idteams_between,
      function(team_str) { 
        if (is.na(team_str) || team_str == "") return(NA_integer_)
         
        teams <- str_split(team_str, ",\\s*")[[1]]
         
        teams <- ifelse(teams == "NA", NA_character_, teams)
         
        if (length(teams) <= 1) return(NA_integer_)
        
        first_team <- teams[1]
         
        if (is.na(first_team)) return(NA_integer_)
         
        for (i in 2:length(teams)) {
          current_team <- teams[i]
          if (!is.na(current_team) && current_team != first_team) {
            return(i)
          }
        }
         
        return(NA_integer_)
      }
    )
    
    
    
  )


Presses_with_events_teams <- Presses_with_events_teams %>%
  arrange(source)

Presses_with_events_teams <- Presses_with_events_teams %>%
  mutate(
    absorbing_status_new = case_when(
      is.na(ob_index) & is.na(first_turnover_index) ~ "BP",
      !is.na(ob_index) & is.na(first_turnover_index) ~ "OB",
      is.na(ob_index) & !is.na(first_turnover_index) ~ "TO",
      !is.na(ob_index) & !is.na(first_turnover_index) & first_turnover_index < ob_index ~ "TO",
      !is.na(ob_index) & !is.na(first_turnover_index) & first_turnover_index > ob_index ~ "OB",
      TRUE ~ NA_character_
    )
  )


Presses_with_events_teams <- Presses_with_events_teams %>%
  mutate(
    absorbing_status_time = pmap_dbl(
      list(absorbing_status = absorbing_status_new,
           times_between_str = times_between,
           ob_idx = ob_index,
           to_idx = first_turnover_index,
           cross_time = ball_cross_time),
      function(absorbing_status, times_between_str, ob_idx, to_idx, cross_time) {
 
        times_vec <- if (!is.na(times_between_str)) {
          as.numeric(str_split(times_between_str, ",\\s*")[[1]])
        } else {
          numeric(0)
        }
         
        if (absorbing_status == "BP") {
          cross_time
        } else if (absorbing_status == "OB") {
          if (!is.na(ob_idx) && length(times_vec) >= ob_idx) {
            times_vec[ob_idx]
          } else {
            NA_real_
          }
        } else if (absorbing_status == "TO") {
          if (!is.na(to_idx) && length(times_vec) >= to_idx) {
            times_vec[to_idx]
          } else {
            NA_real_
          }
        } else {
          NA_real_
        }
      }
    )
  )

Presses_with_events_teams <- Presses_with_events_teams %>%
  rowwise() %>%
  mutate(
    absorbing_status_time_ce = if (!is.na(absorbing_status_time)) {
      ceiling(absorbing_status_time / 100) * 100
    } else { 
      file <- list.files(
        tracking_folder, 
        pattern = paste0("^tracking_df_", match_id, "\\.rds$"), 
        full.names = TRUE
      )
      if (length(file) == 1) {
        tracking <- read_tracking(file)
        last_time <- tracking %>%
          filter(IsBall %in% c(TRUE, "True"), IdHalf == IdHalf) %>%
          summarise(max_T = max(T, na.rm = TRUE)) %>%
          pull(max_T)
        ifelse(is.finite(last_time), last_time, NA_real_)
      } else {
        NA_real_
      }
    }
  ) %>%
  ungroup()








Presses_with_events_teams <- Presses_with_events_teams %>%
  mutate(
    idactors_between_event_upto_outcome = pmap_chr(
      list(MatchId = match_id,
           IdHalf_val = IdHalf,
           press_time = press_time,
           output_time = absorbing_status_time),
      function(MatchId, IdHalf_val, press_time, output_time) {
        ev <- read_events(MatchId)
        if (is.null(ev)) return(NA_character_)
        ev_sub <- ev %>% filter(IdHalf == IdHalf_val,
                                T >= press_time,
                                T <= output_time)
        if (nrow(ev_sub) == 0) return(NA_character_)
        paste(ev_sub$IdActor1, collapse = ", ")
      }
    ) 
    
  )






Presses_with_events_teams <- Presses_with_events_teams %>%
  mutate(
    ball_locations = pmap(
      list(MatchId = match_id,
           IdHalf_val = IdHalf,
           start_time = press_time,
           end_time = absorbing_status_time_ce),
      function(MatchId, IdHalf_val, start_time, end_time) {
 
        file <- list.files(
          tracking_folder, 
          pattern = paste0("^tracking_df_", MatchId, "\\.rds$"), 
          full.names = TRUE
        )
        if (length(file) != 1) return(data.frame())
        
        tracking <- read_tracking(file)
         
        ball_df <- tracking %>%
          filter(IsBall %in% c(TRUE, "True"),
                 IdHalf == IdHalf_val,
                 T >= start_time,
                 T <= end_time) %>%
          select(T, X, Y) 
        if (nrow(ball_df) == 0) return(data.frame()) 
        ball_df %>%
          mutate(X = as.numeric(X),
                 Y = as.numeric(Y))
      }
    )
  )

library(tidyr)


Presses_with_events_teams <- Presses_with_events_teams %>%
  mutate(
    ball_locations = pmap(
      list(MatchId = match_id,
           IdHalf_val = IdHalf,
           start_time = press_time,
           end_time = absorbing_status_time_ce,
           idactors_str = idactors_between_event_upto_outcome),
      function(MatchId, IdHalf_val, start_time, end_time, idactors_str) { 
        file <- list.files(
          tracking_folder,
          pattern = paste0("^tracking_df_", MatchId, "\\.rds$"),
          full.names = TRUE
        )
        if (length(file) != 1) return(data.frame())
        
        tracking <- read_tracking(file) 
        ball_df <- tracking %>%
          filter(IsBall %in% c(TRUE, "True"),
                 IdHalf == IdHalf_val,
                 T >= start_time,
                 T <= end_time) %>%
          select(T, X, Y) %>%
          arrange(T)
        if (nrow(ball_df) == 0) return(data.frame()) 
        player_df <- tracking %>%
          filter(!(IsBall %in% c(TRUE, "True")),
                 IdHalf == IdHalf_val,
                 T >= start_time,
                 T <= end_time) %>%
          select(T, IdActor, X, Y) 
        ball_with_closest <- ball_df %>%
          rowwise() %>%
          mutate(
            closest_info = list({
              players_at_time <- player_df %>% filter(T == .data$T)
              if (nrow(players_at_time) == 0) return(list(IdActor = NA, X = NA, Y = NA, distance = NA_real_))
              
              dists <- sqrt((players_at_time$X - X)^2 + (players_at_time$Y - Y)^2)
              closest <- players_at_time[which.min(dists), ]
              dist_val <- min(dists)
              
              list(
                IdActor  = closest$IdActor,
                X        = closest$X,
                Y        = closest$Y,
                distance = dist_val
              )
            }),
            closest_player_IdActor = closest_info$IdActor,
            closest_player_X       = closest_info$X,
            closest_player_Y       = closest_info$Y,
            closest_distance       = closest_info$distance
          ) %>%
          ungroup() %>%
          select(-closest_info) %>%
          arrange(T) 
        filtered <- ball_with_closest %>%
          filter(!is.na(closest_player_IdActor),
                 closest_distance <= 100) %>%
          arrange(T) 
        player_run <- filtered %>%
          arrange(closest_player_IdActor, T) %>%
          group_by(closest_player_IdActor) %>%
          mutate(
            time_gap = T - lag(T),
            new_sequence = if_else(is.na(time_gap) | time_gap > 100, 1, 0),   # gap threshold = 100
            sequence_id = cumsum(new_sequence)
          ) %>%
          group_by(closest_player_IdActor, sequence_id) %>%
          arrange(T, .by_group = TRUE) %>%
          mutate(
            dist_step = sqrt(
              (closest_player_X - lag(closest_player_X))^2 +
                (closest_player_Y - lag(closest_player_Y))^2
            ),
            dist_step = ifelse(is.na(dist_step), 0, dist_step),
            run_distance = sum(dist_step, na.rm = TRUE)
          ) %>%
          ungroup() %>%
          select(T, closest_player_IdActor, closest_player_X, closest_player_Y, closest_distance, run_distance) %>%
          arrange(closest_player_IdActor, T)
        
        
        
      }
      
      
    )
  )

Presses_with_events_teams <- Presses_with_events_teams %>%
  mutate(
    ball_locations = map(ball_locations, function(df) {
      if (is.null(df) || nrow(df) == 0) return(df)  
      
      df %>%
        arrange(T) %>%
        mutate(row_id = row_number()) %>%
        filter(run_distance > 0 | row_id == max(row_id)) %>%
        select(-row_id)
    })
  )



library(sp)    
x_min <- -5250
x_max <- 5250
penalty_area_width <- 4020
penalty_area_depth <- 1652.8
left_pen_line <- -5250 + penalty_area_depth
right_pen_line <- 5250 - penalty_area_depth

polygons <- list(
  lb = list(x = c(x_min, left_pen_line, left_pen_line, x_min),
            y = c(-penalty_area_width/2, -penalty_area_width/2, penalty_area_width/2, penalty_area_width/2)),
  ld = list(x = c(-3600, -1750, -1750),
            y = c(-3400, -1652.8, -3400)),
  lf = list(x = c(-3600, -1750, -1750),
            y = c(3400, 1652.8, 3400)),
  le = list(x = c(-3600, -2867.3, -1750, -1750, -2867.3, -3600),
            y = c(2016, 2708, 1652.8, -1652.8, -2708, -2016)),
  lc = list(x = c(-5250, -3600, -2867.3, -3600, -5250),
            y = c(2016, 2016, 2708, 3400, 3400)),
  la = list(x = c(-5250, -3600, -2867.3, -3600, -5250),
            y = c(-2016, -2016, -2708, -3400, -3400)),
  rb = list(x = c(right_pen_line, x_max, x_max, right_pen_line),
            y = c(-penalty_area_width/2, -penalty_area_width/2, penalty_area_width/2, penalty_area_width/2)),
  rf = list(x = c(3600, 1750, 1750),
            y = c(-3400, -1652.8, -3400)),
  rd = list(x = c(3600, 1750, 1750),
            y = c(3400, 1652.8, 3400)),
  re = list(x = c(3600, 2867.3, 1750, 1750, 2867.3, 3600),
            y = c(2016, 2708, 1652.8, -1652.8, -2708, -2016)),
  ra = list(x = c(5250, 3600, 2867.3, 3600, 5250),
            y = c(2016, 2016, 2708, 3400, 3400)),
  rc = list(x = c(5250, 3600, 2867.3, 3600, 5250),
            y = c(-2016, -2016, -2708, -3400, -3400))
)
 
get_region_from_coords <- function(x, y, polygons) {
  for (region_name in names(polygons)) {
    poly <- polygons[[region_name]]
    inside <- point.in.polygon(x, y, poly$x, poly$y)
    if (inside > 0) return(region_name)
  }
  return(NA_character_)
}

 
Presses_with_events_teams <- Presses_with_events_teams %>%
  mutate(
    ball_locations = map(ball_locations, ~{
      df <- .x
      if (is.null(df) || nrow(df) == 0) return(df)
      
      df %>%
        mutate(
          closest_player_region = pmap_chr(
            list(X = closest_player_X, Y = closest_player_Y, dist = closest_distance),
            function(X, Y, dist) {
              if (!is.na(X) && !is.na(Y) && !is.na(dist) && dist <= 100) {
                get_region_from_coords(X, Y, polygons)
              } else {
                NA_character_
              }
            }
          )
        )
    })
  )




Presses_with_events_teams <- Presses_with_events_teams %>%
  mutate(
    ball_locations = map(ball_locations, ~{
      df <- .x
      if (is.null(df) || nrow(df) == 0) return(df)
      
      # Step 1: Find the first player row (pressed player)
      df <- df %>% arrange(T)
      first_X <- df$closest_player_X[1]
      first_Y <- df$closest_player_Y[1]
      
      first_player_region <- if (!is.na(first_X) && !is.na(first_Y)) {
        get_region_from_coords(first_X, first_Y, polygons)
      } else {
        NA_character_
      } 
      
      df <- df %>%
        arrange(closest_player_IdActor, T) %>%
        group_by(closest_player_IdActor) %>%
        mutate(
          time_gap = T - lag(T),
          new_sequence = if_else(is.na(time_gap) | time_gap > 100, 1, 0),
          sequence_id = cumsum(new_sequence)
        ) %>%
        group_by(closest_player_IdActor, sequence_id) %>%
        arrange(T, .by_group = TRUE) %>%
        mutate( 
          n_regions = n_distinct(closest_player_region),
          player_region = case_when(
            run_distance >= 200 ~ closest_player_region,
            run_distance < 200 & n_regions == 1 ~ closest_player_region,
            run_distance < 200 & n_regions > 1 ~ first(closest_player_region)
          )
        ) %>%
        ungroup() %>%
        arrange(T) 
      
      df$player_region[1] <- first_player_region
      
      df
    })
  )

 

Presses_with_events_teams <- Presses_with_events_teams %>%
  mutate(
    ball_locations = map(ball_locations, ~{
      df <- .x
       
      if (is.null(df) || nrow(df) == 0) return(df)
       
      if (all(is.na(df$player_region))) { 
        first_X <- df$closest_player_X[1]
        first_Y <- df$closest_player_Y[1]
         
        region <- if (!is.na(first_X) && !is.na(first_Y)) {
          get_region_from_coords(first_X, first_Y, polygons)
        } else {
          NA_character_
        }
         
        df$player_region <- region
      }
      
      return(df)
    })
  )
 

Presses_with_events_teams <- Presses_with_events_teams %>%
  mutate( 
    closest_player_IdActor = map_chr(
      ball_locations,
      ~ if (is.null(.x) || nrow(.x) == 0 || !"closest_player_IdActor" %in% names(.x)) {
        NA_character_
      } else {
        paste(.x$closest_player_IdActor, collapse = ", ")
      }
    ),
     
    player_region = map_chr(
      ball_locations,
      ~ if (is.null(.x) || nrow(.x) == 0 || !"player_region" %in% names(.x)) {
        NA_character_
      } else {
        paste(.x$player_region, collapse = ", ")
      }
    )
  )



Presses_with_events_teams <- Presses_with_events_teams %>%
  mutate(
    collapsed_region_sequence = map_chr(
      ball_locations,
      ~ if (is.null(.x) || nrow(.x) == 0 || 
            !"player_region" %in% names(.x) || 
            !"player_region" %in% names(.x)) {
        NA_character_
      } else { 
        .x <- .x %>%
          filter(!is.na(player_region),
                 !is.na(closest_player_IdActor),
                 player_region != "NA",
                 closest_player_IdActor != "NA")
        
        if (nrow(.x) == 0) return(NA_character_)
        
        regions <- .x$player_region
        players <- .x$closest_player_IdActor
        pairs <- paste(players, regions, sep = "_")
        
        keep <- c(TRUE, pairs[-1] != pairs[-length(pairs)])
        collapsed <- regions[keep]
        
        paste(collapsed, collapse = ", ")
      }
    ) 
  )


Presses_with_events_teams <- Presses_with_events_teams %>%
  mutate(
    collapsed_region_sequence_nolr = map_chr(
      strsplit(collapsed_region_sequence, ",\\s*"),
      ~ {
        seq_nolr <- sub("^[rl]", "", .x)   
        if (length(seq_nolr) == 1) {
          paste(rep(seq_nolr, 2), collapse = ",")   
        } else {
          paste(seq_nolr, collapse = ",")
        }
      }
    )
  )

Presses_with_events_teams <- Presses_with_events_teams %>%
  mutate(
    collapsed_region_sequence_nolr = ifelse(
      is.na(idactors_between_event_upto_outcome), 
      sapply(strsplit(collapsed_region_sequence_nolr, ",\\s*"), function(x) {
        paste(head(x, 2), collapse = ", ")
      }),
     
      collapsed_region_sequence_nolr
    )
  )


Presses_with_events_teams <- Presses_with_events_teams %>%
  mutate(
    output = map_chr(
      strsplit(collapsed_region_sequence_nolr, ",\\s*"),
      ~ tail(.x, 1)  
    )
  )

Presses_with_events_teams$item_counts <- sapply(strsplit(Presses_with_events_teams$collapsed_region_sequence_nolr, ","), length)-1


Presses_with_events_teams %>%
  select(-ball_locations) %>%   
  write.csv("U:/9_26/Presses_final_tracking_W_2m_wo_event_9_26.csv", row.names = FALSE)

 