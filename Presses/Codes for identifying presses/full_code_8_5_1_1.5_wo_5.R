
execution_time <- system.time({
  
  library(dplyr)  
  library(purrr)
  
  xmin = -5250
  xmax = 5250
  ymin = -3400
  ymax = 3400
  
  base_dir <- "C:/Users/pavan/Desktop/SFU/Research/Datasets/8_5/complete_data_8_5_1_1.5_wo_5"
  
  tracking_files <- list.files(path = base_dir, 
                               pattern = "^tracking_df_.*\\.rds$",
                               full.names = TRUE)
  
  for (tracking_file in tracking_files) { 
    base_name <- sub("^tracking_df_", "", sub("\\.rds$", "", basename(tracking_file)))
    
    pd_path <- file.path(base_dir, paste0("player_", base_name, ".csv"))
    ed_path <- file.path(base_dir, paste0("events_", base_name, ".csv"))
    
    if (!file.exists(pd_path) || !file.exists(ed_path)) {
      warning(paste("Skipping", base_name, "- missing player or event file"))
      next
    }
    
    td <- readRDS(tracking_file)
    pd <- read.csv(pd_path, header = TRUE)
    ed <- read.csv(ed_path, header = TRUE)
    
    td <- rename(td, Time = T)
    
    ed <- ed %>%
      left_join(pd %>% select(IdActor, IdTeam), by = c("IdActor1" = "IdActor"))
    
    td1 = subset(td, select = -c(.id, Z,NbPoints))
    
    td1[, c("Time", "X", "Y", "IdActor", "IdHalf")] = 
      lapply(td1[, c("Time", "X", "Y", "IdActor", "IdHalf")], as.numeric)
    
    td1_player = subset(td1 %>% filter(IsBall == 'False'), select = -c(IsBall))
    td1_ball = subset(td1 %>% filter(IsBall == 'True'), 
                      select = -c(IsBall, IdActor))
    
    
    td1_ball$dx = xmax-td1_ball$X
    td1_ball$dy = ymax-td1_ball$Y
    
    
    
    
    td1_ball <- td1_ball %>%
      group_by(IdHalf) %>%
      arrange(IdHalf, Time, .by_group = TRUE) %>%
      mutate(
        dx_diff = dx - lag(dx),
        dy_diff = dy - lag(dy)
      ) %>%
      ungroup() %>%
      arrange(IdHalf, Time)
    
    pd_player = pd[, c('IdActor', 'IdTeam', 'Type', 'Position')]
    
    pd_player$IdActor = as.numeric(pd_player$IdActor)
    pd_player$IdTeam = as.numeric(pd_player$IdTeam)
    
    TP = left_join(td1_player, pd_player, by = "IdActor")
    
    
    TP1 = TP
    
    
    TP1$dx = xmax-TP1$X
    TP1$dy = ymax-TP1$Y
    
    TP1 <- TP1 %>%
      group_by(IdActor, IdHalf) %>%
      arrange(IdActor, IdHalf, Time, .by_group = TRUE) %>%
      mutate(
        dx_diff = dx - lag(dx),
        dy_diff = dy - lag(dy)
      ) %>%
      ungroup() %>%
      arrange(IdActor, IdHalf, Time)
    
    
    player_ball = TP1 %>%
      left_join(td1_ball, by = c("IdHalf", "Time") ) %>%
      rename(
        X_player = X.x,
        Y_player = Y.x,
        X_ball = X.y,
        Y_ball = Y.y,
        dx_player = dx.x,
        dy_player = dy.x,
        dx_diff_player = dx_diff.x,
        dy_diff_player = dy_diff.x,
        dx_ball = dx.y,
        dy_ball = dy.y,
        dx_diff_ball = dx_diff.y,
        dy_diff_ball = dy_diff.y
        
      )
    
    
    player_ball$player2ball = sqrt((player_ball$X_player - player_ball$X_ball)^2 + (player_ball$Y_player - player_ball$Y_ball)^2)
    
    player_ball <- player_ball %>%
      group_by(IdHalf, Time) %>%
      mutate(
        Possession = {
          close_players <- which(player2ball < 100)
          if (length(close_players) == 0) {
            rep(FALSE, n())
          } else {
            possession_flag <- rep(FALSE, n()) 
            closest <- close_players[which.min(player2ball[close_players])]
            possession_flag[closest] <- TRUE
            possession_flag
          }
        }
      ) %>%
      ungroup()
    
    
    player_ball <- player_ball %>%
      group_by(IdHalf, IdActor) %>%   
      mutate(IsPossession = lag(Possession,2) & lag(Possession) & Possession & lead(Possession) & lead(Possession,2)) %>%
      ungroup()
    
    # calculate_distance = function(x1, y1, x2, y2) {
    #   sqrt((x1 - x2)^2 + (y1 - y2)^2)
    # } 
    # 
    # player_ball = player_ball %>%
    #   group_by(IdHalf, Time) %>%   
    #   mutate(
    #     clear_space_team5 = sapply(1:n(), function(i) { 
    #       current_x = X_player[i]
    #       current_y = Y_player[i]
    #       current_team = IdTeam[i]
    #       
    #       teammate_indices = which(IdTeam == current_team & row_number() != i)
    #       
    #       if (length(teammate_indices) == 0) {
    #         return(TRUE)  
    #       }
    #       
    #       distances = calculate_distance(current_x, current_y, X_player[teammate_indices], Y_player[teammate_indices])
    #       
    #       all(distances > 500)
    #     })
    #   ) %>%
    #   ungroup()
    
    calculate_distance <- function(x1, y1, x2, y2) {
      sqrt((x1 - x2)^2 + (y1 - y2)^2)
    } 
    player_ball <- player_ball %>%
      mutate(
        player_half = ifelse(X_player < 0, "left", "right")
      ) 
    player_ball <- player_ball %>%
      group_by(IdHalf, Time) %>%
      mutate(
        opponents_within_15 = map_int(seq_len(n()), function(i) {
          current_x <- X_player[i]
          current_y <- Y_player[i]
          current_team <- IdTeam[i]
          
          opponent_indices <- which(IdTeam != current_team)
          if (length(opponent_indices) == 0) return(0)
          
          distances <- calculate_distance(
            current_x, current_y,
            X_player[opponent_indices], Y_player[opponent_indices]
          )
          
          sum(distances <= 15 * 100)  
        })
      ) %>%
      ungroup()
    
    player_ball <- player_ball %>%
      group_by(IdHalf, Time) %>%
      mutate(
        opponents_greater_15 = map_int(seq_len(n()), function(i) {
          current_x <- X_player[i]
          current_y <- Y_player[i]
          current_team <- IdTeam[i]
          current_half <- player_half[i]
          
          opponent_indices <- which(IdTeam != current_team & player_half == current_half)
          if (length(opponent_indices) == 0) return(0)
          
          dist_to_opponents <- calculate_distance(
            current_x, current_y,
            X_player[opponent_indices], Y_player[opponent_indices]
          )
          
          distant_opponents_idx <- opponent_indices[dist_to_opponents > 15 * 100]
          if (length(distant_opponents_idx) == 0) return(0)
          
          teammate_indices <- which(IdTeam == current_team &
                                      row_number() != i &
                                      player_half == current_half)
          if (length(teammate_indices) == 0) return(0)
          
          sum(map_lgl(distant_opponents_idx, function(opp_i) {
            dist_teammates_to_opp <- calculate_distance(
              X_player[opp_i], Y_player[opp_i],
              X_player[teammate_indices], Y_player[teammate_indices]
            )
            any(dist_teammates_to_opp <= 300)  
          }))
        })
      ) %>%
      ungroup()
    
    
    
    
    
    get_delta = function(Time) {
      if (Time == 0) return(NA)  
      else if (Time == 100) return(100)
      else if (Time == 200) return(200)
      else if (Time == 300) return(300)
      else return(400)
    } 
    calculate_speed = function(data) {
      data %>%
        group_by(IdHalf, IdActor) %>% 
        arrange(Time) %>%
        mutate(
          delta = sapply(Time, get_delta),  
          vx = (X_player[match(Time + delta, Time)] - X_player[match(Time - delta, Time)]) / (2 * delta),
          vy = (Y_player[match(Time + delta, Time)] - Y_player[match(Time - delta, Time)]) / (2 * delta),
          v = sqrt(vx^2 + vy^2)  
        ) %>%
        ungroup() %>%
        select(-delta)
    }
    
    player_ball <- as.data.frame(calculate_speed(player_ball))
    
    
    GKH1 = as.numeric(
      TP[TP$Position == 'Goalkeeper' & TP$Type == 'HomeTeam' 
         & TP$IdHalf == '1', "X"][1])
    
    if (GKH1<0){ 
      player_ball$final_third = dplyr::case_when(
        player_ball$X_player < -1750 & player_ball$Type == 'HomeTeam' & player_ball$IdHalf == 1 & 
          player_ball$Position != "Goalkeeper" ~ 'Yes',
        player_ball$X_player >  1750 & player_ball$Type == 'AwayTeam' & player_ball$IdHalf == 1 & 
          player_ball$Position != "Goalkeeper" ~ 'Yes',
        player_ball$X_player < -1750 & player_ball$Type == 'AwayTeam' & player_ball$IdHalf == 2 & 
          player_ball$Position != "Goalkeeper" ~ 'Yes',
        player_ball$X_player >  1750 & player_ball$Type == 'HomeTeam' & player_ball$IdHalf == 2 & 
          player_ball$Position != "Goalkeeper" ~ 'Yes',
        TRUE ~ 'No')
    }else{ 
      player_ball$final_third <- dplyr::case_when(
        player_ball$X_player < -1750 & player_ball$Type == 'AwayTeam' & player_ball$IdHalf == 1 & 
          player_ball$Position != "Goalkeeper" ~ 'Yes',
        player_ball$X_player >  1750 & player_ball$Type == 'HomeTeam' & player_ball$IdHalf == 1 & 
          player_ball$Position != "Goalkeeper" ~ 'Yes',
        player_ball$X_player < -1750 & player_ball$Type == 'HomeTeam' & player_ball$IdHalf == 2 & 
          player_ball$Position != "Goalkeeper" ~ 'Yes',
        player_ball$X_player >  1750 & player_ball$Type == 'AwayTeam' & player_ball$IdHalf == 2 & 
          player_ball$Position != "Goalkeeper" ~ 'Yes',
        TRUE ~ 'No')
    }
    
    
    player_ball <- player_ball %>%
      arrange(IdActor, IdHalf, Time) %>%
      mutate(dist = sqrt((X_ball - X_player)^2 + (Y_ball - Y_player)^2)) %>%
      group_by(IdHalf, IdActor) %>%
      mutate( 
        is_close = (dist <= 100), 
        reached = purrr::map_lgl(row_number(), ~ {
          any(is_close[.x:min(.x + 20, n())], na.rm = TRUE)
        })
      ) %>%
      ungroup() %>%
      select(-is_close)  
    
    
    library(data.table)
    
    setDT(player_ball)
    
    player_ball[, ball_loc := fifelse(X_ball < 0, "Left", fifelse(X_ball > 0, "Right", "Center"))]
    
    
    player_ball[, player_loc := fifelse(X_player < 0, "Left",
                                        fifelse(X_player > 0, "Right", "Center"))]
    
    player_ball = player_ball %>%
      mutate(attacking_dir = case_when(
        GKH1 < 0 & Type == "HomeTeam" & IdHalf == 1 ~ "Right",
        GKH1 < 0 & Type == "AwayTeam" & IdHalf == 1 ~ "Left",
        GKH1 < 0 & Type == "HomeTeam" & IdHalf == 2 ~ "Left",
        GKH1 < 0 & Type == "AwayTeam" & IdHalf == 2 ~ "Right",
        GKH1 > 0 & Type == "HomeTeam" & IdHalf == 1 ~ "Left",
        GKH1 > 0 & Type == "AwayTeam" & IdHalf == 1 ~ "Right",
        GKH1 > 0 & Type == "HomeTeam" & IdHalf == 2 ~ "Right",
        GKH1 > 0 & Type == "AwayTeam" & IdHalf == 2 ~ "Left",
        TRUE ~ NA_character_
      ))
    
    
    player_ball = player_ball %>%
      mutate(mightbe = case_when(
        player_loc == attacking_dir ~ "pr",
        TRUE ~ "po"
      ))
    
    player_ball <- player_ball %>%
      mutate(Q_player = case_when(
        X_player < 0 & X_player >= xmin & Y_player <= ymax & Y_player > 0  ~ 1,
        X_player <= xmax & X_player > 0 & Y_player <= ymax & Y_player > 0  ~ 2,
        X_player <= xmax & X_player > 0 & Y_player < 0 & Y_player >= ymin  ~ 3,
        X_player < 0 & X_player >= xmin & Y_player < 0 & Y_player >= ymin  ~ 4,
        TRUE ~ NA_real_  
      ))
    
    
    player_ball <- player_ball %>%
      group_by(IdHalf, Time) %>%
      mutate(
        teammates_under_pressure = sapply(1:n(), function(i) {
          current_x <- X_player[i]
          current_y <- Y_player[i]
          current_team <- IdTeam[i]
          
          current_half_side <- ifelse(current_x < 0, "left", "right") 
          
          selected_teammates <- which(
            IdTeam == current_team &
              row_number() != i &
              ifelse(X_player < 0, "left", "right") == current_half_side
          )
          if (length(selected_teammates) == 0) return(0)
          
          opponent_indices <- which(
            IdTeam != current_team &
              ifelse(X_player < 0, "left", "right") == current_half_side
          )
          if (length(opponent_indices) == 0) return(0)
          
          sum(sapply(selected_teammates, function(teammate_i) {
            dist_to_opponents <- calculate_distance(
              X_player[teammate_i], Y_player[teammate_i],
              X_player[opponent_indices], Y_player[opponent_indices]
            )
            any(dist_to_opponents <= 500)
          }))
        })
      ) %>%
      ungroup()
    
    calculate_distance <- function(x1, y1, x2, y2) {
      sqrt((x1 - x2)^2 + (y1 - y2)^2)
    }
    
    player_ball <- player_ball %>%
      group_by(IdHalf, Time) %>%
      mutate(
        player_half = ifelse(X_player < 0, "left", "right"),
        
        closest_two_opponents = sapply(1:n(), function(i) {
          current_x <- X_player[i]
          current_y <- Y_player[i]
          current_team <- IdTeam[i]
          
          other_indices <- which(row_number() != i)
          if (length(other_indices) < 2) re 
          distances <- calculate_distance(
            current_x, current_y,
            X_player[other_indices], Y_player[other_indices]
          ) 
          sorted_indices <- other_indices[order(distances)]
          closest_2 <- sorted_indices[1:2] 
          all(IdTeam[closest_2] != current_team)
        })
      ) %>%
      ungroup()
    
    pb_name <- paste0("PB_", base_name)
    output_path <- file.path(base_dir, paste0(pb_name, ".rds"))
    
    saveRDS(player_ball, output_path)
    
    write.csv(player_ball, sub("\\.rds$", ".csv", output_path), row.names = FALSE)
    
    
    player_ball_po = player_ball %>%
      filter(IsPossession == TRUE,  mightbe == 'po', clear_space_team5==TRUE, final_third=="Yes", v<0.2) 
    
    player_ball_pr = player_ball %>%
      filter(mightbe == 'pr', reached ==TRUE)
    
    Final <- merge(
      player_ball_po, 
      player_ball_pr, 
      by = c("IdHalf", "Time"),                         
      suffixes = c("_po", "_pr")
    ) 
    Final <- Final[order(Final$IdHalf, Final$Time), ]
    
    
    
    Final <- Final %>%
      mutate(IsPressing = case_when(
        Q_player_po == Q_player_pr &
          (sign(dx_diff_ball_po) != sign(dx_diff_player_pr) |
             sign(dy_diff_ball_po) != sign(dy_diff_player_pr)) ~ TRUE,
        TRUE ~ FALSE
      )) 
    
    Final1 = Final 
    unique_consec <- Final1$Time[c(TRUE, diff(Final1$Time) != 0)]
    
    diffs <- c(NA, diff(unique_consec))
    group_id <- cumsum(is.na(diffs) | diffs != 100) 
    
    run_lengths <- rle(Final1$Time)$lengths
    expanded_group_id <- rep(group_id, run_lengths)
    
    Final1$Time_median_group <- ave(Final1$Time, expanded_group_id, FUN = function(x) median(unique(x)))
    
    Final1$Time_median_group <- as.numeric(Final1$Time_median_group)
    ed$Time <- as.numeric(ed$Time)
    Final1$IdHalf <- as.integer(Final1$IdHalf)
    ed$IdHalf <- as.integer(ed$IdHalf)
    
    get_closest_event_info <- function(med_time, half_id) {
      ed_half <- ed %>% filter(IdHalf == half_id)
      
      if (nrow(ed_half) == 0) {
        return(c(NA, NA))
      }
      
      ed_half <- ed_half %>%
        mutate(time_diff = abs(Time - med_time)) %>%
        arrange(time_diff) %>%
        slice(1)
      
      return(c(ed_half$EventName, ed_half$IdActor1))
    }
    
    event_info <- mapply(get_closest_event_info, 
                         Final1$Time_median_group, 
                         Final1$IdHalf)
    
    Final1$ClosestEventToMedian <- event_info[1, ]
    Final1$ClosestActorToMedian <- event_info[2, ]
    
    
    get_next_newest_non_same_actor <- function(t, half_id, current_actor) {
      matched <- ed %>%
        filter(
          IdHalf == half_id,
          Time > t,
          IdActor1 != current_actor
        ) %>%
        arrange(Time)
      
      if (nrow(matched) == 0) return(NA)
      
      next_actor <- matched$IdActor1[1]
      next_event <- matched$Event[1]
      
      if (is.na(next_actor)) {
        return(next_event)
      } else {
        return(next_actor)
      }
    }
    
    
    Final1$NextNewestActor <- mapply(
      get_next_newest_non_same_actor,
      Final1$Time_median_group,
      Final1$IdHalf,
      Final1$IdActor_po
    )
    
    
    get_previous_non_closest_actor <- function(t, half_id, closest_actor) {
      matched <- ed %>%
        filter(IdHalf == half_id, Time < t, IdActor1 != closest_actor) %>%
        arrange(desc(Time))  
      
      if (nrow(matched) == 0) return(NA)
      
      prev_actor <- matched$IdActor1[1]
      prev_event <- matched$Event[1]
      
      if (is.na(prev_actor)) {
        return(prev_event)
      } else {
        return(prev_actor)
      }
    }
    
    Final1$PreviousActor <- mapply(
      get_previous_non_closest_actor,
      Final1$Time_median_group,
      Final1$IdHalf,
      Final1$IdActor_po
    )
    
    Final1$PreviousActor <- as.numeric(Final1$PreviousActor) 
    Final1$IdActor_po <- as.numeric(Final1$IdActor_po)
    
    get_previous_x <- function(t, half_id, closest_actor) {
      matched <- ed %>%
        filter(IdHalf == half_id, Time < t, IdActor1 != closest_actor) %>%
        arrange(desc(Time)) 
      
      if (nrow(matched) == 0) return(NA)
      
      prev_x <- matched$LocationX[1]
      return(prev_x)
      
    }
    
    Final1$PreviousX <- mapply(
      get_previous_x,
      Final1$Time_median_group,
      Final1$IdHalf,
      Final1$IdActor_po
    )
    
    get_previous_y <- function(t, half_id, closest_actor) {
      matched <- ed %>%
        filter(IdHalf == half_id, Time < t, IdActor1 != closest_actor) %>%
        arrange(desc(Time))  
      
      if (nrow(matched) == 0) return(NA)
      
      prev_y <- matched$LocationY[1]
      return(prev_y)
      
    }
    
    Final1$PreviousY <- mapply(
      get_previous_y,
      Final1$Time_median_group,
      Final1$IdHalf,
      Final1$IdActor_po
    )
    
    actor_team_lookup <- pd %>%
      select(IdActor, IdTeam) %>%
      distinct()
    
    Final1 <- Final1 %>%
      left_join(actor_team_lookup, by = c("PreviousActor" = "IdActor")) %>%
      rename(PrevActorTeam = IdTeam)
    
    Final1 <- Final1 %>%
      left_join(actor_team_lookup, by = c("IdActor_po" = "IdActor")) %>%
      rename(MedActorTeam = IdTeam)
    
    Final1 <- Final1 %>%
      left_join(actor_team_lookup, by = c("NextNewestActor" = "IdActor")) %>%
      rename(NextActorTeam = IdTeam)
    
    Final1 <- Final1 %>%
      mutate(
        PrevNowSameTeam = case_when(
          is.na(PreviousActor) | is.na(IdActor_po) ~ NA,
          PrevActorTeam == MedActorTeam ~ TRUE,
          PrevActorTeam != MedActorTeam ~ FALSE
        )
      )
    
    
    Final1 <- Final1 %>%
      mutate(
        NextNowSameTeam = case_when(
          is.na(PreviousActor) | is.na(IdActor_po) ~ NA,
          NextActorTeam == MedActorTeam ~ TRUE,
          NextActorTeam != MedActorTeam ~ FALSE
        )
      )
    
    ed <- ed %>%
      left_join(pd %>% select(IdActor, IdTeam), by = c("IdActor1" = "IdActor"))
    
    ed <- ed %>%
      rename(IdTeam = IdTeam.y) %>%
      select(-IdTeam.x)
    
    ed <- ed %>%
      arrange(IdHalf, Time) %>%
      group_by(IdHalf) %>%
      mutate(
        PrevTeam = lag(IdTeam),
        PossessionChange = ifelse(!is.na(PrevTeam) & IdTeam != PrevTeam, TRUE, FALSE)
      ) %>%
      ungroup()
    
    ed <- ed %>%
      select(Time, IdHalf, IdTeam, IdActor1, EventName, PossessionChange, everything())
    
    count_passes_excluding_pressing_player <- function(t, half_id, pressing_actor) {
      future_events <- ed %>%
        filter(IdHalf == half_id, Time > t) %>% 
        arrange(Time)
      
      if (nrow(future_events) == 0) return(0)
      
      initial_state <- ed %>%
        filter(IdHalf == half_id, Time == t) %>%
        pull(PossessionChange)
      
      if (length(initial_state) == 0 || is.na(initial_state)) initial_state <- FALSE
      
      flip_index <- which(future_events$PossessionChange != initial_state | is.na(future_events$PossessionChange))[1]
      
      if (is.na(flip_index)) {
        sub_events <- future_events
      } else {
        sub_events <- future_events[1:(flip_index - 1), ]
      }
      
      pass_count <- sum(sub_events$EventName == "Pass" & sub_events$IdActor1 != pressing_actor)
      
      return(pass_count)
    }
    
    Final1$PassCountExclPressingPlayer <- mapply(
      count_passes_excluding_pressing_player,
      Final1$Time_median_group,
      Final1$IdHalf,
      Final1$IdActor_po
    )
    
    library(stringr)
    
    termination_events <- c(
      "Out for throw-in", "Clearance", "Clearance uncontrolled", "Foul - Direct free-kick",
      "Foul - Indirect free-kick", "Off side", "Shot on target", "Shot not on target",
      "Goal", "Out for goal kick", "Out for corner", "End of Half", "Interruption"
    )
    
    get_actors_and_team_switches <- function(t, half_id, closest_actor) {
      future_events <- ed %>%
        filter(IdHalf == half_id, Time > t) %>%
        arrange(Time)
      
      if (nrow(future_events) == 0) return(c(NA, NA))
      
      if (future_events$EventName[1] %in% termination_events) {
        first_actor <- future_events$IdActor1[1]
        if (is.na(first_actor) || first_actor == closest_actor) {
          return(c(NA, future_events$EventName[1]))
        }
      }
      
      end_index <- which(future_events$EventName %in% termination_events)
      if (length(end_index) == 0) {
        sub_events <- future_events
        termination_event <- NA
      } else {
        termination_idx <- min(end_index)
        sub_events <- future_events[1:termination_idx, ]
        termination_event <- future_events$EventName[termination_idx]
      }
      
      if (nrow(sub_events) == 0) return(c(NA, NA))
      
      sub_events <- sub_events[sub_events$EventName == "Pass", ]
      
      if (nrow(sub_events) == 0) return(c(NA, termination_event))
      
      actors <- sub_events$IdActor1
      teams <- sub_events$IdTeam
      start_index <- which(actors != closest_actor)[1]
      if (is.na(start_index)) return(c(NA, termination_event))  
      
      actors <- actors[start_index:length(actors)]
      teams <- teams[start_index:length(teams)]
      
      initiator_team <- pd %>% filter(IdActor == closest_actor) %>% pull(IdTeam) %>% unique()
      
      team_changes <- ifelse(teams == initiator_team, "Same", "Different")
      
      actors_str <- paste(actors, collapse = ", ")
      teams_str <- paste(team_changes, collapse = ", ")
      
      if (!is.na(termination_event)) {
        teams_str <- paste0(teams_str, "; ", termination_event)
      }
      
      return(c(actors_str, teams_str))
    }
    
    results <- mapply(get_actors_and_team_switches,
                      Final1$Time_median_group,
                      Final1$IdHalf,
                      Final1$IdActor_po)
    
    Final1$TeamChangeStatus <- results[2, ] 
    
    check_foul_nearby <- function(t, half_id) {
      any(ed$IdHalf == half_id &
            ed$EventName == "Foul - Direct free-kick" &
            ed$Time >= (t - 2000) &
            ed$Time <= (t + 2000))
    }
    
    Final1$FoulWithin1Sec <- mapply(check_foul_nearby, Final1$Time_median_group, Final1$IdHalf)
    
    out_of_bounds_events <- c("Out for throw-in", "Out for goal kick", "Out for corner")
    
    Final1$OB <- Final1$TeamChangeStatus %in% out_of_bounds_events
    
    Final1$match_id <- rep(base_name, nrow(Final1))
    Final1$HomeTeam <- rep(pd$IdTeam[pd$Type == 'HomeTeam'][1], nrow(Final1))
    
    Final1 <- Final1 %>%
      select(match_id, HomeTeam, everything())
    
    Final1$goals_po = 0
    
    for (i in 1:nrow(Final1)) {
      
      if (Final1$IdHalf[i] == 1 && Final1$Type_po[i] == 'HomeTeam') {
        match = ed$ScoreHomeTeam[ed$Time == min(ed$Time[ed$Time > Final1$Time[i] & ed$IdHalf == 1])]
        if (length(match) >= 1) {
          Final1$goals_po[i] = match[1]
        } else {
          Final1$goals_po[i] = NA
        } 
        
      } else if (Final1$IdHalf[i] == 1 && Final1$Type_po[i] == 'AwayTeam') {
        match = ed$ScoreAwayTeam[ed$Time == min(ed$Time[ed$Time > Final1$Time[i] & ed$IdHalf == 1])]
        if (length(match) >=1 ) {
          Final1$goals_po[i] = match[1]
        } else {
          Final$goals_po[i] = NA
        }
        
      } else if (Final1$IdHalf[i] == 2 && Final1$Type_po[i] == 'AwayTeam') {
        match = ed$ScoreAwayTeam[ed$Time == min(ed$Time[ed$Time > Final1$Time[i] & ed$IdHalf == 2])]
        if (length(match) >= 1) {
          Final1$goals_po[i] = match[1]
        } else {
          Final1$goals_po[i] = NA
        }
        
      } else if (Final1$IdHalf[i] == 2 && Final1$Type_po[i] == 'HomeTeam') {
        match = ed$ScoreHomeTeam[ed$Time == min(ed$Time[ed$Time > Final1$Time[i] & ed$IdHalf == 2])]
        if (length(match) >= 1) {
          Final1$goals_po[i] = match[1]
        } else {
          Final1$goals_po[i] = NA
        }
        
      } else {
        Final1$goals_po[i] = NA
      }
    }
    
    Final1$goals_pr = 0
    
    for (i in 1:nrow(Final1)) {
      
      if (Final1$IdHalf[i] == 1 && Final1$Type_pr[i] == 'HomeTeam') {
        match = ed$ScoreHomeTeam[ed$Time == min(ed$Time[ed$Time > Final1$Time[i] & ed$IdHalf == 1])]
        if (length(match) >= 1) {
          Final1$goals_pr[i] = match[1]
        } else {
          Final1$goals_pr[i] = NA
        }
        
      } else if (Final1$IdHalf[i] == 1 && Final1$Type_pr[i] == 'AwayTeam') {
        match = ed$ScoreAwayTeam[ed$Time == min(ed$Time[ed$Time > Final1$Time[i] & ed$IdHalf == 1])]
        if (length(match) >= 1) {
          Final1$goals_pr[i] = match[1]
        } else {
          Final1$goals_pr[i] = NA
        }
        
      } else if (Final1$IdHalf[i] == 2 && Final1$Type_pr[i] == 'AwayTeam') {
        match = ed$ScoreAwayTeam[ed$Time == min(ed$Time[ed$Time > Final1$Time[i] & ed$IdHalf == 2])]
        if (length(match) >= 1) {
          Final1$goals_pr[i] = match[1]
        } else {
          Final1$goals_pr[i] = NA
        }
        
      } else if (Final1$IdHalf[i] == 2 && Final1$Type_pr[i] == 'HomeTeam') {
        match = ed$ScoreHomeTeam[ed$Time == min(ed$Time[ed$Time > Final1$Time[i] & ed$IdHalf == 2])]
        if (length(match) >= 1) {
          Final1$goals_pr[i] = match[1]
        } else {
          Final1$goals_pr[i] = NA
        }
        
      } else {
        Final1$goals_pr[i] = NA
      }
    }
    
    Final1 <- Final1 %>%
      mutate(
        distance_to_ball_pr = sqrt((X_player_pr - X_ball_po)^2 + (Y_player_pr - Y_ball_po)^2)
      )
    
    final1_name <- paste0("Final1_", base_name)
    output_path <- file.path(base_dir, paste0(final1_name, ".rds"))
    
    saveRDS(Final1, output_path)
    
    write.csv(Final1, sub("\\.rds$", ".csv", output_path), row.names = FALSE)
    
    Presses <- Final1 %>%
      arrange(IdHalf, Time) %>%
      group_by(IdHalf) %>%
      mutate(
        time_diff = c(Inf, diff(Time)),
        press_group = cumsum(time_diff > 300)
      ) %>%
      group_by(IdHalf, press_group) %>%
      filter(n() > 1) %>%   
      mutate( 
        max_opp = max(opponents_within_15_po, na.rm = TRUE),
        opponents_within_15_po = max_opp,
        
        max_t_up = max(teammates_under_pressure_po, na.rm = TRUE),
        teammates_under_pressure_po = max_t_up
      ) %>% 
      mutate(
        closest_idx = which.min(distance_to_ball_pr),
        closest_IdActor_pr = IdActor_pr[closest_idx],
        closest_Position_pr = Position_pr[closest_idx]
      ) %>% 
      mutate(
        IdActor_pr = if_else(row_number() == n(), closest_IdActor_pr, NA_real_),
        Position_pr = if_else(row_number() == n(), closest_Position_pr, NA_character_)
      )%>%
      slice_tail(n = 1) %>%  
      ungroup() %>%
      select(-time_diff, -press_group, -max_opp, -max_t_up, -closest_idx, -closest_IdActor_pr, -closest_Position_pr)
    
    Presses <- Presses %>%
      filter(!(ClosestEventToMedian %in% c("Foul - Direct free-kick", "Shot on target")))
    
    
    Presses <- Presses[
      Presses$opponents_within_15_po > 1 &
        (Presses$teammates_under_pressure_po > 0 |Presses$closest_two_opponents_po == TRUE),
    ]
    
    
    Presses = Presses[Presses$FoulWithin1Sec==FALSE,]
    
    mid_x <- 0
    mid_y <- 0
    
    ref_vec_x1 <- mid_x - Presses$X_ball_po
    ref_vec_y1 <- mid_y - Presses$Y_ball_po
    
    player_vec_x1 <- Presses$X_player_pr - Presses$X_ball_po
    player_vec_y1 <- Presses$Y_player_pr - Presses$Y_ball_po
    
    cross_prod1 <- ref_vec_x1 * player_vec_y1 - ref_vec_y1 * player_vec_x1
    
    dot_prod1 <- ref_vec_x1 * player_vec_x1 + ref_vec_y1 * player_vec_y1
    
    theta_rad1 <- atan2(cross_prod1, dot_prod1)
    
    theta_deg1 <- theta_rad1 * (180 / pi)
    
    is_within_200_mid <- abs(theta_deg1) <= 100
    
    Presses$theta_deg1 <- theta_deg1
    Presses$is_within_200_mid <- is_within_200_mid
    
    ref_vec_x2 <- Presses$PreviousX - Presses$X_ball_po
    ref_vec_y2 <- Presses$PreviousY - Presses$Y_ball_po
    
    player_vec_x2 <- Presses$X_player_pr - Presses$X_ball_po
    player_vec_y2 <- Presses$Y_player_pr - Presses$Y_ball_po
    
    cross_prod2 <- ref_vec_x2 * player_vec_y2 - ref_vec_y2 * player_vec_x2
    
    dot_prod2 <- ref_vec_x2 * player_vec_x2 + ref_vec_y2 * player_vec_y2
    
    theta_rad2 <- atan2(cross_prod2, dot_prod2)
    
    theta_deg2 <- theta_rad2 * (180 / pi)
    
    is_within_200 <- abs(theta_deg2) <= 100
    
    Presses$theta_deg2 <- theta_deg2
    Presses$is_within_200 <- is_within_200
    
    
    
    Presses <- Presses %>%
      filter(
        X_player_po >= -5250, X_player_po <= 5250,
        Y_player_po >= -3400, Y_player_po <= 3400
      )
    
    
    press_name <- paste0("Presses_", base_name)
    output_path <- file.path(base_dir, paste0(press_name, ".rds"))
    
    saveRDS(Presses, output_path)
    
    write.csv(Presses, sub("\\.rds$", ".csv", output_path), row.names = FALSE)
    
    cat("Processed", base_name, "- saved to", output_path, "\n")
  }
  
  cat("Processing complete!\n")
  
})

print(execution_time)