library(ggplot2)
data = read.csv("C:/Users/pavan/Desktop/SFU/Research/Datasets/9_26/Presses_final_tracking_W_2m_wo_event_9_26.csv")
dim(data)

# traditional presses
Ng_data = data[data$PrevNowSameTeam_press==TRUE,]
dim(Ng_data)

Ng_data_less_1.5 <- Ng_data[(Ng_data$condition_press == '<1' | Ng_data$condition_press == '1-1.5'), ]
Ng_data_1.5_2 <- Ng_data[Ng_data$condition_press == '1.5-2', ]
Ng_data_greater_2 <- Ng_data[(Ng_data$condition_press == '2-2.5' | Ng_data$condition_press == '2.5-3'), ]

press_summary <-  Ng_data_less_1.5%>% 
  mutate(
    Intensity = case_when(
      To_identify_pressers_po %in% c(2, 3) ~ "n = 2,3", 
      To_identify_pressers_po >= 4 ~ "n >= 4",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Intensity)) %>%
  group_by(Intensity) %>%
  summarise(
    number_of_presses = n(),
    percent_out_of_bounds = mean(OB_press == TRUE, na.rm = TRUE) * 100,
    percent_turnover = mean(NextNowSameTeam_press == FALSE, na.rm = TRUE) * 100,
    number_of_passes = sum(PassCountExclPressingPlayer_press, na.rm = TRUE),
    avg_passes_per_press = mean(PassCountExclPressingPlayer_press, na.rm = TRUE),
    number_out_of_bounds = sum(OB_press == TRUE, na.rm = TRUE),
    number_turnovers = sum(NextNowSameTeam_press == FALSE, na.rm = TRUE)
  ) %>%
  arrange(Intensity)

press_summary

press_summary <- Ng_data_less_1.5 %>%
  summarise(
    number_of_presses = n(),
    percent_out_of_bounds = mean(OB_press == TRUE, na.rm = TRUE) * 100,
    percent_turnover = mean(NextNowSameTeam_press == FALSE, na.rm = TRUE) * 100,
    number_of_passes = sum(PassCountExclPressingPlayer_press, na.rm = TRUE),
    avg_passes_per_press = mean(PassCountExclPressingPlayer_press, na.rm = TRUE),
    number_out_of_bounds = sum(OB_press == TRUE, na.rm = TRUE),
    number_turnovers = sum(NextNowSameTeam_press == FALSE, na.rm = TRUE)
  )  

press_summary

press_summary <- Ng_data_1.5_2 %>%
  mutate(
    Intensity = case_when(
      To_identify_pressers_po %in% c(2, 3) ~ "n = 2,3", 
      To_identify_pressers_po >= 4 ~ "n >= 4",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Intensity)) %>%
  group_by(Intensity) %>%
  summarise(
    number_of_presses = n(),
    percent_out_of_bounds = mean(OB_press == TRUE, na.rm = TRUE) * 100,
    percent_turnover = mean(NextNowSameTeam_press == FALSE, na.rm = TRUE) * 100,
    number_of_passes = sum(PassCountExclPressingPlayer_press, na.rm = TRUE),
    avg_passes_per_press = mean(PassCountExclPressingPlayer_press, na.rm = TRUE),
    number_out_of_bounds = sum(OB_press == TRUE, na.rm = TRUE),
    number_turnovers = sum(NextNowSameTeam_press == FALSE, na.rm = TRUE)
  ) %>%
  arrange(Intensity)

press_summary




press_summary <- Ng_data_1.5_2 %>%
  summarise(
    number_of_presses = n(),
    percent_out_of_bounds = mean(OB_press == TRUE, na.rm = TRUE) * 100,
    percent_turnover = mean(NextNowSameTeam_press == FALSE, na.rm = TRUE) * 100,
    number_of_passes = sum(PassCountExclPressingPlayer_press, na.rm = TRUE),
    avg_passes_per_press = mean(PassCountExclPressingPlayer_press, na.rm = TRUE),
    number_out_of_bounds = sum(OB_press == TRUE, na.rm = TRUE),
    number_turnovers = sum(NextNowSameTeam_press == FALSE, na.rm = TRUE)
  )  

press_summary

press_summary <- Ng_data_greater_2 %>%
  mutate(
    Intensity = case_when(
      To_identify_pressers_po %in% c(2, 3) ~ "n = 2,3", 
      To_identify_pressers_po >= 4 ~ "n >= 4",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Intensity)) %>%
  group_by(Intensity) %>%
  summarise(
    number_of_presses = n(),
    percent_out_of_bounds = mean(OB_press == TRUE, na.rm = TRUE) * 100,
    percent_turnover = mean(NextNowSameTeam_press == FALSE, na.rm = TRUE) * 100,
    number_of_passes = sum(PassCountExclPressingPlayer_press, na.rm = TRUE),
    avg_passes_per_press = mean(PassCountExclPressingPlayer_press, na.rm = TRUE),
    number_out_of_bounds = sum(OB_press == TRUE, na.rm = TRUE),
    number_turnovers = sum(NextNowSameTeam_press == FALSE, na.rm = TRUE)
  ) %>%
  arrange(Intensity)

press_summary




press_summary <- Ng_data_greater_2 %>%
  summarise(
    number_of_presses = n(),
    percent_out_of_bounds = mean(OB_press == TRUE, na.rm = TRUE) * 100,
    percent_turnover = mean(NextNowSameTeam_press == FALSE, na.rm = TRUE) * 100,
    number_of_passes = sum(PassCountExclPressingPlayer_press, na.rm = TRUE),
    avg_passes_per_press = mean(PassCountExclPressingPlayer_press, na.rm = TRUE),
    number_out_of_bounds = sum(OB_press == TRUE, na.rm = TRUE),
    number_turnovers = sum(NextNowSameTeam_press == FALSE, na.rm = TRUE)
  )  

press_summary

 

# EDA

team_abbr <- data.frame(
  IdTeam_pr_press = c(34090, 33478, 35118, 35091, 33765, 33101, 33483, 33754, 35371, 33767, 33481, 37240, 33768, 31992, 33769, 35372),  # Replace with actual team codes
  TeamAbbr = c("GET", "TEDA", "GRF", "DY", "CDL", "BSG", "SLT", "HJ", "SIPG", "JS", "SGS", "HCF", "S", "TT", "BR", "WZ")  # Replace with actual abbreviations
)
team_abbr

press_count <- Ng_data %>% 
  group_by(match_id, IdTeam_pr_press) %>%
  summarise(press_count = n(), .groups = "drop") %>%
  left_join(team_abbr, by = "IdTeam_pr_press")

 
p <- ggplot(press_count, aes(x = TeamAbbr, y = press_count)) +
  geom_boxplot(fill = "skyblue") +
  labs(
    x = "Team",
    y = "Number of Pressing Occasions per Game"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.title = element_text(size = 14),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    plot.background = element_rect(fill = "white", colour = "black", linewidth = 1)
  )

p

 
# ggsave(
#   filename = "C:/Users/pavan/Desktop/SFU/Research/Datasets/8_5/Figures/fig1.jpeg",
#   plot = p,
#   width = 10,
#   height = 6,
#   dpi = 300
# )



# Turnover_perc_team

 
press_efficiency <- Ng_data %>%
  filter(!is.na(NextNowSameTeam_press), !is.na(IdTeam_pr_press)) %>%
  group_by(IdTeam_pr_press) %>%
  summarise(
    total_presses = n(),
    turnovers = sum(NextNowSameTeam_press == FALSE, na.rm = TRUE),
    efficiency = turnovers / total_presses,
    .groups = "drop"
  ) %>%
  left_join(team_abbr, by = "IdTeam_pr_press")

press_efficiency <- press_efficiency %>%
  arrange(TeamAbbr) %>%
  mutate(TeamAbbr = factor(TeamAbbr, levels = TeamAbbr))
 
p <- ggplot(press_efficiency, aes(x = TeamAbbr, y = efficiency)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = scales::percent(efficiency, accuracy = 0.1)), 
            vjust = -0.5, size = 3.5) +
  labs(
    x = "Team",
    y = "Turnover Percentage"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.title = element_text(size = 14),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    plot.background = element_rect(fill = "white", colour = "black", linewidth = 1)
  ) +
  coord_cartesian(ylim = c(0, 1))

p

 
# ggsave(
#   filename = "C:/Users/pavan/Desktop/SFU/Research/Datasets/8_5/Figures/fig2-revision.jpeg",
#   plot = p,
#   width = 10,
#   height = 6,
#   dpi = 300
# )


# pressing_time_histogram_all
  
Ng_data <- Ng_data %>%
  mutate(
    Time_continuous = case_when(
      IdHalf == 1 ~ Time,
      IdHalf == 2 ~ Time + 45 * 60000,
      TRUE ~ NA_real_
    ) / 60000
  )

 
filtered_data <- Ng_data %>%
  filter(Time_continuous >= 0, Time_continuous <= 90)

 
breaks_vec <- seq(0, 90, by = 5)

 
bin_labels <- paste0(
  breaks_vec[-length(breaks_vec)], "–", breaks_vec[-1]
)

 
bin_midpoints <- head(breaks_vec, -1) + diff(breaks_vec) / 2

 
p <- ggplot(filtered_data, aes(x = Time_continuous)) +
  geom_histogram(
    breaks = breaks_vec,
    color = "black",
    fill = "skyblue",
    closed = "right"
  ) +
  scale_x_continuous(
    breaks = bin_midpoints,
    labels = bin_labels,
    limits = c(0, 90)
  ) +
  labs(
    x = "Time Interval (minutes)",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.title = element_text(size = 14),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    plot.background = element_rect(fill = "white", colour = "black", linewidth = 1)
  )

p

 
# ggsave(
#   filename = "C:/Users/pavan/Desktop/SFU/Research/Datasets/8_5/Figures/fig3-revision.jpeg",
#   plot = p,
#   width = 10,
#   height = 6,
#   dpi = 300
# )

press_hist_data <- Ng_data %>%
  filter(
    !is.na(goals_pr_press),
    !is.na(goals_po_press),
    !is.na(Time),
    !is.na(IdHalf)
  ) %>%
  mutate(
    goal_diff = goals_pr_press - goals_po_press,
    goal_diff_group = case_when(
      goal_diff > 0 ~ "Leading",
      goal_diff == 0 ~ "Tied",
      goal_diff < 0 ~ "Trailing"
    ),
    goal_diff_group = factor(
      goal_diff_group,
      levels = c("Leading", "Tied", "Trailing")
    ),
    Time_minutes = Time / 60000  # keep halves separate, no gap
  )

library(dplyr)
library(tidyr)
library(ggplot2)
 
press_hist_binned_avg <- press_hist_data %>%
  mutate(
    Time_bin = floor(Time_minutes / 5) * 5,
    Match_Time_bin = ifelse(IdHalf == 1, Time_bin, Time_bin + 45)
  ) %>%
  group_by(match_id, IdHalf, Match_Time_bin, goal_diff_group) %>%
  summarise(press_count = n(), .groups = "drop") %>%
  complete(
    match_id,
    IdHalf,
    Match_Time_bin = seq(0, 85, by = 5),
    goal_diff_group,
    fill = list(press_count = 0)
  ) %>%
  group_by(Match_Time_bin, goal_diff_group) %>%
  summarise(avg_press_count = mean(press_count), .groups = "drop") %>%
  mutate(
    Time_interval = factor(
      Match_Time_bin,
      levels = seq(0, 85, by = 5),
      labels = c(
        "0-5", "5-10", "10-15", "15-20", "20-25",
        "25-30", "30-35", "35-40", "40-45",
        "45-50", "50-55", "55-60", "60-65", "65-70",
        "70-75", "75-80", "80-85", "85-90"
      )
    )
  )%>%
  filter(!is.na(Time_interval))
 
press_first_half <- press_hist_binned_avg %>%
  filter(Match_Time_bin <= 40)

plot_first_half <- ggplot(
  press_first_half,
  aes(x = Time_interval, y = avg_press_count, color = goal_diff_group, group = goal_diff_group)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_brewer(palette = "Set1", name = "Goal Differential") +
  labs(
    title = "First Half",
    x = "Minutes in Match",
    y = "Average Number of Presses"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12), 
    plot.background = element_rect(fill = "white", colour = "black", linewidth = 1),
    legend.position = "bottom"
  )

 
press_second_half <- press_hist_binned_avg %>%
  filter(Match_Time_bin >= 45)

plot_second_half <- ggplot(
  press_second_half,
  aes(x = Time_interval, y = avg_press_count, color = goal_diff_group, group = goal_diff_group)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_brewer(palette = "Set1", name = "Goal Differential") +
  labs(
    title = "Second Half",
    x = "Minutes in Match",
    y = "Average Number of Presses"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12), 
    plot.background = element_rect(fill = "white", colour = "black", linewidth = 1),
    legend.position = "bottom"
  )

 
print(plot_first_half)
print(plot_second_half)

library(cowplot)
library(grid)
library(patchwork)

plot_first_half <- plot_first_half + 
  theme(
    panel.border = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    axis.text.y = element_text(),
    axis.title.y = element_text(angle = 90),    
    axis.title.x = element_blank()      
  )

plot_second_half <- plot_second_half + 
  theme(
    panel.border = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank()
  )

 
library(ggplot2)
library(patchwork)
library(cowplot)
library(grid)

 
combined_no_legend <- (plot_first_half + plot_second_half) +
  plot_layout(ncol = 2, guides = "collect") +
  plot_annotation(
    theme = theme(
      legend.position = "none",
      axis.title.x = element_blank()
    )
  )

 
combined_with_legend <- (plot_first_half + plot_second_half) +
  plot_layout(ncol = 2, guides = "collect") +
  plot_annotation(
    theme = theme(
      legend.position = "bottom",
      axis.title.x = element_blank()
    )
  )

 
legend <- get_legend(combined_with_legend)

 
label_grob <- textGrob(
  "Minutes in Half",
  gp = gpar(fontsize = 14)
)

 
stacked_plot <- plot_grid(
  combined_no_legend,
  label_grob,
  legend,
  ncol = 1,
  rel_heights = c(1, 0.05, 0.12)
)
 
final_plot <- ggdraw(stacked_plot) +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
    plot.background = element_rect(fill = "white", colour = "black", linewidth = 1)
  )

 
print(final_plot)
 
 
# ggsave(
#   filename = "C:/Users/pavan/Desktop/SFU/Research/Datasets/8_5/Figures/fig4_new.jpeg",
#   plot = final_plot,
#   width = 18,   
#   height = 7,
#   dpi = 300
# )

# Fig 5

Ng_data$X_player_pr <- Ng_data$X_player_pr / 100
Ng_data$Y_player_pr <- Ng_data$Y_player_pr / 100

Ng_data$X_player_po <- Ng_data$X_player_po / 100
Ng_data$Y_player_po <- Ng_data$Y_player_po / 100

Ng_data$NextNewestActorEvent_X = Ng_data$NextNewestActorEvent_X/100
Ng_data$NextNewestActorEvent_Y = Ng_data$NextNewestActorEvent_Y/100

library(ggplot2)
library(dplyr)
library(MASS)  
 
field_length <- 105
field_width <- 68
goal_width <- 7.32
third_length <- field_length / 3 

 
circle <- data.frame(
  x = 9.15 * cos(seq(0, 2 * pi, length.out = 200)),
  y = 9.15 * sin(seq(0, 2 * pi, length.out = 200))
)

 
smooth_turnover_data <- Ng_data %>%
  filter(!is.na(NextNewestActorEvent_X), !is.na(NextNewestActorEvent_Y)) %>%
  filter(NextNowSameTeam == FALSE)  # turnovers only

 
home_def_final <- smooth_turnover_data %>%
  filter(Type_po == "HomeTeam", NextNewestActorEvent_X < (-field_length / 2 + third_length))

away_def_final <- smooth_turnover_data %>%
  filter(Type_po == "AwayTeam", NextNewestActorEvent_X > (field_length / 2 - third_length)) %>%
  mutate( 
    NextNewestActorEvent_X = -NextNewestActorEvent_X,
    NextNewestActorEvent_Y = -NextNewestActorEvent_Y
  )

final_third_data <- bind_rows(home_def_final, away_def_final)

 
hx <- bandwidth.nrd(final_third_data$NextNewestActorEvent_X)
hy <- bandwidth.nrd(final_third_data$NextNewestActorEvent_Y)

dens <- with(final_third_data, kde2d(
  x = NextNewestActorEvent_X,
  y = NextNewestActorEvent_Y,
  n = 300,
  lims = c(-field_length / 2, -field_length / 2 + third_length,
           -field_width / 2, field_width / 2),
  h = c(hx, hy)
))

dens_df <- expand.grid(x = dens$x, y = dens$y)
dens_df$density <- as.vector(dens$z)
dens_df$density_norm <- dens_df$density / max(dens_df$density)

 
ggplot() + 
  geom_raster(data = dens_df, aes(x = x, y = y, fill = density_norm), interpolate = TRUE) +
  scale_fill_gradient(low = "lightyellow", high = "darkred", name = "Turnover Density") +
   
  geom_rect(aes(xmin = -field_length/2, xmax = 0,
                ymin = -field_width/2, ymax = field_width/2),
            fill = NA, color = "black") +
   
  geom_rect(aes(xmin = -field_length / 2, xmax = -field_length / 2 + 16.5,
                ymin = -20.16, ymax = 20.16),
            fill = NA, color = "black") +
   
  geom_rect(aes(xmin = -field_length / 2, xmax = -field_length / 2 + 5.5,
                ymin = -9.16, ymax = 9.16),
            fill = NA, color = "black") +
   
  geom_segment(aes(x = -field_length / 2, xend = -field_length / 2,
                   y = -goal_width / 2, yend = goal_width / 2),
               color = "black") +
   
  geom_segment(aes(x = 0, xend = 0, y = -field_width/2, yend = field_width/2),
               color = "black") +
   
  geom_path(data = circle %>% filter(x <= 0),
            aes(x = x, y = y), color = "black") +
   
  annotate("point", x = -field_length / 2 + 11, y = 0, color = "black") +
   
  coord_fixed(ratio = 1,
              xlim = c(-field_length/2, 0),
              ylim = c(-field_width/2, field_width/2)) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 14),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    plot.background = element_rect(fill = "white", colour = "black", linewidth = 1)
  )
 
# ggsave(
#   filename = "C:/Users/pavan/Desktop/SFU/Research/Datasets/8_5/Figures/fig5.jpeg",
#   width = 10,
#   height = 7,
#   dpi = 300
# )

library(stringr)
Ng_data <- Ng_data %>%
  mutate(first_region = str_split(collapsed_region_sequence_nolr, ",") %>% 
           sapply(function(x) x[1]))

table(Ng_data$first_region)

 