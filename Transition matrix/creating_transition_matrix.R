library(dplyr)
library(tidyr) 

all_presses =  read.csv("U:/9_26/Presses_final_tracking_W_2m_wo_event_9_26.csv")
seqs <- gsub("\\s*,\\s*", ",", all_presses$collapsed_region_sequence_nolr )


get_transitions <- function(seq_string) {
  regions <- unlist(strsplit(seq_string, ","))
  if(length(regions) < 2) return(NULL)
  data.frame(from = regions[-length(regions)], to = regions[-1])
}

all_transitions <- bind_rows(lapply(seqs, get_transitions))
transition_counts <- table(all_transitions$from, all_transitions$to)

transition_df <- as.data.frame.matrix(transition_counts) %>%
  mutate(region = rownames(.))

absorbing_counts <- all_presses %>%
  group_by(output, absorbing_status) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = absorbing_status, values_from = n, values_fill = 0) %>%
  rename(region = output)

final_table <- transition_df %>%
  left_join(absorbing_counts, by = "region") %>%
  select(region, everything())

 
for (col in c("TO","OB","BP")) if (!(col %in% colnames(final_table))) final_table[[col]] <- 0
 
final_table <- final_table[, c("region", setdiff(colnames(final_table), "region"))]

 
total_TO <- sum(final_table$TO, na.rm = TRUE)
total_OB <- sum(final_table$OB, na.rm = TRUE)
total_BP <- sum(final_table$BP, na.rm = TRUE)

new_rows <- as.data.frame(matrix(0, nrow=3, ncol=ncol(final_table)))
colnames(new_rows) <- colnames(final_table)
new_rows$region <- c("TO","OB","BP")
new_rows[1,"TO"] <- total_TO
new_rows[2,"OB"] <- total_OB
new_rows[3,"BP"] <- total_BP

final_table <- bind_rows(final_table, new_rows)

final_table
 
write.csv(final_table, "U:/9_26/transition_table_w_2m_wo_events_updated.csv", row.names = FALSE)
